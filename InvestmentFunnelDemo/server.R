shinyServer(function(input, output) {

  #######################################  Menu #######################################

  output$plotAssetData <- renderPlot({
    tempAssetData <- na.omit(sqlQuery(paste0("SELECT date AS Date, adjusted_close AS Price
                                             FROM historicaldata WHERE symbol = '", input$assetSelection, "'")))

    tempAssetData$Date <- as.Date(tempAssetData$Date)
    ggplot(tempAssetData, aes(y = Price, x = Date)) +
      geom_line()
  })

  output$tableAssetData <- renderTable({
    assetInfo= na.omit(sqlQuery(paste0("SELECT m.fund, m.symbol, m.issuer, m.segment, m.expenseRatio, m.category, m.underlyingIndex, p.priceTr1Mo
                                       FROM metadata m
                                       INNER JOIN performancemeasures p ON p.symbol = m.symbol
                                       WHERE m.symbol = '", input$assetSelection, "'"))
    )
  })

  ####################################### ASSET SELECTION - Screeing #######################################

  dataSelection <- reactiveVal()
  dataSelection(assets) # initialize

  observeEvent({input$assetClasses
    input$assetRegion
    input$yearsOfExistence
    input$oneYearP
    input$threeYearP
    input$leveregedEtfs
    input$shortEtfs
    input$EGSEtfs
  },
  {

    switch(input$EGSEtfs,
           "FALSE" = {
             newValueTest <- na.omit(sqlQuery(paste0("SELECT m.symbol AS ticker FROM metadata m
                                                     INNER JOIN performancemeasures p ON m.symbol = p.symbol
                                                     WHERE m.launchDate BETWEEN CURDATE() - INTERVAL ", input$yearsOfExistence[2],  " YEAR
                                                     AND CURDATE() - INTERVAL ", input$yearsOfExistence[1], " YEAR AND
                                                     m.assetClass IN ('", paste(input$assetClasses,  collapse = "', '"), "') AND
                                                     m.region IN ('", paste(input$assetRegion,  collapse = "', '"), "') AND
                                                     p.priceTr1Yr > ", input$oneYearP[1]/100, " AND
                                                     p.priceTr3YrAnnualized > ", input$threeYearP[1]/100, " AND
                                                     m.leveraged != '", input$leveregedEtfs, "' AND
                                                     m.inverse != '", input$shortEtfs, "'")))

           },
           "TRUE" = {newValueTest <- na.omit(sqlQuery(paste0("SELECT m.symbol AS ticker FROM metadata m INNER JOIN performancemeasures p ON m.symbol = p.symbol WHERE m.launchDate BETWEEN CURDATE() - INTERVAL ", input$yearsOfExistence[2],
                                                             " YEAR AND CURDATE() - INTERVAL ", input$yearsOfExistence[1],
                                                             " YEAR AND m.symbol IN ('", paste0(ESGetf, collapse = "', '"), "')")))
           }
             )
    newValueTest1 <- data.frame(ticker = setdiff(newValueTest$ticker, c('ADRD', 'ADRE', 'ADRA', 'ADRU')))
    dataSelection(newValueTest1)
           }
  )

  ####################################### Status Plots #######################################

  observeEvent({input$assetClasses
    input$assetRegion
    input$yearsOfExistence
    input$oneYearP
    input$threeYearP
  },
  {
    output$plotStatus <- renderPlot({
      Category <- c("Data", "Screeing", "Clustering", "Optimization")
      Percent <- c(length(assets), length(dataSelection()$ticker), input$numberOfClusters, input$numberInPortfolio)
      circBarPlot(Percent, Category)
    })

    output$plotStatus1 <- renderPlot({
      Category <- c("Data", "Screeing", "Clustering", "Optimization")
      Percent <- c(length(assets), length(dataSelection()$ticker), input$numberOfClusters, input$numberInPortfolio)
      circBarPlot(Percent, Category)
    })

    output$plotStatus2 <- renderPlot({
      Category <- c("Data", "Screeing", "Clustering", "Optimization")
      Percent <- c(length(assets), length(dataSelection()$ticker), input$numberOfClusters, input$numberInPortfolio)
      circBarPlot(Percent, Category)
    })

    output$plotStatus3 <- renderPlot({
      Category <- c("Data", "Screeing", "Clustering", "Optimization")
      Percent <- c(length(assets), length(dataSelection()$ticker), input$numberOfClusters, input$numberInPortfolio)
      circBarPlot(Percent, Category)
    })
  })

  ####################################### Screeing #######################################




  ####################################### Clustering  #######################################


  clustering <- reactiveValues(clustering = NULL)

  observeEvent(input$generateClustering, {
    #print('Begin Clustering')
    withProgress(message = 'Performing Clustering', value = 0.02, {
    clustering$AssetPrices <- na.omit(sqlQuery(paste0("SELECT date AS Date, symbol, adjusted_close AS Price FROM historicaldata WHERE symbol IN ('",
                                                      paste0(dataSelection()$ticker, collapse = "', '"), "') AND
                                                      Date < CURDATE() - INTERVAL ", input$backtestYears ," YEAR"))) %>% spread(symbol, Price)

    ### Could dump AssetPrices into AssetReturns Right Away
    row.names(clustering$AssetPrices) <- clustering$AssetPrices[, 1]
    clustering$AssetReturns <- apply(clustering$AssetPrices[,-1], 2, returnsCalc)
    clustering$AssetReturns <- clustering$AssetReturns[complete.cases(clustering$AssetReturns), ]

    # Create a Progress object
    #progress <- shiny::Progress$new(style = 'notification' , min = 0, max = 10)
     #progress$set(message = "Performing Clustering",
      #            detail = "This may take a while...",
     #             value = 5)

    # Close the progress when this reactive exits (even if there's an error)
    #on.exit(progress$close())

    clustering$c <- switch(input$corMethod,
                           pearson = cor(clustering$AssetReturns, method = 'pearson'),
                           spearman = cor(clustering$AssetReturns, method = 'spearman'),
                           kendall = cor(clustering$AssetReturns, method = 'kendall')
    )

    clustering$d <- switch(input$distMetric,
                           A = as.dist(1-abs(clustering$c))^2,
                           B = as.dist(1-clustering$c)^2,
                           C = as.dist(abs(1-clustering$c))^2
    )

    clustering$hc <- switch(input$linkage,
                            single = hclust(clustering$d, method = "single"),
                            complete = hclust(clustering$d, method = "complete"),
                            centroid = hclust(clustering$d, method = "centroid"),
                            median = hclust(clustering$d, method = "median"),
                            wardD = hclust(clustering$d, method = "ward.D"),
                            wardD2 = hclust(clustering$d, method = "ward.D2")
    )
    incProgress(amount = 0.9)
    Sys.sleep(1)
    #print('--Clustering Nearly Done--')

    clustering$Cno <- input$numberOfClusters
    clustering$memb <- cutree(clustering$hc, k = clustering$Cno)


    ### HERE NEED TO ADD STATISTICS WITH SWITCH

    clustering$statsAll <- data.frame(ETF = names(clustering$memb), Cluster = as.factor(clustering$memb),
                                      Return = apply(clustering$AssetReturns[,names(clustering$memb)], 2, geomAveCalc), Std = apply(clustering$AssetReturns[,names(clustering$memb)], 2, sd),
                                      SR = apply(clustering$AssetReturns[,names(clustering$memb)], 2, sharpeRatioCalc) )

    clustering$Gselect <- rep(0,clustering$Cno)  # storage
    clustering$time <- rownames(data) # time parameters
    clustering$clustCount <- rep(0,clustering$Cno)


    for(i in 1:clustering$Cno){
      clustering$Gnames <- names(which(clustering$memb == i))   # the names of the assets in clusters i
      clustering$criteria <- rep(0,length(clustering$Gnames))   # storage for the selection criteria
      clustering$clustCount[i] <- length(clustering$Gnames)
      for(j in 1:length(clustering$Gnames)){

        clustering$criteria[j] <- switch(input$selectionCriteria,
                                         highestReturn = geomAveCalc(clustering$AssetReturns[,clustering$Gnames[j]]),
                                         minimumStd  =  sd(clustering$AssetReturns[,clustering$Gnames[j]]),
                                         highestSharpe = sharpeRatioCalc(clustering$AssetReturns[,clustering$Gnames[j]]
                                         #mostRepresentive = dist(clustering$c[,clustering$Gnames])^2
                                         )
        )

      }
      clustering$Gselect[i] <- switch(input$selectionCriteria,
                                      highestReturn = clustering$Gnames[which(max(clustering$criteria) == clustering$criteria)],
                                      minimumStd =  clustering$Gnames[which(min(clustering$criteria) == clustering$criteria)],
                                      highestSharpe =  clustering$Gnames[which(max(clustering$criteria) == clustering$criteria)]
                                      #mostRepresentive =  clustering$Gnames[which(max(clustering$criteria) == clustering$criteria)]
                                      )
    }
    setProgress(5)

    clustering$ClustSize <- data.frame(Cluster = sprintf("Clst%d", 1:clustering$Cno), Size = clustering$clustCount)
    clustering$statsClust <- clustering$statsAll[which(clustering$statsAll$ETF %in% clustering$Gselect),]

    #print('--Clustering Done--')
    incProgress(amount = 1, detail = paste("Clustering Done"))
    Sys.sleep(2)})

  })

  observeEvent(input$reset, {
    clustering$c <- NULL
    clustering$d <- NULL
    clustering$hc <- NULL
    clustering$Gselect <- NULL
    clustering$Gnames <- NULL
    clustering$Criteria <- NULL
    clustering$memb <- NULL
    clustering$time <- NULL
  })


  #####################################  OPTIMIZATION  #########################################

  #### DYNAMIC UI INPUT #####
  output$ui <- renderUI({
    # Depending on input$modelChoices, we'll generate a different
    # UI component and send it to the client.
    switch(input$modelChoices,
           "MeanVar" = sliderInput("dynamic", "Gamma (Risk aversion parameter. Gamma = 100% is risk-averse investor) ",
                                   min = 0, max = 100, value = 50, post="%"),
           "VaR" = radioButtons("dynamic2", "Confidence Level",
                                choices = c("90%" = "option1",
                                            "95%" = "option2",
                                            "99%" = "option3"),
                                selected = "option2"
           )#,
           #"CVaR" =  radioButtons("dynamic", "Confidence Level",
            #                      choices = c("90%" = "option1",
              #                                "95%" = "option2",
                 #                             "99%" = "option3"),
                     #             selected = "option2"
           #),

          # "EWS" = checkboxInput("dynamic", "Dynamic",
                        #         value = TRUE)
    )
  })

  ## Result Data from clustering

  clusterResultPrice <- reactiveVal()
  clusterResultReturns <- reactiveVal()
  clusterResultMeta <- reactiveVal()


  observeEvent(input$generateClustering,{
    newClusterPrice <- clustering$AssetPrices[,clustering$Gselect]
    newClusterReturns <- clustering$AssetReturns[,clustering$Gselect]
    newClusterMeta <- na.omit(sqlQuery(paste0("SELECT symbol, assetClass AS 'AssetClass', region AS Region, geography AS Geography, focus AS Focus
                                              FROM metadata WHERE symbol IN ('", paste0(clustering$statsClust$ETF, collapse = "', '"), "')" ) ))
    clusterResultPrice(newClusterPrice)
    clusterResultReturns(newClusterReturns)
    clusterResultMeta(newClusterMeta)

  })

  ## Optimization in R

  optimizeVal <- eventReactive(input$optimizeButton, {
    #print('Begin Optimization')
    withProgress(message = 'Optimizing', value = 0.03, {

    # Asset Names
    outputAssets <- data.frame(Assets = clustering$Gselect)
    attr(outputAssets, "symName") <-  "Asset"
    attr(outputAssets, "ts") <-  "Set of Assets"

    # Dates
    outputDates <-  data.frame(Date = rownames(clusterResultReturns()))
    attr(outputDates, "symName") <- "Date"
    attr(outputDates, "ts") <- "Set of Dates"

    # # Asset Returns
    outputAssetReturns <- cbind(Date = rownames(clusterResultReturns()), clusterResultReturns()) %>%
      as.data.frame() %>% gather(Stock, Returns, -Date)
    outputAssetReturns$Date <- factor(outputAssetReturns$Date)
    outputAssetReturns$Stock <- factor(outputAssetReturns$Stock)
    outputAssetReturns$Returns <- as.numeric(outputAssetReturns$Returns)
    attr(outputAssetReturns, "symName") <- "AssetReturns"
    attr(outputAssetReturns, "domains") <- "c(t, i)"
    attr(outputAssetReturns, "ts") <- "Assetreturns of each asset and each day"
    print(str(outputAssetReturns))
    #
    # # Expected Returns
    tempExpRet <- clusterResultReturns() %>% apply(2, geomAveCalc)
    outputExpRet <- data.frame(Asset = names(tempExpRet),ExpectedReturns = tempExpRet, row.names=NULL)
    rm(tempExpRet)
    attr(outputExpRet, "symName") <-  'ExpectedReturns'
    attr(outputExpRet, "domains") <- "i"
    attr(outputExpRet, "ts") <- "Expected Returns for each asset"

    # VarCovariance Matrix
    tempVarCov <- cov(clusterResultReturns())
    outputVarCovMat <- data.frame(cbind(i=rownames(tempVarCov), tempVarCov), row.names = NULL) %>%
      gather(j,value, -i)
    #rm(tempVarCov)
    outputVarCovMat$j <- factor(outputVarCovMat$j)
    outputVarCovMat$value <- as.numeric(outputVarCovMat$value)
    attr(outputVarCovMat, "symName") <-  "VarCov"
    attr(outputVarCovMat, "domains") <-  "c(i, j)"
    attr(outputVarCovMat, "ts") <-  "Variance-Covariance matrix"

    # # Set GAMS directory
    # igdx("/Applications/GAMS24.8/sysdir")
    #
    # # Save GDX-file to GDXfiles folder
    # wgdx.lst("/Users/apple/Dropbox/InvestmentFunnel/Developercopy/GAMSandGDXfiles/outputMeanVariance.gdx",
    #          outputAssets, outputDates, outputAssetReturns, outputExpRet, outputVarCovMat, squeeze = FALSE)
    #
    # # Run Markovitch GAMS model with GAMS
    # setwd("/Users/apple/Dropbox/InvestmentFunnel/Developercopy/GAMSandGDXfiles/")
    #
    # gams("Markovitch.gms")
    # gams("VaR_CVaR.gms")
    #
    #
    # # Read results from GAMS model (.gdx file)
    # #6. Read results from GAMS Model (GDX-file)
    # resultAllocation <- rgdx.param(gdxName = "/Users/apple/Dropbox/InvestmentFunnel/Developercopy/GAMSandGDXfiles/resultsMarkovitch.gdx",
    #                                symName = 'RunningAllocation', squeeze = FALSE) %>%
    #   spread(i,RunningAllocation,fill=0) # %>% t()
    # print(class(resultAllocation))
    # resultPortReturn <- rgdx.param(gdxName = "/Users/apple/Dropbox/InvestmentFunnel/Developercopy/GAMSandGDXfiles/resultsMarkovitch.gdx",
    #                                symName = 'RunningReturn')
    #
    # resultPortVariance <- rgdx.param(gdxName = "/Users/apple/Dropbox/InvestmentFunnel/Developercopy/GAMSandGDXfiles/resultsMarkovitch.gdx",
    #                                  symName = 'RunningVariance', names = c('Portfolio','Variance'))
    #
    # VaR_resultAllocation <- rgdx.param(gdxName = "/Users/apple/Dropbox/InvestmentFunnel/Developercopy/GAMSandGDXfiles/resultsVaR_CVaR.gdx",
    #                                   symName = 'VaR_x', squeeze = FALSE)
    #
    # CVaR_resultAllocation <- rgdx.param(gdxName = "/Users/apple/Dropbox/InvestmentFunnel/Developercopy/GAMSandGDXfiles/resultsVaR_CVaR.gdx",
    #                                    symName = 'CVaR_x', squeeze = FALSE)



    switch(input$modelChoices,
           "MeanVar" = {
             #  Markowitz Mean-Var Portfolio Optimization ---------------------------------------

             n=length(clustering$Gselect)
             mu = outputExpRet$ExpectedReturns
             Sigma = tempVarCov
             gamma = input$dynamic

             w <- Variable(n)
             ret <- t(mu) %*% w
             risk <- quad_form(w, Sigma)
             obj <- (1- gamma)*ret - gamma * risk
             constr <- list(w >= 0, sum(w) == 1)
             #constr <- list(p_norm(w,1) <= Lmax, sum(w) == 1) #allow shorting
             prob <- Problem(Maximize(obj), constr)
             result <- solve(prob)
             result$getValue(risk)
             result$getValue(ret)

             MeanVaR_resultAllocation=data.frame(i = clustering$Gselect, EW_x = result$getValue(w))

             resultMarkovitch <- list(MeanVaR_Allocation = MeanVaR_resultAllocation)
             # ---------------------------------------
           },
           "EW" = {


             EW_resultAllocation <- data.frame(i = clustering$Gselect, EW_x = rep(1/length(clustering$Gselect),length(clustering$Gselect)))

             resultMarkovitch <- list(EW_Allocation = EW_resultAllocation)

             #print('Optimization Done')

             resultMarkovitch
             #MeanVaR_resultAllocation
           })
    incProgress(amount = 1,detail = paste("Optimization done"))
    Sys.sleep(1) })
  })

  observeEvent(input$optimizeButton,
               {
                 print(optimizeVal())
                 # print(optimizeVal()$CVaR_Allocation)
                 # print(optimizeVal()$VaR_Allocation$i)
                 # print(optimizeVal()[[3]]$Variance)
                 # print(optimizeVal()[[2]]$RunningReturn)
                 #
                 # #print(initialPortVal)
                 # print(names(optimizeVal()[[1]][1,] ))
                 # print(names(colnames(as.data.frame(clusterResultReturns()))))
                 # print(colnames(clusterResultReturns()[ ,c(1,3)]))
                 # print(clusterResultReturns()[ ,match(names(optimizeVal()[[1]][1,])[-1], colnames(clusterResultReturns()))])
               })



  ####################################### RESULTS #######################################


  #### CLUSTERING RESULTS ####
  output$plotResultAssets <- renderPlot({
    input$generateClustering
    if (is.null(clustering$hc)) {
      return()
    }
    plotData <- clustering$AssetPrices[, c(clustering$Gselect,'Date')]
    plotData <- plotData[complete.cases(plotData), ]

    plotData$Date <- as.Date(plotData$Date)

    assetPlotData <- gather(plotData, key = "Asset", value = "Price", -Date)

    ggplot(assetPlotData,
           aes(x=Date,
               y=Price,
               color=Asset)) +
      geom_line()

  })

  output$plotClusterPieChart <- renderPlot({
    input$generateClustering
    if (is.null(clustering$hc)) {
      return()
    }
    bp <- ggplot(clustering$ClustSize, aes(x="", y=Size, fill=Cluster)) +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Blues") +
      theme_minimal() + ggtitle("Size of Clusters") +
      theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(),
            axis.title.y=element_blank())
    bp
  })

  output$tableClustering <- renderTable({
    if (is.null(optimizeVal)) {
      return()
    }

    df <- clustering$statsClust[order(clustering$statsClust$Cluster), ] %>%
      merge(clusterResultMeta(), by.x = 'ETF', by.y = 'symbol')
    df
  })

  output$plotClusteringAssetClass <- renderPlot({
    input$generateClustering
    if (is.null(clustering$c)) {
      return()
    }
    assetClass <- as.data.frame(table(clusterResultMeta()$AssetClass))

    bp1 <- ggplot(assetClass, aes(x="", y=Freq, fill=Var1)) +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Blues") +
      theme_minimal() + ggtitle("Asset Classes") +
      theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(),
            axis.title.y=element_blank())

    bp1


  })

  output$plotClusteringRegion <- renderPlot({
    input$generateClustering
    if (is.null(clustering$c)) {
      return()
    }
    region <- as.data.frame(table(clusterResultMeta()$Region))

    bp2 <- ggplot(region, aes(x="", y=Freq, fill=Var1)) +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Blues") +
      theme_minimal() + ggtitle("Regions") +
      theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(),
            axis.title.y=element_blank())

    bp2

  })

  output$plotClusteringGeography <- renderPlot({
    input$generateClustering
    if (is.null(clustering$c)) {
      return()
    }

    geography <- as.data.frame(table(clusterResultMeta()$Geography))

    bp3 <- ggplot(geography, aes(x="", y=Freq, fill=Var1)) +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Blues") +
      theme_minimal() + ggtitle("Geography") +
      theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(),
            axis.title.y=element_blank())

    bp3

  })

  output$plotClusteringFocus <- renderPlot({
    input$generateClustering
    if (is.null(clustering$c)) {
      return()
    }

    focus <- as.data.frame(table(clusterResultMeta()$Focus))

    bp4 <- ggplot(focus, aes(x="", y=Freq, fill=Var1)) +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Blues") +
      theme_minimal() + ggtitle("Focus") +
      theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(),
            axis.title.y=element_blank())
    bp4

  })

  output$plotClusterCompareReturn <- renderPlot({
    input$generateClustering
    if (is.null(clustering$c)) {
      return()
    }

    ggplot(clustering$statsAll,
           aes(x = Cluster,
               y = Return))  +
      geom_point() +
      geom_point(data = clustering$statsClust, aes(x = Cluster, y = Return), colour = "red", size = 3) +
      ggtitle("Expected Returns for each cluster")

  })

  output$plotClusterCompareStd <- renderPlot({
    input$generateClustering
    if (is.null(clustering$c)) {
      return()
    }

    ggplot(clustering$statsAll,
           aes(x = Cluster,
               y = Std))  +
      geom_point() +
      geom_point(data = clustering$statsClust, aes(x = Cluster, y = Std), colour = "red", size = 3) +
      ggtitle("Standard Deviation for each cluster")
  })

  output$plotClusterCompareSR <- renderPlot({
    input$generateClustering
    if (is.null(clustering$c)) {
      return()
    }

    ggplot(clustering$statsAll,
           aes(x = Cluster,
               y = SR))  +
      geom_point() +
      geom_point(data = clustering$statsClust, aes(x = Cluster, y = SR), colour = "red", size = 3) +
      ggtitle("Sharpe Ratio for each cluster")
  })


  ### REDO THIS CODE
  portReturnCalc <- function(assetWeigths, initialPortVal = 100 ,portAssetReturns){

    assetValPer <- matrix(assetWeigths*initialPortVal, ncol = length(assetWeigths))
    for (i in 1:dim(portAssetReturns)[1]){
      assetValPer <- assetValPer %>% rbind(assetValPer[i, ] * (1 + portAssetReturns[i, ]))
    }
    return( apply(assetValPer, 1, sum) )
  }

  #   output$plotEfficientFrontier <- renderPlot({
  #   input$optimizeButton
  #   if (is.null(optimizeVal)) {
  #     return()
  #   }
  #   plot(sqrt(optimizeVal()[[3]]$Variance), optimizeVal()[[2]]$RunningReturn, type = "o", xlab = 'Std', ylab = 'Return', main = 'Efficient Frontier')
  # })

  #   output$tableEfficientFrontier <- renderTable({
  #   if (is.null(optimizeVal)) {
  #     return()
  #   }
  #   optimizeVal()[[1]]
  # })

  output$randomBacktestPlot <- renderPlot({
    input$optimizeButton
    if (is.null(optimizeVal)) {
      return()
    }
    #    randomPort_returns <- data.frame(fread('RandomPortfoliosReturns.csv', select = 1:101), row.names = 1)
    ##### Random Part #####
    randomPort_returns$V1 <- as.Date(randomPort_returns$V1)

    randomPort_returns_reduced <- randomPort_returns[,1:input$numb_rand_port]  %>% filter(V1 > (today()-years(input$backtestYears)))
    row.names(randomPort_returns_reduced) <- randomPort_returns_reduced$V1
    randomPort_returns_reduced <- randomPort_returns_reduced[,-1]

    portfolio_value <- matrix(0L, nrow = dim(randomPort_returns_reduced)[1] + 1, ncol = dim(randomPort_returns_reduced)[2],
                              dimnames = list(c(as.character(as.Date(row.names(randomPort_returns_reduced)[1])-1), row.names(randomPort_returns_reduced)),
                                              colnames(randomPort_returns_reduced)))

    portfolio_value[1, ] <- rep(100, dim(randomPort_returns_reduced)[2])

    for (i in 1:dim(randomPort_returns_reduced)[1]){
      portfolio_value[i+1, ] <- portfolio_value[i, ] * (1+as.numeric(randomPort_returns_reduced[i, ]))
    }
    print('--A--')
    print(head(portfolio_value))
    print(tail(portfolio_value))


    portfolioPlotData<- melt(portfolio_value)
    colnames(portfolioPlotData) <- c('Date', 'Portfolio', 'Price')
    portfolioPlotData$Date <- as.Date(portfolioPlotData$Date)
    portfolioPlotData$Portfolio <- as.factor(portfolioPlotData$Portfolio)

    ##### Optimization Part #####
    testPerAssetPrices <-
      na.omit(sqlQuery(
        paste0(
          "SELECT date AS Date, symbol, adjusted_close AS Price FROM historicaldata WHERE symbol IN ('",
          paste0(clustering$Gselect, collapse = "', '"),
          "') AND Date > CURDATE() - INTERVAL ",
          input$backtestYears ,
          " YEAR"
        )
      )) %>% spread(symbol, Price)



    rownames(testPerAssetPrices) <- testPerAssetPrices$Date
    testPerAssetPrices <- na.omit(testPerAssetPrices[, -which(colnames(testPerAssetPrices) == 'Date')])


    print('--B--')
    print(head(testPerAssetPrices))
    print(tail(testPerAssetPrices))

    #   print('--C--')
    #   print(head(models_df))
    # print(tail(models_df))

    switch(input$EGSEtfs,
           "FALSE" = {
             switch(input$modelChoices,
                    "MeanVar" = {
                      MeanVaR_portfolio_value <-
                        PortfolioBackTest(
                          assets = optimizeVal()$MeanVaR_Allocation$i,
                          asset_weights = optimizeVal()$MeanVaR_Allocation$EW_x,
                          asset_prices = testPerAssetPrices[, as.character(optimizeVal()$MeanVaR_Allocation$i)]
                        )
                      models_df <- data.frame(Date = names(MeanVaR_portfolio_value), MeanVar = MeanVaR_portfolio_value)
                    },
                    #
                    # VaR_portfolio_value <-
                    #   PortfolioBackTest(
                    #     assets = optimizeVal()$VaR_Allocation$i,
                    #     asset_weights = optimizeVal()$VaR_Allocation$VaR_x,
                    #     asset_prices = testPerAssetPrices[, as.character(optimizeVal()$VaR_Allocation$i)]
                    #   )
                    "EW" = {
                      EW_portfolio_value <-
                        PortfolioBackTest(
                          assets = optimizeVal()$EW_Allocation$i,
                          asset_weights = optimizeVal()$EW_Allocation$EW_x,
                          asset_prices = testPerAssetPrices[, as.character(optimizeVal()$EW_Allocation$i)]
                        )
                      models_df <- data.frame(Date = names(EW_portfolio_value), EW = EW_portfolio_value)
                    })
           },

           "TRUE" = { EW_portfolio_value <-
             PortfolioBackTest(
               assets = optimizeVal()$EW_Allocation$i,
               asset_weights = optimizeVal()$EW_Allocation$EW_x,
               asset_prices = testPerAssetPrices[complete.cases(testPerAssetPrices), as.character(optimizeVal()$EW_Allocation$i)])
           models_df <- data.frame(Date = names(EW_portfolio_value), EW = EW_portfolio_value)
           })

    optimData <- gather(models_df, key = 'Portfolio', value = 'Price', -Date)
    optimData$Date <- as.Date(optimData$Date)
    optimData$Portfolio <- as.factor(optimData$Portfolio)

    ##### Plot Function #####

    ggplot(portfolioPlotData,
           aes(x = Date,
               y = Price,
               group = factor(Portfolio))) +
      geom_line(color = "grey") +
      geom_line(data = optimData, aes(x = Date,y = Price, col = Portfolio))

    # ggplot(portfolioPlotData,
    #        aes(x = Date,
    #            y = Price,
    #            group = factor(Portfolio))) +
    #     geom_line(color = "grey")

    # ggplot(optimData,
    #        aes(x = Date,
    #            y = Price,
    #            col = Portfolio))+
    #   geom_line()

  })


  })

