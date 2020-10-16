## server.R ##

shinyServer(function(input, output) {

  #######################################  Menu #######################################

  output$plotAssetData <- renderPlot({
    tempAssetData <- na.omit(sqlQuery(paste0("SELECT date AS Date, adjusted_close AS Price
                                             FROM historicaldata WHERE symbol = '", input$assetSelection, "'")))

    tempAssetData$Date <- as.Date(tempAssetData$Date)
    ggplot(tempAssetData, aes(y = Price, x = Date)) +
      geom_line()
  })


  # EMIL - I HAVE CHANGED NUMBERS TO PERCENT
  output$tableAssetData <- renderTable({
    assetInfo= na.omit(sqlQuery(paste0("SELECT m.fund, m.symbol, m.issuer, m.segment, (100*m.expenseRatio) as expenseRatio , m.category, m.underlyingIndex, (100*p.priceTr1Mo) as priceTR1Mo
                                       FROM metadata m
                                       INNER JOIN performancemeasures p ON p.symbol = m.symbol
                                       WHERE m.symbol = '", input$assetSelection, "'"))
    )
  })


  ####################################### ANALYSIS PREFERENCES #######################################

  observeEvent(input$dataCurrency, {})
  currency <- eventReactive(input$dataCurrency, {
    currency <<- input$dataCurrency
    return(input$dataCurrency)
  })

  observeEvent(input$dataFrequency, {print(input$dataFrequency)})
  frequency <- eventReactive(input$dataFrequency, {
    dataFrequency <<- input$dataFrequency
    return(input$dataFrequency)
  })

  observeEvent(input$dataUseYears, {print(input$dataUseYears)})
  dataUse <<- eventReactive(input$dataUseYears, {
    return(input$dataUseYears)
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
      #      Category <- c("Data", "Screeing", "Clustering", "Optimization")
      Category <- c("Data", "Screening", "Clustering")
      Percent <- c(length(assets), length(dataSelection()$ticker), input$numberOfClusters, input$numberInPortfolio)
      circBarPlot(Percent, Category)
    })

    output$plotStatus1 <- renderPlot({
      Category <- c("Data", "Screening", "Clustering", "Optimization")
      Percent <- c(length(assets), length(dataSelection()$ticker), input$numberOfClusters, input$numberInPortfolio)
      circBarPlot(Percent, Category)
    })

    output$plotStatus2 <- renderPlot({
      Category <- c("Data", "Screening", "Clustering", "Optimization")
      Percent <- c(length(assets), length(dataSelection()$ticker), input$numberOfClusters, input$numberInPortfolio)
      circBarPlot(Percent, Category)
    })

    output$plotStatus3 <- renderPlot({
      Category <- c("Data", "Screening", "Clustering", "Optimization")
      Percent <- c(length(assets), length(dataSelection()$ticker), input$numberOfClusters, input$numberInPortfolio)
      circBarPlot(Percent, Category)
    })
  })

  ####################################### Screening #######################################




  ####################################### Clustering  #######################################


  clustering <- reactiveValues(clustering = NULL)
  boundsvalues = reactiveValues()

  observeEvent(input$generateClustering, {
    regenerate <<- TRUE

    #print('Begin Clustering')
    withProgress(message = 'Performing Clustering', value = 0.02, {
      if(input$dataFrequency == "daily") {
        clustering$AssetPrices <- na.omit(sqlQuery(paste0("SELECT date AS Date, symbol, adjusted_close AS Price FROM historicaldata WHERE symbol IN ('",
                                                          paste0(dataSelection()$ticker, collapse = "', '"), "') AND
                                                          Date > CURDATE() - INTERVAL ", input$dataUseYears ," YEAR"))) %>% spread(symbol, Price)
      }
      else {
        if(input$dataFrequency == "weekly") {
          clustering$AssetPrices <- na.omit(sqlQuery(paste0("SELECT date AS Date, symbol, adjusted_close AS Price FROM historicaldata WHERE symbol IN ('",
                                                            paste0(dataSelection()$ticker, collapse = "', '"), "') AND
                                                            Date > CURDATE() - INTERVAL ", input$dataUseYears ," YEAR AND
                                                            Date IN ('", paste0(weekDates, collapse =  "', '" ), "')"))) %>% spread(symbol, Price)
        }
        else {
          clustering$AssetPrices <- na.omit(sqlQuery(paste0("SELECT date AS Date, symbol, adjusted_close AS Price FROM historicaldata WHERE symbol IN ('",
                                                            paste0(dataSelection()$ticker, collapse = "', '"), "') AND
                                                            Date > CURDATE() - INTERVAL ", input$dataUseYears ," YEAR AND
                                                            Date IN ('", paste0(monthDates, collapse =  "', '" ), "')"))) %>% spread(symbol, Price)
        }
        }

      # Currency conversion
      # In this version, all funds are denominated in USD, in the future: any currency - then we have to loop
      # and compute cross currency for each asset
      # For now, just pick the exchange rate vs USD on the relevant dates


      ISO_logical <- which(as.character(exchangeRates$ISO) == input$dataCurrency) # Pick records for selected ISO
      ISO_ExchangeRates <- exchangeRates[ISO_logical,]

      logicalDates <- which(ISO_ExchangeRates$date %in% clustering$AssetPrices[,1])
      exchange_rates <- ISO_ExchangeRates[logicalDates,]

      # Now have exchange rates to apply. Apply them to prices in USD
      nasset <- length(clustering$AssetPrices[1,])
      for(i in 2:nasset) {
        ourdata <- clustering$AssetPrices[,i]
        ourdata <- ourdata*exchange_rates$Price
        clustering$AssetPrices[,i] <- ourdata
      }

      asset_prices_ok <- find_sound_data(clustering$AssetPrices)
      clustering$AssetPrices <- keep_sound_pricedata(clustering$AssetPrices, asset_prices_ok)

      ### Could dump AssetPrices into AssetReturns Right Away
      row.names(clustering$AssetPrices) <- clustering$AssetPrices[, 1]
      clustering$AssetReturns <- apply(clustering$AssetPrices[,-1], 2, returnsCalc)
      clustering$AssetReturns <- clustering$AssetReturns[complete.cases(clustering$AssetReturns), ]

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
      incProgress(amount = 0.8)
      Sys.sleep(0.5)
      #print('--Clustering Nearly Done--')

      clustering$Cno <- input$numberOfClusters
      clustering$memb <- cutree(clustering$hc, k = clustering$Cno)


      ### HERE NEED TO ADD STATISTICS WITH SWITCH

      #### EMIL : I have changed output return etc to percents. Shouldn't we also annualize?
      clustering$statsAll <- data.frame(ETF = names(clustering$memb), Cluster = as.factor(clustering$memb),
                                        Return = 100*apply(clustering$AssetReturns[,names(clustering$memb)], 2, geomAveCalc), Std = 100*apply(clustering$AssetReturns[,names(clustering$memb)], 2, sd),
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
                                           highestSharpe = sharpeRatioCalc(clustering$AssetReturns[,clustering$Gnames[j]]),
                                           mostRepresentative = 0 #TODO: Remove

          )

        }
        incProgress(amount = 0.9)
        clustering$Gselect[i] <- switch(input$selectionCriteria,
                                        highestReturn = clustering$Gnames[which(max(clustering$criteria) == clustering$criteria)],
                                        minimumStd =  clustering$Gnames[which(min(clustering$criteria) == clustering$criteria)],
                                        highestSharpe =  clustering$Gnames[which(max(clustering$criteria) == clustering$criteria)],
                                        mostRepresentative = "" #TODO: Remove
        )
      }

      if (input$selectionCriteria == "mostRepresentative") {
        clustering$Gselect = sapply(unique(clustering$memb), clust.medoid, as.matrix(clustering$d), clustering$memb)

      }

      clustering$ClustSize <- data.frame(Cluster = sprintf("Clst%d", 1:clustering$Cno), Size = clustering$clustCount)
      clustering$statsClust <- clustering$statsAll[which(clustering$statsAll$ETF %in% clustering$Gselect),]

      #print('--Clustering Done--')
      incProgress(amount = 1, detail = paste("Clustering Done"))
      Sys.sleep(1)})

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
    ) # switch
  }) # renderui

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

  }) # observe

  ## Optimization in R

  ################################### SET OPTIMIZATION EXPECTED RETURNS AND BOUNDS ########################################

   optidata = reactive({
    DF = NULL

    if(regenerate == FALSE) {
      DF = hot_to_r(input$optimizationbounds)
#      print(DF$Lower)
    }

    if(regenerate == TRUE) {
#      print("Regenerating")
      zeros <- rep(0, length(clustering$Gselect))
      hundreds <- rep(100, length(clustering$Gselect))
      # Annualize historical returns AND show them as 0.0 in UI
      avghistreturns <- get_average_over_time_returns(clusterResultReturns())
      if(input$dataFrequency == "daily") {
        returnfactor <- 250*100
      }
      else {
        if(input$dataFrequency == "weekly") {
          returnfactor <- 52*100
        }
        else
          returnfactor <- 12*100
      }

      optimizationdf <- data.frame("ExpectedReturns" = round(returnfactor*avghistreturns, 1), "Lower" = zeros, "Upper" = hundreds)
      rownames(optimizationdf) <- clustering$Gselect

      DF = optimizationdf
      boundsvalues[["DF"]] = DF
    }
    regenerate <<- FALSE
    boundsvalues[["DF"]] = DF
    boundsvalues[["DF"]]

  }) # optidata

  observeEvent(input$generateClustering, {
    output$optimizationbounds <-
      renderRHandsontable({

        DF = optidata()
#        print("DF")
#        print(DF)

        if (!is.null(DF))
          rhandsontable(DF, useTypes = FALSE, stretchH = "all")
      }) # handsontable
  }) #eventreactive

  # Send message if problem with bounds
  output$optimizationboundsmessage <- renderText({

    DF <- hot_to_r(input$optimizationbounds)
    if(sum(DF$Lower) > 100) {
      "Sum of lower bounds > 100"
    }
    else
    {
      if(sum(DF$Upper) < 100) {
        "Sum of upper bounds < 100"
      }
      else
      {
        if(sum((DF$Lower > DF$Upper) == TRUE) > 0) {
          "Lower bound > Upper bound"
        }
      }
    }
  }
  )

  optimize <- function(){
    # get data
    # asset returns
    assetreturns <- 100*clusterResultReturns()

    # data frequency
    frequency <- input$dataFrequency

    DF = hot_to_r(input$optimizationbounds)
    # expected returns
    expret <- DF$ExpectedReturns

    # bounds
    lower <- DF$Lower/100
    upper <- DF$Upper/100

    # Maximum risk
    maxrisk <- input$maxrisk

    if(frequency == "daily") {
      maxrisk <- maxrisk/sqrt(250)
    }
    else {
      if(frequency == "weekly"){
        maxrisk <- maxrisk/sqrt(52)
      }
      else {
        maxrisk <- maxrisk/sqrt(12)
      }
    }


    switch(input$modelChoices,
           "Markowitz" = {
             res <- markowitz(assetreturns, expret, lower, upper, maxrisk)
           },
           "EW" = {
             res <- equal_weights(assetreturns, lower, upper)
           },
           "MeanVar" = {
             gamma <- input$dynamic
             res <- oldmarkowitz(assetreturns, expret, lower, upper, gamma)
           },
           "riskParity" = {
             res <- risk_parity(assetreturns, lower, upper)
           },
           "MaxDiv" = {
             res <- max_diversification(assetreturns, lower, upper)
           }
           ,
           "MaxSharpe" = {
             res <- max_sharpe(assetreturns, expret, lower, upper)
           }
    )
    optimizationResult <<- res
    output$optimizationstatusmessage <- renderText({get_portfolio_optimization_status(input$modelChoices, res)})
    return(res)
  }

  ############################### OPTIMIZATION RESULTS ###################################

  generate_Optimization_Report <- function() {
    # PLOT WEALTH
    output$plotPortfolioWealth <- renderPlot({
      optiresult <- optimizationResult
      returns <- clusterResultReturns()

      # HERE CHECK FOR Optimality
      #    if (is.null(clustering$hc)) {
      #      return()
      #    }

      # Get weights
      model <- input$modelChoices
      weights <- get_portfolio_optimization_weights(model, optiresult)
#      print("PF")
#      print(weights)

      rownames(weights) <- names(returns)
      pfreturns <- generate_portfolio_returns(returns, weights)

      pfwealth <- as.data.frame(get_portfolio_wealth(pfreturns))
      names(pfwealth) <- "Wealth"
      rownames(pfwealth) <- rownames(returns)

      dates <-as.Date(rownames(pfwealth))
      plotData <- pfwealth
      plotData$Date <- dates

      assetPlotData <- gather(plotData, key = "Portfolio", value = "Wealth" , -Date)

      ggplot(assetPlotData,
             aes(x=Date,
                 y=Wealth) #,
             #               color=Asset)
      ) +
        geom_line()

    })

    # PLOT DRAWDOWNS
    output$plotPortfolioDrawdowns <- renderPlot({
      optiresult <- optimizationResult
      returns <- clusterResultReturns()


      # HERE CHECK FOR Optimality
      #    if (is.null(clustering$hc)) {
      #      return()
      #    }

      # Get weights
      model <- input$modelChoices
      weights <- get_portfolio_optimization_weights(model, optiresult)

      rownames(weights) <- names(returns)
      pfreturns <- generate_portfolio_returns(returns, weights)
      pfwealth <- get_portfolio_wealth(pfreturns)
      dds <- as.data.frame(get_portfolio_drawdowns(pfwealth))

      names(dds) <- "Drawdowns"
      rownames(dds) <- rownames(returns)

      dates <-as.Date(rownames(dds))
      plotData <- dds
      plotData$Date <- dates

      assetPlotData <- gather(plotData, key = "Portfolio", value = "Drawdowns" , -Date)

      ggplot(assetPlotData,
             aes(x=Date,
                 y=Drawdowns)
      ) +
        geom_line()

    }
    )

    # TABLE OF PORTFOLIO STATISTICS
    output$tablePfStats <- renderTable({
      optiresult <- optimizationResult
      returns <- clusterResultReturns()

      # Get weights
      model <- input$modelChoices
      weights <- get_portfolio_optimization_weights(model, optiresult)

      rownames(weights) <- names(returns)
      pfreturns <- generate_portfolio_returns(returns, weights)
      pfwealth <- get_portfolio_wealth(pfreturns)
      dds <- get_portfolio_drawdowns(pfwealth)

      frequency <- input$dataFrequency
      nyears <- input$dataUseYears

      meanreturn <- 100*(tail(pfwealth, n=1)^(1/nyears)-1)

      stdev <- sd(pfreturns)
      if(frequency == "daily") {
        stdev <- stdev*sqrt(250)
      }
      else {
        if(frequency == "weekly") {
          stdev <- stdev*sqrt(52)
        }
        else
          stdev <- stdev*sqrt(12)
      }
      stdev <- 100*stdev

      maxdd <- 100*(1-min(dds))

      eret <- 0
      DF <- DF = hot_to_r(input$optimizationbounds)

      # expected returns
      expret <- DF$ExpectedReturns
#      print(expret)

      eret <- 0
      for(i in 1:length(weights[,1])) {
        eret <- eret + weights[i,1] * expret[i]
      }
#      print(eret)
      df <- data.frame("ExpectedReturn" = eret, "HistoricalReturn" = meanreturn, "StandardDeviation" = stdev, "MaxDrawdown" = maxdd)
      return(df)
    })
    get_optimization_result_df <- function() {
      optiresult <- optimizationResult
      # Get weights
      model <- input$modelChoices
      weights <- get_portfolio_optimization_weights(model, optiresult)

      returns <- clusterResultReturns()
      meta <- clusterResultMeta()

      # Compute risk contributions
      vcv <- cov(returns)
      pfvar <- weights[,1] %*% vcv %*% weights[,1]
      pfv <- pfvar[1,1]
      marg <- vcv %*% weights[,1]
      contrib <- weights[,1] * marg/pfv

      rownames(weights) <- rownames(contrib)
      names(weights) <- "Weight"

      # Join together in one df

      outputdf <- data.frame("Weights" = 100*weights, "RiskContribution" = 100*contrib)
      metadf <- data.frame(meta[,2:length(meta[1,])])
      rownames(metadf) <- meta$symbol
      outd2 <- merge(outputdf, metadf, by=0)
      nams <- c("Security", names(outputdf), names(metadf))
      names(outd2) <- nams
#      print(outd2)
      return(outd2)
    }

    # TABLE OF PORTFOLIO CONTENTS
    output$tablePfSecurities <- renderTable({
      get_optimization_result_df()

    })

    ########### THE NEXT ONES CAN BE MADE SIMPLER - NEXT ROUND

    # Plot portfolio weights as pie
    output$plotPortfolioComposition <- renderPlot({
      optiresult <- optimizationResult
      model <- input$modelChoices
      weights <- get_portfolio_optimization_weights(model, optiresult)
      returns <- clusterResultReturns()
      vcv <- cov(returns)
      pfvar <- weights[,1] %*% vcv %*% weights[,1]
      pfv <- pfvar[1,1]
      marg <- vcv %*% weights[,1]
      contrib <- weights[,1] * marg/pfv

      rownames(weights) <- rownames(contrib)
      names(weights) <- "Weights"
      Asset <- rownames(contrib)

      bp <- ggplot(weights, aes(x="", y=weights, fill=Asset)) +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Blues") +
        theme_minimal() + ggtitle("Weights") +
        theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(),
              axis.title.y=element_blank())
      return(bp)
    })

    # Plot portfolio risk contributions as pie
    output$plotPortfolioRiskComposition <- renderPlot({
      optiresult <- optimizationResult
      # Get weights
      model <- input$modelChoices
      weights <- get_portfolio_optimization_weights(model, optiresult)
      returns <- clusterResultReturns()

      # Compute risk contributions
      vcv <- cov(returns)
      pfvar <- weights[,1] %*% vcv %*% weights[,1]
      pfv <- pfvar[1,1]
      marg <- vcv %*% weights[,1]
      contrib <- weights[,1] * marg/pfv
      Asset <- rownames(contrib)

      contrib <- data.frame("Contribution" = contrib[,1] )

      bp <- ggplot(contrib, aes(x="", y=contrib, fill=Asset)) +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Blues") +
        theme_minimal() + ggtitle("abs(Risk Contributions)") +
        theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(),
              axis.title.y=element_blank())
      return(bp)

    })

    # Plot portfolio weight by assetclass
    output$plotPortfolioAllocationByAssetClass <- renderPlot({
      df <- get_optimization_result_df()
      agg <- aggregate(df$Weight, by = list(df$AssetClass), sum)
      plotdf <- data.frame("Weight" = agg[,2], row.names = agg[,1])
      AssetClass <- rownames(plotdf)
      bp <- ggplot(plotdf, aes(x="", y=plotdf, fill=AssetClass)) +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Blues") +
        theme_minimal() + ggtitle("Allocation by Asset Class") +
        theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(),
              axis.title.y=element_blank())
      return(bp)

    })

    # Plot portfolio weight by region
    output$plotPortfolioAllocationByRegion <- renderPlot({
      df <- get_optimization_result_df()
      agg <- aggregate(df$Weight, by = list(df$Region), sum)
      plotdf <- data.frame("Weight" = agg[,2], row.names = agg[,1])
      Region <- rownames(plotdf)
      bp <- ggplot(plotdf, aes(x="", y=plotdf, fill=Region)) +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Blues") +
        theme_minimal() + ggtitle("Allocation by Region") +
        theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(),
              axis.title.y=element_blank())
      return(bp)

    })

    # Plot portfolio risk by assetclass
    output$plotPortfolioRiskByAssetClass <- renderPlot({
      df <- get_optimization_result_df()
      agg <- aggregate(df$RiskContribution, by = list(df$AssetClass), sum)
      plotdf <- data.frame("Weight" = abs(agg[,2]), row.names = agg[,1])
      AssetClass <- rownames(plotdf)
      bp <- ggplot(plotdf, aes(x="", y=plotdf, fill=AssetClass)) +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Blues") +
        theme_minimal() + ggtitle("Risk by Asset Class") +
        theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(),
              axis.title.y=element_blank())
      return(bp)

    })

    # Plot portfolio weight by region
    output$plotPortfolioRiskByRegion <- renderPlot({
      df <- get_optimization_result_df()
      agg <- aggregate(df$RiskContribution, by = list(df$Region), sum)
      plotdf <- data.frame("Weight" = abs(agg[,2]), row.names = agg[,1])
      Region <- rownames(plotdf)
      bp <- ggplot(plotdf, aes(x="", y=plotdf, fill=Region)) +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Blues") +
        theme_minimal() + ggtitle("Risk by Region") +
        theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(),
              axis.title.y=element_blank())
      return(bp)

    })
  }


  optimizeVal <- observeEvent(input$optimizeButton, {
    print('Begin Optimization')
    withProgress(message = 'Optimizing', value = 0.03, {
      res <- optimize()
#      print(res)
      incProgress(amount = 1,detail = paste("Optimization done"))
      Sys.sleep(1) })
    # HERE UPDATE ALL OUTPUTS
    generate_Optimization_Report()
    return(res)
  })

  ####################################### CLUSTERING RESULTS #######################################

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
    localStats <- clustering$statsClust
    if(input$dataFrequency == "daily") {
      annualR <- 250
      annualS <- sqrt(250)
    }
    else {
      if(input$dataFrequency == "weekly"){
        annualR <- 52
        annualS <- sqrt(52)
      }
      else {
        annualR <- 12
        annualS <- sqrt(12)
      }}

    localStats$Return <- 100*((1+.01*localStats$Return)^annualR -1)
    localStats$Std <- localStats$Std * annualS
    localStats$SR <- localStats$Return / localStats$Std
    df <- localStats[order(clustering$statsClust$Cluster), ] %>%
#    df <- clustering$statsClust[order(clustering$statsClust$Cluster), ] %>%
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
      ggtitle("Historical Returns for each cluster")

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

})
