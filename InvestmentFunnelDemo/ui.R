## ui.R ##
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Investment Funnel"),
    
####################################### Sidebar content #######################################
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Data Inspection", tabName = "menu", icon = icon("home")),
        menuItem("Screening", tabName = "Screening", icon = icon("bars")),
        menuItem("Analysis Preferences", tabName = "analysisPreferences", icon = icon("bars")),
        menuItem("Clustering", tabName = "clustering", icon = icon("bars")),
        menuItem("Clustering Results", tabName = "clusteringresults", icon = icon("check")),
        menuItem("Portfolio Optimization", tabName = "optimization", icon = icon("bars")),
        menuItem("Optimization Results", tabName = "optimizationresults", icon = icon("check"))
        
      )),
    
    
    
####################################### MENU TAB #######################################
    dashboardBody(
      tabItems(
        tabItem(tabName = "menu",
                
                fluidRow(
                  column(12, align="center", offset = 3,
                         box(
                           plotOutput("plotStatus", height = '500px', width = '60%'),
                           solidHeader = T,
                           collapsible = F,
                           collapsed = F,
                           width = 6,
                           title = "InvestmenFunnel Overview",
                           status = "primary",
                           align = "center"))),
                
                br(),
                
                box(
                  fluidRow(
                    column(width = 2,
                           selectInput("assetSelection", label = h3("Inspect Asset"),
                                       choices = sort(assets),
                                       selected = "SPY")
                    ),
                    column(width = 8, offset = 2,
                           plotOutput("plotAssetData", height = '400px', width = '100%')
                    )
                  ),
                  
                  br(),
                  
                  tableOutput("tableAssetData"),
                  
                  solidHeader = T,
                  collapsible = T,
                  collapsed = F,
                  width = 12,
                  title = "Data Overview",
                  status = "primary")
        ), #menu
        
        
####################################### Screening  #######################################
        tabItem(tabName = "Screening",
                fluidRow(
                  column(12, align="center", offset = 3,
                         box(
                           plotOutput("plotStatus1", height = '500px', width = '60%'),
                           solidHeader = T,
                           collapsible = F,
                           collapsed = F,
                           width = 6,
                           title = "InvestmenFunnel Overview",
                           status = "primary",
                           align = "center"
                         ))),
                
                br(),
                
                box(
                  fluidRow(
                    column(width = 6,
                           checkboxGroupInput("assetClasses",
                                              label = h3("Select Asset classes"),
                                              choices = list("Equity" = "Equity",
                                                             "Commodities" = "Commodities",
                                                             "Fixed-Income" = "Fixed Income",
                                                             "Alternatives" = "Alternatives",
                                                             "Asset Allocation" = "Asset Allocation",
                                                             "Currency" = "Currency"),
                                              selected = c("Equity","Commodities","Fixed Income", "Alternatives", "Asset Allocation", "Currency"))),
                    
                    column(width = 6, checkboxGroupInput("assetRegion",
                                                         label = h3("Select Regions"),
                                                         choices = list("North America" = "North America",
                                                                        "Europe" = "Europe",
                                                                        "Asia-Pacific" = "Asia-Pacific",
                                                                        "Latin America" = "Latin America",
                                                                        "Global" = "Global",
                                                                        "Emerging Markets" = "Emerging Markets",
                                                                        "Middle East & Africa" = "Middle East & Africa",
                                                                        "Frontier Markets" = "Frontier Markets",
                                                                        "Developed Markets" = "Developed Markets",
                                                                        "Global Ex-U.S." = "Global Ex-U.S." ),
                                                         selected = c("North America","Europe","Asia-Pacific","Latin America", "Global", "Emerging Markets", "Middle East & Africa", "Frontier Markets", "Developed Markets", "Global Ex-U.S.")))
                  ),
                  
                  br(),
                  
                  fluidRow(
                    column(width = 6,radioButtons("leveregedEtfs", label = h3("Include Leveraged Etfs"), choices = list("Yes" = "X",
                                                                                                                        "No" = "TRUE"))),
                    
                    column(width = 6, radioButtons("shortEtfs", label = h3("Include Short Etfs"), choices = list("Yes" = "x",
                                                                                                                 "No" = "TRUE")))),
                  
                  fluidRow(
                    column(width = 6,radioButtons("EGSEtfs", label = h3("Only Sustainable ETFs"), choices = list("No" = "FALSE",
                                                                                                                 "Yes" = "TRUE")))),
                  
                  solidHeader = T,
                  collapsible = T,
                  collapsed = F,
                  width = 12,
                  title = "Asset Class", status = "primary"),
                
                br(),
                
                box(sliderInput("yearsOfExistence", "Years of Exsistence:", min = 0, max = 30, value = c(0,30), post="y"),
                    solidHeader = T,
                    collapsible = T,
                    collapsed = F,
                    width = 12,
                    title = "History", status = "primary"),
                
                box(#sliderInput("YTD", "Year-to-Date (YTD):", min = -99, max = 350, post  =" %",value = c(-99,350)),
                  sliderInput("oneYearP", "1-Year Performance:", min = -99, max = 350, post  =" %",value = c(-99,350)),
                  sliderInput("threeYearP", "3-Year Performance (Annualized):", min = -99, max = 350, post  =" %",value = c(-99,350)),
                  #sliderInput("fiveYearP", "5-Year Performance (Annualized):", min = -99, max = 350, post  =" %",value = c(-99,350)),
                  solidHeader = T,
                  collapsible = T,
                  collapsed = F,
                  width = 12,
                  title = "Performance", status = "primary"),
                
                box(sliderInput("beta", "Beta", min = -125, max = 15,value = c(-125,15)),
                    sliderInput("NOH", "Number of Holdings", min = 1, max = 18000 ,value = c(1,18000)),
                    sliderInput("assetsTopTen", "% Assets in Top Ten", min = 1, max = 100, value = c(1,100), post =" %"),
                    sliderInput("avgDvol", "Average Daily Volume", min = 1, max = 90000000, pre  = "$",value = c(1,90000000)),
                    solidHeader = T,
                    collapsible = T,
                    collapsed = T,
                    width = 12,
                    title = "Volatility & Holdings", status = "primary"),
                
                box(sliderInput("expenseRatio", "Expense Ratio", min = 0.01, max = 10, post  = " %",value = c(0.01,10)),
                    sliderInput("AUM", "Assets Under Management (AUM) (in 000s)", min = 1000, max = 200000000, pre ="$",value = c(1000,200000000)),
                    sliderInput("sharePrice", "Share Price", min = 1, max = 300, value = c(1,300), pre ="$"),
                    sliderInput("dividendYield", "Dividend Yield", min = 0.01, max = 35, post  = " %",value = c(0.01,35)),
                    solidHeader = T,
                    collapsible = T,
                    collapsed = T,
                    width = 12,
                    title = "Expenses, Liquidity & Yield", status = "primary")),
            
####################################### Analysis preferences  #######################################

        
        tabItem(tabName = "analysisPreferences",

                br(),
                
                  fluidRow(
                    box(
                      h2("Choose analysis parameters"),
                      
                      hr(),
                      
                      
                      column(width = 3, radioButtons("dataFrequency", label = h3("Data Frequency"),
                                                   choices = list("Daily" = 'daily',
                                                                  "Weekly" = 'weekly',
                                                                  "Monthly" = 'monthly'),
                                                              selected = 'monthly')),
                      # hack here - use db instead...
                      column(width = 3, selectInput("dataCurrency", label = h3("Currency"),
                                                     choices = list("USD", "EUR", "GBP", "JPY", "SEK", "DKK", "NOK", 
                                                                    "CHF", "AED", "AUD", "BRL", "CAD", "CNH", "EGP", 
                                                                    "HKD", "HUF", "IDR", "ILS", "INR", "ISK", "KRW",
                                                                    "MXN", "NGN", "NZD", "PHP", "PLN", "RON", "RUB",
                                                                    "SAR", "SGD", "THB", "TRY", "TWD", "VND", "ZAR"), 
                                                    selected = "USD")),
                      column(width = 3, numericInput("dataUseYears", label = h3("Use Years"), value = 10, 
                                                     min = 1, max = 20)),
                      
                    solidHeader = T,
                    collapsible = F,
                    collapsed = F,
                    width = 12,
                    title = "Analysis parameters", 
                    status = "primary", 
                    align =  "center") #box
              ) # fluidrow
              ), #tab preferences
        

####################################### Clustering  #######################################
        
        tabItem(tabName = "clustering",

                box(
                  h2("Choose specification for Hierarchical clustering"),
                  
                  fluidRow(
                    column(width = 12, sliderInput("numberOfClusters", "Number of Clusters", min = 1, max = 15, value = 10 ))),
                  
                  hr(),
                  
                  fluidRow(
                    column(width = 3, radioButtons("corMethod", label = h3("Correlation Coefficient"),
                                                   choices = list("Pearson" = 'pearson',
                                                                  "Spearman" = 'spearman'))),
                    
                    column(width = 3, radioButtons("distMetric", label = h3("Distance Metric"),
                                                   choices = list("1-abs(corr)" = 'A',
                                                                  "1-corr" = 'B',
                                                                  "abs(1-corr)" = 'C'))),
                    column(width = 3,
                           radioButtons("linkage", label = h3("Linkage"),
                                        choices = list("Single" = 'single',
                                                       "Complete" = 'complete',
                                                       "Centroid" = 'centroid',
                                                       "Median" = 'median',
                                                       "Ward.D" = 'wardD',
                                                       "Ward.D2" = 'wardD2'))),
                    column(width = 3,
                           radioButtons("selectionCriteria", label = h3("Selection Criteria"),
                                        choices = list("Highest Return" = 'highestReturn',
                                                       "Minimum Standard Deviation" = 'minimumStd',
                                                       "Highest Sharpe Ratio" = 'highestSharpe',
													   "Most Representive" = 'mostRepresentive')))
                  ), #fluidrow
                  
                  hr(),
                  
                  
                  ## Action Button To Generate Clustering
                  column(8, align="center", offset = 2, actionButton("generateClustering", "Perform Clustering"), actionButton("reset", "Clear")),
                  
                  solidHeader = T,
                  collapsible = F,
                  collapsed = F,
                  width = 12,
                  title = "Clustering", status = "primary"
                ) #box
              ), # tab clustering

######################## Results from Clustering ################################
    
    tabItem(tabName = "clusteringresults",
        box(
          h3("Clustering"),
          fluidRow(
            column(width = 6, plotOutput("plotResultAssets", height = '400px', width = '100%')),
            column(width = 6, plotOutput("plotClusterPieChart", height = '400px', width = '100%'))
          ),
          
          br(),
          
          fluidRow(
            column(width = 9, tableOutput('tableClustering'))
          ),
          
          fluidRow(
            column(width = 3, plotOutput("plotClusteringAssetClass", height = '300px', width = '100%')),
            column(width = 3, plotOutput("plotClusteringRegion", height = '300px', width = '100%')),
            column(width = 3, plotOutput("plotClusteringGeography", height = '300px', width = '100%')),
            column(width = 3, plotOutput("plotClusteringFocus", height = '300px', width = '100%'))
          ),
          
          fluidRow(
            column(width = 4, plotOutput("plotClusterCompareReturn", height = '300px', width = '100%')),
            column(width = 4, plotOutput("plotClusterCompareStd", height = '300px', width = '100%')),
            column(width = 4, plotOutput("plotClusterCompareSR", height = '300px', width = '100%'))
          ),
          
          hr(),
          
          solidHeader = T,
          collapsible = F,
          collapsed = F,
          width = 12,
          title = "Clustering Results", status = "primary"
        ) #box
  ), #tab clusteringresults

                
####################### PORTFOLIO OPTIMIZATION MODELS #############################
  tabItem(tabName = "optimization",
    box(
      fluidRow(
        column(width = 6,
          radioButtons("modelChoices",
          label = h3("Select Model"),
          choices = list("Constrained Markowitz" = "Markowitz",
                         "Risk Parity" = "riskParity",
                         "Mean-Variance" = "MeanVar",
                         "Equal-weights" = "EW",
                         "Max Diversification" = "MaxDiv"))
                        ),
              column(width = 6, uiOutput("ui")),
              column(width = 3,
                numericInput("maxrisk", label = h3("Maximum risk, % (If appropriate)"), value = 10, min = 0))
      ),  #fluidrow

      column(width = 8, align="center", offset = 2,actionButton("optimizeButton", "Optimize!")),
      solidHeader = T,
      collapsible = T,
      collapsed = F,
      width = 12,
      title = "Optimization Models", status = "primary"
    ),  #box
    br(),
    box(
      rHandsontableOutput("optimizationbounds", width = 350),
      solidHeader = T,
      collapsible = T,
      collapsed = F,
      width = 12,
      title = "Optimization Bounds", status = "primary"
    ),
    box(
      textOutput("optimizationboundsmessage"),
      solidHeader = T,
      collapsible = T,
      collapsed = F,
      width = 12,
      title = "Messages", status = "primary"
    )
  ), #tab optimization
       
####################################### Optimization Results  #######################################
        
  tabItem(tabName = "optimizationresults",
    box(
      h3("Wealth and drawdowns"),
      fluidRow(
        column(width = 6, plotOutput("plotPortfolioWealth", height = '300px', width = '100%')),
        column(width = 6, plotOutput("plotPortfolioDrawdowns", height = '300px', width = '100%'))
      )
    ) ,
    br(),
              
    fluidRow(column(width = 9, tableOutput('tablePfStats')) ),
    br(),
    fluidRow(column(width = 9, tableOutput('tablePfSecurities'))),

    br(),
    
    box(
      h3("Asset and risk composition"),
      fluidRow(column(width = 6, plotOutput("plotPortfolioComposition", height = '400px', width = '100%')),
               column(width = 6, plotOutput("plotPortfolioRiskComposition", height = '400px', width = '400%'))
      )
    )
  )

                  
#                  fluidRow(
#                    column(width = 3, plotOutput("plotClusteringAssetClass", height = '300px', width = '100%')),
#                    column(width = 3, plotOutput("plotClusteringRegion", height = '300px', width = '100%')),
#                    column(width = 3, plotOutput("plotClusteringGeography", height = '300px', width = '100%')),
#                    column(width = 3, plotOutput("plotClusteringFocus", height = '300px', width = '100%'))
#                  ),
                  
#                  fluidRow(
#                    column(width = 4, plotOutput("plotClusterCompareReturn", height = '300px', width = '100%')),
#                    column(width = 4, plotOutput("plotClusterCompareStd", height = '300px', width = '100%')),
#                    column(width = 4, plotOutput("plotClusterCompareSR", height = '300px', width = '100%'))
#                  ),
                  
#                  hr(),
                  
                  ###### Backtest ######
                  # h3("Equal-Weigths"),
                  # hr(),
                  #
                  # h3("Mean-Variance / Markowitz"),
                  # fluidRow(
                  #   column(width = 6, plotOutput("plotEfficientFrontier", height = '400px', width = '100%')),
                  #   column(width = 6, tableOutput('tableEfficientFrontier'))
                  #
                  # hr(),
                  # h3("Value-at-Risk / VaR"),
                  # hr(),
                  # h3("Conditional-Value-at-Risk / CVaR"),
                  # hr(),
                  
#                  h3("Backtests"),
#                  fluidRow(
##                    column(width = 6, plotOutput("randomBacktestPlot", height = '400px', width = '100%')),
#                    column(width = 6, plotOutput("xxx", height = '400px', width = '100%'))
#                  ),
#                  
#                  hr(),
                  
#                  solidHeader = T,
#                  collapsible = F,
#                  collapsed = F,
#                  width = 12,
#                  title = "Clustering Results", status = "primary"
#                )
#        )
        
#      ) #box
#    ) #tab optimizationresults
  ) #tabitems
) #dashboardbody
) #dashboardpage
) #app