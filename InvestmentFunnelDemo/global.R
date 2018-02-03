library(shinydashboard)
library(shiny)
library(DBI)

library(ggplot2)
library(ggdendro)
library(scales)
library(grid)
library(gridExtra)

library(tidyr)
library(dplyr)

library(RColorBrewer)
library(plotrix)
library(lubridate)
library(reshape2)

#library(gdxrrw)
library(RMySQL)
library(data.table)
library(CVXR)

ESGetf <- c("DSI", "SUSA", "CRBN", "TAN",  "SHE",  "SPYX", "ESGD", "CATH", "ESGG", "EFAX",
"KRMA", "NUBD", "NUSC", "NULG", "NULV", "NUDM", "NUMG", "NUMV", "RODI", "WIL",
"NUEM", "MPCT", "ESGL", "ETHO", "ESGF",  "IBD",  "ESGU", "EEMX", "EQLT", "ESGN",
"ESG" , "ORG" , "SUSC", "SUSB", "GUDB", "HECO", "ESGW", "BIBL", "ESGS", "KGRN",
"ICAN", "LRGE", "CHGX", "YLDE", "GRN" , "BOSS", "ESGQ", "GRNB", "XSOE", "EVX",
"YLCO", "ISMD", "MAGA", "FIW" , "GGW",  "PZD",  "KLD" , "LOWC", "ICLN", "PBW",
"PUW" , "QCLN", "GEX" , "PBS" , "NLR",  "FAN",  "GIVE", "ESGE", "CXSE", "MXDU",
"PXW" , "PBD" , "BLES", "NUBQ")
randomPort_returns <- data.frame(fread('RandomPortfoliosReturns.csv', select = 1:501))

# Function that executes SQL queries in the database "investmentfunnel"
sqlQuery <- function (query) {
  # creating DB connection object with RMysql package
  DB <- dbConnect(MySQL(),
                  user = 'X',
                  password = 'X',
                  host = 'investmentfunneldbinstance.c7kykd0usi6b.us-east-2.rds.amazonaws.com',
                  dbname='investmentfunnel')

  # send Query to obtain result set
  rs <- dbSendQuery(DB, query)
  # get elements from result sets and convert to dataframe
  result <- dbFetch(rs, -1)
  # close db connection
  dbDisconnect(DB)
  # return the dataframe
  return(result)
}

assets <- sqlQuery("SELECT DISTINCT symbol FROM metadata")$symbol

# Calculate Continous Returns
returnsCalc <- function(x){
  diff(x)/x[-length(x)]
}

# Calculate Geometric Average of returns
geomAveCalc <- function(x){
  (prod((1+x)))^(1/length(x))-1
}

# Calculate Returns from 1 period to last
totReturnsCalc <- function(x){
  (x[length(x)]-x[1]) / x[1]
}

# Calculate Sharpe-Ratio
sharpeRatioCalc <- function(x){
  geomAveCalc(x) / sd(x)
}

circBarPlot <- function(x = c(length(dataMeta$ticker), length(dataSelection$ticker), input$numberOfClusters, input$numberInPortfolio),
                        labels = c("Data", "Due Dil.", "Clustering", "Optimization"),
                        colors=brewer.pal(length(x), "Blues"), cex.lab=1) {
  plot(0,xlim=c(-1.1,1.1),ylim=c(-1.1,1.1),type="n",axes=F, xlab=NA, ylab=NA)
  radii <- seq(1, 0.3, length.out=length(x))
  draw.circle(0,0,radii,border="lightgrey")
  angles <- (1/4 - x/x[1])*2*pi
  draw.arc(0, 0, radii, angles, pi/2, col=colors, lwd=130/length(x), lend=2, n=100)
  ymult <- (par("usr")[4]-par("usr")[3])/(par("usr")[2]-par("usr")[1])*par("pin")[1]/par("pin")[2]
  text(x=0.2, y=radii*ymult, labels=paste(labels," - ", x, sep=""), pos=2, cex=cex.lab)
  text(0,0,"Portfolio",cex=1.5,col="grey")
}

randomPortGen <- function(asset_universe = FALSE, min_assets = 2, max_assets = 10, n_samples = 500, from = 10 ) {
  if (!asset_universe){
    asset_universe <- na.omit(sqlQuery(paste0("SELECT DISTINCT symbol FROM metadata WHERE launchDate < CURDATE() - INTERVAL ",
                                      from,
                                      " YEAR"))$symbol)
  }

  asset_prices <- na.omit(sqlQuery(paste0("SELECT date AS Date, symbol, adjusted_close AS Price FROM historicaldata WHERE symbol IN ('",
                                  paste0(asset_universe, collapse = "', '"), "') AND date BETWEEN CURDATE() - INTERVAL ",
                                  from, " YEAR AND '2017-10-17'"))) %>%
    spread(symbol, Price)

  row.names(asset_prices) <- asset_prices$Date
  asset_prices <- asset_prices[,-which(colnames(asset_prices) == 'Date')]
  asset_prices <- asset_prices[, complete.cases(t(asset_prices))]
  asset_universe <- colnames(asset_prices)

  port_sizes <- base::sample(min_assets:max_assets, n_samples, replace = TRUE)
  rand_portfolios <- list()

  p_names <- sprintf("PP_%02d", 1:n_samples)

  for(i in 1:n_samples){
    assets <- base::sample(asset_universe, port_sizes[i])
    rand_portfolios[[p_names[i]]] <- list(assets=assets)
  }

  portfolio_returns <- matrix(0L, nrow = dim(asset_prices)[1] - 1, ncol = n_samples, dimnames = list(row.names(asset_prices)[-1], p_names))
  for (i in 1:n_samples){
    temp_assets <- rand_portfolios[[i]]$assets
    temp_weights <- rep(1/length(temp_assets), length(temp_assets))
    temp_asset_prices <- asset_prices[, c(temp_assets)]
    temp_port_prices <- PortfolioBackTest(temp_assets, temp_weights, temp_asset_prices)
    temp_port_returns <- diff(temp_port_prices) / temp_port_prices[-length(temp_port_prices)]
    rand_portfolios[[p_names[i]]]$port_returns <- temp_port_returns
    portfolio_returns[,i] <- temp_port_returns
  }

  write.csv(portfolio_returns, 'RandomPortfoliosReturns.csv', row.names = TRUE)
  return(portfolio_returns)
}

#randomPortGen()

PortfolioBackTest <- function(assets, asset_weights, asset_prices=NULL, initial_budget = 100, asset_returns=NULL){
  if(is.null(asset_returns)){
    asset_returns <- apply(asset_prices, 2, returnsCalc)
    # To get around NA's this line is included (should be removed when solution to NAs has been implemened)
    asset_returns <- asset_returns[complete.cases(asset_returns), ]
  }

  asset_value <- matrix(0L, nrow = dim(asset_returns)[1] + 1, ncol = length(assets),
                        dimnames = list(c(row.names(asset_prices)[1], rownames(asset_returns)), assets))
  asset_value[1,] <- initial_budget * asset_weights
  for (i in 1:dim(asset_returns)[1]){
    asset_value[i+1, ] <- asset_value[i, ] * (1 + asset_returns[i, ])
  }
  portfolio_value = apply(asset_value, 1, sum)
  return(portfolio_value)
}

