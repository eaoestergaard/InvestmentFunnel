## global.R ##

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
library(nloptr)
library(rhandsontable)

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
  geomAveCalc(x) / sd(x)*sqrt(250)
}

# function to find medoid in cluster i
clust.medoid = function(i, distMatrix, clusters) {
  ind = (clusters == i)
  if (!is.matrix(distMatrix[ind,ind]) && distMatrix[ind, ind] == 0){
    names(which(ind))
  }else{
    names(which.min(rowSums( distMatrix[ind, ind] )))
  }
}

circBarPlot <- function(x = c(length(dataMeta$ticker), length(dataSelection$ticker), input$numberOfClusters, input$numberInPortfolio),
                        labels = c("Data", "Screening", "Clustering", "Optimization"),
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

############################# ALLOW FOR DATA FREQUENCY ############################
# this is clearly a quick and (very) dirty hack. Use database instead. Later version

weekDates <- c("2000-01-07", "2000-01-14", "2000-01-21", "2000-01-28", "2000-02-04", "2000-02-11", "2000-02-18",
  "2000-02-25", "2000-03-03", "2000-03-10", "2000-03-17", "2000-03-24", "2000-03-31", "2000-04-07", "2000-04-14",
  "2000-04-21", "2000-04-28", "2000-05-05", "2000-05-12", "2000-05-19", "2000-05-26", "2000-06-02", "2000-06-09",
  "2000-06-16", "2000-06-23", "2000-06-30", "2000-07-07", "2000-07-14", "2000-07-21", "2000-07-28", "2000-08-04",
  "2000-08-11", "2000-08-18", "2000-08-25", "2000-09-01", "2000-09-08", "2000-09-15", "2000-09-22", "2000-09-29",
  "2000-10-06", "2000-10-13", "2000-10-20", "2000-10-27", "2000-11-03", "2000-11-10", "2000-11-17", "2000-11-24",
  "2000-12-01", "2000-12-08", "2000-12-15", "2000-12-22", "2000-12-29",
  "2001-01-05", "2001-01-12", "2001-01-19", "2001-01-26", "2001-02-02", "2001-02-09", "2001-02-16", "2001-02-23",
  "2001-03-02", "2001-03-09", "2001-03-16", "2001-03-23", "2001-03-30", "2001-04-06", "2001-04-13", "2001-04-20",
  "2001-04-27", "2001-05-04", "2001-05-11", "2001-05-18", "2001-05-25", "2001-06-01", "2001-06-08", "2001-06-15",
  "2001-06-22", "2001-06-29", "2001-07-06", "2001-07-13", "2001-07-20", "2001-07-27", "2001-08-03", "2001-08-10",
  "2001-08-17", "2001-08-24", "2001-08-31", "2001-09-07", "2001-09-14", "2001-09-21", "2001-09-28", "2001-10-05",
  "2001-10-12", "2001-10-19", "2001-10-26", "2001-11-02", "2001-11-09", "2001-11-16", "2001-11-23", "2001-11-30",
  "2001-12-07", "2001-12-14", "2001-12-21", "2001-12-28",
  "2002-01-04", "2002-01-11", "2002-01-18", "2002-01-25", "2002-02-01", "2002-02-08", "2002-02-15", "2002-02-22",
  "2002-03-01", "2002-03-08", "2002-03-15", "2002-03-22", "2002-03-29", "2002-04-05", "2002-04-12", "2002-04-19",
  "2002-04-26", "2002-05-03", "2002-05-10", "2002-05-17", "2002-05-24", "2002-05-31", "2002-06-07", "2002-06-14",
  "2002-06-21", "2002-06-28", "2002-07-05", "2002-07-12", "2002-07-19", "2002-07-26", "2002-08-02", "2002-08-09",
  "2002-08-16", "2002-08-23", "2002-08-30", "2002-09-06", "2002-09-13", "2002-09-20", "2002-09-27", "2002-10-04",
  "2002-10-11", "2002-10-18", "2002-10-25", "2002-11-01", "2002-11-08", "2002-11-15", "2002-11-22", "2002-11-29",
  "2002-12-06", "2002-12-13", "2002-12-20", "2002-12-27",
  "2003-01-03", "2003-01-10", "2003-01-17", "2003-01-24", "2003-01-31", "2003-02-07", "2003-02-14", "2003-02-21",
  "2003-02-28", "2003-03-07", "2003-03-14", "2003-03-21", "2003-03-28", "2003-04-04", "2003-04-11", "2003-04-18",
  "2003-04-25", "2003-05-02", "2003-05-09", "2003-05-16", "2003-05-23", "2003-05-30", "2003-06-06", "2003-06-13",
  "2003-06-20", "2003-06-27", "2003-07-04", "2003-07-11", "2003-07-18", "2003-07-25", "2003-08-01", "2003-08-08",
  "2003-08-15", "2003-08-22", "2003-08-29", "2003-09-05", "2003-09-12", "2003-09-19", "2003-09-26", "2003-10-03",
  "2003-10-10", "2003-10-17", "2003-10-24", "2003-10-31", "2003-11-07", "2003-11-14", "2003-11-21", "2003-11-28",
  "2003-12-05", "2003-12-12", "2003-12-19", "2003-12-26",
  "2004-01-02", "2004-01-09", "2004-01-16", "2004-01-23", "2004-01-30", "2004-02-06", "2004-02-13", "2004-02-20",
  "2004-02-27", "2004-03-05", "2004-03-12", "2004-03-19", "2004-03-26", "2004-04-02", "2004-04-09", "2004-04-16",
  "2004-04-23", "2004-04-30", "2004-05-07", "2004-05-14", "2004-05-21", "2004-05-28", "2004-06-04", "2004-06-11",
  "2004-06-18", "2004-06-25", "2004-07-02", "2004-07-09", "2004-07-16", "2004-07-23", "2004-07-30", "2004-08-06",
  "2004-08-13", "2004-08-20", "2004-08-27", "2004-09-03", "2004-09-10", "2004-09-17", "2004-09-24", "2004-10-01",
  "2004-10-08", "2004-10-15", "2004-10-22", "2004-10-29", "2004-11-05", "2004-11-12", "2004-11-19", "2004-11-26",
  "2004-12-03", "2004-12-10", "2004-12-17", "2004-12-24", "2004-12-31",
  "2005-01-07", "2005-01-14", "2005-01-21", "2005-01-28", "2005-02-04", "2005-02-11", "2005-02-18", "2005-02-25",
  "2005-03-04", "2005-03-11", "2005-03-18", "2005-03-25", "2005-04-01", "2005-04-08", "2005-04-15", "2005-04-22",
  "2005-04-29", "2005-05-06", "2005-05-13", "2005-05-20", "2005-05-27", "2005-06-03", "2005-06-10", "2005-06-17",
  "2005-06-24", "2005-07-01", "2005-07-08", "2005-07-15", "2005-07-22", "2005-07-29", "2005-08-05", "2005-08-12",
  "2005-08-19", "2005-08-26", "2005-09-02", "2005-09-09", "2005-09-16", "2005-09-23", "2005-09-30", "2005-10-07",
  "2005-10-14", "2005-10-21", "2005-10-28", "2005-11-04", "2005-11-11", "2005-11-18", "2005-11-25", "2005-12-02",
  "2005-12-09", "2005-12-16", "2005-12-23", "2005-12-30",
  "2006-01-06", "2006-01-13", "2006-01-20", "2006-01-27", "2006-02-03", "2006-02-10", "2006-02-17", "2006-02-24",
  "2006-03-03", "2006-03-10", "2006-03-17", "2006-03-24", "2006-03-31", "2006-04-07", "2006-04-14", "2006-04-21",
  "2006-04-28", "2006-05-05", "2006-05-12", "2006-05-19", "2006-05-26", "2006-06-02", "2006-06-09", "2006-06-16",
  "2006-06-23", "2006-06-30", "2006-07-07", "2006-07-14", "2006-07-21", "2006-07-28", "2006-08-04", "2006-08-11",
  "2006-08-18", "2006-08-25", "2006-09-01", "2006-09-08", "2006-09-15", "2006-09-22", "2006-09-29", "2006-10-06",
  "2006-10-13", "2006-10-20", "2006-10-27", "2006-11-03", "2006-11-10", "2006-11-17", "2006-11-24", "2006-12-01",
  "2006-12-08", "2006-12-15", "2006-12-22", "2006-12-29",
  "2007-01-05", "2007-01-12", "2007-01-19", "2007-01-26", "2007-02-02", "2007-02-09", "2007-02-16", "2007-02-23",
  "2007-03-02", "2007-03-09", "2007-03-16", "2007-03-23", "2007-03-30", "2007-04-06", "2007-04-13", "2007-04-20",
  "2007-04-27", "2007-05-04", "2007-05-11", "2007-05-18", "2007-05-25", "2007-06-01", "2007-06-08", "2007-06-15",
  "2007-06-22", "2007-06-29", "2007-07-06", "2007-07-13", "2007-07-20", "2007-07-27", "2007-08-03", "2007-08-10",
  "2007-08-17", "2007-08-24", "2007-08-31", "2007-09-07", "2007-09-14", "2007-09-21", "2007-09-28", "2007-10-05",
  "2007-10-12", "2007-10-19", "2007-10-26", "2007-11-02", "2007-11-09", "2007-11-16", "2007-11-23", "2007-11-30",
  "2007-12-07", "2007-12-14", "2007-12-21", "2007-12-28",
  "2008-01-04", "2008-01-11", "2008-01-18", "2008-01-25", "2008-02-01", "2008-02-08", "2008-02-15", "2008-02-22",
  "2008-02-29", "2008-03-07", "2008-03-14", "2008-03-21", "2008-03-28", "2008-04-04", "2008-04-11", "2008-04-18",
  "2008-04-25", "2008-05-02", "2008-05-09", "2008-05-16", "2008-05-23", "2008-05-30", "2008-06-06", "2008-06-13",
  "2008-06-20", "2008-06-27", "2008-07-04", "2008-07-11", "2008-07-18", "2008-07-25", "2008-08-01", "2008-08-08",
  "2008-08-15", "2008-08-22", "2008-08-29", "2008-09-05", "2008-09-12", "2008-09-19", "2008-09-26", "2008-10-03",
  "2008-10-10", "2008-10-17", "2008-10-24", "2008-10-31", "2008-11-07", "2008-11-14", "2008-11-21", "2008-11-28",
  "2008-12-05", "2008-12-12", "2008-12-19", "2008-12-26",
  "2009-01-02", "2009-01-09", "2009-01-16", "2009-01-23", "2009-01-30", "2009-02-06", "2009-02-13", "2009-02-20",
  "2009-02-27", "2009-03-06", "2009-03-13", "2009-03-20", "2009-03-27", "2009-04-03", "2009-04-10", "2009-04-17",
  "2009-04-24", "2009-05-01", "2009-05-08", "2009-05-15", "2009-05-22", "2009-05-29", "2009-06-05", "2009-06-12",
  "2009-06-19", "2009-06-26", "2009-07-03", "2009-07-10", "2009-07-17", "2009-07-24", "2009-07-31", "2009-08-07",
  "2009-08-14", "2009-08-21", "2009-08-28", "2009-09-04", "2009-09-11", "2009-09-18", "2009-09-25", "2009-10-02",
  "2009-10-09", "2009-10-16", "2009-10-23", "2009-10-30", "2009-11-06", "2009-11-13", "2009-11-20", "2009-11-27",
  "2009-12-04", "2009-12-11", "2009-12-18", "2009-12-25",
  "2010-01-01", "2010-01-08", "2010-01-15", "2010-01-22", "2010-01-29", "2010-02-05", "2010-02-12", "2010-02-19",
  "2010-02-26", "2010-03-05", "2010-03-12", "2010-03-19", "2010-03-26", "2010-04-02", "2010-04-09", "2010-04-16",
  "2010-04-23", "2010-04-30", "2010-05-07", "2010-05-14", "2010-05-21", "2010-05-28", "2010-06-04", "2010-06-11",
  "2010-06-18", "2010-06-25", "2010-07-02", "2010-07-09", "2010-07-16", "2010-07-23", "2010-07-30", "2010-08-06",
  "2010-08-13", "2010-08-20", "2010-08-27", "2010-09-03", "2010-09-10", "2010-09-17", "2010-09-24", "2010-10-01",
  "2010-10-08", "2010-10-15", "2010-10-22", "2010-10-29", "2010-11-05", "2010-11-12", "2010-11-19", "2010-11-26",
  "2010-12-03", "2010-12-10", "2010-12-17", "2010-12-24", "2010-12-31",
  "2011-01-07", "2011-01-14", "2011-01-21", "2011-01-28", "2011-02-04", "2011-02-11", "2011-02-18", "2011-02-25",
  "2011-03-04", "2011-03-11", "2011-03-18", "2011-03-25", "2011-04-01", "2011-04-08", "2011-04-15", "2011-04-22",
  "2011-04-29", "2011-05-06", "2011-05-13", "2011-05-20", "2011-05-27", "2011-06-03", "2011-06-10", "2011-06-17",
  "2011-06-24", "2011-07-01", "2011-07-08", "2011-07-15", "2011-07-22", "2011-07-29", "2011-08-05", "2011-08-12",
  "2011-08-19", "2011-08-26", "2011-09-02", "2011-09-09", "2011-09-16", "2011-09-23", "2011-09-30", "2011-10-07",
  "2011-10-14", "2011-10-21", "2011-10-28", "2011-11-04", "2011-11-11", "2011-11-18", "2011-11-25", "2011-12-02",
  "2011-12-09", "2011-12-16", "2011-12-23", "2011-12-30",
  "2012-01-06", "2012-01-13", "2012-01-20", "2012-01-27", "2012-02-03", "2012-02-10", "2012-02-17", "2012-02-24",
  "2012-03-02", "2012-03-09", "2012-03-16", "2012-03-23", "2012-03-30", "2012-04-06", "2012-04-13", "2012-04-20",
  "2012-04-27", "2012-05-04", "2012-05-11", "2012-05-18", "2012-05-25", "2012-06-01", "2012-06-08", "2012-06-15",
  "2012-06-22", "2012-06-29", "2012-07-06", "2012-07-13", "2012-07-20", "2012-07-27", "2012-08-03", "2012-08-10",
  "2012-08-17", "2012-08-24", "2012-08-31", "2012-09-07", "2012-09-14", "2012-09-21", "2012-09-28", "2012-10-05",
  "2012-10-12", "2012-10-19", "2012-10-26", "2012-11-02", "2012-11-09", "2012-11-16", "2012-11-23", "2012-11-30",
  "2012-12-07", "2012-12-14", "2012-12-21", "2012-12-28",
  "2013-01-04", "2013-01-11", "2013-01-18", "2013-01-25", "2013-02-01", "2013-02-08", "2013-02-15", "2013-02-22",
  "2013-03-01", "2013-03-08", "2013-03-15", "2013-03-22", "2013-03-29", "2013-04-05", "2013-04-12", "2013-04-19",
  "2013-04-26", "2013-05-03", "2013-05-10", "2013-05-17", "2013-05-24", "2013-05-31", "2013-06-07", "2013-06-14",
  "2013-06-21", "2013-06-28", "2013-07-05", "2013-07-12", "2013-07-19", "2013-07-26", "2013-08-02", "2013-08-09",
  "2013-08-16", "2013-08-23", "2013-08-30", "2013-09-06", "2013-09-13", "2013-09-20", "2013-09-27", "2013-10-04",
  "2013-10-11", "2013-10-18", "2013-10-25", "2013-11-01", "2013-11-08", "2013-11-15", "2013-11-22", "2013-11-29",
  "2013-12-06", "2013-12-13", "2013-12-20", "2013-12-27",
  "2014-01-03", "2014-01-10", "2014-01-17", "2014-01-24", "2014-01-31", "2014-02-07", "2014-02-14", "2014-02-21",
  "2014-02-28", "2014-03-07", "2014-03-14", "2014-03-21", "2014-03-28", "2014-04-04", "2014-04-11", "2014-04-18",
  "2014-04-25", "2014-05-02", "2014-05-09", "2014-05-16", "2014-05-23", "2014-05-30", "2014-06-06", "2014-06-13",
  "2014-06-20", "2014-06-27", "2014-07-04", "2014-07-11", "2014-07-18", "2014-07-25", "2014-08-01", "2014-08-08",
  "2014-08-15", "2014-08-22", "2014-08-29", "2014-09-05", "2014-09-12", "2014-09-19", "2014-09-26", "2014-10-03",
  "2014-10-10", "2014-10-17", "2014-10-24", "2014-10-31", "2014-11-07", "2014-11-14", "2014-11-21", "2014-11-28",
  "2014-12-05", "2014-12-12", "2014-12-19", "2014-12-26",
  "2015-01-02", "2015-01-09", "2015-01-16", "2015-01-23", "2015-01-30", "2015-02-06", "2015-02-13", "2015-02-20",
  "2015-02-27", "2015-03-06", "2015-03-13", "2015-03-20", "2015-03-27", "2015-04-03", "2015-04-10", "2015-04-17",
  "2015-04-24", "2015-05-01", "2015-05-08", "2015-05-15", "2015-05-22", "2015-05-29", "2015-06-05", "2015-06-12",
  "2015-06-19", "2015-06-26", "2015-07-03", "2015-07-10", "2015-07-17", "2015-07-24", "2015-07-31", "2015-08-07",
  "2015-08-14", "2015-08-21", "2015-08-28", "2015-09-04", "2015-09-11", "2015-09-18", "2015-09-25", "2015-10-02",
  "2015-10-09", "2015-10-16", "2015-10-23", "2015-10-30", "2015-11-06", "2015-11-13", "2015-11-20", "2015-11-27",
  "2015-12-04", "2015-12-11", "2015-12-18", "2015-12-25",
  "2016-01-01", "2016-01-08", "2016-01-15", "2016-01-22", "2016-01-29", "2016-02-05", "2016-02-12", "2016-02-19",
  "2016-02-26", "2016-03-04", "2016-03-11", "2016-03-18", "2016-03-25", "2016-04-01", "2016-04-08", "2016-04-15",
  "2016-04-22", "2016-04-29", "2016-05-06", "2016-05-13", "2016-05-20", "2016-05-27", "2016-06-03", "2016-06-10",
  "2016-06-17", "2016-06-24", "2016-07-01", "2016-07-08", "2016-07-15", "2016-07-22", "2016-07-29", "2016-08-05",
  "2016-08-12", "2016-08-19", "2016-08-26", "2016-09-02", "2016-09-09", "2016-09-16", "2016-09-23", "2016-09-30",
  "2016-10-07", "2016-10-14", "2016-10-21", "2016-10-28", "2016-11-04", "2016-11-11", "2016-11-18", "2016-11-25",
  "2016-12-02", "2016-12-09", "2016-12-16", "2016-12-23", "2016-12-30",
  "2017-01-06", "2017-01-13", "2017-01-20", "2017-01-27", "2017-02-03", "2017-02-10", "2017-02-17", "2017-02-24",
  "2017-03-03", "2017-03-10", "2017-03-17", "2017-03-24", "2017-03-31", "2017-04-07", "2017-04-14", "2017-04-21",
  "2017-04-28", "2017-05-05", "2017-05-12", "2017-05-19", "2017-05-26", "2017-06-02", "2017-06-09", "2017-06-16",
  "2017-06-23", "2017-06-30", "2017-07-07", "2017-07-14", "2017-07-21", "2017-07-28", "2017-08-04", "2017-08-11",
  "2017-08-18", "2017-08-25", "2017-09-01", "2017-09-08", "2017-09-15", "2017-09-22", "2017-09-29", "2017-10-06",
  "2017-10-13", "2017-10-20", "2017-10-27", "2017-11-03", "2017-11-10", "2017-11-17", "2017-11-24", "2017-12-01",
  "2017-12-08", "2017-12-15", "2017-12-22", "2017-12-29",
  "2018-01-05", "2018-01-12", "2018-01-19", "2018-01-26", "2018-02-02", "2018-02-09", "2018-02-16", "2018-02-23",
  "2018-03-02" )

monthDates <- c("2000-01-31", "2000-02-29", "2000-03-31", "2000-04-28", "2000-05-31", "2000-06-30",
                "2000-07-31", "2000-08-31", "2000-09-29", "2000-10-31", "2000-11-30", "2000-12-29",
                "2001-01-31", "2001-02-28", "2001-03-30", "2001-04-30", "2001-05-31", "2001-06-29",
                "2001-07-31", "2001-08-31", "2001-09-28", "2001-10-31", "2001-11-30", "2001-12-31",
                "2002-01-31", "2002-02-28", "2002-03-29", "2002-04-30", "2002-05-31", "2002-06-28",
                "2002-07-31", "2002-08-30", "2002-09-30", "2002-10-31", "2002-11-29", "2002-12-31",
                "2003-01-31", "2003-02-28", "2003-03-31", "2003-04-30", "2003-05-30", "2003-06-30",
                "2003-07-31", "2003-08-29", "2003-09-30", "2003-10-31", "2003-11-28", "2003-12-31",
                "2004-01-30", "2004-02-27", "2004-03-31", "2004-04-30", "2004-05-31", "2004-06-30",
                "2004-07-30", "2004-08-31", "2004-09-30", "2004-10-29", "2004-11-30", "2004-12-31",
                "2005-01-31", "2005-02-28", "2005-03-31", "2005-04-29", "2005-05-31", "2005-06-30",
                "2005-07-29", "2005-08-31", "2005-09-30", "2005-10-31", "2005-11-30", "2005-12-30",
                "2006-01-31", "2006-02-28", "2006-03-31", "2006-04-28", "2006-05-31", "2006-06-30",
                "2006-07-31", "2006-08-31", "2006-09-29", "2006-10-31", "2006-11-30", "2006-12-29",
                "2007-01-31", "2007-02-28", "2007-03-30", "2007-04-30", "2007-05-31", "2007-06-29",
                "2007-07-31", "2007-08-31", "2007-09-28", "2007-10-31", "2007-11-30", "2007-12-31",
                "2008-01-31", "2008-02-29", "2008-03-31", "2008-04-30", "2008-05-30", "2008-06-30",
                "2008-07-31", "2008-08-29", "2008-09-30", "2008-10-31", "2008-11-28", "2008-12-31",
                "2009-01-30", "2009-02-27", "2009-03-31", "2009-04-30", "2009-05-29", "2009-06-30",
                "2009-07-31", "2009-08-31", "2009-09-30", "2009-10-30", "2009-11-30", "2009-12-31",
                "2010-01-29", "2010-02-26", "2010-03-31", "2010-04-30", "2010-05-31", "2010-06-30",
                "2010-07-30", "2010-08-31", "2010-09-30", "2010-10-29", "2010-11-30", "2010-12-31",
                "2011-01-31", "2011-02-28", "2011-03-31", "2011-04-29", "2011-05-31", "2011-06-30",
                "2011-07-29", "2011-08-31", "2011-09-30", "2011-10-31", "2011-11-30", "2011-12-30",
                "2012-01-31", "2012-02-29", "2012-03-30", "2012-04-30", "2012-05-31", "2012-06-29",
                "2012-07-31", "2012-08-31", "2012-09-28", "2012-10-31", "2012-11-30", "2012-12-31",
                "2013-01-31", "2013-02-28", "2013-03-29", "2013-04-30", "2013-05-31", "2013-06-28",
                "2013-07-31", "2013-08-30", "2013-09-30", "2013-10-31", "2013-11-29", "2013-12-31",
                "2014-01-31", "2014-02-28", "2014-03-31", "2014-04-30", "2014-05-30", "2014-06-30",
                "2014-07-31", "2014-08-29", "2014-09-30", "2014-10-31", "2014-11-28", "2014-12-31",
                "2015-01-30", "2015-02-27", "2015-03-31", "2015-04-30", "2015-05-29", "2015-06-30",
                "2015-07-31", "2015-08-31", "2015-09-30", "2015-10-30", "2015-11-30", "2015-12-31",
                "2016-01-29", "2016-02-29", "2016-03-31", "2016-04-29", "2016-05-31", "2016-06-30",
                "2016-07-29", "2016-08-31", "2016-09-30", "2016-10-31", "2016-11-30", "2016-12-30",
                "2017-01-31", "2017-02-28", "2017-03-31", "2017-04-28", "2017-05-31", "2017-06-30",
                "2017-07-31", "2017-08-31", "2017-09-29", "2017-10-31", "2017-11-30", "2017-12-29",
                "2018-01-31", "2018-02-28")

################### DATA DIAGNOSTICS ####################################

suspicious_data_level1 <- function(df) {
  # check for index level data suspicion. df is a single column data frame
  # First: If not numeric, return NOTNUMERIC
  # Second: If no rows: Return NODATA
  # Third: check for NA in series. If present return NA
  # Fourth: check for 0 in series, If present return TRUE
  # If no suspicion detected, return FALSE
  if(!is.numeric(df)) return("NOTNUMERIC")
  l <- length(df)
  if(l <= 0) return("NODATA")
  l1 <- length(na.omit(df))
  if(l1 < l) return("NA")
  l0 <- sum(na.omit(df) == 0)
  if(l0 > 0) return("TRUE") else return("FALSE")
}

suspicious_data_level2 <- function(df, make_returns = TRUE, n0 = .2, maxret = 2, maxvol = 1, maxskew = 10, maxkurt = 20) {
  # check for returns to find suspicionness in an index data series. df is a single column data frame holding indes values
  # If not numeric, return(NOTNUMERIC)
  # First, check for NA in the corresponding relative change series
  #   - 2) Then check for infinities
  #   - 3) Check for returns <= -1
  #   - 4) Check for returns >= maxret
  # Second, check for fraction of zeros in relative change series. These indicate no index value change.
  # If >= n0 * length of series, flag and return ZEROS (point is slow data updates)
  # Third, check for outliers
  # - 1) Excessive volatility (stdev > maxvol)
  # - 2) Extreme skewness (|skewness| > maxskew)
  # - 3) Extreme kurtosis (|kurtosis| > maxkurt)
  # If first check (NA) fails, return NA
  # - If first checK 2) fails, return INFINITY
  # - if first check 3) fails, return NEGRETURNS
  # - if first check 4) fails, return POSRETURNS
  # If second check (number of zeros) fails return ZEROS
  # If third check 1) fails, return VOLATILITY
  # If third check 2) fails, return SKEWNESS
  # If third check 3) fails, return KURTOSIS
  # Otherwise return FALSE, indicating no obvious suspicion
  # NOtice: moments are not robust, and they should not be. Similary, we do not care about precise degrees of freedom,
  # since we are looking only for extremes, not precision

  if(!is.numeric(df)) return("NOTNUMERIC")

  if(make_returns == FALSE) returns <- df
  else {
    returns <-df[1:length(df)-1]
    for(i in 1:length(returns))
      returns[i] <- df[i+1]/df[i] - 1
  }
  l <- length(returns)
  l1 <- length(na.omit(returns))
  if(l1 < l) return("NA")
  l1 <- sum(is.infinite(returns))
  if(l1 > 0) return("INFINITY")
  l1 <- sum(returns == 0)
  if (l1 >= n0 * l) return("ZEROS")
  minr <- min(returns)
  if(minr <= -1) return("NEGRETURNS")
  maxr <- max(returns)
  if(maxr >= maxret) return("POSRETURNS")
  # Now standard statistics
  mn <- mean(returns)

  std <- sd(returns)
  if(std >= maxvol) return("VOLATILITY")

  skewness <- sum((returns-mn)^3)/length(returns)/(std^3)
  if(abs(skewness) >= maxskew) return("SKEWNESS")

  kurtosis <- sum((returns-mn)^4)/length(returns)/std^4
  if(abs(kurtosis-3) >= maxkurt) return("KURTOSIS")

  return("FALSE")
}

find_sound_data <- function(pricedata) {
  oks <- c(1)
  for(i in 2:dim(pricedata)[2]){
    stat <- suspicious_data_level1(pricedata[,i])
    if(stat == FALSE)
      stat <- suspicious_data_level2(pricedata[,i], n0 = .1)
    if(stat == FALSE)
      oks <- c(oks, i)
  }
  return(oks)
}

keep_sound_pricedata <- function(pricedata, oks) {
  ok_pricedata <- pricedata[oks]
  return(ok_pricedata)
}

keep_sound_metadata <- function(metadata, oks) {
  ok_metadata <- metadata[oks[-1]-1,]
  return(ok_metadata)
}

########################## SMALL UTILITIES ###############################

get_average_over_time_returns <- function(df) {
  # Compute the average returns by asset
  means <- df[1,]
  for(i in 1:ncol(df)) {
    means[i] <- mean(df[,i])
  }
  return(means)
}

get_average_over_asset_returns <- function(df) {
  # Compute the average returns across assets
  means <- df[,1]
  for(i in 1:nrow(df)) {
    means[i] <- mean(df[i,])
  }
  return(means)
}

get_portfolio_wealth <- function(df) {
  # df should contain returns over time for the portfolio
  # Function returns wealth over time
  wealth <- df
  wealth[1] <- 1+df[1]
  for(i in 2:length(df)) {
    wealth[i] <- wealth[i-1]*(1+df[i])
    print(wealth[i])
  }
  return(wealth)
}


######################## OPTIMIZATION MODELS ##############################

risk_parity <- function(asset_returns, lower_limits, upper_limits) {
  #' @title Find the closest possible risk parity portfolio
  #'
  #' @description The risk parity portfolio has equal contributions to risk from each asset. When subject to allocation
  #' constraints, only an approximate risk parity portfolio can be found. That happens here
  #'
  #' @usage res <- risk_parity(asset_returns, lower_limits, upper_limits)
  #'
  #' @param asset_returns A data frame with times (down) and assets (across)
  #' @param lower_limits A list of lower bounds for asset holdings (typically rep(0, nasset))
  #' @param upper_limits A list of upper bounds for asset holdings (eg., rep(0.2, nasset))
  #'
  #' @return An nloptr object with information about the optimal solution
  #'
  #' @examples
  #'   rp <- risk_parity(asset_returns, rep(0, nasset), rep(.3, nasset))
  #'   weights <- rp$solution

  # Establish an all-weather portfolio subject to holdings restrictions
  covar <- cov(asset_returns)
  nasset <- ncol(asset_returns)

  # Objective function
  obj <- function(w) {
    risk_contributions <- w * covar %*% w
    mean_contribution <- mean(risk_contributions)
    ssq <- sum((risk_contributions-mean_contribution)^2)
    return(ssq)
  }

  # equality constraints - not used
  eval_g_eq <- function(w) {
    constr <- sum(w) - 1
    return(list("constraints" = constr, "jacobian" = rep(1, nasset)))
  }

  # inequality constraints
  eval_g_neq <- function(w) {
    constr <- c(sum(w) - 1, 1-sum(w))
    return(list("constraints" = constr, "jacobian" = c(rep(1, nasset), rep(-1, nasset))))
  }


  # initial values
  w0 <- rep(1/nasset, nasset)

  # options
  opts <- list("algorithm" = "NLOPT_LN_COBYLA", "maxeval" = 100000)

  res <- nloptr(x0=w0, eval_f = obj, lb = lower_limits, ub = upper_limits, eval_g_ineq = eval_g_neq, opts = opts)
  return(res)
}

risk_contributions <- function(optimization_result, covar) {
  #' @title Gives the relative risk contributions of assets in a portfolio
  #'
  #' @description The relative risk contribution of asset i equals the marginal risk contribution multiplied by the
  #' weight of i
  #'
  #' @usage res <- risk_contributions(optimization_restult, covariance_matrix)
  #'
  #' @param optimization_result A nloptr object from solving a model
  #' @param covar A covariance matrix of returns
  #'
  #' @return A list of relative contributions to portfolio risk (sums to 1)
  #'
  #' @examples
  #'   rp <- risk_parity(asset_returns, rep(0, nasset), rep(.3, nasset))
  #'   risk_contributions(rp, cov(asset_returns))


  w <- optimization_result$solution
  contributions <- w*covar %*% w
  sumcontributions <- sum(contributions)
  return(contributions/sumcontributions)
}


relative_markowitz <- function(asset_returns, bm_returns, expected_returns, lower_limits, upper_limits, max_te) {
  #' @title Make Markowitz optimization relative to a benchmark
  #'
  #' @description In contrast to normal Markowitz optimization, this model is based on excess returns relative to the
  #' benchmark, ie., not only expected returns but also the covariance matrix reflects excess returns.
  #'
  #' @usage res <- relative_markowitz(asset_returns, benchmark_returns, expected_returns, lower_limits, upper_limits, max_te)
  #'
  #' @param asset_returns A data frame with times (down) and assets (across)
  #' @param bm_returns A data frame column with times (down), and holding benchmark returns
  #' @param expected_returns Expected returns for the assets
  #' @param lower_limits A list of lower bounds for asset holdings (typically rep(0, nasset))
  #' @param upper_limits A list of upper bounds for asset holdings (eg., rep(0.2, nasset))
  #' @param max_te The maximum tracking error allowed
  #'
  #' @return A list of weights, expected return, tracking error, and solver status
  #'
  #' @examples
  #'   result <- relative_markowitz(asset_returns, bm_returns, rep(0, nasset), rep(.3, nasset), 2)
  #'   weights <- result$getValue(w)

  # generate relative returns
  relative_returns <- asset_returns
  nasset <- ncol(asset_returns)
  for(i in 1:nasset) {
    relative_returns[,i] <- asset_returns[,i] - bm_returns
  }

  # covariance matrix of excess returns
  covar <- cov(relative_returns)

  # Variables
  w <- Variable(ncol(asset_returns))

  # Expected (excess) return
  eret <- t(expected_returns %*% w)

  # Squared tracking error
  te2 <- quad_form(w, covar)

  # Objective: Maximize expected returns
  obj <- eret

  # constraints
  constr <- list(w >= lower_limits, w <= upper_limits, sum(w) == 1, te2 <= max_te^2)

  # Solve
  prob <- Problem(Maximize(obj), constr)
  result <- solve(prob)

  wgts <- list(result$getValue(w))
  rownames(wgts[[1]]) <- names(asset_returns)

  return(c("Weights" = wgts, "ExpectReturn" =result$getValue(eret), "Stdev" = sqrt(result$getValue(te2)), "OptStatus" = result$status))
}

markowitz <- function(asset_returns, expected_returns, lower_limits, upper_limits, max_risk) {
  #' @title Make Markowitz optimization
  #'
  #' @description Normal constrained Markowitz optimization
  #'
  #' @usage res <- markowitz(asset_returns, expected_returns, lower_limits, upper_limits, max_risk)
  #'
  #' @param asset_returns A data frame with times (down) and assets (across)
  #' @param expected_returns Expected returns for the assets
  #' @param lower_limits A list of lower bounds for asset holdings (typically rep(0, nasset))
  #' @param upper_limits A list of upper bounds for asset holdings (eg., rep(0.2, nasset))
  #' @param max_risk The maximum portfolio standard deviation allowed
  #'
  #' @return A list of weights, expected return, tracking error, and solver status
  #'
  #' @examples
  #'   result <- markowitz(asset_returns, rep(0, nasset), rep(.3, nasset), 2)
  #'   weights <- result$getValue(w)

  # generate relative returns
  nasset <- ncol(asset_returns)

  # covariance matrix of returns
  covar <- cov(asset_returns)

  # Variables
  w <- Variable(ncol(asset_returns))

  # Expected (excess) return
  eret <- t(expected_returns %*% w)

  # Squared tracking error
  te2 <- quad_form(w, covar)

  # Objective: Maximize expected returns
  obj <- eret

  # constraints
  constr <- list(w >= lower_limits, w <= upper_limits, sum(w) == 1, te2 <= max_risk^2)

  # Solve
  prob <- Problem(Maximize(obj), constr)
  result <- solve(prob)

  wgts <- list(result$getValue(w))
  rownames(wgts[[1]]) <- names(asset_returns)

    return(c("Weights" = wgts, "ExpectReturn" =result$getValue(eret), "Stdev" = sqrt(result$getValue(te2)), "OptStatus" = result$status))
}

oldmarkowitz <- function(asset_returns, expected_returns, lower_limits, upper_limits, gamma) {
  #' @title Make Markowitz optimization using trade-off between risk and return
  #'
  #' @description Normal constrained Markowitz optimization using trade-off between risk and return
  #'
  #' @usage res <- oldmarkowitz(asset_returns, expected_returns, lower_limits, upper_limits, gamma)
  #'
  #' @param asset_returns A data frame with times (down) and assets (across)
  #' @param expected_returns Expected returns for the assets
  #' @param lower_limits A list of lower bounds for asset holdings (typically rep(0, nasset))
  #' @param upper_limits A list of upper bounds for asset holdings (eg., rep(0.2, nasset))
  #' @param gamma Risk aversion parameter
  #'
  #' @return A list of weights, expected return, tracking error, and solver status
  #'
  #' @examples
  #'   result <- oldmarkowitz(asset_returns, rep(0, nasset), rep(.3, nasset), 0.5)
  #'   weights <- result$getValue(w)

  nasset <- ncol(asset_returns)

  # covariance matrix of returns
  covar <- cov(asset_returns)

  # Variables
  w <- Variable(ncol(asset_returns))

  # Expected (excess) return
  eret <- t(expected_returns %*% w)

  # Squared tracking error
  te2 <- quad_form(w, covar)

  # Objective: Maximize expected returns
  obj <- eret - gamma*te2

  # constraints
  constr <- list(w >= lower_limits, w <= upper_limits, sum(w) == 1)

  # Solve
  prob <- Problem(Maximize(obj), constr)
  result <- solve(prob)

  wgts <- list(result$getValue(w))
  rownames(wgts[[1]]) <- names(asset_returns)

  return(c("Weights" = wgts, "ExpectReturn" =result$getValue(eret), "Stdev" = sqrt(result$getValue(te2)), "OptStatus" = result$status))
}

max_diversification <- function(asset_returns, lower_limits, upper_limits) {
  #'
  #' @title Maximum diversification subject to holdings constraints
  #'
  #' @description Maximum diversification is obtained by maximizing the diversification ratio defined by the ratio of
  #' portfolio variance if all correlations are 0 to the portfolio variance at th given covariance matrix.
  #'
  #' @usage res <- max_diversification(asset_returns, lower_limits, upper_limits)
  #'
  #' @param asset_returns A data frame with times (down) and assets (across)
  #' @param lower_limits A list of lower bounds for asset holdings (typically rep(0, nasset))
  #' @param upper_limits A list of upper bounds for asset holdings (eg., rep(0.2, nasset))
  #'
  #' @return An nloptr object holding the solution
  #'
  #' @examples
  #'   rp <- max_diversification(asset_returns, rep(0, nasset), rep(.3, nasset))
  #'   weights <- rp$solution

  nasset <- ncol(asset_returns)
  # covariance matrix for asset_returns and assuming no correlation
  covar <- cov(asset_returns)
  covar0 <- diag(covar)

  # Objective function
  obj <- function(w) {
    pf_variance <- w %*% covar %*% w
    pf_variance0 <- covar0 %*% w^2
    diversification_ratio <- log(pf_variance0) - log(pf_variance)
    return(-diversification_ratio)
  }

  # equality constraints
  eval_g_eq <- function(w) {
    constr <- sum(w) - 1
    return(list("constraints" = constr, "jacobian" = rep(1, nasset)))
  }

  # equality constraints
  eval_g_neq <- function(w) {
    constr <- c(sum(w) - 1, 1-sum(w))
    return(list("constraints" = constr, "jacobian" = c(rep(1, nasset), rep(-1, nasset))))
  }
  # initial values
  w0 <- rep(1/nasset, nasset)

  # options & solve
  opts <- list("algorithm" = "NLOPT_LN_COBYLA", "maxeval" = 10000000)

  res <- nloptr(x0=w0, eval_f = obj, lb = lower_limits, ub = upper_limits, eval_g_ineq = eval_g_neq, opts = opts)
  return(res)
}

equal_weights <- function(asset_returns, lower_limits, upper_limits) {
  #' @title Find the closest possible equally weighted portfolio
  #'
  #' @description Constrained equally weighted portfolio
  #'
  #' @usage res <- equal_weights(asset_returns, lower_limits, upper_limits)
  #'
  #' @param asset_returns A data frame with times (down) and assets (across)
  #' @param lower_limits A list of lower bounds for asset holdings (typically rep(0, nasset))
  #' @param upper_limits A list of upper bounds for asset holdings (eg., rep(0.2, nasset))
  #'
  #' @return An nloptr object with information about the optimal solution
  #'
  #' @examples
  #'   rp <- equal_weights(asset_returns, rep(0, nasset), rep(.3, nasset))
  #'   weights <- result$getValue(w)

  nasset <- ncol(asset_returns)

  # Variables
  w <- Variable(nasset)

  equalweights <- rep(1/nasset, nasset)

  slack <- w - equalweights

  # Objective: Minimize sum of squared slacks
  obj <- sum(square(slack))

  # constraints
  constr <- list(w >= lower_limits, w <= upper_limits, sum(w) == 1)

  # Solve
  prob <- Problem(Minimize(obj), constr)
  result <- solve(prob)

  wgts <- list(result$getValue(w))
  rownames(wgts[[1]]) <- names(asset_returns)

  return(c("Weights" = wgts, "OptStatus" = result$status))
}

########################## PORTFOLIO RESULT UTILITIES
get_portfolio_returns <- function(returns, weights) {
  #' @title Compute historical portfolio returns
  #'
  #' @description Given historical asset returns andd portfolio weights, compute portfolio returns
  #'
  #' @usage res <- get_portfolio_returns(returns, weights)
  #'
  #' @param returns A dataframe with returns for a number of securities
  #' @param weights Weights of assets in the portfolio
  #'
  #' @return A data frame of returns for the portfolio
  #'
  #' @examples
  #'   ret <- get_portfolio_returns(asset_returns, portfolio_weights)
  #'

  df <- returns[,1]
  rets <- returns %*% weights
  df[,1] <- rets
  names(df) <- "Portfolio"
  return(df)
}

get_portfolio_wealth <- function(portfolio_returns) {
  #' @title Construct a historical total return index given portfolio returns
  #'
  #' @description Given historical portfolio returns, compute the corresponding wealth development
  #'
  #' @usage res <- get_portfolio_wealth(portfolio_returns)
  #'
  #' @param portfolio_returns A dataframe with returns for the portfolio
  #'
  #' @return A data frame of wealth for the portfolio
  #'
  #' @examples
  #'   ret <- get_portfolio_wealth(portfolio_returns)
  #'
  #'
  wealth <- portfolio_returns
  wealth[1] <- 1+portfolio_returns[[1]]
  for(i in 2:length(portfolio_returns))
    wealth[i] = wealth[i-1]*(1+portfolio_returns[[i]])
  return(wealth)
}

get_portfolio_drawdowns <- function(portfolio_wealth) {
  #' @title Compute historical wealth drawdowns for a portfolio
  #'
  #' @description Given historical portfolio wealth development, compute drawdowns
  #'
  #' @usage res <- get_portfolio_drawdowns(portfolio_wealth)
  #'
  #' @param portfolio_wealth A dataframe with historical portfolio wealth
  #'
  #' @return A data frame of drawdowns
  #'
  #' @examples
  #' pf_returns <- get_portfolio_returns(asset_returns, weights)
  #' pf_wealth <- get_portfolio_wealth(pf_returns)
  #' dd <- get_portfolio_drawdowns(pf_wealth)
  #' max_drawdown <- 1 - min(dd)
  #'

  dd <- portfolio_wealth
  max <- portfolio_wealth[[1]]
  for(i in 1:length(portfolio_wealth)) {
    if(portfolio_wealth[i] > max)
      max <- portfolio_wealth[[i]]
    dd[i] <- portfolio_wealth[[i]]/max
  }
  return(dd)
}

generate_portfolio_returns <- function(returns, weights) {
  pfreturns <- returns[,1]
  for(i in 1:length(pfreturns)) {
    ret <- 0
    for(j in 1:length(returns[1,])) {
      ret <- ret + weights[j,1]*returns[i,j]
    }
    pfreturns[i] <- ret
  }
  return(pfreturns)
}

portfolio_risk_contributions <- function(weights, asset_returns) {
  #' @title Compute the relative risk contributions of assets in a portfolio
  #'
  #' @description Compute the relative risk contributions of assets in a portfolio
  #'
  #' @usage res <- portfolio_risk_contributions(portfolio_weights, asset_returns)
  #'
  #' @param portfolio_weights Weights of assets in portfolio
  #' @param asset_returns A dataframe with returns for the assets
  #'
  #' @return A data frame of risk contributions for the assets in the portfolio
  #'
  #' @examples
  #'   ret <- portfolio_risk_contributions(portfolio_weights, asset_returns)
  #'
  vcv <- cov(asset_returns)
  pfvar <- weights %*% vcv %*% weights
  pfv <- pfvar[1,1]
  marg <- vcv %*% weights
  contrib <- weights * marg/pfv
  return(contrib)
}

################ UNIFICATION OF ALGORITHM RETURNS #########################
get_portfolio_optimization_weights <- function(model, result, variablenames = NULL) {
  # Get the solution optimal weights.
  # The format of these depends on the model used (input), since this may be covered by CVXR or NLOPTR.
  # The input result is the optimization output from the relevant model
  if(model == 'Markowitz' | model == "MeanVar" | model == "EW") {
    df <- as.data.frame(result$Weights[,1])
    names(df) <- "weights"
    return(df)
  }
  else {
    df <- as.data.frame(result$solution)
    names(df) <- c("weights")
    rownames(df) <- variablenames
    return(df)
  }
}
