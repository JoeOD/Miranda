#' Copyright (C) 2011-2014 Guy Yollin
#' License: http://www.gnu.org/licenses/gpl.html GPL version 2 or higher
#' http://www.r-programming.org/papers
checkBlotterUpdate <- function(port.st = portfolio.st,
                               account.st = account.st,
                               verbose = TRUE) {

    ok <- TRUE
    p <- getPortfolio(port.st)
    a <- getAccount(account.st)
    syms <- names(p$symbols)
    port.tot <- sum(
        sapply(
            syms,
            FUN = function(x) eval(
                parse(
                    text = paste("sum(p$symbols",
                                 x,
                                 "posPL.USD$Net.Trading.PL)",
                                 sep = "$")))))

    port.sum.tot <- sum(p$summary$Net.Trading.PL)

    if(!isTRUE(all.equal(port.tot, port.sum.tot))) {
        ok <- FALSE
        if(verbose) print("portfolio P&L doesn't match sum of symbols P&L")
    }

    initEq <- as.numeric(first(a$summary$End.Eq))
    endEq <- as.numeric(last(a$summary$End.Eq))

    if(!isTRUE(all.equal(port.tot, endEq - initEq)) ) {
        ok <- FALSE
        if(verbose) print("portfolio P&L doesn't match account P&L")
    }

    if(sum(duplicated(index(p$summary)))) {
        ok <- FALSE
        if(verbose)print("duplicate timestamps in portfolio summary")

    }

    if(sum(duplicated(index(a$summary)))) {
        ok <- FALSE
        if(verbose) print("duplicate timestamps in account summary")
    }
    return(ok)
}

#' Copyright (C) 2011-2014 Guy Yollin
#' License: http://www.gnu.org/licenses/gpl.html GPL version 2 or higher
#' http://www.r-programming.org/papers
osFixedDollar <- function(timestamp, orderqty, portfolio, symbol, ruletype, ...) {
    if(!exists("trade_size")) stop("You must set trade_size")
    ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
    orderqty <- round(trade_size/ClosePrice,-2)
    return(orderqty)
}

#Function for finding BayesPv
BayesPv <- function(RAWt, PriorOdds){
  MinBayesFactor <- exp((-RAWt^2)/2)
  bpv <- (c(MinBayesFactor) * PriorOdds)/(1+c(MinBayesFactor)*PriorOdds)
  outputs <- list(MinBayesFactor, bpv)
  output_labels <- c("Minimum Bayes Factor", "Bayesianized p-value")
  names(outputs) <- output_labels
  return(outputs)
}

#Sharpe function for use by pbo function
sharpe <- function(x,rf=0.03/252) {
  sr <- apply(x,2,function(col) {
    er = col - rf
    return(mean(er)/sd(er))
  })
  return(sr)
}

#Creates list of dataframes from multiple optimizations with adjusted p-values

MasterAdj <- function(AllSumLists, filename = NULL){

  #Bind together dataframes with same names

  master_list <- lapply(split(AllSumLists,sub('.*\\.', '', names(AllSumLists))), function(x) do.call(rbind, x))

  #Adjust p-values for multiple testing---

  for (a in length(master_list)){
    pvals <- master_list[[a]]$`Non-adj p-value`
    adjusted <- p.adjust(pvals, method = "BY")
    AdjPvalsBY <- data.frame(adjusted)
    master_list[[a]]$AdjPvalsBY <- NULL
    master_list[[a]] <- cbind(master_list[[a]], AdjPvalsBY)
  }

  #Option to save results to disk--------
  if(!is.null(filename)){
    save(master_list, file = filename)}

  #Return results------------------------
  return(master_list)
}

#Calculates mode (central tendency)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' ---------------------------------------------------------------------
#' ---------------------------------------------------------------------
#' ------------------Perfeci Finance Custom Functions-------------------
#' ---------------------------------------------------------------------
#' ---------------------------------------------------------------------
find_sector <- function(ticker) {
  sector <- "No Match"

  if (nrow(sqldf(paste("SELECT Sector FROM df_nasdaq where LTRIM(RTRIM(Symbol)) = ",
    ticker, sep = "")))) {
    sector <- sqldf(paste("SELECT Sector FROM df_nasdaq where LTRIM(RTRIM(Symbol)) = ",
      ticker, sep = ""))[1]$Sector
  } else if (nrow(sqldf(paste("SELECT Sector FROM df_nyse where LTRIM(RTRIM(Symbol)) = ",
    ticker, sep = "")))) {
    sector <- sqldf(paste("SELECT Sector FROM df_nyse where LTRIM(RTRIM(Symbol)) = ",
      ticker, sep = ""))[1]$Sector
  } else if (nrow(sqldf(paste("SELECT Sector FROM df_amex where LTRIM(RTRIM(Symbol)) = ",
    ticker, sep = "")))) {
    sector <- sqldf(paste("SELECT Sector FROM df_amex where LTRIM(RTRIM(Symbol)) = ",
      ticker, sep = ""))[1]$Sector
  }

  return(sector)
}

getData <- function(ticker){
  dataAPI <- read.zoo(paste("~/printing-press-data/av/data/success", ticker, sep='/'), header = TRUE, sep = ",", FUN = function(x) as.Date(x, origin = "1899-12-30"))
  dataXTS <- as.xts(dataAPI)
  return (dataXTS)
}

buyAndHoldReturn <- function(ticker, dateTestingStart, dateTestingFinish){
  data <- getData(ticker)
  data <- data[paste(dateTestingStart, dateTestingFinish, sep='/' )]
  return  ((as.numeric(data[nrow(data)]$adjusted_close) - as.numeric(data[1]$adjusted_close))/as.numeric(data[1]$adjusted_close))
}

strategyReturn <- function(inPosition, positionQty, startingCap, workingCap, dataASXTS){
  if(inPosition){
    workingCap <- workingCap + (as.numeric(dataASXTS[nrow(dataASXTS)]$adjusted_close) * positionQty)
  }
  return ((workingCap - startingCap)/startingCap)
}

#MarketSubsetter
MarketSubsetter <- function(MaxPriceNow = Inf, MaxStdDev = Inf, MinStdDev = 0, startD = "N/A"
                            , endD ="N/A", Directory = FALSE, Savelist = FALSE, SaveAs = "N/A"
                            , BySector = FALSE, MySector = "N/A", RiskLevel = ""){

  market <- data.frame()
  listcsv <- dir(path = data_source, pattern = "*.csv")

  #------Option to Use Entire Data Repo for Backtesting------

  if (Directory == TRUE){
    assign("listcsv", listcsv, envir = .GlobalEnv)
    break
  }

  #------------------Find Desirable Tickers------------------

  for (p in 1:length(listcsv)) {

    #Establish
    ticker <- gsub("^\\s+|\\s+$", "", listcsv[p])
    ticker <- gsub(".csv", "", ticker)
    sector <- find_sector(paste(paste('\'', toupper(ticker), sep=''), '\'', sep=''))

    #Check if csv is empty
    info = file.info(paste(data_source, paste(ticker, ".csv", sep=""), sep = ""))
    empty = rownames(info[info$size < 10, ])

    #Skip csv if empty
    if (!is.na(empty[1])){
      next
    }

    #Prep and pull custom data
    toDate <- function(x) as.Date(x, origin = "1899-12-30")
    date_range = paste(startD, endD, sep = "/")

    dataCSV <- read.zoo(paste(data_source, paste(ticker, ".csv", sep=""), sep = ""), header = TRUE, sep = ",", FUN = toDate)
    desired_range <- as.xts(dataCSV)[date_range]

    #Skip empty dataframes and tickers that don't meet requirements
    if (length(desired_range) == 0){
      next
    } else if (index(first(desired_range)) != startD){
      next
    } else if (index(last(desired_range)) != endD){
      next
    } else if (last(desired_range$adjusted_close) > MaxPriceNow){
      next
    } else if (round(sd(desired_range$adjusted_close), digits = 2) > MaxStdDev){
      next
    } else if (round(sd(desired_range$adjusted_close), digits = 2) < MinStdDev){
      next
    }

    #Create objects to populate market object
    sd_price <- round(sd(desired_range$adjusted_close), digits = 2)
    last_price <- last(desired_range$adjusted_close)
    nrow_data <- nrow(desired_range)
    temp_row_of_data <- data.frame(ticker, sector, nrow_data, sd_price, last_price)
    colnames(temp_row_of_data) <- c("Ticker", "sector", "Num of Rows", "Std Dev Adj Close", "Last Adj Close")

    #Create/modify market object
    if (!nrow(market)) {
      market <- temp_row_of_data
    } else {
      market <- rbind(market, temp_row_of_data)
    }
  }
  
  #Subset market object to only include tickers with mode number of rows
  market <- market[market$`Num of Rows` == Mode(market$`Num of Rows`),]

  #Option to select for risk/volatility level
  MarketRisk <- quantile(market$`Std Dev Adj Close`, c(.05, .32, .68, .95))

  if (RiskLevel == "very low"){
  market <- market[market$`Std Dev Adj Close` < MarketRisk[[1]],]
  } else if (RiskLevel == "low"){
    market <- market[market$`Std Dev Adj Close` > MarketRisk[[1]] & market$`Std Dev Adj Close` < MarketRisk[[2]],]
  } else if (RiskLevel == "moderate"){
    market <- market[market$`Std Dev Adj Close` > MarketRisk[[2]] & market$`Std Dev Adj Close` < MarketRisk[[3]],]
  } else if (RiskLevel == "high"){
    market <- market[market$`Std Dev Adj Close` > MarketRisk[[3]] & market$`Std Dev Adj Close` < MarketRisk[[4]],]
  } else if (RiskLevel == "very high"){
    market <- market[market$`Std Dev Adj Close` > MarketRisk[[4]],]
  }

  #Option to select a sector
  if (BySector == TRUE){
    market <- market[market$sector %in% MySector,]
  }

  if(nrow(market) == 0){
    stop("No tickers meet criteria")
  }

  rownames(market) <- 1:nrow(market)

  #--------------------------Output--------------------------

  rm(listcsv)
  listcsv <- paste(as.character(market[,1]), ".csv", sep = "")

  #Save list of tickers that meet requirements option
  if (Savelist == TRUE){
    save(listcsv, file = paste(data_dump, SaveAs, ".RData", sep = ""))
  }

  assign("listcsv", listcsv, envir = .GlobalEnv)
  assign("market", market, envir = .GlobalEnv)
}

#Directory subsetter
DirectorySubsetter <- function(MaxPriceNow = Inf, MaxStdDev = Inf, MinStdDev = 0, startD = "N/A"
                            , endD ="N/A", Directory = FALSE, Savelist = FALSE, SaveAs = "N/A"
                            , BySector = FALSE, MySector = "N/A", RiskLevel = ""){
  
  market <- data.frame()
  listcsv <- dir(path = data_source, pattern = "*.csv")
  
  #------------------Find Desirable Tickers------------------
  
  for (p in 1:length(listcsv)) {
    
    #Establish
    ticker <- gsub("^\\s+|\\s+$", "", listcsv[p])
    ticker <- gsub(".csv", "", ticker)
    sector <- find_sector(paste(paste('\'', toupper(ticker), sep=''), '\'', sep=''))
    
    #Check if csv is empty
    info = file.info(paste(data_source, paste(ticker, ".csv", sep=""), sep = ""))
    empty = rownames(info[info$size < 10, ])
    
    #Skip csv if empty
    if (!is.na(empty[1])){
      next
    }
    
    #Prep and pull custom data
    toDate <- function(x) as.Date(x, origin = "1899-12-30")
    date_range = paste(startD, endD, sep = "/")
    
    dataCSV <- read.zoo(paste(data_source, paste(ticker, ".csv", sep=""), sep = ""), header = TRUE, sep = ",", FUN = toDate)
    desired_range <- as.xts(dataCSV)[date_range]
    
    #Skip empty dataframes and tickers that don't meet requirements
    if (length(desired_range) == 0){
      next
    } else if (index(first(desired_range)) != startD){
      next
    } else if (index(last(desired_range)) != endD){
      next
    } else if (last(desired_range$adjusted_close) > MaxPriceNow){
      next
    } else if (round(sd(desired_range$adjusted_close), digits = 2) > MaxStdDev){
      next
    } else if (round(sd(desired_range$adjusted_close), digits = 2) < MinStdDev){
      next
    }
    
    #Create objects to populate market object
    sd_price <- round(sd(desired_range$adjusted_close), digits = 2)
    last_price <- last(desired_range$adjusted_close)
    nrow_data <- nrow(desired_range)
    temp_row_of_data <- data.frame(ticker, sector, nrow_data, sd_price, last_price)
    colnames(temp_row_of_data) <- c("Ticker", "sector", "Num of Rows", "Std Dev Adj Close", "Last Adj Close")
    
    #Create/modify market object
    if (!nrow(market)) {
      market <- temp_row_of_data
    } else {
      market <- rbind(market, temp_row_of_data)
    }
  }
  
  #Subset market object to only include tickers with mode number of rows
  market <- market[market$`Num of Rows` == Mode(market$`Num of Rows`),]
  
  #Option to select for risk/volatility level
  MarketRisk <- quantile(market$`Std Dev Adj Close`, c(.05, .32, .68, .95))
  
  if (RiskLevel == "very low"){
    market <- market[market$`Std Dev Adj Close` < MarketRisk[[1]],]
  } else if (RiskLevel == "low"){
    market <- market[market$`Std Dev Adj Close` > MarketRisk[[1]] & market$`Std Dev Adj Close` < MarketRisk[[2]],]
  } else if (RiskLevel == "moderate"){
    market <- market[market$`Std Dev Adj Close` > MarketRisk[[2]] & market$`Std Dev Adj Close` < MarketRisk[[3]],]
  } else if (RiskLevel == "high"){
    market <- market[market$`Std Dev Adj Close` > MarketRisk[[3]] & market$`Std Dev Adj Close` < MarketRisk[[4]],]
  } else if (RiskLevel == "very high"){
    market <- market[market$`Std Dev Adj Close` > MarketRisk[[4]],]
  }
  
  #Option to select a sector
  if (BySector == TRUE){
    market <- market[market$sector %in% MySector,]
  }
  
  if(nrow(market) == 0){
    stop("No tickers meet criteria")
  }
  
  rownames(market) <- 1:nrow(market)
  
  #--------------------------Output--------------------------
  
  rm(listcsv)
  listcsv <- paste(as.character(market[,1]), ".csv", sep = "")
  listcsvforcopy <- 
  
  #Save list of tickers that meet requirements option
  if (Savelist == TRUE){
    save(listcsv, file = paste(data_dump, SaveAs, ".RData", sep = ""))
  }
  assign("listcsv", listcsv, envir = .GlobalEnv)
  assign("market", market, envir = .GlobalEnv)
  
  for(i in 1:length(listcsv)){
    file.copy(from = paste(data_source, listcsv[i], sep = ""), to = paste(data_target, listcsv[i], sep = ""))
  }
}

###
getTickerList <- function(startDate, endDate){
  listcsv <- dir(path="~/printing-press-data/av/data/success", pattern = "*.csv")
  return (listcsv)
}

dataFrame <- function(){
  #new_row <- data.frame(
  #  "test_id" = testUUID,
  #  "ticker" = ticker,
  #  "baseLine_ticker" = baseLineTicker,
  #  "starting_cap" = startingCap,
  #  "date_testing_start" = dateTestingStart,
  #  "date_testing_finish" = dateTestingFinish,
  #  "fast_type" = fastType,
  #  "fast_value" = fastValue,
  #  "slow_type" = slowType,
  #  "slow_value" = slowValue,
  #  "in_position" = inPosition,
  #  "position_qty" = positionQty,
  #  "strategy" = A_strategy,
  #  "strategy_buy_and_hold" = A_strategyBuyAndHOld,
  #  "baseline_buy_and_hold" = A_baselineBuyAndHold
  #)
}
