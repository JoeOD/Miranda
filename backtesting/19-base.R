suppressMessages(library(quantstrat))
suppressMessages(library(knitr))
suppressMessages(library(SharpeR))
data_source <- "~/printing-press-data/av/data/success/"

#Load custom functions
source("~/printing-press/backtesting/_functions.R")

#Setup test variables
if(exists('ticker')){
  tickerToTest <- ticker
}else{
  tickerToTest <- "call"
}

init_date <- "2005-01-03"
start_date = "2005-01-04"
end_date = "2018-03-01"
init_equity <- 10000

toDate <- function(x) as.Date(x, origin = "1899-12-30")
date_range = paste(start_date, end_date, sep = "/")

# Prep and pull custom data
dataCSV <- read.zoo(paste(data_source, paste(tickerToTest, ".csv", sep=""), sep = ""), header = TRUE, sep = ",", FUN = toDate)
desired_range <- as.xts(dataCSV)[date_range]
colnames(desired_range)[4] <- "Price"
tickerToTest <- paste(tickerToTest, "_stock", sep = "")
assign(tickerToTest, desired_range)

#---------------------------------------------------------
#-----------------Start Startegy Backtest-----------------
#---------------------------------------------------------
portfolio.st <- "19-base.Port" 
account.st <- "19-base.Acct" 
strategy.st <- "19-base.Strat" 
adjustment <- TRUE
orderQty <- 100
SMAn <- 10
.threshold <- .0005
.txnfees <- -(orderQty*.005)
Sys.setenv(TZ = "UTC" )
currency("USD")
stock(tickerToTest,
      currency = "USD",
      multiplier = 1 ) 
rf <- (.03/252)

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(name = portfolio.st,
          symbols = tickerToTest,
          initDate = init_date)

initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)

initOrders(portfolio = portfolio.st,
           symbols =  tickerToTest,
           initDate = init_date)

strategy(strategy.st, store = TRUE)

add.indicator(strategy = strategy.st,
              name = "SMA", 
              arguments = list(x = quote(mktdata[,c("adjusted_close")]),
                               n = SMAn), 
              label = "nSlow") 

add.signal(strategy = strategy.st,
           name= "sigCrossover", 
           arguments = list(columns = c("adjusted_close", "nSlow"), relationship = "gt"),
           label = "filter")

add.signal(strategy.st, name="sigCrossover",
           arguments=list(columns=c("adjusted_close", "nSlow"), relationship="lt"),
           label="filterExit")

add.rule(strategy = strategy.st,
         name = "ruleSignal", 
         arguments = list(sigcol = "filter", 
                          sigval = TRUE,
                          orderqty = orderQty,
                          ordertype = "stoplimit", 
                          orderside = "long",
                          threshold = .threshold, 
                          prefer = "High", 
                          TxnFees = .txnfees, 
                          replace = FALSE), 
         type = "enter", 
         label = "EnterLONG") 

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "filterExit",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = .txnfees,
                          replace = TRUE),
         type = "exit",
         label = "Exitfilter")

# Run the strategy
results <- applyStrategy(strategy.st, portfolios = portfolio.st, verbose = TRUE)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)

#---------------------------------------------------------
#-----------------End Startegy Backtest-------------------
#---------------------------------------------------------

#---------------------------------------------------------
#----------------Start Buy-Hold Backtest------------------
#---------------------------------------------------------
rm.strat("BuyHold.Port")
rm.strat("BuyHold.Acct")

BuyHold <- "BuyHold"
BuyHoldPort <- "BuyHold.Port"
BuyHoldAcct <- "BuyHold.Acct"

if(!is.null(tradeStats(portfolio.st, inclZeroDays = TRUE))) {
  initPortf(BuyHoldPort, tickerToTest, initDate = init_date)
  initAcct(BuyHoldAcct, portfolios = BuyHoldPort,
           initDate = init_date, initEq = init_equity)
  
  CurrentDate <- time(getTxns(Portfolio = portfolio.st, Symbol = tickerToTest))[2]
  equity = getEndEq(BuyHoldAcct, CurrentDate)
  ClosePrice <- as.numeric(Cl(mktdata[CurrentDate,]))
  UnitSize = as.numeric(trunc(equity/ClosePrice))
  addTxn(BuyHoldPort, Symbol = tickerToTest, TxnDate = CurrentDate, TxnPrice = ClosePrice,
         TxnQty = UnitSize, TxnFees = .txnfees)
  
  LastPrice <- as.numeric(Cl(mktdata[end_date,]))
  addTxn(BuyHoldPort, Symbol = tickerToTest, TxnDate = end_date, TxnPrice = LastPrice,
         TxnQty = -UnitSize , TxnFees = .txnfees)
  
  updatePortf(Portfolio = BuyHoldPort)
  updateAcct(name = BuyHoldAcct)
  updateEndEq(Account = BuyHoldAcct)
}
#---------------------------------------------------------
#----------------End Buy-Hold Backtest--------------------
#---------------------------------------------------------

#---------------------------------------------------------
#-------------------Report Results------------------------
#---------------------------------------------------------

profitAmount <- "N/A"
profitPercentage <- "N/A"
profitAmountBH <- "N/A"
profitPercentageBH <- "N/A"

if(!is.null(tradeStats(portfolio.st, inclZeroDays = TRUE))) {
  
  #------------------Strategy Results------------------
  
  #Trade stats and returns
  tstats <- tradeStats(portfolio.st, inclZeroDays = TRUE)
  rets <- PortfReturns(account.st)
  dstats <- dailyStats(portfolio.st)
  
  profitAmount <- tstats$Net.Trading.PL
  profitPercentage <- (tstats$Net.Trading.PL/init_equity)
  CumReturns <- Return.cumulative(rets, geometric = FALSE)
  
  #Profit and loss series
  PLSeries <-  getAccount(account.st) 
  PLSeries <- PLSeries$summary[,8] 
  PLSeriesRf <- PLSeries - rf
  
  #Risk adjusted return metrics
  SharpeDataRawNonAn <- as.sr(PLSeries, ope = 1, c0 = rf) 
  SharpeDataRawAn <- as.sr(PLSeries, ope = 252, c0 = rf) 
  SharpeRawSigData <- sr_test(PLSeriesRf, zeta=0, alternative="two.sided")
  ValueAtR <- VaR(rets, method = "historical")
  ValueAtR[2] <- VaR(rets, method = "modified")
  CondValueAtR <- CVaR(rets, method = "historical")
  CondValueAtR[2] <- CVaR(rets, method = "modified")
  
  #Bayesianized p-value calculations
  BayesianPs <- BayesPv(RAWt = SharpeRawSigData$statistic, PriorOdds = c(1,4,99))
  
  #Subtract annualized sharpe ratio out of tstats object
  tstats_no_ann_sharpe <- tstats[-c(20,21,22,23)]
  
  #Print out stats
  print(paste("### Results for", strategy.st,"###"))
  print(paste("Cash made: $", round(profitAmount, digits = 2), sep=""))
  print(paste("Cumulative returns: ", Return.cumulative(rets, geometric = FALSE), sep=""))
  print(paste("Profit percentage: ", round((profitPercentage*100), digits = 2), "%",sep=""))
  print(paste("Non-adjusted, non-annualized Sharpe ratio:", SharpeDataRawNonAn$sr[1], sep=" "))
  print(paste("Non-adjusted, annualized Sharpe ratio:", SharpeDataRawAn$sr[1], sep=" "))
  print(paste("Non-adjusted t-stat: ", SharpeRawSigData$statistic, sep=" "))
  print(paste("Non-adjusted p-value: ", SharpeRawSigData$p.value, sep=" "))
  print(paste("df: ", SharpeRawSigData$parameter, sep=" "))
  print(paste("Bayesian p-value w/ 1:1 odds, i.e. strategy on solid-footing:", BayesianPs$`Bayesianized p-value`[1], sep=" "))
  print(paste("Bayesian p-value w/ 4:1 odds, i.e. strategy perhaps true:", BayesianPs$`Bayesianized p-value`[2], sep=" "))
  print(paste("Bayesian p-value w/ 99:1 odds, i.e. strategy a stretch :", BayesianPs$`Bayesianized p-value`[3], sep=" "))
  print(paste("Minimum Bayes factor:", BayesianPs$`Minimum Bayes Factor`, sep=" "))
  print(paste("Value-at-Risk (historical):", ValueAtR[1]))
  print(paste("Value-at-Risk (modified):", ValueAtR[2]))
  print(paste("Conditional Value-at-Risk (historical)", CondValueAtR[1]))
  print(paste("Conditional Value-at-Risk (modified)", CondValueAtR[2]))
  print(kable(t(tstats_no_ann_sharpe)))
  print(kable(t(dstats[c(5,6,11)])))
  print(noquote("______________________________"))
  
  #------------------Buy-Hold Results------------------
  
  if(!is.null(tradeStats(BuyHoldPort, inclZeroDays = TRUE))) {
    
    #Trade stats and returns
    tstatsBuyHold <- tradeStats(BuyHoldPort, inclZeroDays = TRUE)
    retsBuyHold <- PortfReturns(BuyHoldAcct)
    
    profitAmountBH <- tstatsBuyHold$Net.Trading.PL
    profitPercentageBH <- (tstatsBuyHold$Net.Trading.PL/init_equity)
    
    #Profit and loss series
    PLSeriesBH <-  getAccount(BuyHoldAcct) 
    PLSeriesBH <- PLSeriesBH$summary[,8] 
    PLSeriesBHRf <- PLSeriesBH - rf
    
    #Risk adjusted return metrics
    SharpeDataRawAnBH <- as.sr(PLSeriesBHRf, ope = 252) 
    
    #Print out stats
    print(paste("### Results for", BuyHold, "###"))
    print(paste("Cash made: $", round(profitAmountBH, digits = 2), sep=""))
    print(paste("Cumulative returns: ", Return.cumulative(retsBuyHold, geometric = FALSE), sep=""))
    print(paste("Profit percentage: ", round((profitPercentageBH*100), digits = 2), "%",sep=""))
    print(paste("Non-adjusted, annualized Sharpe ratio:", SharpeDataRawAnBH$sr[1], sep=" "))
  }
  #----------------------------------------------------
}else{
  print(profitAmount)
  print(profitPercentage)
  
  print(profitAmountBH)
  print(profitPercentageBH)
}