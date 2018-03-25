suppressMessages(library(quantstrat))
suppressMessages(library(knitr))
suppressMessages(library(SharpeR))
data_source <- "~/printing-press-data/av/data/success/"

#Load Custom Functions
my_functions <- getURL("https://raw.githubusercontent.com/JoeOD/miranda/master/backtesting/_functions.R", ssl.verifypeer = FALSE)
eval(parse(text = my_functions))

#Enable Parallel Processing
library(parallel)
detectCores()

if( Sys.info()['sysname'] == "Windows" )
{
  library(doParallel)
  registerDoParallel(cores=detectCores())
} else {
  library(doMC)
  registerDoMC(cores=detectCores())
}

#Setup Test Variables
if(exists('ticker')){
  tickerToTest <- ticker
}else{
  tickerToTest <- "a" 
}

init_date <- "2005-01-03"
start_date = "2005-01-04"
end_date = "2018-03-02"
init_equity <- 10000

toDate <- function(x) as.Date(x, origin = "1899-12-30")
date_range = paste(start_date, end_date, sep = "/")

# Prep and Pull Custom Data
#dataCSV <- read.zoo(text=getURL("https://raw.githubusercontent.com/perfeci/printing-press-data/master/av/data/success/a.csv?token=AY-o9eyFSA4owC5dBf7o9TGUZS10KZLJks5avDBdwA%3D%3D"), header = TRUE, sep = ",", FUN = toDate)
dataCSV <- read.zoo(text=getURL(paste("https://raw.githubusercontent.com/JoeOD/miranda-data/master/success4/", tickerToTest, ".csv", sep = "")), header = TRUE, sep = ",", FUN = toDate)
desired_range <- as.xts(dataCSV)[date_range]
colnames(desired_range)[4] <- "Price"
tickerToTest <- paste(tickerToTest, "_stock", sep = "")
assign(tickerToTest, desired_range)

#---------------------------------------------------------
#-----------------Start Startegy Optimization-------------
#---------------------------------------------------------
portfolio.st <- "19-optimize.R-Port"
account.st <- "19-optimize.R-Acct"
strategy.st <- "19-optimize.R-Strat"
orderQty <- 100
SMAn <- 10
.slowSMA <- (10:13)
.nsamples <- 4
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

add.distribution(strategy.st,
                 paramset.label = "SMA",
                 component.type = "indicator",
                 component.label = "nSlow",
                 variable = list(n = .slowSMA),
                 label = "SMA")

results <- apply.paramset(strategy.st, paramset.label = "SMA", portfolio.st = portfolio.st,
                          account.st = account.st, nsamples = .nsamples, audit = .blotter, verbose = TRUE)

updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)

results <- results[lapply(results,length)>2]

#---------------------------------------------------------
#-----------------End Startegy backtest-------------------
#---------------------------------------------------------

#---------------------------------------------------------
#-------------------Report Results------------------------
#---------------------------------------------------------

PandL_csv <- data.frame()
for_csv_analysis <- data.frame()
port_names_all <- c()
master_names <- data.frame()

if (length(results) > 0){
  
  if (!is.null(tradeStats(paste("portfolio.", results[[1]]$portfolio.st, sep = ""), inclZeroDays = TRUE))){
    
    for (k in 1:length(results)) {
      
      if (k == 2){
        next
      }
      
      if(!is.null(tradeStats(paste("portfolio.", results[[k]]$portfolio.st, sep = ""), inclZeroDays = TRUE))){
        
        #----------------Optimization Results------------------
        
        #Subset out relevant results from list of all results
        results_sub <- results[k] 
        port_name <- paste("portfolio.", results_sub[[1]]$portfolio.st, sep = "")
        port_names_all[k] <- paste(results[[k]]$portfolio.st, sep = "")
        port <- getPortfolio(port_name)
        
        #Trade stats and returns
        tstats <- tradeStats(port_name, inclZeroDays = TRUE)
        rets <- "N/A"
        dstats <- dailyStats(port_name)
        
        profitAmount <- tstats$Net.Trading.PL
        profitPercentage <- (tstats$Net.Trading.PL/init_equity)
        CumReturns <- "N/A"
        
        #Profit and loss series
        PLSeries <- port$summary$Net.Trading.PL
        PLSeriesRf <- PLSeries - rf
        
        #Risk adjusted return metrics
        SharpeDataRawNonAn <- as.sr(PLSeries, ope = 1, c0 = rf)
        SharpeDataRawAn <- as.sr(PLSeries, ope = 252, c0 = rf) 
        SharpeRawSigData <- sr_test(PLSeriesRf, zeta=0, alternative="two.sided")
        ValueAtR <- "N/A"
        ValueAtR[2] <- "N/A"
        CondValueAtR <- "N/A"
        CondValueAtR[2] <- "N/A"
        
        #Bayesianized p-value calculations
        BayesianPs <- BayesPv(RAWt = SharpeRawSigData$statistic, PriorOdds = c(1,4,99))
        
        #Subtract annualized sharpe ratio out of tstats object
        tstats_no_ann_sharpe <- tstats[-23]
        
        #----Populate Data Structure for summary CSV----------
        sector = "N/A"
        
        temp_row_of_data <- data.frame(results_sub[[1]]$tradeStats$Symbol, sector, profitPercentage, CumReturns, ValueAtR[1], ValueAtR[2], CondValueAtR[1], CondValueAtR[2], SharpeDataRawNonAn$sr[1], SharpeDataRawAn$sr[1]
                                       , SharpeRawSigData$statistic, SharpeRawSigData$p.value, SharpeRawSigData$parameter, BayesianPs$`Bayesianized p-value`[1], BayesianPs$`Bayesianized p-value`[2], BayesianPs$`Bayesianized p-value`[3]
                                       , BayesianPs$`Minimum Bayes Factor`, tstats_no_ann_sharpe$Num.Txns, tstats_no_ann_sharpe$Num.Trades, tstats_no_ann_sharpe$Net.Trading.PL
                                       , tstats_no_ann_sharpe$Avg.Trade.PL, tstats_no_ann_sharpe$Med.Trade.PL, tstats_no_ann_sharpe$Largest.Winner, tstats_no_ann_sharpe$Largest.Loser
                                       , tstats_no_ann_sharpe$Gross.Profits, tstats_no_ann_sharpe$Gross.Losses, tstats_no_ann_sharpe$Std.Dev.Trade.PL, tstats_no_ann_sharpe$Percent.Positive
                                       , tstats_no_ann_sharpe$Percent.Negative, tstats_no_ann_sharpe$Profit.Factor, tstats_no_ann_sharpe$Avg.Win.Trade, tstats_no_ann_sharpe$Med.Win.Trade
                                       , tstats_no_ann_sharpe$Avg.Losing.Trade, tstats_no_ann_sharpe$Med.Losing.Trade, dstats$Avg.Day.PL, dstats$Med.Day.PL
                                       , dstats$Std.Dev.Daily.PL, tstats_no_ann_sharpe$Max.Drawdown, tstats_no_ann_sharpe$Profit.To.Max.Draw, tstats_no_ann_sharpe$Avg.WinLoss.Ratio
                                       , tstats_no_ann_sharpe$Med.WinLoss.Ratio, tstats_no_ann_sharpe$Max.Equity, tstats_no_ann_sharpe$Min.Equity, tstats_no_ann_sharpe$End.Equity)
        
        colnames(temp_row_of_data) <- c("Ticker", "Sector", "Profit Pct","Cumul Returns", "VaR Hist", "VaR Mod", "Con VaR Hist", "Con VaR Mod", "Non-adj, non-ann Sharpe", "Non-adj, ann Sharpe", "Non-adj t-stat", "Non-adj p-value", "df"
                                        , "Bayesian p-value w/ 1:1 odds", "Bayesian p-value w/ 4:1 odds", "Bayesian p-value w/ 99:1 odds", "Minimum Bayes Factor"
                                        , "Num of txns", "Num of trades", "Net Trading PL", "Avg Trade PL", "Med Trade PL", "Largest winner", "Largest loser", "Gross profits", "Gross losses"
                                        , "Sd Trade PL", "Pct trades pos", "Pct trades neg", "Profit factor", "Avg win trade", "Med win trade", "Avg losing trade", "Med losing trade"
                                        , "Avg daily PL", "Med daily PL", "SD daily PL", "Max drawdown", "Profit to max dd", "Avg WinLoss ratio", "Med WinLoss ratio", "Max equity"
                                        , "Min equity", "End Equity")
        
        if (!nrow(for_csv_analysis)) {
          for_csv_analysis <- temp_row_of_data
        } else {
          for_csv_analysis <- rbind(for_csv_analysis, temp_row_of_data)
        }
        
        #----Populate Data Structure for P&L CSV--------------
        
        temp_col_of_data <- data.frame(PLSeries)
        
        colnames(temp_col_of_data) <- paste(results_sub[[1]]$portfolio.st,"-", results[[1]]$tradeStats$Symbol)
        
        if (!nrow(PandL_csv)) {
          PandL_csv <- temp_col_of_data
        } else {
          PandL_csv <- cbind(PandL_csv, temp_col_of_data)
        }
      }  
    }  
  }  
}

if (nrow(for_csv_analysis) > 0){
  port_names_all <- port_names_all[-2]
  row.names(for_csv_analysis) <- port_names_all[!is.na(port_names_all)]
  
  master_names <- data.frame(port_names_all)
  
  #Adjust p-values for multiple testing using Benjamini & Yekutieli method
  pvals <- for_csv_analysis$`Non-adj p-value`
  AdjPvalsBY <- p.adjust(pvals, method = "BY")
  AdjPvalsBY <- as.data.frame(AdjPvalsBY)
  for_csv_analysis <- cbind(for_csv_analysis, AdjPvalsBY)
  Adj_tstat <- (qt(p=(for_csv_analysis$AdjPvalsBY/2), df = for_csv_analysis$df[1]))
  Adj_Sharpe <- (qt(p=(for_csv_analysis$AdjPvalsBY/2), df = for_csv_analysis$df[1]))/(sqrt(for_csv_analysis$df[1]/252))
  for_csv_analysis <- cbind(for_csv_analysis, Adj_Sharpe)
  for_csv_analysis <- cbind(for_csv_analysis, Adj_tstat)
  
  #Report findings
  if(!nrow(for_csv_analysis[for_csv_analysis[,"AdjPvalsBY"] < .05 & for_csv_analysis[,"Adj_Sharpe"] > 0,])){
    print(paste("No potential investments discovered for", tickerToTest, sep = " "))
  } else {
    print(paste(nrow(for_csv_analysis[for_csv_analysis[,"AdjPvalsBY"] < .05 & for_csv_analysis[,"Adj_Sharpe"] > 0,]), " ", "potential investments discovered!", sep = ""))
  }
}