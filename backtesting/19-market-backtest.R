suppressMessages(library(xts))
suppressMessages(library(quantmod))
suppressMessages(library(sqldf))
options("scipen"=100, "digits"=4)

source("~/printing-press/backtesting/_functions.R")

scriptID <- "19-base"
data_source <- '~/printing-press-data/av/data/success/'
data_meta_source <- '~/printing-press-data/av/data/temp/'
data_dump <- "~/printing-press/backtesting/_output/base/"
data_results <- "~/printing-press-results/"
for_csv_analysis <- data.frame()
PandL_csv <- data.frame()
Portfolio_list <- list()
Account_list <- list()
Portfolio_listBH <- list()
Account_listBH <- list()

df_amex <- read.csv(paste(data_meta_source, "amex.csv", sep = ""), header = TRUE)
df_nasdaq <- read.csv(paste(data_meta_source, "nasdaq.csv", sep = ""), header = TRUE)
df_nyse <- read.csv(paste(data_meta_source, "nyse.csv", sep = ""), header = TRUE)

MarketSubsetter(startD = "2005-01-03", endD = "2018-03-02", RiskLevel = "")

for (k in 1:length(listcsv)) { 
  
  ticker <- gsub("^\\s+|\\s+$", "", listcsv[k])
  ticker <- gsub(".csv", "", ticker)
  sector <- find_sector(paste(paste('\'', toupper(ticker), sep=''), '\'', sep=''))
  
  #Import Script to Run
  source(paste("~/printing-press/backtesting/",scriptID,".R", sep=""))

  if(!is.null(tradeStats(portfolio.st, inclZeroDays = TRUE))) { 
    #----Populate Data Structure for summary CSV----------
    temp_row_of_data <- data.frame(tickerToTest, sector, profitPercentage, CumReturns, ValueAtR[1], ValueAtR[2], CondValueAtR[1], CondValueAtR[2], SharpeDataRawNonAn$sr[1], SharpeDataRawAn$sr[1]
                                   , SharpeRawSigData$statistic, SharpeRawSigData$p.value, SharpeRawSigData$parameter, BayesianPs$`Bayesianized p-value`[1], BayesianPs$`Bayesianized p-value`[2], BayesianPs$`Bayesianized p-value`[3]
                                    , BayesianPs$`Minimum Bayes Factor`, tstats_no_ann_sharpe$Num.Txns, tstats_no_ann_sharpe$Num.Trades, tstats_no_ann_sharpe$Net.Trading.PL
                                    , tstats_no_ann_sharpe$Avg.Trade.PL, tstats_no_ann_sharpe$Med.Trade.PL, tstats_no_ann_sharpe$Largest.Winner, tstats_no_ann_sharpe$Largest.Loser
                                    , tstats_no_ann_sharpe$Gross.Profits, tstats_no_ann_sharpe$Gross.Losses, tstats_no_ann_sharpe$Std.Dev.Trade.PL, tstats_no_ann_sharpe$Percent.Positive
                                    , tstats_no_ann_sharpe$Percent.Negative, tstats_no_ann_sharpe$Profit.Factor, tstats_no_ann_sharpe$Avg.Win.Trade, tstats_no_ann_sharpe$Med.Win.Trade
                                    , tstats_no_ann_sharpe$Avg.Losing.Trade, tstats_no_ann_sharpe$Med.Losing.Trade, dstats$Avg.Day.PL, dstats$Med.Day.PL
                                    , dstats$Std.Dev.Daily.PL, tstats_no_ann_sharpe$Max.Drawdown, tstats_no_ann_sharpe$Profit.To.Max.Draw, tstats_no_ann_sharpe$Avg.WinLoss.Ratio
                                    , tstats_no_ann_sharpe$Med.WinLoss.Ratio, tstats_no_ann_sharpe$Max.Equity, tstats_no_ann_sharpe$Min.Equity, tstats_no_ann_sharpe$End.Equity, "N/A", "N/A", "N/A", "N/A")
  
    colnames(temp_row_of_data) <- c("Ticker", "Sector", "Profit Pct","Cumul Returns", "VaR Hist", "VaR Mod", "Con VaR Hist", "Con VaR Mod", "Non-adj, non-ann Sharpe", "Non-adj, ann Sharpe", "Non-adj t-stat", "Non-adj p-value", "df"
                                    , "Bayesian p-value w/ 1:1 odds", "Bayesian p-value w/ 4:1 odds", "Bayesian p-value w/ 99:1 odds", "Minimum Bayes Factor"
                                    , "Num of txns", "Num of trades", "Net Trading PL", "Avg Trade PL", "Med Trade PL", "Largest winner", "Largest loser", "Gross profits", "Gross losses"
                                    , "Sd Trade PL", "Pct trades pos", "Pct trades neg", "Profit factor", "Avg win trade", "Med win trade", "Avg losing trade", "Med losing trade"
                                    , "Avg daily PL", "Med daily PL", "SD daily PL", "Max drawdown", "Profit to max dd", "Avg WinLoss ratio", "Med WinLoss ratio", "Max equity"
                                    , "Min equity", "End Equity", "AdjPvalsBY", "Adj_Sharpe", "Adj_tstat", "doom_names_final")
    
    if (!nrow(for_csv_analysis)) {
      for_csv_analysis <- temp_row_of_data
    } else {
      for_csv_analysis <- rbind(for_csv_analysis, temp_row_of_data)
      rownames(for_csv_analysis) <- 1:nrow(for_csv_analysis)
    }
  
    #----Populate Data Structure for P&L CSV-------------
    temp_col_of_data <- data.frame(PLSeries)
    colnames(temp_col_of_data) <- c(ticker)
    
    if (!nrow(PandL_csv)) {
      PandL_csv <- temp_col_of_data
    } else {
      PandL_csv <- cbind(PandL_csv, temp_col_of_data)
    }
    #----Create Lists of Results-------------------------
    
    #Strategy Portfolio List
    Port_to_save <- getPortfolio(portfolio.st)
    Portfolio_list[[k]] <- Port_to_save
    
    #Strategy Account List)
    Acct_to_save <- getAccount(account.st)
    Account_list[[k]] <- Acct_to_save
    
    #Buy-Hold Portfolio List
    Port_to_saveBH <- getPortfolio(BuyHoldPort)
    Portfolio_listBH[[k]] <- Port_to_saveBH
    
    #Buy-Hold Account List
    Acct_to_saveBH <- getAccount(BuyHoldAcct)
    Account_listBH[[k]] <- Acct_to_saveBH
  }
  
  #----Save Results--------------------------------------
  if(k == length(listcsv)){
    
    #Save Strategy
    my_strategy <- get(as.character(strategy.st), pos = .strategy, inherits = TRUE)
    save(my_strategy, file = paste(data_dump, paste(strategy.st,"-","Market-Backtest-", start_date,"-to-",end_date,"-","Run-",Sys.Date(),".RData", sep = ""), sep = ""))
    
    #Save Strategy Portfolio List
    save(Portfolio_list, file = paste(data_dump, paste(portfolio.st,"-list","-","Market-Backtest-", start_date,"-to-",end_date,"-","Run-",Sys.Date(),".RData", sep = ""), sep = ""))

    #Save Strategy Account List
    save(Account_list, file = paste(data_dump, paste(account.st,"-list","-","Market-Backtest-", start_date,"-to-",end_date,"-","Run-",Sys.Date(),".RData", sep = ""), sep = ""))
    
    #Save Buy-Hold Portfolio List
    save(Portfolio_listBH, file = paste(data_dump, paste(scriptID,"-", BuyHoldPort,"-","BuyHold","- list","-","Market-Backtest-", start_date,"-to-",end_date,"-","Run-",Sys.Date(),".RData", sep = ""), sep = ""))
    
    #Save Buy-Hold Account List
    save(Account_listBH, file = paste(data_dump, paste(scriptID,"-", BuyHoldAcct,"-","BuyHold","- list","-","Market-Backtest-", start_date,"-to-",end_date,"-","Run-",Sys.Date(),".RData", sep = ""), sep = ""))
    
    #Save Summary of Results
    save(for_csv_analysis, file = paste(data_dump,scriptID,".R","-", "Market-Backtest-Summary-",start_date,"-to-", end_date,"-","Run-",Sys.Date(),".RData", sep = ""))
    write.table(for_csv_analysis, file = paste(paste(data_results, substring(scriptID, 1, 2),"/", sep = ""),substring(scriptID, 1, 2) ,"-Market-Backtest-Doom-", start_date,"-to-",end_date,"-","Run-",Sys.Date(),".csv", sep = ""), row.names = FALSE)
    
    if(nrow(for_csv_analysis) > 0){
      if(nrow(for_csv_analysis[for_csv_analysis[,"Non-adj p-value"] < .05 & for_csv_analysis[,"Non-adj, ann Sharpe"] > 0,]) > 0){
        write.table(for_csv_analysis[for_csv_analysis[,"Non-adj p-value"] < .05 & for_csv_analysis[,"Non-adj, ann Sharpe"] > 0,]
                    , file = paste(paste(data_results, substring(scriptID, 1, 2),"/", sep = ""),substring(scriptID, 1, 2) ,"-Market-Backtest-Potential-Investments-"
                                   , start_date,"-to-",end_date,"-","Run-",Sys.Date(),".csv", sep = ""), row.names = FALSE)
      }
    }
    #Save Profit and Loss Series
    save(PandL_csv, file = paste(data_dump,scriptID,".R-Market-Backtest-P&Ls-",start_date,"-to-", end_date,"-","Run-",Sys.Date(),".RData", sep = ""))
  }
}

#Report findings
if(!nrow(for_csv_analysis[for_csv_analysis[,"Non-adj p-value"] < .05 & for_csv_analysis[,"Non-adj, ann Sharpe"] > 0,])){
  print("No potential investments discovered for any ticker")
} else {
  print(paste(nrow(for_csv_analysis[for_csv_analysis[,"Non-adj p-value"] < .05 & for_csv_analysis[,"Non-adj, ann Sharpe"] > 0,]), " ", "potential investment(s) discovered!", sep = ""))
}