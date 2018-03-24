suppressMessages(library(xts))
suppressMessages(library(quantmod))
suppressMessages(library(sqldf))
suppressMessages(library(magrittr))
suppressMessages(library(data.table))
options("scipen"=100, "digits"=4)

source("~/printing-press/backtesting/_functions.R")

scriptID <- "19-optimize"
data_source <- '~/printing-press-data/av/data/success/'
data_meta_source <- '~/printing-press-data/av/data/temp/'
data_dump <- "~/printing-press/backtesting/_output/optimization/"
data_results <- "~/printing-press-results/"
for_csv_analysis <- data.frame()
results_list <- list()
summary_list <- list()
PandL_list <- list()
doom_names <- list()

df_amex <- read.csv(paste(data_meta_source, "amex.csv", sep = ""), header = TRUE)
df_nasdaq <- read.csv(paste(data_meta_source, "nasdaq.csv", sep = ""), header = TRUE)
df_nyse <- read.csv(paste(data_meta_source, "nyse.csv", sep = ""), header = TRUE)

MarketSubsetter(startD = "2005-01-04", endD = "2018-03-02", RiskLevel = "")

for (i in 1:length(listcsv)) { 

  ticker <- gsub("^\\s+|\\s+$", "", listcsv[i])
  ticker <- gsub(".csv", "", ticker)
  sector <- find_sector(paste(paste('\'', toupper(ticker), sep=''), '\'', sep=''))
  to_name <- gsub(".csv", "", listcsv)
  
  #Import Script to Run
  source(paste("~/printing-press/backtesting/",scriptID,".R", sep=""))
  
  #---Create Lists of Results---------------------------
  
  #Create Summary List Object
  summary_list[[i]] <- for_csv_analysis
  
  #Create Proft and Loss List
  PandL_list[[i]] <- PandL_csv
  
  #Create Optimization Results List
  results_list[[i]] <- results
  
  #Create List of Portfolio Names
  doom_names[[i]] <- master_names
  
  #----Save Results------------------------------------
  if (i == length(listcsv)){
    #Save Strategy
    my_strategy <- get(as.character(strategy.st), pos = .strategy, inherits = TRUE)
    save(my_strategy, file = paste(data_dump, paste(strategy.st,"-","Market-Backtest-Optimize-Strategy-", start_date,"-to-",end_date,"-","Run-",Sys.Date(),".RData", sep = ""), sep = ""))
    
    #Save Summary Output aka List of for_csv_analysis Objects
    names(summary_list) <- to_name
    save(summary_list, file = paste(data_dump, paste(strategy.st,"-","Market-Backtest-Optimize-Summary-List","-", start_date,"-to-",end_date,"-","Run-",Sys.Date(),".RData", sep = "")))
    
    #Save P&L Output aka List of PandL_csv Objects
    save(PandL_list, file = paste(data_dump, paste(strategy.st,"-","Market-Backtest-Optimize-P&L-List","-", start_date,"-to-",end_date,"-","Run-",Sys.Date(),".RData", sep = "")))
    
    #Save Optimization Results List
    save(results_list, file = paste(data_dump, paste(strategy.st,"-","Market-Backtest-Optimize-Results-List","-", start_date,"-to-",end_date,"-","Run-",Sys.Date(),".RData", sep = "")))
  }
}

#----Find Optimizations worth Looking Into--------------
doom_names_final <- rbindlist(doom_names)
doom_names_final <- as.character(doom_names_final$port_names_all)
DoomFrame <- rbindlist(summary_list) %>% as.matrix()
DoomFrame <- cbind(DoomFrame, doom_names_final)
PotInvestments <- DoomFrame[DoomFrame[,"AdjPvalsBY"] < .05 & DoomFrame[,"Adj_Sharpe"] > 0,]
save(DoomFrame, file = paste(data_dump, paste(strategy.st,"-","Market-Backtest-Optimize-Doom-Frame","-", start_date,"-to-",end_date,"-","Run-",Sys.Date(),".RData", sep = ""))) 
write.table(DoomFrame, file = paste(paste(data_results, substring(scriptID, 1, 2),"/", sep = ""),substring(scriptID, 1, 2),"-Market-Backtest-Optimize-Doom-"
                                         , start_date,"-to-",end_date,"-","Run-",Sys.Date(),".csv", sep = ""), row.names = FALSE)

if(nrow(PotInvestments) > 0){
  save(PotInvestments, file = paste(data_dump, paste(strategy.st,"-","Market-Backtest-Optimize-Potential-Investments","-", start_date,"-to-",end_date,"-","Run-",Sys.Date(),".RData", sep = "")))
  write.table(PotInvestments, file = paste(paste(data_results, substring(scriptID, 1, 2),"/", sep = ""),substring(scriptID, 1, 2) ,"-Market-Backtest-Optimize-Potential-Investments-"
                             , start_date,"-to-",end_date,"-","Run-",Sys.Date(),".csv", sep = ""), row.names = FALSE)
} 

if(!nrow(PotInvestments)){
  print("No potential investments discovered for any ticker")
} else {
  print(paste(nrow(PotInvestments), " ", "potential investment(s) discovered!", sep = ""))
}