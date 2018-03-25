suppressMessages(library(xts))
suppressMessages(library(quantmod))
suppressMessages(library(sqldf))
suppressMessages(library(magrittr))
suppressMessages(library(data.table))
suppressMessages(library(RCurl))
suppressMessages(library(repmis))
options("scipen"=100, "digits"=4)

my_functions <- getURL("https://raw.githubusercontent.com/JoeOD/miranda/master/backtesting/_functions.R", ssl.verifypeer = FALSE)
eval(parse(text = my_functions))

for_csv_analysis <- data.frame()
results_list <- list()
summary_list <- list()
PandL_list <- list()
doom_names <- list()

df_amex <- read.csv(text=getURL("https://raw.githubusercontent.com/JoeOD/miranda-data/master/temp/amex.csv"), skip=7, header=T)
df_nasdaq <- read.csv(text=getURL("https://raw.githubusercontent.com/JoeOD/miranda-data/master/temp/nasdaq.csv"), skip=7, header=T)
df_nyse <- read.csv(text=getURL("https://raw.githubusercontent.com/JoeOD/miranda-data/master/temp/nyse.csv"), skip=7, header=T)

#listcsv <- source_data("https://github.com/JoeOD/miranda-data/blob/master/listcsv1.csv?raw=true")
listcsv <- source_data("https://raw.githubusercontent.com/JoeOD/miranda-data/master/alldataframe.csv")
listcsv <- as.character(listcsv[,2])

for (i in 1:length(listcsv)) { 

  ticker <- gsub("^\\s+|\\s+$", "", listcsv[i])
  ticker <- gsub(".csv", "", ticker)
  #sector <- find_sector(paste(paste('\'', toupper(ticker), sep=''), '\'', sep=''))
  to_name <- gsub(".csv", "", listcsv)
  
  #Import Script to Run
  script <- getURL("https://raw.githubusercontent.com/JoeOD/miranda/master/backtesting/19-optimize-AWS1.R", ssl.verifypeer = FALSE)
  eval(parse(text = script))
  
  #---Create Lists of Results---------------------------
  
  #Create Summary List Object
  summary_list[[i]] <- for_csv_analysis
  
  #Create Proft and Loss List
  PandL_list[[i]] <- PandL_csv
  
  #Create Optimization Results List
  results_list[[i]] <- results
  
  #Create List of Portfolio Names
  doom_names[[i]] <- master_names
}

#----Find Optimizations worth Looking Into--------------
doom_names_final <- rbindlist(doom_names)
doom_names_final <- as.character(doom_names_final$port_names_all)
DoomFrame <- rbindlist(summary_list) %>% as.matrix()
DoomFrame <- cbind(DoomFrame, doom_names_final)
PotInvestments <- DoomFrame[DoomFrame[,"AdjPvalsBY"] < .05 & DoomFrame[,"Adj_Sharpe"] > 0,]

if(!nrow(PotInvestments)){
  print("No potential investments discovered for any ticker")
} else {
  print(paste(nrow(PotInvestments), " ", "potential investment(s) discovered!", sep = ""))
}