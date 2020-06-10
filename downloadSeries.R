library(readr)

#Ambev
ambev = read.csv("https://query1.finance.yahoo.com/v7/finance/download/ABEV3.SA?period1=947030400&period2=1591747200&interval=1d&events=history",stringsAsFactors = FALSE,
                 na.strings = c("NA", "N/A", "Unknown*", "null", ".P"))
ambev = ambev$Close
ambev<-ambev[!is.na(ambev)]

#Petrobras
petro = read.csv("https://query1.finance.yahoo.com/v7/finance/download/PETR4.SA?period1=947030400&period2=1591747200&interval=1d&events=history",stringsAsFactors = FALSE,
                 na.strings = c("NA", "N/A", "Unknown*", "null", ".P"))
petro = petro$Close
petro<-petro[!is.na(petro)]

#Vale
vale = read.csv("https://query1.finance.yahoo.com/v7/finance/download/VALE3.SA?period1=946857600&period2=1591574400&interval=1d&events=history",stringsAsFactors = FALSE,
                na.strings = c("NA", "N/A", "Unknown*", "null", ".P"))
vale = vale$Close
vale<-vale[!is.na(vale)]

#Bradesco
brad = read.csv("https://query1.finance.yahoo.com/v7/finance/download/BBDC4.SA?period1=1199232000&period2=1591747200&interval=1d&events=history",,stringsAsFactors = FALSE,
                na.strings = c("NA", "N/A", "Unknown*", "null", ".P"))
brad = brad$Close
brad<-brad[!is.na(brad)]

#Itaú
itau = read.csv("https://query1.finance.yahoo.com/v7/finance/download/ITUB4.SA?period1=977356800&period2=1591660800&interval=1d&events=history",,stringsAsFactors = FALSE,
                na.strings = c("NA", "N/A", "Unknown*", "null", ".P"))
itau = itau$Close
itau<-itau[!is.na(itau)]


#Ibovespa
ibov = read.csv("https://query1.finance.yahoo.com/v7/finance/download/%5EBVSP?period1=735868800&period2=1591747200&interval=1d&events=history",,stringsAsFactors = FALSE,
                na.strings = c("NA", "N/A", "Unknown*", "null", ".P"))
ibov = ibov$Close
ibov<-ibov[!is.na(ibov)]

