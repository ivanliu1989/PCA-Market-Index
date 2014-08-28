setwd<-'C:\\Documents and Settings\\Macro\\Desktop\\Ivandata\\PCA-Market-Index'
prices <- read.csv('data/stock_prices.csv')
head(prices)
require(lubridate)
price <- transform(prices, Date=ymd(Date))
require(reshape)
date.stock.matrix <- cast(prices,Date~Stock,value='Close')
head(date.stock.matrix)
