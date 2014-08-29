setwd<-'C:\\Documents and Settings\\Macro\\Desktop\\Ivandata\\PCA-Market-Index'
prices <- read.csv('data/stock_prices.csv')
head(prices)
require(lubridate)
price <- transform(prices, Date=ymd(Date))
require(reshape)
date.stock.matrix <- cast(prices,Date~Stock,value='Close')
head(date.stock.matrix)
prices <- subset(prices, Date != ymd('2002-02-01'))
prices <- subset(prices, Stock != 'DDR')
date.stock.matrix <- cast(prices, Date~Stock, value='Close')

# find correlations
cor.matrix <- cor(date.stock.matrix[,2:ncol(date.stock.matrix)])
correlations <- as.numeric(cor.matrix)
require(ggplot2)
p <- ggplot(data.frame(Correlation=correlations), aes(x=Correlation, fill=1))
p + geom_density() + theme(legend.position='none') + ggsave('cor.png')

# pca
pca <- princomp(date.stock.matrix[,2:ncol(date.stock.matrix)])
pca
principal.component <- pca$loadings[,1]
loadings <- as.numeric(principal.component)
p2 <- ggplot(data.frame(Loading=loadings), aes(x=Loading,fill=1))
p2+geom_density()+theme(legend.position='none')+ggsave('pca1.png')

# predict
market.index <- predict(pca)[,1]
dji.prices <- read.csv('data/DJI.csv')
dji.prices <- transform(dji.prices,Date=ymd(Date))
dji.prices <- subset(dji.prices,Date>ymd('2001-12-31'))
dji.prices <- subset(dji.prices, Date != ymd('2002-02-01'))
dji <- with(dji.prices, rev(Close))
dates <- with(dji.prices, rev(Date))

# plot
comparison <- data.frame(Date=dates,MarketIndex=market.index, DJI=dji)
p3 <- ggplot(comparison, aes(x=MarketIndex,y=DJI))
p3 + geom_point()+geom_smooth(method='lm',se=F)+ggsave('comparison.png')
