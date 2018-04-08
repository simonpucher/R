# Correlation between two assets from Yahoo Finance using Quantmod
if (!require(quantmod)) install.packages('quantmod')
library(quantmod)

getSymbols(c("SPY","^DJI"),src="yahoo")
data=data.frame(SPY[,6],DJI[,6])
data=as.xts(data,order.by=as.Date(row.names(data),"%Y-%m-%d"))
c1=rollapply(data,65,cor,by.column=F)
Correlation=c1[,2]; chartSeries(Correlation)
