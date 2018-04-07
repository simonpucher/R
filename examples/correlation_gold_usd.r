# Print correlation matrix between three instruments

#1. install and load the packages
if (!require(quantmod)) install.packages('quantmod')
library('quantmod')

if (!require(corrplot)) install.packages('corrplot')
library(corrplot)

getSymbols("USD/EUR",src="oanda",from="2018-01-01")
getSymbols("XAU/USD",src="oanda",from="2018-01-01")
getSymbols("USD/CAD",src="oanda",from="2018-01-01")

y = USDEUR
x = XAUUSD
z = USDCAD

chartSeries(y, TA = c(addSMA(50),addVo(),addBBands()), subset='2018-01::2018-04')

stocks = as.data.frame(merge(x,y,z))

cor(stocks)
plot(stocks)


plot(x ~ y,
     data=Data,
     pch=16,
     xlab = "XAU/USD",
     ylab = "USD/EUR")
