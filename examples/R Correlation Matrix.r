# Print correlation matrix with corrplot in R

#1. install and load the packages
if (!require(quantmod)) install.packages('quantmod')
library('quantmod')

if (!require(corrplot)) install.packages('corrplot')
library(corrplot)

#2. params
# Pay attention currencys and stock do not trade always at the same day
TIMEFRAME = '2018-01-05'
WATCHLIST = c('V','FB', 'AMZN', 'NFLX', 'GOOG', 'GE', 'XOM','CAT','NKE','MMM', 'MCD','HD','DIS')
ALLSYMBOLS = getSymbols(WATCHLIST, from=TIMEFRAME)

#3. get some data (you may get a warning here about the hyperparameter auto.assign)
# amzn = getSymbols('AMZN', src='google', from='2016-06-15', auto.assign=FALSE)
# DJI = getSymbols('^DJI', from=TIMEFRAME, auto.assign=FALSE)

#4. quick exploration of quantmod charting capabilities
# chartSeries(AMZN, TA = c(addSMA(50),addVo(),addBBands()), subset='2016-07::2018-03')

#5. get comparison data for FB, GOOG, NFLX
# FB = getSymbols('FB', from=TIMEFRAME, auto.assign=FALSE)
# GOOG = getSymbols('GOOG', from=TIMEFRAME, auto.assign=FALSE)
# NFLX = getSymbols('NFLX', from=TIMEFRAME, auto.assign=FALSE)
# GE = getSymbols('GE', from=TIMEFRAME, auto.assign=FALSE)
# XOM = getSymbols('XOM', from=TIMEFRAME, auto.assign=FALSE)
## USD_EUR = getSymbols('USD/EUR', from=TIMEFRAME, auto.assign=FALSE, src="oanda")
ALLSYMBOLS = getSymbols(WATCHLIST, from=TIMEFRAME)

#6. take a quick look at what the data looks like
# head(NFLX)

#7. merge data we want into new object, cast as data.frame
# stocks = as.data.frame(merge(FB$FB.Close, AMZN$AMZN.Close,NFLX$NFLX.Close, GOOG$GOOG.Close, GE$GE.Close, XOM$XOM.Close, USD_EUR$USD.EUR))
stocks = as.data.frame(merge(V$V.Close , FB$FB.Close, AMZN$AMZN.Close,NFLX$NFLX.Close, GOOG$GOOG.Close, GE$GE.Close, XOM$XOM.Close, CAT$CAT.Close, NKE$NKE.Close, MMM$MMM.Close, MCD$MCD.Close, HD$HD.Close, DIS$DIS.Close))


#8. see what we have
# head(stocks)

#9. change column names
# names(stocks) = c('FB', 'AMZN', 'NFLX', 'GOOG', 'GE', 'XOM', 'USDEUR')
# names(stocks) = c('FB', 'AMZN', 'NFLX', 'GOOG', 'GE', 'XOM')
names(stocks) = WATCHLIST;

#10. store daily returns in new dataframe
# quantmod also has a function periodReturn(stock, period='daily, monthly, etc.') and allReturns()
stocks_returns = data.frame(diff(as.matrix(log(stocks))))

#11. check transformed data
# head(stocks_returns)

#12. get correlations
cor(stocks_returns)

#13. plot scatter matrix, top row second col x = fb, second row first col x = amzn, etc
plot(stocks_returns)

#14. plot it beauty
m <- cor(stocks_returns)
corrplot.mixed(m, order = "hclust", addrect = 2, tl.col = "black", tl.srt = 45)
#corrplot.mixed(m)
