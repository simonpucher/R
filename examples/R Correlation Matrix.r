# Print correlation matrix with corrplot in R

# delete workspace
remove(list = ls())
# clear console window in rstudio
cat("\014")

# 1. install and load the packages
if (!require(quantmod)) install.packages('quantmod')
library('quantmod')

if (!require(corrplot)) install.packages('corrplot')
library(corrplot)

if (!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)

# 2. params
# Pay attention currencys and stock do not trade always at the same day
STARTDATE = '2018-01-05'
WATCHLIST = c('MMM','AXP','AAPL','BA','CAT','CVX','CSCO','KO','DIS','DWDP','XOM','GE','GS','HD','IBM','INTC','JNJ','MCD','MRK','MSFT','NKE','PFE','PG','TRV','UTX','UNH','VZ','V','WMT')

# 3. get data
ALLSYMBOLS = getSymbols(WATCHLIST, from=STARTDATE)

# merge daily return to a matrix
# Stocks = lapply(WATCHLIST, function(sym) {
#   dailyReturn(na.omit(getSymbols(sym, from=STARTDATE, auto.assign=FALSE)))
# })
# stocks <- do.call(merge, Stocks)

# 4. merge the data to a close price matrix
ClosePrices <- do.call(merge, lapply(WATCHLIST, function(x) Cl(get(x))))
head(ClosePrices)

# 5. quick exploration of quantmod charting capabilities
# chartSeries(AMZN, TA = c(addSMA(50),addVo(),addBBands()), subset='2016-07::2018-03')

# 6. take a quick look at what the data looks like
# head(AMZN)

# 7. merge data we want into new object, cast as data.frame
# stocks = as.data.frame(merge(FB$FB.Close, AMZN$AMZN.Close,NFLX$NFLX.Close, GOOG$GOOG.Close, GE$GE.Close, XOM$XOM.Close, USD_EUR$USD.EUR))
# stocks = as.data.frame(merge(V$V.Close , FB$FB.Close, AMZN$AMZN.Close,NFLX$NFLX.Close, GOOG$GOOG.Close, GE$GE.Close, XOM$XOM.Close, CAT$CAT.Close, NKE$NKE.Close, MMM$MMM.Close, MCD$MCD.Close, HD$HD.Close, DIS$DIS.Close))

# 8. see what we have
# head(stocks)

# 9. change column names
# names(stocks) = c('FB', 'AMZN', 'NFLX', 'GOOG', 'GE', 'XOM', 'USDEUR')
# names(stocks) = c('FB', 'AMZN', 'NFLX', 'GOOG', 'GE', 'XOM')
names(ClosePrices) = WATCHLIST;

# 10. store daily returns in new dataframe
# quantmod also has a function periodReturn(stock, period='daily, monthly, etc.') and allReturns()
stocks_returns = data.frame(diff(as.matrix(log(ClosePrices))))

# 11. check transformed data
# head(stocks_returns)

# 12. get correlations
cor(stocks_returns)

# 13. plot scatter matrix, top row second col x = fb, second row first col x = amzn, etc
plot(stocks_returns)

# 14.make a coorelation
m <- cor(stocks_returns)
#corrplot.mixed(m, order = "hclust", addrect = 2, tl.col = "black", tl.srt = 45)
#corrplot.mixed(m)

# 16 plot it
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(m, method="color", col=col(200),
         type="upper", order="hclust",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
         )


 corrplot(m, type="upper", order="hclust",
 col=brewer.pal(n=8, name="RdYlBu"))




# Computing the p-value of correlations
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# matrix of the p-value of the correlation
p.mat <- cor.mtest(ClosePrices)
head(p.mat[, 1:5])

siglevel = 0.01
M <- cor(ClosePrices)
corrplot(M, type="upper", order="hclust", p.mat = p.mat, sig.level = siglevel, insig = "blank")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200), type="upper", order="hclust",
addCoef.col = "black", # Add coefficient of correlation
tl.col="black", tl.srt=45, #Text label color and rotation
# Combine with significance
p.mat = p.mat, sig.level = siglevel, insig = "blank",
# hide correlation coefficient on the principal diagonal
diag=FALSE
)


corrplot(M, type = "upper", order = "hclust",
         col = brewer.pal(n = 8, name = "RdYlBu"))
