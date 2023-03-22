load("dataSymbols.RData")

amz_share<- BatchGetSymbols::BatchGetSymbols(tickers = amz,
                              do.complete.data = T, be.quiet = F,
                              do.cache = F,
                              first.date = Sys.Date()-600,
                              last.date = Sys.Date(), 
                              freq.data = 'daily')

dt=amz_share$df.tickers

dt=dt%>%dplyr::select(ref.date,price.open,
                      price.high,price.low,price.close,
                      price.adjusted)%>%
  tidyr::pivot_longer(cols = 2:6)%>%
  mutate(name=plyr::mapvalues(name,
                              from=c("price.open","price.high",
                                     "price.low","price.close",
                                     "price.adjusted"),
                              to=c("open","high","low",
                                   "close","adjusted")))%>%
  rename(date=ref.date)
dt%>%head(2)
library(ggplot2)
library(dplyr)

p <- ggplot(dt, aes(x=date, y=value,color=name,group=name)) +
  geom_line() + 
  xlab("")+ylab("")+theme_bw()
p+theme(legend.position = 'top',legend.title = element_blank())

zz=dt%>%filter(name=='adjusted')%>%dplyr::select(value)%>%
  tibble::deframe()
zz%>%ggtsdisplay(theme = theme_bw())

zz%>%diff(lag=5)%>%ggtsdisplay(theme = theme_bw())

zz%>%diff(lag=5)%>%lag.plot(9,c(3,3))

x=tsoutliers(zz,lambda = NULL)
x$index

m0=auto.arima(zz,lambda = NULL,ic = 'aic')
summary(m0)

m0
