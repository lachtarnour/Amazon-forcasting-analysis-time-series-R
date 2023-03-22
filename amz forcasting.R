
load("dataSymbols.RData")
amz=dataSymbols$symbol[13300]
amz_share<- BatchGetSymbols::BatchGetSymbols(tickers = amz,
                            do.complete.data = T, be.quiet = F,
                            do.cache = F,
                            first.date = Sys.Date()-1000,
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


###frequency 5 (seasonality)

zz%>%diff(difference=5)%>%ggtsdisplay(theme = theme_bw())


library(caschrono)
library(forecast)

### Box cox trans

lam1=BoxCox.lambda(zz,method = "loglik",lower = 0,upper = 10)
lam1
lam2=BoxCox.lambda(zz,method = "guerrero",lower = 0,upper = 10)
lam2

x=tsoutliers(zz,lambda = NULL)
x$index
x1=tsoutliers(zz,lambda = lam1)
x1$index
x2=tsoutliers(zz,lambda = lam2)
x2$index


### Autoarima

lmA=auto.arima(zz,lambda = lam1)
lmA

lmB=auto.arima(zz,lambda = lam2)
lmB

caschrono::t_stat(lmA)


residuals=lmA$residuals
stdres <- residuals/sqrt(lmA$sigma2)
stdres%>%ggtsdisplay(lag.max=30,theme = theme_bw())


t1<-stdres%>%urca::ur.df(type = "none")
urca::summary(t1)


armaselect(stdres,nbmod = 10)


### Model Selection


zzA=forecast::BoxCox(zz,lambda = lam1)
zzA=ts(zzA[-c(1:200)])

zzA%>%diff(lag=5)%>%autoplot()+theme_bw()+xlab("Date")+ylab("")
zzA%>%diff(lag=5)%>%diff(difference=2)%>%autoplot()+theme_bw()+xlab("Date")+ylab("")


t1=zzA%>%diff(lag=5)%>%diff(difference=2)%>%urca::ur.df(type="none",selectlags = 'AIC')
urca::summary(t1)


t2=zzA%>%diff(lag=5)%>%diff(difference=2)%>%urca::ur.df(type="drift",selectlags = 'AIC')
urca::summary(t2)

zzA%>%diff(lag=5)%>%diff(difference=2)%>%ggtsdisplay(theme = theme_bw(),main="",xlab="Date")

### q=0:4, Q=0, D=5, p=0,1, P=0:4

orders=list(p=c(0,1,2,3),d=2,q=c(0,1,2,3,4),P=c(0,1,2,3),D=1,Q=c(0,1,2),T=12)



all_orders=expand.grid(orders$p,orders$d,orders$q,orders$P,orders$D,orders$Q, orders$T)

colnames(all_orders)=c("p","d","q","P","D","Q","T")
head(all_orders,n = 3)
dim(all_orders)

### Selecting models
models=vector('list',nrow(all_orders))
all_orders=as.matrix(all_orders)
for(i in 1:nrow(all_orders)){
  cat('\r',i,'/',length(models),sep='')
  od=all_orders[i,1:3]
  od_s=all_orders[i,4:6]
  TT=all_orders[i,7]
  models[[i]]=try(Arima(zzA,order=od,
                        seasonal=list(order=od_s,period=TT),
                        lambda=NULL,
                        include.drift = T),silent = T)
}

zz=unlist(lapply(models, length))
zz
models = models[zz>1]
length(models)


x=rep(NA,length(models))
for(i in 1:length(models)){
  x[i]=try(prod(caschrono::t_stat(models[[i]])[2,]<=0.05))
}
x=as.numeric(x)
table(x)
models=models[which(x==1)]
length(models)


x=rep(NA,length(models))
for(i in 1:length(models)){
  cat("\r",i,"/",length(models),sep="")
  m=models[[i]]
  tt=armaselect(m$residuals,nbmod = 4)
  x[i]=sum(rowSums(tt[,1:2])==0)
}
table(x)

aic=unlist(lapply(models, function(x) x$aic))
aic

j=order(aic,decreasing = F)

best_models=models[j]


best_models[[1]]%>%forecast(h=10,level=95,lambda=NULL)%>% autoplot()+theme_bw()

