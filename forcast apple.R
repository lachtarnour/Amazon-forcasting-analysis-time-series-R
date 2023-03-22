library(BatchGetSymbols)
library(forecast)
library(ggplot2)
library(tsm)
library(urca)
library(caschrono)

library(zoo)


grep("apple",tolower(dataSymbols$name),value=T)
tickers=c(dataSymbols$symbol[11191])

first.date <- Sys.Date() - 365
last.date <- Sys.Date()
freq.data <- 'daily'

apple_data=BatchGetSymbols(tickers = tickers, 
                          first.date = first.date,
                          last.date = last.date, 
                          freq.data = freq.data,
                          do.complete.data = T,
                          be.quiet = F,
                          do.cache=F)
#do.cache=T : ajouter les dernieres lignes sur la base deja recuperé 
dt=apple_data$df.tickers

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

p <- ggplot(dt, aes(x=date, y=value,color=name,group=name)) +
  geom_line() + 
  xlab("")+ylab("")+theme_bw()
p+theme(legend.position = 'top',legend.title = element_blank())

#recuperant la serie des prix ajuster et le convertir en dataframe
serie=dt%>%filter(name=='adjusted')%>%dplyr::select(value)%>%
  tibble::deframe()

#representation de serie + autocorrelation + scatter
ggtsdisplay(serie,theme = theme_bw())

print("il s'agit probablement d'un AR(1)")
print("autocorreclation partiel entre x(t) et x(t-1) est proche de 1 le modele le plus edequate est un marche aleatoire")

#(1-B)x(t)
diff(serie,difference=1)%>%ggtsdisplay(theme = theme_bw())
print("il s'agit probablement d'un bruit blanc")
#(1-B)²x(t)
diff(serie,difference=2)%>%ggtsdisplay(theme = theme_bw())

#test Dichey-fuller de stationarité H0: non stationnaire
t1=serie%>%diff(difference=1)%>%ur.df(type='none',lag=3)
summary(t1)
print("delta(w(t))=phi1 x w(t) avec w(t)=delta(x(t))")
print("-7.6669<-2.58 : H0 rejetée il est stationnaire")

#test Box-test pour verifier s'il s'agit d'un bruit blanc 
Box.test.2(serie%>%diff(difference=1),nlag=1:10,type="Ljung-Box")
print('les p-valeurs sont tous superieurs à 5% : il n y pas d autocorrelation')

print("il s agit d bruit blanc" )

#ajuster les données avec un SARIMA(0,1,0)(0,0,0)[0]
fit=astsa::sarima(xdata=serie,0,1,0,0,0,0,0,details=F,no.constant=T)
fit
fit$fit$loglik
print("log de vraissemblance est bien negative")

#intervalle de confiance de x(t+1)
IC_0.95=c(last(serie)-sqrt(fit$fit$sigma2)*1.96,last(serie)+sqrt(fit$fit$sigma2)*1.96)
estimated=serie-fit$fit$residuals
serie=ts(serie)
autoplot(serie,colour='blue')+forecast::autolayer(estimated)+theme_bw()

