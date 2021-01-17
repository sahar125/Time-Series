#?tape 1: Charger les packages R 
library('ggplot2')
library('forecast')
library('tseries')
library(readr)
data <- read_csv("C:/Users/INFOTEC/Downloads/data_scientist_intern_assessment.zip")
pulses=as.numeric(data$pulses)


#?tape 2: examinez vos donn?es
#Un bon point de d?part est de tracer la s?rie et de l'examiner visuellement pour y d?tecter d'?ventuelles valeurs aberrantes,
#de la volatilit? ou des irr?gularit?s


ggplot(data, aes(as.Date(timestamp), as.numeric(pulses))) + geom_line() + scale_x_date('Days')  + ylab("Daily Capital Checkouts") +
  xlab("")




#Dans certains cas,la valeur de pulses depasse  le 200 .
# Ce sont des valeurs suspectes qui pourraient biaiser le mod?le en faussant les r?sum?s statistiques.
#R constitue une m?thode pratique pour ?liminer les valeurs ?loign?es des s?ries chronologiques: 
#tsclean () dans le cadre de son progiciel de pr?vision. 
#tsclean () identifie et remplace les valeurs aberrantes ? l'aide du lissage et de la d?composition de s?ries

#Cette m?thode est ?galement capable d'entrer les valeurs manquantes.


count_ts = ts(data[, c('pulses')],frequency=30)
plot(tsclean(count_ts))
data$clean_cnt = tsclean(count_ts)
acf(count_ts)
plot(count_ts)
ggplot() +
  geom_line(data = data[1:1900,], aes(x = timestamp, y = clean_cnt)) + ylab('Cleaned data')

#M?me apr?s la suppression des valeurs aberrantes, les donn?es quotidiennes restent assez volatiles. 
#Visuellement, nous pourrions tracer une ligne ? travers la s?rie en tra?ant ses creux et ses pics plus importants tout en lissant les fluctuations bruyantes.
#Cette ligne peut ?tre d?crite par l'un des concepts les plus simples ,  mais aussi tr?s utiles , de l'analyse des s?ries chronologiques,appel? moyenne mobile.
#C'est un concept intuitif qui fait la moyenne des points sur plusieurs p?riodes de temps, 
#lissant ainsi les donn?es observ?es en une s?rie pr?visible plus stable.
#Plus la fen?tre de la moyenne mobile est large, plus la s?rie originale est lisse. 




data$cnt_ma = ma(data$clean_cnt, order=7) # using the clean count with no outliers
data$cnt_ma30 = ma(data$clean_cnt, order=30)
ggplot() +
  geom_line(data = data[1:600,], aes(x = timestamp, y = clean_cnt, colour = "Counts")) +
  geom_line(data = data[1:600,], aes(x = timestamp, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = data[1:600,], aes(x = timestamp, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('pulsesl')


#?tape 3: d?composez vos donn?es
#Les ?l?ments constitutifs d'une analyse de s?rie chronologique sont la saisonnalit?, la tendance et le cycle.
#Ces composants intuitifs capturent les mod?les historiques de la s?rie.
count_ma = ts(na.omit(data$cnt_ma), frequency=30)#Puisque nous utilisons des donn?es quotidiennes liss?es, nous avons 30 observations par mois.
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

#?tape 4: stationnarit?
library(aTSA)
adf.test(count_ma )
#p-value=0.01 donc notre serie est stationnaire


Acf(count_ma, main='')
Pacf(count_ma, main='')



#?tape 6: Monter un mod?le ARIMA
auto.arima(deseasonal_cnt, seasonal=FALSE)
#Le coefficient AR (1) p = 0.3441  
#nous indique que la valeur suivante de la s?rie est consid?r?e comme une valeur ant?rieure amortie par un facteur de 0.3441  et d?pend du retard d'erreur


#?tape 7: ?valuer et it?rer
#
fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(2,1,0) Model Residuals')
#Il existe une tendance claire dans ACF / PACF et les trac?s de r?sidus de mod?le se r?p?tant
#au d?calage 8. Cela sugg?re que notre mod?le peut ?tre pr?f?rable avec une sp?cification diff?rente,
#telle que p = 8 ou q = 8 . 
fit2 = arima(deseasonal_cnt,order=c(2,1,8))
fit2
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')
arima(x = deseasonal_cnt, order = c(2, 1, 8))
#Nous pouvons observer que l'AIC est ?galement plus petit pour la structure (1, 1, 8) :


fcast <- forecast(fit2,7)
plot(fcast,type = "l")
hold <- window(ts(deseasonal_cnt), start=1500)
fit_no_holdout = arima(ts(deseasonal_cnt[-c(1500:1998)]), order=c(2,1,7))
fcast_no_holdout <- forecast(fit_no_holdout,25)
plot(forecast(fit_no_holdout,25),type = "l")
plot(fcast_no_holdout, main=" ")



#fit_w_seasonality = auto.arima(x=deseasonal_cnt, stationary = TRUE )
#fit_w_seasonality
#fit_w_seasonality1<-arima(x=deseasonal_cnt, order=c(1,0,5) )
#seas_fcast <- forecast(fit_w_seasonality1,13)
#plot(seas_fcast,type='l')

#outliers
#1
outlier_val <- boxplot.stats(data$pulses)$out
outlier_val
outlier_idx <-which(data$pulses %in% c(outlier_val))
outlier_idx
suv[outlier_idx,]
#2
outLiers=subset(data, pulses>mean(pulses)*3)
outLiers

