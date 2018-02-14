#can we forecast los angeles studio apartment prices? 
#import libraries 
library(TTR)
library(forecast)
library(ggplot2)
#remove outliers from time series
mia_clean$clean_value=tsclean(value_ts1) #removes time series outliers 
head(mia_clean)
str(mia_clean) #clean value=time series type

#subset downtown los angeles listings
la_downtown<-subset(mia_clean,RegionName=="Downtown" & City=="Los Angeles")
dim(la_downtown) #69,14
plot.ts(la_downtown$clean_value)

#use the HoltWinters() to fit a simple exponential smoothing predictive model
la_downtown_forecasts<-HoltWinters(la_downtown$clean_value,beta=FALSE,gamma=FALSE)
la_downtown_forecasts
#notas: When α is closer to 1 we consider this fast learning because the algorithm gives more weight to the most recent observation

#fitted values
la_downtown_forecasts$fitted #forecasts made by holtwinters() are stored in 'fitted', which is a list variable
plot(la_downtown_forecasts) #red predictions
#SSE
la_downtown_forecasts$SSE 
#specify initial value level fpr holtwinters() $1657.500
HoltWinters(la_downtown,beta=FALSE,gamma=FALSE,l.start=1657.5) #error here?

#la forecasts (a year into the future)
la_downtown_forecasts1<-forecast(la_downtown_forecasts,h=12)
la_downtown_forecasts1
#e.j. 95% confident that average rental studio price lies between $1955-$2334 for march 2018 in downtown la
#the average studio rental price is projected to increase on upper 95% C.I.
plot(la_downtown_forecasts1)
#forecast errors =residuals 
acf(la_downtown_forecasts1,lag.max=12)
#test for significant evidence for non-zero auto correlations at lags 1-12
Box.test(la_downtown_forecasts1$residuals, lag=12, type="Ljung-Box")
#there is little evidence of non-zero auto correlations at lags 1-12 in the
#sample forecast errors