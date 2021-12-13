if(!require(quantmod)) install.packages("quantmod")
if(!require(forecast)) install.packages("forecast")
if(!require(xlsx)) install.packages("xlsx")
if(!require(tseries)) install.packages("tseries")
if(!require(timeSeries)) install.packages("timeSeries")
if(!require(dplyr)) install.packages("dplyr")
if(!require(fGarch)) install.packages("fGarch")
if(!require(prophet)) install.packages("prophet")
if(!require(prophet)) install.packages("tidyquant")
library(prophet)
library(quantmod)
library(forecast)
library("xlsx")
library(tseries)
library(timeSeries)
library(dplyr)
library(fGarch)
library(tidyquant)
library(tidyverse)
library(RColorBrewer)
library(ggTimeSeries)

CRWD <- read_csv("CRWD1.csv")

#I. Import and Visualize dataset
#Visualize data
CRWD %>%
  ggplot(aes(x = Date, y = Close, 
             open = Open, high = High, low = Low, close = Close)) +
  geom_line(stat='identity') +
  geom_bbands(ma_fun = SMA, sd = 2, n = 20, linetype = 5) +
  labs(title = "CrowdStrike", 
       subtitle = "BBands with SMA Applied", 
       y = "Close", x = "") + 
  theme_tq()

# Visualize data 
CRWD$date <- as.Date(CRWD$Date) ## transfer to date format
CRWD$year <- as.integer(strftime(CRWD$Date,'%Y'))  ## extract year
CRWD$month <- as.integer(strftime(CRWD$Date,'%m')) ## extract month
CRWD$week <- as.integer(strftime(CRWD$Date,'%W'))  ## extract week
month_locat <- c()
for(i in 1:12){
  month_locat[i] <- mean(CRWD$week[which(CRWD$month==i)])  ##find the average of every month
}

ggplot(data = CRWD, aes(date=Date,fill=Close))+
  stat_calendar_heatmap()+
  scale_fill_gradientn(colours= rev(brewer.pal(11,'Spectral')))+
  facet_wrap(~year,ncol = 1,strip.position = 'top')+  ## strip.position:'right','left','top'和'bottom'
  scale_y_continuous(breaks=seq(7,1,-1), labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")) +
  scale_x_continuous(breaks=month_locat,
                     labels=c("Jan","Feb","Mar","Apr","May","Jue",
                              "Jul","Aug","Sep","Oct","Nov","Dec"), 
                     expand = c(0,0))+
  xlab("Month")+ylab("Day of the Week")+
  theme( panel.background = element_blank(),
         panel.border = element_rect(colour="grey60",fill=NA),  ## border for columns
         axis.line = element_line(colour="black",size=0.25),  
         axis.text = element_text(size = 6))

#II Applying Model to Predict CRWD Stock Price
##II.1 Arima: Autoregresive Intergated Moving Average

### ADF test for dataset
print(adf.test(CRWD$Close))
acf(CRWD$Close)
pacf(CRWD$Close)

# Run auto.arima() to find out the suitable model for the dataset
modelfit <- auto.arima(CRWD$Close, lambda = "auto")
modelfit #PACF value is 2, Time series is non-stationary, ACF value is 2 (the lag after which the acf value becomes zero in the graph is 2)

autoplot(modelfit) # plot the model created by auto.arima
checkresiduals(modelfit) # check residuals
autoplot(forecast(modelfit)) #plot the ARIMA based forecast

#Box test for lag = 2
Box.test(modelfit$residuals, lag = 2, type = "Ljung-Box")

Box.test(modelfit$residuals, type = "Ljung-Box")

# ARIMA forecast result
autoplot(forecast(modelfit))

# dataset forecast mean first 5 values
price_prediction <- forecast(modelfit, h = 30)
head(price_prediction$mean)
#dataset prediction lower first 5 values
head(price_prediction$lower)
#dataset prediction upper first 5 values
head(price_prediction$upper)

# Select our training set at 70% out of the dataset, and leave 30% for the testing set.
# Order of the observations does matter in time series
N = length(CRWD$Close)
n = 0.7*N
train = CRWD$Close[1:n]
test  = CRWD$Close[(n+1):N]
train_arimafit <- auto.arima(train, lambda = "auto")
predlen=length(test)
train_arimafit <- forecast(train_arimafit, h=predlen)

# Plotting mean predicted values vs real data
# The tendency of red line shows a considerably good approach predicting the future direction of the close price
# Except from the day 130th, there is a completely downturn of close price, indicating that something really unusual happened
means <- as.vector(train_arimafit$mean)
realpr <- as.vector(test)
plot(means, type = "l", col ="red") 
lines(realpr)

# II.2  GARCH - Generalized Autoregressive Conditional Heteroskedasticity
# based on there are periods with relative stable movements and course of times with real high volatility
# GARCH to downplay the volatility effect
if(!require(rugarch)) install.packages('rugarch')
library(rugarch)
# Fit and check the parameters significance of Standard GARCH (1,1)
garch_check_1 = garchFit(formula = ~garch(1,0), data = CRWD$Close, trace = FALSE, cond.dist = "QMLE" )
summary(garch_check_1)
# Fit and check the parameters significance of GARCH (1,0) or ARCH(1)
garch_check_2 = garchFit(formula = ~garch(1,1), data = CRWD$Close, trace = FALSE, cond.dist = "QMLE" )
summary(garch_check_2)
# Choose GARCH (1,0) - 1 lag variance and 0 residual error
# Run auto arfima function to find ARFIMA parameters to find ARMA
fitarfima2 = autoarfima(data = AMZN$Close, 
                        ar.max = 2,
                        ma.max = 2,
                        criterion = "AIC",
                        method = "full")
fitarfima2 
# Forecasting with mean and variance - GARCH(1,0) and ARMA(2,2)
garch_close<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)), 
                  mean.model = list(armaOrder = c(2, 2), include.mean = FALSE), 
                  distribution.model = "norm")
garch_close_fit<-ugarchfit(spec=garch_on_close,data=CRWD$Close)

#Plot of conditional volatility affected by GARCH
forecast.model.fit = ugarchforecast(model.fit, n.ahead = 30)
plot(forecast.model.fit, which=1)

# II.3 LSTM Long Short Term Memory
if(!require(keras)) install.packages("keras")
if(!require(tensorflow)) install.packages("tensorflow")

# Lagged dataset. Lagging the series and have the value at time (t−k) as the input and value
#at time t as the ouput, for a k-step lagged dataset.
lag_transform <- function(x, k= 1){
  
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}
lagged_close = lag_transform(CRWD$Close, 1)
head(lagged_close)

# Use the same training and testing sets from ARIMA
# Normalizing the data
scale_data = function(train, test, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  std_test  = ((test - min(x) ) / (max(x) - min(x)  ))
  
  scaled_train = std_train *(fr_max -fr_min) + fr_min
  scaled_test = std_test *(fr_max -fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
  
}
Scaled = scale_data(train, test, c(-1, 1))
y_train = Scaled$scaled_train[, 2]
x_train = Scaled$scaled_train[, 1]
y_test = Scaled$scaled_test[, 2]
x_test = Scaled$scaled_test[, 1]

# Inverse-transformating
invert_scaling = function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  t = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(t)
  
  for( i in 1:t){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}
# Modeling the data
use_condaenv("r-reticulate")
use_condaenv("r-tensorflow")
model_LSTM <- keras_model_sequential() 
model_LSTM%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
  layer_dense(units = 1)
# Compile the model
model_LSTM %>% compile(
  loss = 'mean_squared_error',
  optimizer = "optimizer",  
  metrics = c('accuracy')
)
# Fit the model
Epochs = 50   
for(i in 1:Epochs ){
  model %>% fit(x_train, y_train,
                epochs=1, 
                batch_size=batch_size, 
                verbose=1)
  model %>% reset_states()
}
# CRWD Price Predictions with LSTM
L = length(test)
scaler = Scaled$scaler
predictions = numeric(L)
for(i in 1:L){
  X = test[i]
  dim(X) = c(1,1,1)
  pred = model %>% predict(X, batch_size = batch_size)
  # invert scaling
  pred = invert_scaling(pred, scaler,  c(-1, 1))
  # invert differencing
  pred  = pred + Series[(n+i)]
  # store
  predictions[i] <- pred
}

# II.5 KNN regression time series forecasting
# Loading k nearest neighbor 
if(!require(tsfknn)) install.packages("tsfknn")
library(tsfknn)
# Predict price for the next 30 days
df_KNN <- data.frame(ds = index(CRWD),
                 y = as.numeric(unlist(CRWD[,"Close"])))
forecast_knn <- knn_forecasting(df_KNN$y, h = 30, lags = 1:30, k = 40, msas = "MIMO")

#Model accuracy for the train set
accr <- rolling_origin(forecast_knn)
print(accr$global_accu)
plot(forecast_knn, type = "l")

# In conclusion, the implementation of GARCH on ARMA worked out the best.
# The K-nearest also provided a considerably good prediction in comparison
# with the real daily close price for the next 30 days.
# LSTM and Prophet approach models did not generate a
# good forecast as well as the others.





