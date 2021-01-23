
################## Arima ########################

library(forecast)

serie=ndq#insert time a serie here
n = round(length(serie) *0.75, digits = 0)
train = serie[1:n]
test = serie[(n+1):length(serie)]


modelo=auto.arima(train,max.p = 20,max.q = 20,max.P = 20,max.Q = 20,seasonal = TRUE,
                 stepwise = FALSE, parallel = TRUE,num.cores = NULL,nmodels = 5000,start.p = 4,
                 start.q = 4,allowdrift = FALSE,allowmean = FALSE,ic = "aicc")


forecast.arima = Arima(window(serie, start=1), model = modelo)
onestep.arima = fitted(forecast.arima)

ar=0
for(i in 1:length(test)) {
    ar[i] <- onestep.arima[i+length(train)]
  }


############ ANN ###################
library(MASS)

serie= ndq
n = round(length(serie) *0.75, digits = 0)
train= serie[1:n]
test = serie[(n+1):length(serie)]


fit <- nnetar(serie,p=9,lambda='auto',repeats = 10)#se ligar pra colocar a serie toda aqui
ann = forecast(fit,h=length(test))

#ann = forecast(fit,h=length(validation))
onestep.ann = fitted(ann)

an=0
for(i in 1:length(test)) {
  an[i] <- onestep.ann[i+length(train)]
}

########## LTSM ###########

library(keras)
library(tensorflow)

serie = ndq
diffed = diff(serie, differences = 1)
#plot(diffed)

lag_transform <- function(x, k= 9){
  
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}

supervised = lag_transform(diffed, 5)
head(supervised)

N = nrow(supervised)
n = round(N *0.75, digits = 0)
train = supervised[1:n, ]
test  = supervised[(n+1):N,  ]

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


dim(x_train) <- c(length(x_train), 1, 1)


X_shape2 = dim(x_train)[2]
X_shape3 = dim(x_train)[3]
batch_size = 1                
units = 1                     


model <- keras_model_sequential() 
model%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
  layer_dense(units = 50)

model %>% compile(
  #loss = 'mean_absolute_error',
  #loss = 'mean_absolute_percentage_error',
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
  metrics = c('accuracy')
)

Epochs = 10  
for(i in 1:Epochs ){
  model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model %>% reset_states()
}

L = length(x_test)
scaler = Scaled$scaler
lstm = numeric(L)

for(i in 1:L){
  X = x_test[i]
  dim(X) = c(1,1,1)
  yhat = model %>% predict(X, batch_size=batch_size)
  # invert scaling
  yhat = invert_scaling(yhat, scaler,  c(-1, 1))
  # invert differencing
  yhat  = yhat + serie[(n+i)]
  # store
  lstm[i] <- yhat
}



############# Simple Average and Simple Median ############

media <- 0
mediana <- 0


for(i in 1:length(lstm)) {
  x <- c(an[i],lstm[i],ar[i])
  media[i] <- mean(x)
  mediana[i] <- median(x)
}

########### Copulas ##############

library(rmutil)
library(fGarch)
library(copula)
library(moments)
library(MASS)

serie= usd
n = length(serie)*0.75
train = serie[1:n]
test = serie[(n+1):length(serie)]

PATH = "/Users/ademirbatista/Desktop/"
source(paste(PATH, "OptimalCombinations.R", sep=""))

forecasts = cbind(ANN=an,ARIMA=ar,LTSM=lstm)#previsoesDosModelos #matrx (cada coluna traz as previsoes para targets, de cada modelo individual)
n = round(.75*length(test), 0)
cCB = getCopulaBasedPredictor(test, forecasts, from=1, to=n)
copula = cCB$forecasts
#accuracy(cCB$forecasts,ggtest)


###### ANN Ensemble ##########
library(neuralnet)
ensemble <- runif(1000, 0.0, 1.0)
df=data.frame(ANN,LSTM,ARIMA,ensemble)
nn=neuralnet(ensemble~ANN+ARIMA+LSTM,data=df, hidden=4,act.fct = "logistic")
plot(nn)
test=data.frame(an,ar,lstm)
Predict=compute(nn,ggtest)
Predict$net.result
ass<-0
min=min(petrotest)
max=max(petrotest)
for(i in 1:length(Predict$net.result)) {
  ass[i] <-Predict$net.result[[i]] *(max - min) + min
}

######## Combinig wiht OPERA ###########
library(forecast)
library(ggplot2)
library(opera)

X <- cbind(Arima=ar, RedesNeurais=an, LSTM=lstm)
#df <- cbind(test, X)
#colnames(df) <- c("Data","ARIMA","ANN","LSTM")
#autoplot(ts.union(a, r, lstm,test), facets = FALSE) + xlab("Data set") + ylab("NQ") 

MLpol0 <- mixture(model = "MLpol", loss.type = "square")
weights <- predict(MLpol0, X, usdtest, type='weights')


z <- ts(predict(MLpol0, X, usdtest, type='response'), start=1, freq=1)
mlp=0
for(i in 1:length(z)) {
  mlp[i] <- z[i]
}


##### T-student residual ########
require(graphics)
library(Rfit)
qqnorm(rstudent(fit<-rfit(ndqtest~an)))
plot(an,rstudent(fit),main="ARIMA") ; abline(h=c(-3,3))
