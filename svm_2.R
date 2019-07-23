getwd()
setwd("E:/cmi2019/asdf/data/DATA/BLOCK1/")          ## change directory to wherever demo_dailydata.rda is stored ##
load("demo_dailydata.rda")
library(PortfolioAnalytics)
library(PortRisk)
library(tseries)
library(xts)
library(zoo)
library(tawny)
library(PerformanceAnalytics)
library(GA)
library(TTR)
library(kernlab)
library(e1071)
library(tseries)
library(Metrics)

### predict daily prices using SVM and return predicted and actual weekly returns ###
svmpreds <- function(company , train){

  r <- dailyreturns[,company]         # get daily returns

  r <- na.omit(r)
  rf <- as.xts(r[,company])
  
  rf <- 1+rf
  rf[1] <- 1
  pf <- cumprod(rf)                    # make a price process starting at $1 with returns
  pf.1 <- lag(pf , 1)                  # one day lagged prices
  
  # calculate technical indicators and lag them #
  
  rsi.1 <- RSI(pf , n = 10)
  lrsi.1 <- lag(rsi.1 ,1)
  ema.1 <- EMA(pf , n = 10 , wilder = TRUE)
  lema.1 <- lag(ema.1 , 1)
  macd.1 <- MACD(pf , 12 , 26 , 9 , maType = "EMA")
  lmacd.1 <- lag(macd.1 , 1)

  dataf <- as.data.frame(as.xts(cbind(pf , pf.1 , lrsi.1 , lema.1 , lmacd.1)))
  
  any(is.na(dataf))
  dataf <- na.omit(dataf)

  # normalize data #
  
  dataf$rsi <- dataf$rsi/max(dataf$rsi)
  dataf$EMA <- dataf$EMA/max(dataf$EMA)
  dataf$macd <- dataf$macd/max(dataf$macd)
  dataf$signal <- dataf$signal/max(dataf$signal)
  
  # fit svm and lm regression models #
  
  model1 <- svm(pf~rsi+EMA+macd+signal , data = dataf[1:train,] , type = "eps-regression")
  model2 <- lm(pf~rsi+EMA+macd+signal , data = dataf[1:train,])
  
  predictions <- list() # list to hold lm and svm predictions
  
  predictions[[1]] <- as.xts(predict(model1 , newdata = dataf[(train+1):nrow(dataf),2:6]))
  predictions[[2]] <- as.xts(predict(model2 , newdata = dataf[(train+1):nrow(dataf),2:6]))
  actuals <- (dataf[(train+1):nrow(dataf),1]) # actual price process of test data
  
  rmse.vals <- numeric()  # rmse of svm and lm
  rets.model <- list()
  ### weekly returns from both models ###
  for(j in 1:2){
    
    # split data on a weekly basis #
    
    predsw <- split(predictions[[j]] , f = "week" , k = 1)
    actualw <- split(as.xts(dataf[(train+1):nrow(dataf),1] , order.by = index(predictions[[j]])) , f="week" , k=1)
    
    if(length(predsw[[1]]) <= 1){predsw[[1]] <- NULL}
    if(length(actualw[[1]]) <= 1){actualw[[1]] <- NULL}
    len <- length(predsw)                                       # corner cases
    if(length(predsw[[len]]) <= 1){predsw[[len]] <- NULL}
    if(length(actualw[[len]]) <= 1){actualw[[len]] <- NULL}
  
    retswp <- list()
    retswnp <- numeric()
    retswa <- list()
    retswna <- numeric()
    dates <- c(Sys.Date())
  
    for(i in 1:length(predsw)){
      retswp[[i]] <- Return.calculate(predsw[[i]])  
      temp <- cumprod(na.omit(1+retswp[[i]]))
      retswnp[i] <- (temp[length(temp)]-1)           # weekly returns - predicted
      dates <- c(dates,as.Date(index(retswp[[i]])[length(retswp[[i]])]))
      retswa[[i]] <- Return.calculate(actualw[[i]])  
      temp <- cumprod(na.omit(1+retswa[[i]]))
      retswna[i] <- (temp[length(temp)]-1)          # weekly returns - actuals
    }

    dates <- dates[-1]
    retswnp <- as.xts(retswnp , dates)
    retswna <- as.xts(retswna , dates)
    rmse.vals[j] <- rmse(retswnp , retswna)
    rets.model[[j]] <- list(preds = retswnp , acts = retswna)
  }
  
  if(rmse.vals[1] < rmse.vals[2]){            # choose model with lower rmse
    model.choice <- 1
  } else {
    model.choice <- 2
  }
  
  
  return(list(predictedw = rets.model[[model.choice]]$preds ,
              actualw = rets.model[[model.choice]]$acts ,
              rets.corr = cor(rets.model[[model.choice]]$acts , rets.model[[model.choice]]$preds) ,
              pred.price = predictions[[model.choice]] ,
              actual.price = actuals , choice = model.choice))
}

## test correlation ##

temp.string <- "comp <- c(-1)
for(i in 1:306){
  if(sum(is.na(dailyreturns[,i])) == 0 ){         ## remove quotes to actually test correlation ##
    comp <- cbind(comp,i)
  }
}
comp<-comp[-1]
comp
comp <- colnames(dailyreturns[,comp])
cors <- numeric()
for(i in 1:length(comp)){
  temp <- svmpreds(comp[i] , 2000)
  cors <- cbind(cors , temp$rets.corr)
}
c(min(cors) , max(cors))
cors
mean(cors)"
