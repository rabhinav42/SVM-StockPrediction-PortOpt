set.seed(123)
source("E:/cmi2019/asdf/svm_2.R")   ## change directory to wherever svm_2.R is stored ##
str(dailyreturns)

comp <- c(-1)
for(i in 1:306){
  if(sum(is.na(dailyreturns[,i])) == 0 ){
    comp <- cbind(comp,i)
  }
}
comp<-comp[-1]
colnames(dailyreturns[,comp])
retdata <- as.xts(dailyreturns[,comp])
companies <- c("BHARTIARTL","DHFL","WIPRO","NDTV","JINDALSTEL","ICICIBANK","FEDERALBNK")

Nifty.50<-get.hist.quote(instrument = "^NSEI"
                         ,start="2014-01-01"
                         ,end="2015-12-31"
                         ,quote="AdjClose"
                         ,provider = "yahoo")
any(is.na(Nifty.50))
Nifty.50 <- as.xts(na.omit(Nifty.50))

Nifty.50.full<-get.hist.quote(instrument = "^NSEI"
                              ,start="2005-04-01"
                              ,end="2015-12-31"
                              ,quote="AdjClose"
                              ,provider = "yahoo")
any(is.na(Nifty.50.full))
Nifty.50.full <- as.xts(na.omit(Nifty.50.full))

retsw.1 <- data.frame()
predretsw <- data.frame()                       ## initialize containers to store relevant variables ##
rmses <- numeric()                                   
choices <- numeric()
cors <- numeric()

for(i in 1:length(companies)){
  temp <- svmpreds(companies[i] , 2000)
  retsw.1 <- cbind(retsw.1 , temp$actualw)
  predretsw <- cbind(predretsw , temp$predictedw)
  rmses <- cbind(rmses , rmse(as.numeric(retsw.1[,i]) , as.numeric(predretsw[,i])))
  choices <- cbind(choices , temp$choice)
  cors <- cbind(cors , temp$rets.corr)
}

colnames(retsw.1) <- companies
colnames(predretsw) <- companies
rmses                   ## RMSE of the predicted weekly returns ##
c(min(rmses) , max(rmses))
c(min(cors) , max(cors))
cors
choices     ## see companies for which "2"-lm is better than "1"-svm ##

vol <- list()
rets.port <- list()             ## lists that contain some values relevant to each portfolio ##
wts <- list()


################################################################

## simple Markowitz Optimization ##

sigma <- cov(retdata['2005/2013-12-31'])
port.optim <- portfolio.optim(retdata['2005/2013-12-31'],
                              covmat = sigma,
                              rf = 4/252,
                              shorts = FALSE,
                              reslow = rep(0,51),
                              reshigh = rep(1,51))

wts$mark <- port.optim$pw
portret <- Return.portfolio(R = retdata['2014-01-01/2015-12-31'] , weights = port.optim$pw , verbose = FALSE)
vol$mark <- portvol(tickers = colnames(retdata) , weights = port.optim$pw , start = "2014-01-01" , end = "2015-12-31" , data = retdata['2014-01-01/2015-12-31'])

par(mfrow = c(1,2))
plot(cumprod(1+portret))
plot(Nifty.50/max(Nifty.50))

reldates <- index(Nifty.50)
ln_Nifty.50 <- diff(log(Nifty.50))
ln_Nifty.50 <- 1 + ln_Nifty.50
ln_Nifty.50[1] <- 1

portrets <- (1+portret)[reldates]
portrets[1] <- 1 

## compare against Nifty 50 returns ##

par(mfrow = c(1,1))
plot(as.xts(cbind(cumprod(portrets) , cumprod(ln_Nifty.50))) , plot.type = "single" , col = c("red" , "black"))
as.numeric(cor(cumprod(ln_Nifty.50) , cumprod(portrets)))
rets.port$mark <- cumprod(portrets)['2015-12-31']-1


################################################################

## Windowed Markowitz Optimization ##

## observe the market from 2005-2006 and then start investing from 2006-01-01 ##

splitrets <- split(retdata , f = "months" , k=3)   ### split data into intervals of 3 months
n <- length(splitrets)
sig <- cov_shrink(splitrets[[1]])              ### shrunk covariance matrix
port.opt <- list()

port.opt[[1]] <- portfolio.optim(splitrets[[1]],         ### weights for next three months based on first three months
                                 covmat = sig,
                                 rf = 4/252,
                                 shorts = FALSE,
                                 reslow = rep(0,51),
                                 reshigh = rep(1,51))
port.opt[[1]]$enddate <- end(splitrets[[1]])

for(i in 2:(n-1)){                                                  ### eval. new sigma (shrunk) every 3 months based on new data
  #splitrets[[i]] <- rbind(splitrets[[i-1]] , splitrets[[i]])        ### and generate weights
  sig <- cov_shrink(splitrets[[i]])
  port.opt[[i]] <- portfolio.optim(splitrets[[i]],                  ### uncomment line180 for using aggregate returns
                                   covmat = sig,
                                   rf = 4/252,
                                   shorts = FALSE,
                                   reslow = rep(0,51),
                                   reshigh = rep(1,51))
  port.opt[[i]]$enddate <- end(splitrets[[i]])  
}


### find returns ###

splitrets.1 <- split(retdata , f = "months" , k=3)
wts$windmark <- list()
wts$windmark[[1]] <- port.opt[[1]]$pw
newrets <- Return.portfolio(R = splitrets.1[[2]] , weights = port.opt[[1]]$pw , verbose = FALSE)
portvols <- numeric()
portvols[1] <- portvol(tickers = colnames(retdata) , weights = port.opt[[1]]$pw , start = start(splitrets.1[[2]]) , end = end(splitrets.1[[2]]) , data = splitrets.1[[2]])
enddates <- as.Date(port.opt[[1]]$enddate)


for(i in 2:(n-1)){
  wts$windmark[[i]] <- port.opt[[i]]$pw
  newrets <- rbind(newrets , Return.portfolio(R = splitrets.1[[i+1]] , weights = port.opt[[i]]$pw , verbose = FALSE))
  portvols[i]<-portvol(tickers = colnames(retdata) , weights = port.opt[[i]]$pw , start = start(splitrets.1[[i+1]]) , end = end(splitrets.1[[i+1]]) , data = splitrets.1[[i+1]])
  enddates <- rbind(enddates , port.opt[[i]]$enddate)
}

### Compare against Nifty.50 ###

reldates.full <- index(Nifty.50.full)

par(mfrow = c(1,2))
plot(cumprod(1+newrets)['2005-04-01/2015-12-31'])
plot(Nifty.50.full)

rets1 <- (1+newrets)[reldates.full]
rets1[1] <- 1

Nifty.2 <- diff(log(Nifty.50.full))
Nifty.2 <- 1 + Nifty.2
Nifty.2[1] <- 1

par(mfrow = c(1,1))
plot(as.xts(cbind(cumprod(rets1) , (cumprod(Nifty.2)))) , plot.type = "single" , col = c("red" , "black"))
as.numeric(cor(cumprod(Nifty.2) , cumprod(rets1)))

rets.port$windmark <- cumprod(rets1)['2015-12-31']-1

enddates <- as.Date(enddates)
portvols <- as.xts(portvols , order.by = enddates)
vol$windmarkmean <- mean(portvols)
plot(portvols)
################################################################

### Brute Force Randomized Weight selection ###

myport <- portfolio.spec(assets = companies)
myport <- add.constraint(myport , type = "weight_sum" , min_sum=0.99 , max_sum=1.01)
myport <- add.constraint(myport , type = "box" , min=0 , max=1)
myport.norm <- add.objective(myport , type = "risk" , name = "StdDev" , target = 0.02)

rp <- random_portfolios(myport.norm , permutations = 5000 , method = "sample")
opt.port <- optimize.portfolio(predretsw['2014-01-01/2015-12-31'] ,
                               portfolio = myport.norm,
                               optimize_method = "random" , rp=rp,
                               trace = TRUE)
summary(opt.port)
opt.port$objective_measures
wts$rand <- opt.port$weights

### Calculate returns ###

retport1 <- Return.portfolio(R = retdata['2014-01-01/2015-12-31' , companies] , weights = opt.port$weights , verbose = FALSE)

### Volatility of the portfolio ###

vol$rand <- portvol(tickers = companies , weights = opt.port$weights , start = "2014-01-01" , end = "2015-12-31" , data = retdata['2014-01-01/2015-12-31',companies])

### compare cumulative returns of portfolio against Nifty-50 ###

par(mfrow = c(1,2))
plot(cumprod(1+retport1))
plot(Nifty.50/max(Nifty.50))

reldates <- index(Nifty.50)
ln_Nifty.50 <- diff(log(Nifty.50))
ln_Nifty.50 <- 1+ln_Nifty.50
ln_Nifty.50[1] <- 1

retports <- (1+retport1)[reldates]
retports[1] <- 1 

### compare against Nifty-50 returns ###

par(mfrow = c(1,1))
plot(as.xts(cbind(cumprod(retports) , cumprod(ln_Nifty.50))) , plot.type = "single" , col = c("red" , "black"))
as.numeric(cor(cumprod(ln_Nifty.50) , cumprod(retports)))
rets.port$rand <- cumprod(retports)['2015-12-31']-1

################################################################

### Genetic algorithm to optimize portfolio ###

retsga <- predretsw['2014-01-01/2015-12-31' , companies]
retsac <- retdata['2014-01-01/2015-12-31' , companies]

#test_portfolio_returns <- function(x) {        # use this function while testing how GA does with a better forecasting mechanism
#  port.rets = 0                                # replace every portfolio_returns(x) with this function below for testing above.
#  for(i in 1:length(x)){
#    port.rets = port.rets+retsac[,i]*x[i]
#  }
#  return(port.rets)
#}

portfolio_returns <- function(x) {
  port.rets = 0
  for(i in 1:length(x)){
    port.rets = port.rets+retsga[,i]*x[i]
  }
  return(port.rets)
}

stddev <- function(x){
  port.returns <- portfolio_returns(x)       
  return(sqrt(var(port.returns)))
}

sharpe_ratio <- function(x){
  port.returns <- portfolio_returns(x)
  return(mean(port.returns)/sqrt(var(port.returns)))
}

## define penalty function for constraints ##

constraint = function(x){
  boundry_constr = (sum(x)-1)**2
  
  for (i in 1:length(x)) {
    boundry_constr=boundry_constr+
      max(c(0,x[i]-1))**2 +
      max(c(0,-x[i]))**2
  }
  return(boundry_constr)
}

## objective function(s) for genetic optimization ##

obj1 <- function(x){
  return(-stddev(x)+100*constraint(x))
}

obj2 <- function(x){
  return(-sharpe_ratio(x)+100*constraint(x))
}

## genetic algorithm ##

ga_res = ga(type="real-valued",
            function(x){-obj2(x)},
            lower = rep(0,ncol(retsga)),
            upper = rep(1,ncol(retsga)),
            maxiter = 50000,
            run = 50,
            parallel = TRUE,
            monitor = TRUE,
            seed = 1)


sol <- as.numeric(summary(ga_res)$solution)
wts$ga <- sol

optimal_returns <- Return.portfolio(R = retdata['2014-01-01/2015-12-31' , companies] ,
                                    weights = sol ,
                                    verbose = FALSE)
vol$ga <- portvol(tickers = companies ,
                  weights = sol ,
                  start = "2014-01-01" ,
                  end = "2015-12-31" ,
                  data = retdata['2014-01-01/2015-12-31',companies])

optimal_returns_comp <- as.xts(1+optimal_returns)
optimal_returns_comp[1] <- 1

Nifty <- diff(log(Nifty.50))
Nifty <- 1 + Nifty
Nifty[1] <- 1

d <- index(Nifty)


### compare against Nifty50 returns ###

par(mfrow = c(1,1))
plot(as.xts(cbind(cumprod(optimal_returns_comp)[d] , cumprod(Nifty))) , plot.type = "single" , col = c("red" , "black"))
as.numeric(cor(cumprod(Nifty) , cumprod(optimal_returns_comp)[d]))
as.numeric(stddev(sol))
rets.port$ga <- cumprod(optimal_returns_comp)['2015-12-31']-1

################################################################

rets.port
vol
as.numeric(rets.port)/as.numeric(vol)
wts

port.ga <- cumprod(optimal_returns_comp)[d]
port.rand <- cumprod(retports)
port.windmark <- cumprod(rets1)
port.mark <- cumprod(portrets)

common.period <- index(port.ga)

plot(cbind(port.ga[common.period] , port.rand[common.period] , port.windmark[common.period] , port.mark[common.period] , cumprod(Nifty)[common.period]) , col = c("red" , "blue" , "brown" , "violet" , "black"))
