# Forecasting Returns and Heuristic Portfolio Optimization

This exploratory project was done as a part of the Summer School in Mathematical Finance 2019 at Chennai Mathematical Institute. 

# Structure of the Project:

1. The primary data that is used in the project is contained within the demo_dailydata.rda file (which is not present in the repo due to license issues), which consists of two dataframes background and dailyreturns. This project only uses the dailyreturns dataframe (improvements to stock selection maybe done with the background dataframe which includes average liquidity measures, market cap data etc, but the project does not use it as of now.). The dailyreturns dataframe consists of daily return data of 306 NSE-listed companies in the time interval 2005-01-04 to 2015-12-31. The .rda file maybe replaced with the user's own dataset of daily returns (while, of course, making appropriate changes to the code).

2. port_2.R implements 4 different portfolio optimization schemes which are described further below. It also imports the svm_2.R code in the very beginning which in turn imports all the libraries necessary for the project and implements the function svmpreds() which is used to get the predicted weekly returns.

# svm_2.R :

This script first imports all the necessary libraries and the dataset from demo_dailydata.rda. It then implements the function svmpreds. svmpreds takes the name (or index) of the company and the size of the training set as the input. 
It then uses the values of several technical indicators(derived from the prices) and the price of the stock from day (t-1) to predict the price of day (t)  a by fitting an SVM(eps-regression) model. It also parallelly fits a linear model with the same features. 

Further, it calculates the predicted weekly returns from the predicted prices for both models. Naive model selection is done based on RMSE of the test set. The last line of the script contains a little piece of code to test correlation between the predicted returns and actual returns of the test set.

(Weekly returns is being calculated specifically because both models seemed to behave very poorly in the daily sense but had very significant long term performance as seen by the correlation between the actual.price and the pred.price which confirms the findings of this [paper](https://www.cs.princeton.edu/sites/default/files/uploads/saahil_madge.pdf).)

# port_2.R :

This is the crux of the project. It first sets up for the portfolio optimization by calling svm_2.R (which imports packages and gives svmpreds). It also downloads Nifty50 data to compare the performance of the optimized portfolio against Nifty as the market. It then implements four different portfolio optimization schemes. (Stocks for (1.) and (2.) were selected based on availability of data and arbitrarily for (3.) and (4.))

 1. Simple Markowitz Optimization : Basic Markowitz optimization is implemented. Annotations in code are self-explanatory.
 
 2. Windowed Markowitz Optimization : Here, the portfolio is changed every 3 months based on Markowitz optimization with the past 3 months data (with unconstrained expected returns and covariance shrinkage). Annotations in code are self-explanatory.
 
 3. Random Portfolio Optimization : This part of the code uses the random portfolio optimization scheme offered by the PortfolioAnalytics package. It uses the predicted weekly returns from the SVM/Linear Model to come up with a set of weights for the relevant period by optimizing objectives like stddev with long-only constraints, etc. 
 
 4. Genetic Algorithm : Like the previous scheme it uses the predicted weekly returns to come up with weights for the relevant period by optimizing objectives like stddev, sharpe ratio etc with long-only constraints. 
 
(Note however that to calculate the weekly returns by day (t+8) we would need the daily return of day (t+7) which is not available at day (t). So, the last two schemes implicity use future data and are not very useful in real situations, which is why it is an exploratory project. However, we may use the predicted and actual returns of the test set to get an idea of the effectiveness of the SVM model and the portfolio optimization scheme.)

Finally all the data about the portfolio returns, volatility etc are aggregated in the end of the script for further study and investigation.

(The final plot of the values of the portfolios if 1 unit was invested in each at the start of the common test period : ![Rplot.jpeg](https://github.com/rabhinav42/StockSVM-PortOpt/blob/master/Rplot.jpeg) where the colours are as specified in the R code.) 
