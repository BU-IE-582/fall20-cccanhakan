require(ggplot2, quietly=TRUE)
require(data.table, quietly=TRUE)
require(MLmetrics,quietly=TRUE)
require(glmnet,quietly=TRUE)
require(tidyverse,quietly=TRUE)

x <- read.csv("/Users/canhakan/Downloads/hw3data_utf.csv", header = TRUE, dec=",",check.names = F)

colnames(x) <- c('Date','Time','Consumption')
x$Consumption <- gsub('\\.','',x$Consumption)
x$Consumption <- gsub(',','\\.',x$Consumption)
x$Consumption <- as.numeric(x$Consumption)

lag48_end <- length(x$Consumption)-48
lag48 <- c(rep(0,48),x$Consumption[1:lag48_end])

lag168_end <- length(x$Consumption)-168
lag168 <- c(rep(0,168),x$Consumption[1:lag168_end])
x <- cbind(x,lag48,lag168)

# xu$Date <- as.Date(xu$Date, "%d.%m.%Y")
# xu$Time <- as.numeric(sub(':.*','',xu$Time))
#
x$Date <- as.Date(x$Date, "%d.%m.%Y")
x$Time <- as.numeric(sub(':.*','',x$Time))

lag48_mape  <- MAPE(x[x$Date >= '2020-11-01',]$lag48,  x[x$Date >= '2020-11-01',]$Consumption)
cat('lag48 mape value is:',lag48_mape,'\n')

lag168_mape <- MAPE(x[x$Date >= '2020-11-01',]$lag168, x[x$Date >= '2020-11-01',]$Consumption)
cat('lag168 mape value is:',lag168_mape)

# end_point <- length(x$Consumption)
# 24*31=744 instances (November 1 to December 1) # bunu date olarak degisiciim
# st_point <- end_point-743

# lag48_mape <- MAPE(x$lag48[st_point:end_point],x$Consumption[st_point:end_point])
# cat('lag48 mape value is:',lag48_mape,'\n')

# lag168_mape <- MAPE(x$lag168[st_point:end_point],x$Consumption[st_point:end_point])
# cat('lag168 mape value is:',lag168_mape)

We can see that the mean absolute error in lag 48 is more than double of the error in lag 168.
In other words, in a naive approach the data from the same day of last week is much more valuable than the data from two days ago.As they have no lag168 or lag48 data we remove the first 7 days below:
end_point <- length(x$Consumption)
x <- x[169:end_point,]
Also we remove the corrupt days below,
which are: 2016-03-27, 2016-03-29, 2016-04-03 (formatted as yyyy-mm-dd)
x <- x[x$Date != '2016-03-27',]
x <- x[x$Date != '2016-03-29',]
x <- x[x$Date != '2016-04-03',]
Here we split our data as Train/Test. Then do a linear regression with the train data using lag48 and lag168 to be a predictor of Consumption.
x_train <- x[x$Date<"2020-11-01",]
x_test <- x[x$Date>="2020-11-01",]

set.seed(2021)
lin_fit <- lm(Consumption~lag48+lag168, x_train)
summary(lin_fit)
Comments on above!

and let's do a prediction for the test dates.
x_pred <- predict(lin_fit,x_test)
We can now calculate the "Mean Absolute Percentage Error":
lin_mape <- MAPE(x_pred, x_test$Consumption)
cat('MAPE value for our linear model is ', lin_mape)
We can see that that MAPE is worse than naive lag_168. So it was not an effective approach.As mentioned earlier, hourly seasonality is important. Although we used the same hour’s consumption value of the past days to handle this problem for part (b), we implicitly impose an assumption that prediction model for each hour has the same coefficients which may not be correct since the consumption behavior at nights can be different than the other hours. Therefore, modeling each hour separately is another way to approach to the same problem. Train linear regression models for each hour using the same training period (24 models) and report your test performance as in part (a).
Now we will check each hour seperately. Similar to what we did in part (b). Only we now create a for loop for each hour and use the data from the same hour for training.
res_mape <- data.frame('Hours'=integer(),
                 'MAPE'=double())
for (t in c(0:23)){
    x_temp <- x[x$Time == t,]
    x_temp_train <- x_temp[x_temp$Date < '2020-11-01',]
    x_temp_test <- x_temp[x_temp$Date >= '2020-11-01',]
    fit_temp <- lm(Consumption~lag48+lag168,x_temp_train)
    x_temp_pred <- predict(fit_temp,x_temp_test)
    res_mape <- rbind(res_mape,c(t,MAPE(x_temp_pred,x_temp_test$Consumption)))
}
colnames(res_mape) <- c('Hour', 'MAPE')
res_mape
Here we can see some of the values are a little better than naive lag_168 approach. Let's take a closer look:
res_mape <- cbind(res_mape,res_mape[,2]-lag168_mape)
res_mape[res_mape[,3] < 0,]
Interestingly, between 7pm and 6am (the darker half of the day), our regression has less error than the naive approach.One of your friends comes up with an alternative approach assuming that all hourly consumption values of last week (same day) can be important in the prediction of the next day’s consumption. In other words, you can use the 24 consumption values of the last week to predict next day’s consumption. This requires the transformation of your data into a so called “wide” format from the “long” format (i.e. the form of the data you used in part (a) is referred to as “long” format). Figure 2 illustrates the wide format for predicting the consumption of 18th hour.

Assume that you have 48 features (hourly consumption from two days ago and last week’s hourly consumption) in total. You are also willing to follow the same logic in part (c) and build a prediction model for each hour separately. Since there is a strong correlation (actually an autocorrelation) between these predictors, you are willing to use penalized regression approaches for modeling. Use L1 penalty in your regression models for each hour. Note that the feature matrix will be the same for all your models, only the target variable will change for this task. In order to determine the regularization parameter (i.e. lambda), perform a 10-fold cross-validation.

Train penalized regression models with L1 penalty (i.e. lasso regression) for each hour using the same training period (24 models) and report your test performance as in part (a). Also comment on the resulting models (i.e. coefficients and etc.).First, we will transform our data to 'wide' format for each hour. So we will have 24 different 'wide' data.
# Get the unique dates
unique_dates <- unique(x$Date)
For representation, I will show the process with detail for Hour 0:
m <- data.frame()
for (date_index in 1:length(unique_dates)){
    date <- as.Date(unique_dates[date_index],origin='2020-11-24')
    m <- rbind(m, cbind.data.frame(date, t(x[x$Date == date,]$lag168), t(x[x$Date == date,]$lag48), x[x$Date == date,][x$Time == 0,]$Consumption[1]))
}
for (j in c(2:25)){
    colnames(m)[j] <- paste('Lag_day7_hour',j-2,sep='_')
}
for (j in c(26:49)){
    colnames(m)[j] <- paste('Lag_day2_hour',j-26,sep='_')
}
colnames(m)[50] <- 'Consumption_0'
colnames(m)[1] <- 'Date'

m
We can see that the first column represents the date which is now unique in our dataframe. Columns from 2 to 25 represent lag168 values of the same day and columns from 26 to 49 represent lag48 values. The 50th column has the actual consumption value at 12 AM of the given date.Now, we split our data and do 10-fold cross validation to find a good lambda.
wide_data <- m
wide_train <-wide_data[wide_data$Date < '2020-11-01', ]
wide_test <- wide_data[wide_data$Date >= '2020-11-01', ]

set.seed(2021)
lasso_d = cv.glmnet(as.matrix(wide_train[,2:49]),as.matrix(wide_train[,50]),nfolds=10,family='gaussian')
plot(lasso_d)
lasso_d
While 2.4 gives the minimum value for lambda, 8.2 is also within 1 standard error. We will use both lambdas and compare them:
model_d_min <- glmnet(as.matrix(wide_train[,2:49]),as.matrix(wide_train[,50]), alpha = 1, lambda = lasso_d$lambda.min)
model_d_1se <- glmnet(as.matrix(wide_train[,2:49]),as.matrix(wide_train[,50]), alpha = 1, lambda = lasso_d$lambda.1se)

predicted_wide_min <- predict(model_d_min,as.matrix(wide_test[,2:49]))
predicted_wide_1se <- predict(model_d_1se,as.matrix(wide_test[,2:49]))

cat('MAPE value for lambda_min is ', MAPE(predicted_wide_min,wide_test[,50]), '\n')
cat('MAPE value for lambda_1se is ', MAPE(predicted_wide_1se,wide_test[,50]))
We can see that the results are pretty close and the error is very low compared to other approaches. Now we will do the same process for other hours and show the errors in a table.
mape_d <- data.frame('Hours'=integer(),
                     'MAPE_min'=double(),
                     'MAPE_1se'=double())
for (t in c(0:23)){
    m <- data.frame()
    for (date_index in 1:length(unique_dates)){
        date <- as.Date(unique_dates[date_index],origin='2020-11-24')
        m <- rbind(m, cbind.data.frame(date, t(x[x$Date == date,]$lag168), t(x[x$Date == date,]$lag48), x[x$Date == date,][x$Time == t,]$Consumption[1]))
    }
    for (j in c(2:25)){
        colnames(m)[j] <- paste('Lag_day7_hour',j-2,sep='_')
    }
    for (j in c(26:49)){
        colnames(m)[j] <- paste('Lag_day2_hour',j-26,sep='_')
    }
    colnames(m)[50] <- paste('Consumption',t,sep='_')
    colnames(m)[1] <- 'Date'

    wide_data <- m
    wide_train <-wide_data[wide_data$Date < '2020-11-01', ]
    wide_test <- wide_data[wide_data$Date >= '2020-11-01', ]

    set.seed(2021)
    lasso_d = cv.glmnet(as.matrix(wide_train[,2:49]),as.matrix(wide_train[,50]),nfolds=10,family='gaussian')
#     plot(lasso_d)
#     lasso_d

    model_d_min <- glmnet(as.matrix(wide_train[,2:49]),as.matrix(wide_train[,50]), alpha = 1, lambda = lasso_d$lambda.min)
    model_d_1se <- glmnet(as.matrix(wide_train[,2:49]),as.matrix(wide_train[,50]), alpha = 1, lambda = lasso_d$lambda.1se)

    predicted_wide_min <- predict(model_d_min,as.matrix(wide_test[,2:49]))
    predicted_wide_1se <- predict(model_d_1se,as.matrix(wide_test[,2:49]))
    
    mape_d <- rbind(mape_d, c(t,  MAPE(predicted_wide_min,wide_test[,50]),  MAPE(predicted_wide_1se,wide_test[,50])))
    cat('MAPE value for lambda_min at hour', t, ' is ', MAPE(predicted_wide_min,wide_test[,50]), '\t')
    cat('MAPE value for lambda_1se at hour', t, ' is ', MAPE(predicted_wide_1se,wide_test[,50]), '\n')
}
We will compare MAPE values
mapes <- cbind(lag48_mape,lag168_mape, lin_mape, res_mape[,2], mape_d[,2], mape_d[,3])

colnames(mapes) <- c('Naive 48', 'Naive 168', 'Daily Linear', 'Hourly Linear', 'Lasso Min', 'Lasso 1se')
mapes

boxplot(mapes, las=2)
We can clearly see that Lasso regression approaches are much better than others and there is not much of a difference between the choices of lambda (min vs 1se).

Also recall that Hourly Linear values have nice results in evening-night times but their mean is still worse than a naive approach. But we can see that hour to hour training reduces the mean mape in linear model.
!jupyter nbconvert --execute duzenlihw3-Copy1.ipynb --to 'html'


