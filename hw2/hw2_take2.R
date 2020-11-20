library(ggplot2)
library(dplyr)
epl2021 <- read.csv(file = 'data/prem 20-21.csv')
epl1920 <- read.csv(file = 'data/prem 19-20.csv')
epl1819 <- read.csv(file = 'data/prem 18-19.csv')

total <- rbind(epl2021,epl1920)
total <- dplyr::bind_rows(total,epl1819) #for merging this easily i used dplyr's bind_rows() function

home_goals <- total$FTHG
away_goals <- total$FTAG
m1 <- mean(home_goals)
m2 <- mean(away_goals)
# zerogames <- total[total$FTHG == 1,]
# nrow(zerogames)

range_goals <- 0:max(max(total$FTHG),max(total$FTAG))
h1 <- hist(home_goals,col="red", xlab="Home Goals",ylab="Number of Games", main="HOME GOALS in EPL last 3 seasons",freq=FALSE)
lines(range_goals, dpois(range_goals,lambda = m1), col="blue", lwd=2)

h2 <- hist(away_goals,col="red", xlab="Away Goals", ylab="Number of Games", main="AWAY GOALS in EPL last 3 seasons",freq=FALSE)
lines(range_goals, dpois(range_goals,lambda = m1), col="blue", lwd=2)

h3 <- hist(home_goals - away_goals,col="red", xlab="Home Goals - Away Goals", ylab="Number of Games", main="HOME GOALS - AWAY GOALS in EPL last 3 seasons",freq=TRUE)

print("end")



x <- mtcars$mpg 
h<-hist(x, breaks=10, col="red", xlab="Miles Per Gallon", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)
