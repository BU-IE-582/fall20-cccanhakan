library(ggplot2)
library(dplyr)
epl2021 <- read.csv(file = 'data/prem 20-21.csv')
epl1920 <- read.csv(file = 'data/prem 19-20.csv')
epl1819 <- read.csv(file = 'data/prem 18-19.csv')

total <- rbind(epl2021,epl1920)
total <- dplyr::bind_rows(total,epl1819) #for merging this easily i used dplyr's bind_rows() function


 
t1a <- ggplot(total, aes(x=FTHG)) + geom_bar(stat="count") +
  labs(title="Home Goals in EPL\n(since the season 18/19)", y="Number of Games", x = "Home Goals")
t1a
ggsave("task1a.png",plot=t1a)
t1b <- ggplot(total, aes(x=FTAG)) + geom_bar(stat="count") +
  labs(title="Away Goals in EPL\n(since the season 18/19)", y="Number of Games", x = "Away Goals")
t1b
t1c <- ggplot(total, aes(x=FTHG-FTAG)) + geom_bar(stat="count") +
  labs(title="The Difference Between Goals Scored in EPL\n(since the season 18/19)", y="Number of Games", x = "Home Goals - Away Goals") +
  geom_density(alpha = .2, fill="#FF6655")
t1c  



s_mean = mean(total$FTHG + total$FTAG) # home + away goals



p1 <- ggplot(total, aes(x=FTHG-FTAG)) + geom_histogram(aes(y=..density..),binwidth=1,colour="black", fill="grey") 
p1
p2 <- p1 + stat_function(fun=dpois, n=10333, args = list(lambda=s_mean))
p2




ggplot(total, aes(x=FTHG-FTAG)) +
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1,colour="black", fill="grey") +
  stat_function(fun = dpois, n=201, args = list(lambda=s_mean), geom = "point")

ggplot() + stat_function(fun = dpois, n=201, args = list(lambda=s_mean), geom = "point")
  
#stat_function(aes(x=FTHG-FTAG), fun = dpois,
#  args = list(lambda = mean),xlim = c(-10,10))   


base <- ggplot() + xlim(-10, 10)


base + stat_function(fun = dpois(-10:10,lambda = s_mean))

