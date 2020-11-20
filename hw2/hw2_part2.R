library(ggplot2)
library(dplyr)
epl2021 <- read.csv(file = 'data/prem 20-21.csv')
epl1920 <- read.csv(file = 'data/prem 19-20.csv')
epl1819 <- read.csv(file = 'data/prem 18-19.csv')

total <- rbind(epl2021,epl1920)
total <- dplyr::bind_rows(total,epl1819) #for merging this easily i used dplyr's bind_rows() function

# 4 bookmarkers : B365, BW, IW, PS

par(mfrow=c(2,2)) # we can start early

# BOOKMARKER 1

home_b1 <- 1/total$B365H
away_b1 <- 1/total$B365A
draw_b1 <- 1/total$B365D

# now let's normalize

home_b1n <- home_b1/(home_b1 + away_b1 + draw_b1)
away_b1n <- away_b1/(home_b1 + away_b1 + draw_b1)
draw_b1n <- draw_b1/(home_b1 + away_b1 + draw_b1)

# print(home_b1n+away_b1n+draw_b1n) we can see the each sum is 1.


# FTR fulltimeresult = 'D'

odd_diff <- home_b1n-away_b1n

total <- cbind(total,odd_diff)


res_list <- c() # res_list[0] is the ratio: draw/all in the threshold (-1,-0.8]

for (n in seq(-1,0.8,by=0.2)) {
  checker <- c()
  for (odd in total$odd_diff){
    checker_val <- odd > n && odd <= n+0.2
    checker <- c(checker, checker_val)
  }
  partial_sum <- sum(checker) # Number of games in the odd category
  print(partial_sum)
  temp <- total[checker,]
  checker2 <- temp$FTR == "D"
  partial_sum2 <- sum(checker2) # Number of games in the category and resulted in draw
  print(partial_sum2)
  partial_div <- partial_sum2/partial_sum
  res_list <- c(res_list,partial_div) # draw/all
  print(partial_div)
  print('---')
}

res_list

plot_data_1 <- cbind(seq(-0.9,0.9,by=0.2),res_list)
plot_data_1

first_plot_1 <- plot(home_b1n - away_b1n, draw_b1n, main="Normalized Probabilities B365", xlab="P(Home) - P(Away) of bookmarker", ylab="P(Draw) of bookmarker")
points(plot_data_1,col="red")

# BOOKMARKER 2

home_b1 <- 1/total$BWH
away_b1 <- 1/total$BWA
draw_b1 <- 1/total$BWD

# now let's normalize

home_b1n <- home_b1/(home_b1 + away_b1 + draw_b1)
away_b1n <- away_b1/(home_b1 + away_b1 + draw_b1)
draw_b1n <- draw_b1/(home_b1 + away_b1 + draw_b1)

# print(home_b1n+away_b1n+draw_b1n) we can see the each sum is 1.


# FTR fulltimeresult = 'D'

odd_diff <- home_b1n-away_b1n

total <- cbind(total,odd_diff)


res_list <- c() # res_list[0] is the ratio: draw/all in the threshold (-1,-0.8]

for (n in seq(-1,0.8,by=0.2)) {
  checker <- c()
  for (odd in total$odd_diff){
    checker_val <- odd > n && odd <= n+0.2
    checker <- c(checker, checker_val)
  }
  partial_sum <- sum(checker) # Number of games in the odd category
  print(partial_sum)
  temp <- total[checker,]
  checker2 <- temp$FTR == "D"
  partial_sum2 <- sum(checker2) # Number of games in the category and resulted in draw
  print(partial_sum2)
  partial_div <- partial_sum2/partial_sum
  res_list <- c(res_list,partial_div) # draw/all
  print(partial_div)
  print('---')
}

res_list

plot_data_2 <- cbind(seq(-0.9,0.9,by=0.2),res_list)
plot_data_2

first_plot_2 <- plot(home_b1n - away_b1n, draw_b1n, main="Normalized Probabilities BW", xlab="P(Home) - P(Away) of bookmarker", ylab="P(Draw) of bookmarker")
points(plot_data_2,col="red")

# BOOKMARKER 3

home_b1 <- 1/total$IWH
away_b1 <- 1/total$IWA
draw_b1 <- 1/total$IWD

# now let's normalize

home_b1n <- home_b1/(home_b1 + away_b1 + draw_b1)
away_b1n <- away_b1/(home_b1 + away_b1 + draw_b1)
draw_b1n <- draw_b1/(home_b1 + away_b1 + draw_b1)

# print(home_b1n+away_b1n+draw_b1n) we can see the each sum is 1.


# FTR fulltimeresult = 'D'

odd_diff <- home_b1n-away_b1n

total <- cbind(total,odd_diff)


res_list <- c() # res_list[0] is the ratio: draw/all in the threshold (-1,-0.8]

for (n in seq(-1,0.8,by=0.2)) {
  checker <- c()
  for (odd in total$odd_diff){
    checker_val <- odd > n && odd <= n+0.2
    checker <- c(checker, checker_val)
  }
  partial_sum <- sum(checker) # Number of games in the odd category
  print(partial_sum)
  temp <- total[checker,]
  checker2 <- temp$FTR == "D"
  partial_sum2 <- sum(checker2) # Number of games in the category and resulted in draw
  print(partial_sum2)
  partial_div <- partial_sum2/partial_sum
  res_list <- c(res_list,partial_div) # draw/all
  print(partial_div)
  print('---')
}

res_list

plot_data_3 <- cbind(seq(-0.9,0.9,by=0.2),res_list)
plot_data_3

first_plot_3 <- plot(home_b1n - away_b1n, draw_b1n, main="Normalized Probabilities IW", xlab="P(Home) - P(Away) of bookmarker", ylab="P(Draw) of bookmarker")
points(plot_data_3,col="red")

# BOOKMARKER 4


home_b1 <- 1/total$IWH
away_b1 <- 1/total$IWA
draw_b1 <- 1/total$IWD

# now let's normalize

home_b1n <- home_b1/(home_b1 + away_b1 + draw_b1)
away_b1n <- away_b1/(home_b1 + away_b1 + draw_b1)
draw_b1n <- draw_b1/(home_b1 + away_b1 + draw_b1)

# print(home_b1n+away_b1n+draw_b1n) we can see the each sum is 1.


# FTR fulltimeresult = 'D'

odd_diff <- home_b1n-away_b1n

total <- cbind(total,odd_diff)


res_list <- c() # res_list[0] is the ratio: draw/all in the threshold (-1,-0.8]

for (n in seq(-1,0.8,by=0.2)) {
  checker <- c()
  for (odd in total$odd_diff){
    checker_val <- odd > n && odd <= n+0.2
    checker <- c(checker, checker_val)
  }
  partial_sum <- sum(checker) # Number of games in the odd category
  print(partial_sum)
  temp <- total[checker,]
  checker2 <- temp$FTR == "D"
  partial_sum2 <- sum(checker2) # Number of games in the category and resulted in draw
  print(partial_sum2)
  partial_div <- partial_sum2/partial_sum
  res_list <- c(res_list,partial_div) # draw/all
  print(partial_div)
  print('---')
}

res_list

plot_data_4 <- cbind(seq(-0.9,0.9,by=0.2),res_list)
plot_data_4

first_plot_4 <- plot(home_b1n - away_b1n, draw_b1n, main="Normalized Probabilities PS", xlab="P(Home) - P(Away) of bookmarker", ylab="P(Draw) of bookmarker")
points(plot_data_4,col="red")
