library(ggplot2)
library(scatterplot3d)
library(plotly)

#reading datas from files (double spaces and spaces are seperators)
xdata <- read.table(text = gsub("  ", "\t", readLines("uWaveGestureLibrary_X_TRAIN")))
ydata <- read.table(text = gsub("  ", "\t", readLines("uWaveGestureLibrary_Y_TRAIN")))
zdata <- read.table(text = gsub("  ", "\t", readLines("uWaveGestureLibrary_Z_TRAIN")))

# let's create a list of plots

#### plots <- data.frame....######
xyzvelo <- c(1:315) #empty df for adding the x,y,z velocity datas for each class
for (i in c(1:8)){ # for time series
  xvelo <- xdata[xdata$V1 == i,][1,2]
  yvelo <- ydata[ydata$V1 == i,][1,2]
  zvelo <- zdata[zdata$V1 == i,][1,2]
  for (j in c(3:316)){ # for time index
    xvelo <- cbind(xvelo,xvelo[j-2]+xdata[1,j]) # we get a vector of size 1x315
    yvelo <- cbind(yvelo,yvelo[j-2]+ydata[1,j])
    zvelo <- cbind(zvelo,zvelo[j-2]+zdata[1,j])
  }
  xyzvelo <- cbind(xyzvelo,cbind(t(xvelo),t(yvelo),t(zvelo))) #columns are for X1,Y1,Z1,X2,Y2,Z2,... 
}

# let's print all plots
for (i in c(1:8)){
  scatterplot3d(xyzvelo[,3*i-1:3*i+1], xlab='X',ylab='Y',zlab='Z',main=c('class',i),box=FALSE, pch=21)
}

# part b

# create a data frame for storing the data as explained
combined <- data.frame(time_series_id=integer(0),time_index=integer(0),x=numeric(0),y=numeric(0),z=numeric(0),class=integer(0))

# for each time series we add 315 rows to the data frame (takes a bit long)
for (m in c(1:896)){
  xyz_tog <- cbind(rep(m,315),c(1:315),t(xdata[m,2:316]),t(ydata[m,2:316]),t(zdata[m,2:316]),rep(xdata[m,1],315))
  colnames(xyz_tog) <- colnames(combined)
  combined <- rbind(combined,xyz_tog)
}

# PCA

pca_res <- prcomp(combined[,3:5]) #built in pca function
var_explained <- pca_res$sdev^2/sum(pca_res$sdev^2) # checking variance 
var_explained # we can see the explanation rates 

# Adding the PCA results to the combined dataframe
combined <- cbind(combined, pca_res$x[,1])
colnames(combined)[7] <- 'PCA'


# Split df by class
cat1 <- combined[combined$class == '1',]
cat2 <- combined[combined$class == '2',]
cat3 <- combined[combined$class == '3',]
cat4 <- combined[combined$class == '4',]
cat5 <- combined[combined$class == '5',]
cat6 <- combined[combined$class == '6',]
cat7 <- combined[combined$class == '7',]
cat8 <- combined[combined$class == '8',]

# comparing 2 elements from each class
plot1 <- data.frame(time_index = c(1:315), var1 = cat1[1:315,]$PCA, var2 = cat1[316:630,]$PCA)
plot2 <- data.frame(time_index = c(1:315), var1 = cat2[1:315,]$PCA, var2 = cat2[316:630,]$PCA)
plot3 <- data.frame(time_index = c(1:315), var1 = cat3[1:315,]$PCA, var2 = cat3[316:630,]$PCA)
plot4 <- data.frame(time_index = c(1:315), var1 = cat4[1:315,]$PCA, var2 = cat4[316:630,]$PCA)
plot5 <- data.frame(time_index = c(1:315), var1 = cat5[1:315,]$PCA, var2 = cat5[316:630,]$PCA)
plot6 <- data.frame(time_index = c(1:315), var1 = cat6[1:315,]$PCA, var2 = cat6[316:630,]$PCA)
plot7 <- data.frame(time_index = c(1:315), var1 = cat7[1:315,]$PCA, var2 = cat7[316:630,]$PCA)
plot8 <- data.frame(time_index = c(1:315), var1 = cat8[1:315,]$PCA, var2 = cat8[316:630,]$PCA)

ggplot(plot1, aes(time_index,var1)) +
  geom_line(aes(y = var1, colour = "var1")) +
  geom_line(aes(y = var2, colour = "var2")) +
  ggtitle("Comparison of 2 elements from \n Class 1")
ggplot(plot2, aes(time_index,var1)) +
  geom_line(aes(y = var1, colour = "var1")) +
  geom_line(aes(y = var2, colour = "var2")) +
  ggtitle("Comparison of 2 elements from \n Class 2")
ggplot(plot3, aes(time_index,var1)) +
  geom_line(aes(y = var1, colour = "var1")) +
  geom_line(aes(y = var2, colour = "var2")) +
  ggtitle("Comparison of 2 elements from \n Class 3")
ggplot(plot4, aes(time_index,var1)) +
  geom_line(aes(y = var1, colour = "var1")) +
  geom_line(aes(y = var2, colour = "var2")) +
  ggtitle("Comparison of 2 elements from \n Class 4")
ggplot(plot5, aes(time_index,var1)) +
  geom_line(aes(y = var1, colour = "var1")) +
  geom_line(aes(y = var2, colour = "var2")) +
  ggtitle("Comparison of 2 elements from \n Class 5")
ggplot(plot6, aes(time_index,var1)) +
  geom_line(aes(y = var1, colour = "var1")) +
  geom_line(aes(y = var2, colour = "var2")) +
  ggtitle("Comparison of 2 elements from \n Class 6")
ggplot(plot7, aes(time_index,var1)) +
  geom_line(aes(y = var1, colour = "var1")) +
  geom_line(aes(y = var2, colour = "var2")) +
  ggtitle("Comparison of 2 elements from \n Class 7")
ggplot(plot8, aes(time_index,var1)) +
  geom_line(aes(y = var1, colour = "var1")) +
  geom_line(aes(y = var2, colour = "var2")) +
  ggtitle("Comparison of 2 elements from \n Class 8")



# part c

# calculating new pca's
new_pca_1 <- prcomp(cat1[,3:5])
new_pca_2 <- prcomp(cat2[,3:5])
new_pca_3 <- prcomp(cat3[,3:5])
new_pca_4 <- prcomp(cat4[,3:5])
new_pca_5 <- prcomp(cat5[,3:5])
new_pca_6 <- prcomp(cat6[,3:5])
new_pca_7 <- prcomp(cat7[,3:5])
new_pca_8 <- prcomp(cat8[,3:5])

# add the new pca to data frames
cat1 <- cbind(cat1,new_pca_1$x[,1])
cat2 <- cbind(cat2,new_pca_2$x[,1])
cat3 <- cbind(cat3,new_pca_3$x[,1])
cat4 <- cbind(cat4,new_pca_4$x[,1])
cat5 <- cbind(cat5,new_pca_5$x[,1])
cat6 <- cbind(cat6,new_pca_6$x[,1])
cat7 <- cbind(cat7,new_pca_7$x[,1])
cat8 <- cbind(cat8,new_pca_8$x[,1])

colnames(cat1)[8] <- 'newPCA'
colnames(cat2)[8] <- 'newPCA'
colnames(cat3)[8] <- 'newPCA'
colnames(cat4)[8] <- 'newPCA'
colnames(cat5)[8] <- 'newPCA'
colnames(cat6)[8] <- 'newPCA'
colnames(cat7)[8] <- 'newPCA'
colnames(cat8)[8] <- 'newPCA'

# Plotting with new PCA values

plot1_new <- data.frame(time_index = c(1:315), var1 = cat1[1:315,]$newPCA, var2 = cat1[316:630,]$newPCA)
ggplot(plot1_new, aes(time_index,var1)) +
  geom_line(aes(y = var1, colour = "var1")) +
  geom_line(aes(y = var2, colour = "var2")) + 
  ggtitle("Comparison of 2 elements from \n Class 1 with new PCA")
plot2_new <- data.frame(time_index = c(1:315), var1 = cat2[1:315,]$newPCA, var2 = cat2[316:630,]$newPCA)
ggplot(plot2_new, aes(time_index,var1)) +
  geom_line(aes(y = var1, colour = "var1")) +
  geom_line(aes(y = var2, colour = "var2")) + 
  ggtitle("Comparison of 2 elements from \n Class 2 with new PCA")
plot3_new <- data.frame(time_index = c(1:315), var1 = cat3[1:315,]$newPCA, var2 = cat3[316:630,]$newPCA)
ggplot(plot3_new, aes(time_index,var1)) +
  geom_line(aes(y = var1, colour = "var1")) +
  geom_line(aes(y = var2, colour = "var2")) + 
  ggtitle("Comparison of 2 elements from \n Class 3 with new PCA")
plot4_new <- data.frame(time_index = c(1:315), var1 = cat4[1:315,]$newPCA, var2 = cat4[316:630,]$newPCA)
ggplot(plot4_new, aes(time_index,var1)) +
  geom_line(aes(y = var1, colour = "var1")) +
  geom_line(aes(y = var2, colour = "var2")) + 
  ggtitle("Comparison of 2 elements from \n Class 4 with new PCA")
plot5_new <- data.frame(time_index = c(1:315), var1 = cat5[1:315,]$newPCA, var2 = cat5[316:630,]$newPCA)
ggplot(plot5_new, aes(time_index,var1)) +
  geom_line(aes(y = var1, colour = "var1")) +
  geom_line(aes(y = var2, colour = "var2")) + 
  ggtitle("Comparison of 2 elements from \n Class 5 with new PCA")
plot6_new <- data.frame(time_index = c(1:315), var1 = cat6[1:315,]$newPCA, var2 = cat6[316:630,]$newPCA)
ggplot(plot6_new, aes(time_index,var1)) +
  geom_line(aes(y = var1, colour = "var1")) +
  geom_line(aes(y = var2, colour = "var2")) + 
  ggtitle("Comparison of 2 elements from \n Class 6 with new PCA")
plot7_new <- data.frame(time_index = c(1:315), var1 = cat7[1:315,]$newPCA, var2 = cat7[316:630,]$newPCA)
ggplot(plot7_new, aes(time_index,var1)) +
  geom_line(aes(y = var1, colour = "var1")) +
  geom_line(aes(y = var2, colour = "var2")) + 
  ggtitle("Comparison of 2 elements from \n Class 7 with new PCA")
plot8_new <- data.frame(time_index = c(1:315), var1 = cat8[1:315,]$newPCA, var2 = cat8[316:630,]$newPCA)
ggplot(plot8_new, aes(time_index,var1)) +
  geom_line(aes(y = var1, colour = "var1")) +
  geom_line(aes(y = var2, colour = "var2")) + 
  ggtitle("Comparison of 2 elements from \n Class 8 with new PCA")

# part d

# we can just take x, y and z at each time series matrix and create the distance  matrix
dist_x <- dist(xdata[,2])
for (i in c(3:316)){
  dist_x <- dist_x + dist(xdata[,i])
}

dist_y <- dist(ydata[,2])
for (i in c(3:316)){
  dist_y <- dist_y + dist(ydata[,i])
}

dist_z <- dist(zdata[,2])
for (i in c(3:316)){
  dist_z <- dist_z + dist(zdata[,i])
}
# euclidean distance calculation
dist_euc <- sqrt(dist_x^2 + dist_y^2 + dist_z^2)

# Multidimensional scaling to 2d
scaled_color <- cmdscale(dist_euc, k=2)
scaled_color <- as.data.frame(scaled_color)
scaled_color <- cbind(scaled_color, as.factor(xdata$V1)) # using class numbers as factor to differentiate classes by color
colnames(scaled_color) <- c('dim1','dim2','class')

# Using custom colors for seperation to be better (for colorblinds)
custom.col <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
                "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")

# Plotting
feature2d <- ggplot(scaled_color, aes(x=dim1, y=dim2, colour=class)) +
  geom_point() +
  scale_color_manual(values = custom.col) +
  ggtitle('Multi-Dimensional Scaling')
feature2d





