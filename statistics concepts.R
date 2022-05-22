# Statistics concept
##########################################
# measure of central tenancy
##########################################
# mid value of dataset
# mean 
x <- 1:5
mean(x)
#-----------------------------------------

# median
x <- 1:10
median(x)
#-----------------------------------------

#mode
x <- c(1,2,3,2,5,6,3,5,2,2,0)
table(x)
which.max(table(x))

###########################################
# measure of variability
###########################################
# how spread out value from each other in a data set

# range
# high value of range tells you that high variability in dataset
x <- 1:10
x
min(x)
max(x)
abs(min(x)-max(x))
#-----------------------------------
# interquatile range
# it is middle half of the data
# find median and the divide into 4 parts the 2 midle part is your interquartile range
x <- 1:20
x
IQR(x)
#-----------------------------------

# variance
# it measute betweent the spread between numbers and data
x <- 1:5
var(x)
#------------------------------------

# standard deviation
# it is the difference between data point and the mean
# small value of std it mean values are grouped together or close to mean
# large value of std it mean values are spread or far from mean
x <- 1:5
sd(x)
#-------------------------------------

# which is best measure of variability
# 1: drop variance because it value deviates 
# 2: if our dataset is small or we perform analysis on sample dataset the use range
# 3: if dataset is skewed then use interquartile range
# 4: for normal data distribution or our data is not skewed then use standard deviation

########################################
#correlation
########################################
#covariance
# it measure how much two variable varies together
x <- 1:5
y <- c(6,2,4,4,10)
cov(x,y)
#-----------------------------

#correlation 
# if correlation value is high it mean these two variable are similar (linear relationship) so we skip one variable
x <-c(1:5)
y <-c(6,2,4,4,10)
cor(x,y)
#graph
install.packages("corrplot")
library(corrplot)
corrplot(x)
?corrplot
#-----------------------------------------
install.packages("tidyr")
library(dplyr)
install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
