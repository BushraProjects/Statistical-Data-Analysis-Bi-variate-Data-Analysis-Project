setwd("D:/Software engineering/Stat Data Analysis Fall 2019/Project2")
library(DescTools)
library(e1071)
library(plyr)
library(frequency)
library(Hmisc)
library(gplots)
library(xtable)
library(stargazer)

data = read.csv('Bushra Naeemi (1).csv')
View(data)

Temperature=data$t4
Rainfall=data$r4
Crop.Net.Income=data$Crop.Net.Income

fun= function(x){
  m = c(mean(x), sd(x), min(x), max(x), skewness(x), kurtosis(x))
  return(m)
}

tem1=fun(Temperature)
ra1=fun(Rainfall)
Income=fun(Crop.Net.Income)
income1=fun(Crop.Net.Income)

tab=matrix(cbind(tem1,ra1, Income, income1), nrow=4, ncol=6, byrow=TRUE)
rownames(tab) <- c("Temperature","Rainfall", "Crop.Net.Income", "Crop.Net.Income(without outlier)")
colnames(tab) <- c("Mean", "Standard.Deviation", "Min","Max","Skewness", "Kurtosis")
tab

xtable(tab)

#---------------Temperature-------------------

q1=quantile(Temperature,0.25)
q3=quantile(Temperature,0.75)

ubt=q3+1.5*(q3-q1)
ult=q3-1.5*(q3-q1)

png(filename = "Temperature.png")
par(mfrow=c(2,1))
boxplot(Temperature, horizontal = TRUE, main = "Boxplot of Temperature in April")
box(which = 'plot', lty=1)
hist(Temperature, col = "yellow", freq = FALSE, xlim=c(ult,ubt),
     main="Distribution Histogram of Temperature")
lines(density(Temperature), col = 'blue')
box(which = 'plot', lty=1)
dev.off()


#-----------------------------Rainfall-----------------------------------------


q1=quantile(Rainfall,0.25)
q3=quantile(Rainfall,0.75)

ulr=q3-1.5*(q3-q1)
ubr=q3+1.5*(q3-q1)

png(filename = "RainFall.png")
par(mfrow=c(2,1))
boxplot(Rainfall, horizontal = TRUE, main = "Boxplot of Rainfall in April")
box(which = 'plot', lty=1)
hist(Rainfall, col = "yellow", freq = FALSE, xlim=c(ulr,ubr),
     main="Rainfall in April")
lines(density(Rainfall), col = 'blue')
box(which = 'plot', lty=1)
dev.off()

#----------------------------Crop.Net.Income--------------------------------------
#with outliers
x = read.csv('Bushra Naeemi.csv')
View(x)
c = x$Crop.Net.Income
png(filename = "income.png")
par(mfrow=c(2,1))
boxplot(c, horizontal = T, main = "Boxplot of Crop.Net.Income With Outlier", col = "yellow")
plot(density(c), col = "blue", main = "Density plot of Crop.Net.Income")
dev.off()

#without outliers
UB = quantile(Crop.Net.Income, 0.75) + 0.8*(quantile(Crop.Net.Income , 0.75)- quantile(Crop.Net.Income, 0.25))
LB = quantile(Crop.Net.Income, 0.75) - 0.8*(quantile(Crop.Net.Income , 0.75)- quantile(Crop.Net.Income, 0.25))
fincome =Crop.Net.Income[Crop.Net.Income<UB & Crop.Net.Income>LB]
png(filename= "income2.png")
par(mfrow=c(2,1))
boxplot(Crop.Net.Income, horizontal = T, main = "Boxplot of Crop.Net.Income Without Outlier", col = "yellow")
plot(density(Crop.Net.Income), col = "blue", main = "Density plot of Crop.Net.Income Without Outlier")
dev.off()
length(Crop.Net.Income)

#-----------------------------Bi_Variate Analysis----------------------------------
#---------------------------Temperature and Crop net Income
png(filename = "temperature_Income.png")
scatter.smooth(Temperature, Crop.Net.Income, 
               col = "red", 
               main = "Temperature and Crop.Net.Income", pch=19)
abline(lm(Crop.Net.Income~Temperature, data = data), col = "blue")
dev.off()

cov(Temperature,  Crop.Net.Income)
cor(Temperature,  Crop.Net.Income)
lm=lm( Crop.Net.Income~Temperature)
lm
summary(lm)

length(Temperature)
length( Crop.Net.Income)

#-------------------------------Rainfall and Crop Net Income
png(filename = "rainfall_Income.png")
scatter.smooth(Rainfall, Crop.Net.Income, 
               col = "red", 
               main = "Rainfall and Crop.Net.Income", pch=19)
abline(lm(Crop.Net.Income~Rainfall, data = data), col = "blue")
dev.off()

cov(Rainfall,  Crop.Net.Income)
cor(Rainfall,  Crop.Net.Income)
lm=lm( Crop.Net.Income~Rainfall)
lm
summary(lm)

length(Rainfall)
length( Crop.Net.Income)
