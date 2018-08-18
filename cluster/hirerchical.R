## hirerchical clusrtering

hcluster <- read.csv(choose.files())
dim(hcluster)
summary(hcluster)
hc <- hcluster[1:3]
hc

## lets start clustering hirerchical
str(hc)

plot(hc$Physics, hc$Math, pch = 21,
     bg=c("red" , "green", "blue",
          "red" ,"green", "blue",
          "red" , "green", "blue" ),
     main = "student")

## obtain the distance matrix
distanceHc <- dist(hc[c(2,3)], method = "euclidean")
print(distanceHc)

## now apply Hirerchical clustering, hclust()
fit <- hclust(distanceHc, method = "ward.D2")
plot(fit)

plot(fit, labels = hc$Student)

## draw dindogram with coloured borders
rect.hclust(fit, k=8, border = "green")
plot(fit, labels = hc$Student)
rect.hclust(fit, k= 7, border = "red")
rect.hclust(fit, k= 6, border = "yellow")
rect.hclust(fit, k = 5, border = "blue")
plot(fit, labels = hc$Physics)
plot(fit, labels = hc$Student)
rect.hclust(fit, k = 3, border = "red")
## generate scree plot for hirerchical clustering using R
##RMS STD within group variance

summary(fit)
fit$height

dendo_height=0

for(i in 2:9){
  dendo_height[i] <- fit$height[i-1]
}
dendo_height
plot(9:1, dendo_height, type = "b")
##height represents the within sum of variance
## reduce within variance


#########################################################
######## K means

my_data <- iris
plot(my_data$Petal.Length, my_data$Petal.Width,
     pch=25, bg = c("red", "blue", "orange","yellow")[unclass(my_data$Species)])

## install plyr()
my_iris <- my_data[-5]

km_fit <- kmeans(my_iris, centers = 4)
km_fit
km_fit$withinss
sum(km_fit$withinss)

withinSS1 <- (nrow(my_iris)-1)* sum(apply(my_iris, 2, var))
for(i in 2:9){
withinSS[i] <- sum(kmeans(my_iris, centers = i)$withinss)
}
withinSS

## the maximum sum of variance is 681.37

plot(1:9, withinSS, type = "b")

km_fit

km_fit$centers
km_fit$size
km_fit$withinss

my_iris <- data.frame(my_data, km_fit$cluster)

table(my_iris$Species, my_iris$km_fit.cluster)
##=======================================

## water treatment

water <- read.csv(choose.files())

dim(water)
any(is.na(water))
x <- colSums(is.na(water))
sum(x)
summary(water)

## 3% of 527 is 15, we will not take anything greater than 20 NA


removeCols <- c(3,4,5,9,11,12,15,17)

x <- colSums(is.na(water))

p <- x>20
p1 <- water[c(p)]
names(p1)
removeColumns <- sapply(water,p )

summary(p1)
p2 <- water[,!c(p)]
 summary(p2)

 ## p2 is the refined data
 
 water2 <- na.omit(p2[,-1])
 
 summary(water2)
 
boxplot(water2) 

##check for outliers
 boxplot(water2$Q.E)
 
benchmark <- 41440+1.5*IQR(length(water2))
data_1 <- water2[water2<benchmark]

water2$Q.E[water2$Q.E>benchmark] <- water2

names(water2[c(2,3,4)])
boxplot(water2[c(2,3,4,5,6)])
boxplot(water2[c(7,8,9,10,11)])

## standarize the variable
## keep a track of mean and standard deviation
colMeans(water2)
dplyr::glympse(water2)
library(dplyr)
str(water2)

ColumnMeans <- colMeans(water2)
StandardDeviation <- apply(water2, 2, sd)

## standardise the DF
water_3 <- data.frame(scale(water2))
colMeans(water_3)## these values are as good as 0
apply(water_3, 2, sd)



## which variables we should select
library(ClustOfVar)
var_tree <- hclustvar(water_3)

var_tree
plot(var_tree)
stability(var_tree)
## its max at 12c lusters

varlist <- cutreevar(var_tree, 12)
?cutreevar()

summary(var_tree)
varlist$E

set.seed(100)

Gain <- numeric(15)

for(i in 2:15){
  Gain[i] <- cutreevar(var_tree, i)$E
}
Gain


plot(1:15, Gain, type = "b", xlab = "number of clusters",
     ylab = "Gain")


## drop unrequired variable
summary(varlist)
var_unreqired <- c("SS.E","SSV.E", "COND.P", "COND.D", "COND.E")

water_4 <- water_3[,!names(water_3) %in% var_unreqired]
water_4

## decide number of clusters

set.seed(100)

withinSS2 <- (nrow(water_4)-1)*sum(apply(water_4, 2, var))
for(i in 2:15){
  withinSS2[i] <- sum(kmeans(water_4, centers = i)$withinss)
}
withinSS


wss <- (nrow(water_4)-1)*sum(apply(water_4, 2, var))

for(i in 2:15){
  wss[i] <- sum(kmeans(water_4, centers = i)$withinss)
}

xc <- kmeans(water_4, centers = 3 )
xc$withinss
wss

plot(1:15, wss, type = "b", xlab = "no. of clusters",
     ylab = "within sum of variance")


## it seems 6 is the optimum number of clusters

water_try <- kmeans(water_4, centers = 6)
water_try
## within group varianve is 41%
##(between_SS / total_SS =  41.9 %)
## therefore we conclude within group variance is too much = 60%


water_try <- kmeans(water_4, centers = 15)
water_try
## within group variance is not reducing
## which mens there is varianve between variables

## _____________________________________
## itterte for final model


colnames(water_4)
round(cor(water_4),2)

water_5 <- water_4[c(1,3,5)]

water_try1 <- kmeans(water_5, centers = 15)
water_try1

## for optimum no. of clusers again itterate
set.seed(100)
wss1 <- (nrow(water_5)-1) * sum(apply(water_5, 2, var))

for(i in 2:15){
  wss1[i] <- sum(kmeans(water_5, centers = i)$withinss)
}
wss1

plot(1:15, wss1, type = "b")


water_try <- kmeans(water_5, centers = 6)
water_try



water_try <- kmeans(water_5, centers = 10)
water_try$cluster

water_5 <- data.frame(water_5, water_try$cluster)

table(water_5$water_try.cluster)

plot(water_5$Q.E, water_5$DQO.D, pch = 21,
     bg = c("red", "blue", "green", "red", "orange", "yellow")
     [unclass(water_5$water_try.cluster)])


## look at all the variable
library(scatterplot3d)
water_6 <- water_5

water_6$pcolor[water_6$water_try.cluster==1] <- "red"
water_6$pcolor[water_6$water_try.cluster==2] <- "blue"
water_6$pcolor[water_6$water_try.cluster==3] <- "green"
water_6$pcolor[water_6$water_try.cluster==4] <- "yellow"
water_6$pcolor[water_6$water_try.cluster==5] <- "black"
water_6$pcolor[water_6$water_try.cluster==6] <- "brown"
View(water_6)



