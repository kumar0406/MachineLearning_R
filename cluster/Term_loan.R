## Hirerchical clustering
#####################################

loan_h <- read.csv("Term-Loan-Subscription.csv")
str(loan_h)
any(is.na(loan_h))
summary(loan_h)

set.seed(100)
loan_h1 <- loan_h[sample(nrow(loan_h), 100 ),]
class(loan_h1)
typeof(loan_h1)

str(loan_h1)
## term deposite - has the client subscribed a term deposite yes or no
## take numerical data only- leave out the factors

##take only numeric days
numeric_variable <- sapply(loan_h1, is.numeric)
loan_h2 <- loan_h1[, numeric_variable]
names(loan_h2)

################### we will not treate outliers#########################
## 1. Not more than 100 observations
## 2. only numeric variable
## 3. standarized the variable

colMean <- colMeans(loan_h2)
colSd <- apply(loan_h2, 2, sd)

loan_h3 <- data.frame(scale(loan_h2))

## check data set has scaled or not
colMeans(loan_h3)
apply(loan_h3, 2, sd)

boxplot(loan_h3)

cor(loan_h3)

distance_h <- dist(loan_h3, method = "euclidean")
distance_h
cluster_h <- hclust(distance_h, method = "ward.D2")
?hclust

plot(cluster_h)

###############Figure out same type of variables#################
###################################
install.packages("ClustOfVar")
library(ClustOfVar)
###################################
var_tree <- hclustvar(loan_h3)
var_tree
plot(var_tree)

## we have paire of variables
## how many clusters we should have?
## 1. stability check
## 2. draw a rectangle on two clusters

stability(var_tree)
## 8 has the maximum value- no. of clusters should be 8
## get the list of those 8 clusters

var_list <-   cutreevar(var_tree,8)
var_list

var_list$E
## 98.2606


set.seed(200)
summary(var_list)
## there are 3 variables in cluster 6

## gain in cohesion is the most imp. thing here
## create a loop to find gain in cohesion


Gain <- numeric(15)
for(i in 2:15){
  Gain[i] <- cutreevar(var_tree,i)$E
}
Gain

plot(1:15, Gain, type = "b")

## after 8 the curve becomes asymptotic

##lets drop unrequired variables

drop_var <- c("emp_var_rate", "nr_employed")

loan_h4 <- loan_h3[,!c(names(loan_h3) %in% drop_var)]

## how many clusters we should create
## find withinSS for deciding no. of clusters

within_ss <- (nrow(loan_h4)-1)*sum(apply(loan_h4, 2, var))

for(j in 2:15){
  within_ss[j] <- sum(kmeans(loan_h4, centers = j)$withinss)
}

within_ss

plot(1:15, within_ss, type = "b")

xs <- kmeans(loan_h4, centers = 2)
xs$withinss

## either 4 or 6 cluster
## lets try a 4 and 6 cluster solution
## and check withinness

loan_try <- kmeans(loan_h4, centers = 4)
sum(loan_try$withinss) ## 45%

loan_try <- kmeans(loan_h4, centers = 6) ## 57%

loan_try <- kmeans(loan_h4, centers = 8)


loan_try_3 <- kmeans(loan_h4[,c(2,5,7)], centers = 4)


names(loan_h4)






































