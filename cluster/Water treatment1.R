## water treatment
##

wtr_1 <- read.csv("Water-Treatment-Plants.csv")
clo_na <- colSums(is.na(wtr_1))
col_above20 <- which(clo_na>20)

## this will give a vector
## convert it to dataframe
## extract out the rownumbers

col_above20 <- as.data.frame(col_above20)
rn <- rownames(col_above20)

col_names <- (col_above20)

wtr_2 <- wtr_1[, !c(names(wtr_1) %in% rn)]

summary(wtr_2)

## remove rows with NA
wtr_3 <- na.omit(wtr_2[,-1])


boxplot(wtr_3)
## remove outliers
## scale them


mean_columns <- colMeans(wtr_3)
sd_columns <- apply(wtr_3, 2, var)


## scale them

wtr_4 <- as.data.frame(scale(wtr_3))
colMeans(wtr_4)
apply(wtr_4, 2, sd)

## it has been scaled
boxplot(wtr_4)

##########################################################################
##########################################################################
##################  Main drama starts ####################################
##########################################################################

### cluster of variable
library(ClustOfVar)

tree_of_variables <- hclustvar(wtr_4)
plot(tree_of_variables)

## how many clusters we need
## stability command

stability_no.of.clusters <- stability(tree_of_variables)
## 12 clusters

## cut a tree into groups of variables
## and get gai in coehesion
## no. of clusters can also be determined by this


variable_list <- cutreevar(tree_of_variables, 12)

variable_list$E

summary(variable_list) ## cluster of variables

###############################################
##get the Gain in coehesion for diff. clusters

set.seed(100)
Gain <- numeric(15)
for(i in 2:15){
  Gain[i] <- cutreevar(tree_of_variables, i)$E
}
Gain

## check the point where gain is not varying much

plot(1:15, Gain, type = "b", xlab = "no. of clusters",
     ylab = "gain")

## after 12 the graph becomes asymptotic

variable_list <- cutreevar(tree_of_variables, 12)
summary(variable_list)

## remove the common variables

rmv_vars <- c("RD.SS.G", "COND.S", "COND.E", "COND.D", "SSV.P", "SS.P")

wtr_5 <- wtr_4[, !c(names(wtr_4) %in% rmv_vars)]

#######################################################
#######################################################
## we have achieved final dataset, with min. no of variables

## make cluster of observation, after making cluster of variables

set.seed(100)
within_sum_sq <- (nrow(wtr_5)-1) * sum(apply(wtr_5, 2, var))

for(i in 2:15){
  within_sum_sq[i] <- sum(kmeans(wtr_5, centers = i)$withinss)
}

within_sum_sq

plot(1:15, within_sum_sq, type = "b",
     xlab = "no. of clusters", ylab = "within_sum_of_squares")

## there will be wither 4 or 6 clusters
## lets try 6 cluster solution

water_try <- kmeans(wtr_5, centers = 6)
##(between_SS / total_SS =  36.5 %)
## which menas within clusters are 64% which is not good
## within variance should be as min as possible

## take few variables, 

water_try6 <- kmeans(wtr_5[,c(2,3,4,5)], centers = 6)
water_try6

## within 30%
wtr_5 <- data.frame(wtr_5, water_try6$cluster)
table(wtr_5$water_try6.cluster)


## ----------------------------------------------------------
######### Produce cluster solution to the business###############

## ## represent the means using original variable value

wtr_5_original <- wtr_3[, c(2,3,4,5)]

colMeans(wtr_5_original)
apply(wtr_5_original, 2, sd)

water_try6$centers
## from these centers we need to calculate cack the original value

water_try6$centers[[1]]

water_try6$centers[1]






















