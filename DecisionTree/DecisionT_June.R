
## Decision tree

##base_data <- read.csv("F:/workspace/gopalPrasad/DecisionTree/DT-02-02-Credit-1.csv")

base_data <- read.csv(choose.files())

str(base_data)

summary(base_data)

## develope a DT Rpart
library(rpart)
base_tree <- rpart(RESPONSE~., data = base_data,
                   control = rpart.control(minsplit = 60,
                                           minbucket = 30,
                                           maxdepth = 4))
base_tree
library(partykit)

plot(as.party(base_tree))
## rpart considered 0 1 as numeric and developed a regression tree
## but its a classification problem

attach(base_data)
target <- ifelse(RESPONSE == 1, "Y", "N")

## target is a vector
## append target to main dataset
## attach data with vector

## DF <- data.frame(DF, vector )
## this will produce a dataframe

base_data <- data.frame(base_data, target)

## develope a tree with response variable which has 0 and 1

faulty_tree <- rpart(target~., data = base_data,
                     control = rpart.control(minsplit = 60,
                                             minbucket = 30,
                                             maxdepth = 4))
plot(as.party(faulty_tree))

## develope a  tree without response variable

base_tree1 <- rpart(target~., data = base_data[,c(-1,-32)],
                    control = rpart.control(minsplit = 60,
                                            minbucket = 30,
                                            maxdepth = 4))
plot(as.party(base_tree1))


base_tree2 <- rpart(target~., data = base_data[,c(-32)],
                    control = rpart.control(minsplit = 60,
                                            minbucket = 30,
                                            maxdepth = 4))
plot(as.party(base_tree2))


################################################################

## wine data

wine_data <- read.csv(choose.files())

any(is.na(wine_data))
colSums(is.na(wine_data))

str(wine_data)
##quality
summary(wine_data)


## create a DT
desired_qual <- ifelse(wine_data$quality >6, 'Y', 'N')

wine_data <- data.frame(wine_data, desired_qual)

table(wine_data$desired_qual)
table(wine_data$quality)

table(wine_data$desired_qual, wine_data$quality)
plot(wine_data$desired_qual)

wine_data1 <- wine_data[-12]

library(rpart)
library(partykit)
wine_tree <- rpart(desired_qual~., data= wine_data1,
                   control = rpart.control(minsplit = 60,
                                           minbucket = 30,
                                           maxdepth = 4))
plot(as.party(wine_tree))


?rpart
