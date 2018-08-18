## tree pruning


base_data <- read.csv("F:/workspace/gopalPrasad/DecisionTree/DT-02-02-Credit-1.csv")
base_data <- read.csv("DT-02-02-Credit-1.csv")

any(is.na(base_data))

dim(base_data)
## change the response variable
## crate a target vector and add it in the data frame

target <- ifelse(base_data$RESPONSE == 1, "Y", "N")
class(target)
str(target)
typeof(target)

## add this to base data

base_data <- data.frame(base_data, target)
##
## remove the response variable
base_data1 <- base_data[,-32]
library(rpart)


dev_model <- rpart(target~., data = base_data1,
                   control = rpart.control(minsplit = 60,
                                           minbucket = 30,
                                           maxdepth = 4))
plot(as.party(dev_model))
dev_model



#####################################################
#### wine data


wine <- read.csv("DT02-06-Wine-Quality-Data.csv")
str(wine)

table(wine$quality)
desired_q <- ifelse(wine$quality >6, "Y", "N")

## a vector is created

wine1 <- data.frame(wine, desired_q)
wine1 <- wine1[,-12]

CT_wine <- rpart(desired_q~., data = wine1,
                 control = rpart.control(minsplit = 60,
                                         minbucket = 30,
                                         maxdepth = 4))
CT_wine

plot(as.party(CT_wine))



##################################################
########## pruning################################

head(base_data1)

## credit card pruning

Dev_model_2 <- rpart(target~., data = base_data1,
                     control = rpart.control(minsplit = 40,
                                             minbucket = 20,
                                             maxdepth = 10))
Dev_model_2
plot(as.party(Dev_model_2))


Dev_model_2$cptable
print(Dev_model_2$cptable)

## find the min. value of CPtable
min_xerror <- which.min(Dev_model_2$cptable[, "xerror"])
min_xerror


cp <- Dev_model_2$cptable[min_xerror, "CP"]


## CP is known so we should prune the tree
## with prune function

Dev_model_2_pruned <-  prune(Dev_model_2, cp = 0.05)
plot(as.party(Dev_model_2_pruned))

##############################################################
##############################################################
##### Heating and cooling load

energy_data <- read.csv("Energy-consumptions-Questions.csv")

heating_data <- energy_data[,-10]

any(is.na(heating_data))
summary(heating_data)

library(caret)
set.seed(100)
div <- createDataPartition(heating_data$Heating_Load, p=0.75, list = FALSE)
tr_heat <- heating_data[div,]
te_heat <- heating_data[div,]

Model_heat_dev <- rpart(Heating_Load~., data = tr_heat,
                        control = rpart.control(minsplit = 60,
                                                minbucket = 30,
                                                maxdepth = 4))
plot(as.party(Model_heat_dev))

summary(Model_heat_dev)

printcp(Model_heat_dev)
plotcp(Model_heat_dev)

Model_heat_dev$cptable

## choose the least CP

min_xerror <- which.min(Model_heat_dev$cptable[, 'xerror'])

min_cp <- Model_heat_dev$cptable[min_xerror, 'CP']

par(mfrow = c(1,2))
rsq.rpart(Model_heat_dev)


## prune
Model_heat_dev_prune <- prune(Model_heat_dev, cp= min_cp)


plot(as.party(Model_heat_dev_prune))


#######
## how does the model generalize

p = predict(Model_heat_dev_prune, newdata = te_heat)

head(p)

class(p)

p1 <- as.data.frame(p)
class(p1)

final <- cbind(te_heat$Heating_Load, p1)
final

cor(final)
cor(te_heat$Heating_Load, p)

## correlation id too high we consider it as a good model

## rmse
library(Metrics)
rmse(te_heat$Heating_Load, p)

## Random forest
library(randomForest)

erf <-  randomForest(Heating_Load~., data = tr_heat)
sort(importance(erf))
summary(Model_heat_dev_prune)

plot(erf)





