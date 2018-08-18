## numeric and classification data

CART_data <- read.csv(choose.files())
str(CART_data)
summary(CART_data)

table(CART_data$horsepower)
table(CART_data$car.name)
## we see ? in horse power

CART_1 <- na.omit(CART_data)
## it did not work

CART_2 <- CART_1[which(CART_1$horsepower!= '?'),]
## there are 392 observations, Therefore it worked

str(CART_2)

## convert horse power as numeric

CART_2$horsepower <- as.numeric(CART_2$horsepower)

## develop a tree

Model_car <- rpart(mpg~., data = CART_2,
                   control = rpart.control(minsplit = 60,
                                           minbucket = 40,
                                           maxdepth = 4))
plot(as.party(Model_car))
## tree is messed up, with car names, so lets drop the car names

CART_2 <- CART_2[,-9]

set.seed(100)
Model_car <- rpart(mpg~., data = CART_2,
                   control = rpart.control(minsplit = 60,
                                           minbucket = 40,
                                           maxdepth = 4))
plot(as.party(Model_car))
print(Model_car)
summary(Model_car)
rsq.rpart(Model_car)

x1 <- 0.17391
Rsquare <- 1-x1
Rsquare

#########################################################
## heating and cooling load

Load_data <-  read.csv(choose.files())
str(Load_data)
summary(Load_data)

Heat_Load <- Load_data[,-10]


set.seed(100)
Model_heat <- rpart(Heating_Load~., data = Heat_Load,method = "anova",
                    control = rpart.control(minsplit = 60, minbucket = 30,
                                            maxdepth = 4))
plot(as.party(Model_heat))
print(Model_heat)
rsq.rpart(Model_heat)

Cooling_Load <- Load_data[,-9]

set.seed(100)
Model_Cooling <- rpart(Cooling_Load~., data = Cooling_Load,method = "anova",
                    control = rpart.control(minsplit = 60, minbucket = 30,
                                            maxdepth = 4))

plot(as.party(Model_Cooling))
print(Model_Cooling)
rsq.rpart(Model_Cooling)
library(caret)
library(rattle)
fancyRpartPlot(Model_Cooling)
??fancyRpartPlot

