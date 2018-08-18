##Gain from a Decision tree - classification

## objective segementation - clear cut oupput defined
## so that they can be treated seperately

base_data <- read.csv(choose.files())
head(base_data)


str(base_data)

install.packages("rpart")
library(rpart)


dev_model_tree <- rpart(RESPONSE~., data = base_data, 
                        control = rpart.control(minsplit = 60,
                                                minbucket = 30,
                                                maxdepth = 4))

plot(as.party(dev_model_tree))

## in the output result we have 0 and 1
## convert 0 and 1 into Y and N

base_data$Target <- ifelse(base_data$RESPONSE==1,"Y","N")
nrow(base_data)

table(base_data$Target)

dev_model_1 <- rpart(Target~., data = base_data,
                     control = rpart.control(minsplit = 60,
                                             minbucket = 40,
                                             maxdepth = 4))
plot(as.party(dev_model_1))

## we get a tree of 
print(dev_model_1)

## now lets remove respond variable
base_data_1 <- base_data[,-32]

dev_model_2 <- rpart(Target~.,data = base_data_1,
                     control = rpart.control(minsplit = 306,
                                             minbucket = 250,
                                             maxdepth = 4))
plot(as.party(dev_model_2))
print(dev_model_2)

