
## wine quality

wine_data <- read.csv(choose.files())

str(wine_data)
summary(wine_data)
## there is no NA
plot(wine_data)

## change the last column, as we need quality above or below 7

wine_quality <- ifelse(wine_data$quality>6, 'Y', 'N')

wine_data_1 <- data.frame(wine_data,wine_quality)
str(wine_data_1)
wine_data_2 <- wine_data_1[,-12]
table(wine_data_2$wine_quality)

plot(wine_data_2$wine_quality)

## make a DT
set.seed(100)
Wine_model_1 <- rpart(wine_quality~.,data = wine_data_2,
                      control = rpart.control(minsplit = 60,
                                              minbucket = 30,
                                              maxdepth = 5))

plot(as.party(Wine_model_1))
print(Wine_model_1)


