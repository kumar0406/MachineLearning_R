## cluster analysis - Hirerchical
## selecting because, less tahn 100 observations are there


iris_data <-  iris
head(iris_data)
colnames(iris_data)



library(ggplot2)
ggplot(data = iris_data, aes(Sepal.Length, Sepal.Width))+
  geom_point(aes(color = (Species)))

summary(iris)

iris_cluster <- kmeans(iris_data[,1:2], 3, nstart = 15)
iris_cluster
summary(iris_cluster)

table(iris_cluster$cluster, iris_data$Species)
## 1,2,3 as factor

is.vector(iris_cluster$cluster)

iris_cluster$cluster <- as.factor(iris_cluster$cluster)
iris_cluster$cluster
typeof(iris_cluster$cluster)
mode(iris_cluster$cluster)
summary(iris_cluster$cluster)

table(iris_cluster$cluster, iris_data$Species)

ggplot(iris_data, aes(Sepal.Length, Sepal.Width, color = iris_cluster$cluster))+
  geom_point()

######## how to finf number of clusters## by scree plot, elbow curve

## a function for itteration

##
install.packages("xlsx")
library(xlsx)

Without_scale <- iris_data[,-5]
with_scale  <- scale(iris_data[,-5])

summary(Without_scale)
summary(with_scale)

write.xlsx(Without_scale, "G:/scale.xlsx")
write.xlsx(with_scale,"G:/scale1.xlsx" )










