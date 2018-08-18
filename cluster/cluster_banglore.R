
## find out the customers whose income is high but they spend less in supermaket

super_mkt <- read.csv(choose.files())
dim(super_mkt)
any(is.na(super_mkt))
super_mkt[super_mkt=""] <- NA

?glimpse
library(dplyr)
glimpse(super_mkt)

str(super_mkt)
head(super_mkt)

set.seed(222)
mkt_fit <- kmeans(super_mkt[,-1], centers = 5)
mkt_fit

centers <- data.frame(mkt_fit$centers)
mkt_fit$size
ratio <- centers$Estimated_income/centers$recent_spends
ratio


mkt_fit$withinss

super_mkt$clusterNo <- mkt_fit$cluster





