## red car percentage

## focus on age variable and red car variable only
## get rid of missing value
## what % of car is red for age group low-35, 36-48, 49-high


## how to read sas7bdat files in R
install.packages("sas7bdat")
library(sas7bdat)

accd <- read.sas7bdat(choose.files())

## now create a CSV file
write.csv(accd, "Q6_accd.csv")

## summary of age and RED CAR
colnames(accd)

summary(accd$AGE) ## there are 5 missing
summary(accd$RED_CAR)

accd_1 <- accd[, c(4,19)]
accd_1

## now get rid of the 5 records, NA
accd_2 <- na.omit(accd_1)

## create the age group variable in order

accd_2$age_gp <- 
  ifelse(accd_2$AGE<=35, "01-low-35",
         ifelse(accd_2$AGE<=48, "02_36-48", "03_48-high"))

## two new variables
accd_2$red_indicator = ifelse(accd_2$RED_CAR =="yes", 1,0)
accd_2$cnt = 1 ## to find total no of records


# lets see the table

table(accd_2$red_indicator, accd_2$RED_CAR)

## we need plyr library

ddply(accd_2, "age_gp",
      summarise, 
      all_cars =  sum(cnt),
      red_cars = sum(red_indicator),
      red_cars_percentage = red_cars / all_cars
      )
#####################################
#######Another method of doing it, with help of table command



























