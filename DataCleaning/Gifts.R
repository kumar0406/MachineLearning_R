##Data cleaning GIFTS

gifts_data <- read.csv(choose.files(), header = TRUE)
head(gifts_data)

any(is.na(gifts_data))

any(is.na(gifts_data$TargetB))

any(is.na(gifts_data$TargetD))

summary(gifts_data)

test1 <- sub("  ", "NA",gifts_data$TargetD)
head(gifts_data$TargetD)
str(gifts_data)
glimpse(gifts_data)
??glimpse
table(gifts_data$TargetB, gifts_data$TargetD)
table(gifts_data$TargetB)
table(gifts_data$TargetD)

test2 <- subset(gifts_data, TargetD != " ")
test3 <- filter(gifts_data, TargetD != 0)
test4 <- as.character(gifts_data$TargetD)
gifts_data_1 <- gifts_data
gifts_data_2 <- gifts_data
gifts_data_3 <- gifts_data
gifts_data_2$char <- as.character(gifts_data_1$TargetD)
str(gifts_data_1)

## changes the factor to character for removing $ sign
gifts_data_3$TargetDChar <- as.character(gifts_data_3$TargetD)
gifts_data_3$TargetDChar1 <- str_remove_all(gifts_data_3$TargetDChar, "[$]")

library(stringr)
gifts_data_3 <- gifts_data_3[,-c(3,29)]
gifts_data_3 <- gifts_data_3[,c(1:2,28,3:27)]
str(gifts_data_3)
gifts_data_3.1 <- gifts_data_3

x1 <-  c("", "2", "3", "", "")
x2 <- c(4,5,6,7,8)
x3 <- data.frame(x1,x2)

x3$x4 <- factor(x3$x1)
x3$x1 <- sapply(x3, function(f)
{
  is.na(f) <- which(f=='');
  f
} )


xy <- data.frame(1,"" , 5)

xy <- sapply(xy, 
             function(f){
  is.na(f)<-which(f == '');
  f
  })
xy <- data.frame(xy)



y1 <- c("", 2, "3", '',8)
y2 <- c(21,'', 22, "", "")
y3 <- data.frame(y1,y2)
str(y3)

y3 <- sapply(y3, removeNa)





removeNa <- function(fu){
  is.na(fu) <- which(fu == '')
  fu
}


gifts_data_3$TargetDChar1 <- sapply(gifts_data_3$TargetDChar1, removeNa)


table(gifts_data_3$TargetB)
table(gifts_data_3$TargetDChar1)
str(gifts_data_3)

sum(is.na(gifts_data_3$TargetDChar1))
## 8 9 10 11

a <- c("","", 2, 4)
a1 <- c(1,2,3,1)
af <- data.frame(a,a1)
af
str(af)

af$a <- as.character(af$a)
af$a <- str_remove_all(af$a, "[ ]")

gifts_data_3.2 <- gifts_data_3

gifts_data_3.2 <- sapply(gifts_data_3.2, as.character)


gifts_data_3.2 <- data.frame(gifts_data_3.2)
str(gifts_data_3.2)

gifts_data_3.2$GiftAvgLast <- str_remove_all(gifts_data_3.2$GiftAvgLast, "[$]")
gifts_data_3.2$GiftAvgCard36 <- str_remove_all(gifts_data_3.2$GiftAvgCard36, "[$]")

names(gifts_data_3.2[8:11])

gifts_data_3.2 <- sapply(gifts_data_3.2, as.integer)

names(is.na(gifts_data_3.2))
str(gifts_data_3.2)

boxplot(gifts_data_3.2)
boxplot(gifts_data_3.2$DemAge)
hist(as.factor(gifts_data_3.2$DemAge))

colSums(is.na(gifts_data_3.2))

plot(gifts_data_3.2$TargetDChar1, gifts_data_3.2$GiftAvgCard36)

gifts_data_1[gifts_data_1==''] <- NA
##


