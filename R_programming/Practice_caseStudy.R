##merge two tables

listA <-  read.csv(choose.files())
listB <- read.csv(choose.files())

## convert to upper case
listAU <- data.frame(listA, toupper(listA$Email_IDs))
listAU

listBU <- data.frame( listB, toupper(listB$Email_IDs))
str(listBU)
listBU

## rename the column name
colnames(listAU)[2] <- "upEmailID"
listAU
colnames(listBU)[2] <- "upEmailID"
listBU
## merge the two tables
## merge command only does the equi join

List_A_B <-  merge(listAU, listBU, by="upEmailID")
List_A_B

## but we r looking both A and B and then remove what is part of A
## outer join     : merge(x=df1, y = df2, by = "ID", all = TRUE)
## Left outer join: merge(x=df1, y = df2, by = "ID", all.x = TRUE)
## Right outer join: merge(x=df1, y = df2, by = "ID", all.y = TRUE)

List_A_B <- merge(listAU, listBU, by= "upEmailID", all = TRUE)

## now remove A from result

New_in_B <- List_A_B[is.na(List_A_B$Email_IDs.x),]
## All rows where List_A_B is NA, all other columns
is.na(List_A_B$Email_IDs.x)
############################################################
#############################################################
##PROBLEM 02
## Substring

## we need to take how namy people took insrance each month, and generate barplot
## bar plot will show month wise intake of insurance

Ins <- read.csv(choose.files())

## we will use gregexpr

Ins_1 <- Ins
substr(Ins_1$date_1,1)
Ins_1$month_1 <- substr(Ins_1$date_1,1,
                        gregexpr(pattern = "/", Ins_1$date_1))

## find the no of character of the column
nchar(Ins_1$month_1)
length(Ins_1$month_1)
nchar(Ins_1$month_1)-1
substr(Ins_1$month_1, 1)

#################################################################





Ins_1$month_2 <- substr(Ins_1$month_1, 1,nchar(Ins_1$month_1)-1)

colnames(Ins_1)

## count the no of births in each month
library(plyr)
ddply(Ins_1, "month_2",
      summarise,
      sum_wt = sum(no_of_births_1))

str(Ins_1)
## month_2 is character

## add addational 0 to each of the single digit months

Ins_1$month_3 <- ifelse((Ins_1$month_2!="10"| Ins_1$month_2!="11"| Ins_1$month_2!="12"),
                        paste("0", Ins_1$month_2, sep = ""),
                        paste("*",Ins_1$month_2, sep = ""))
Ins_1$month_3

















