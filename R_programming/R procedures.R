## sub setting and sorting

## selecting few rows which meets the condition

## pass_fail == "Pass"
total_marks <- Phy_SCR+Math_SCR+Chem_SCR+Lang_SCR
marksheet <- data.frame(marks_final,total_marks)
pass <-  marksheet[which(marks_total$pass_fail=="pass"),] ## only pass rows and all columns

pass
## sort the pass people


marks_total <- data.frame(marks_final, total_marks)
head(marks_total)

sorted_pass <- pass[order(pass$total_marks),]
sorted_pass

pass_top3 <- head(sorted_pass,3)
pass_top3

var_list3 <- c("pass_fail", "total_marks", "Math_SCR")
top3 <- pass_top3[var_list3]
top3

########################################################
## cross tab

table(pass_fail, gender)
student <- read.csv(choose.files())
final <- merge(student, marksheet, by="Roll_No")
final

table(final$Gender, final$Name)
install.packages("gmodel")
library(gmodels)

CrossTable(final$Gender, final$Name)


#substring
## how many students name start with A or B
## take the first name and see the frequency distribution

First_ch <- substr(final$Name, 1,1)
###Second_ch <- substr(final$Name, #StartFromThisCharacter,#GoTillThisCharacter)
Second_ch <- substr(final$Name, 2,3)

table(First_ch)



















