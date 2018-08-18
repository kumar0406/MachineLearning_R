##

data_student <- read.csv(choose.files())

# edit data frame
edit <- edit(data_student)

# but its time consuming, so we can enter text
temp_str <- "sr_no name age
1 abhi 22
2 kumar 12
3 Sharma 221"

clip_data <- read.delim("clipboard", header = TRUE)
clip_data

### ###################################
#### viewing data/ metadata

##str command

#######################################
 #####frequency distribution
head(Students)

Students <- read.csv(choose.files())
attach(Students)

#with attach function no need to put $ sign
table(Gender, Name)

table(Gender)
plot(Gender)


########################################
#### univariate analysis
marks <- read.csv(choose.files())
hist(Height)
boxplot(Height)
boxplot(marks$Phy_SCR)

## group by in R -- plyr package

ddply(Students, "Gender", summarise, mean_ht = mean(Height),
      sum_weight= sum(Weight), sd_ht= sd(Height))



ddply(Students, "Gender",  mean_ht = mean(Height),
      sum_weight= sum(Weight), sd_ht= sd(Height))

## derive new variable
Students
marks
 
attach(marks)
detach(Students)
head(marks)
marks_1 <-  marks
marks_1

pass_fail <- ifelse((Phy_SCR<30)| (Math_SCR<30)| (Chem_SCR<30), "Fail", "pass")
pass_fail

## merge pass_fail with marks_1 data frame

marks_final <- data.frame(marks_1, pass_fail)
marks_final
dummyVar <- c(1,2,1,2,1,3,4,4,3)
add_dummy <- data.frame(marks_1, dummyVar)















