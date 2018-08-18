########################################################
#################    PROBLEM 03  #######################

## find te relationship btween water weight and air weight
## and then find body weight
## data cleaning , dealing with the stars in the data
## discover linear relationship

## relationship between weights
Hlth <- read.csv(choose.files())
str(Hlth)
summary(Hlth)
## make another copy and replace * character with space character

Hlth1 <- Hlth
## replace for airweight
## ?trimws - Remove Leading/Trailing Whitespace
Hlth1$Air_Weight <- ifelse(trimws(Hlth1$Air_Weight)=="*","",
                           as.character.factor(Hlth1$Air_Weight))

str(Hlth1$Air_Weight)
## we see air weight is character- change it to numeric
Hlth1$Air_Weight <- as.numeric(Hlth1$Air_Weight)
str(Hlth1$Air_Weight)

## do the same with water weight
Hlth1$Water_Weight <- ifelse(trimws(Hlth1$Water_Weight)=="*","",
                             as.character.factor(Hlth1$Water_Weight))

Hlth1$Water_Weight <- as.numeric(Hlth1$Water_Weight)

summary(Hlth1)
## now * have been removed, now we need to remove body fat column

Hlth1 <- Hlth1[,-5]
## we get water as na only one NA, so we remove it.
Hlth_2 <- na.omit(Hlth1)

##Checking for linear relationship graphically
## we will need lattice package

library(lattice)
with(data= Hlth_2, xyplot(Air_Weight~Water_Weight))

## there is a linear relationship
## running regression
fit <- lm(Air_Weight~Water_Weight, data = Hlth_2)
plot(fit)

summary(fit)
## p-value is quit significant , therefore we can say that there is a ralationship

## if we add body part and gender will it provide better fit or not

## we have dropped body fat, lets consider that as well

Hlth$Body_Fat <- ifelse(trimws(Hlth$Body_Fat)=="*","",
                        as.character.factor(Hlth$Body_Fat))
Hlth$Body_Fat <- as.numeric(Hlth$Body_Fat)

Hlth1 <- na.omit(Hlth1)


summary(Hlth1)

## running regression for all the variables

fit2 <- lm(Air_Weight~Water_Weight+Body_Fat+GENDER, data = Hlth1)
summary(fit2)
## p-value: < 2.2e-16, R-squared:  0.8402,	Adjusted R-squared:  0.8374 
## previous value was p-value: < 2.2e-16, R-squared:  0.4716,	Adjusted R-squared:  0.4686 

## p-value is same but R squared has increased so its a better fit

summary(Hlth1)
quantile(Hlth1$Air_Weight, c(0.2,0.4,0.6,0.8))

Hlth1$Air_Weight_grp <- 
  ifelse(Hlth1$Air_Weight <=57.16,"01",
         ifelse(Hlth1$Air_Weight <= 63.06, "02",
         ifelse(Hlth1$Air_Weight <= 70.94,"03",
         ifelse(Hlth1$Air_Weight <= 77.64, "04", "05"))))
Hlth1$Air_Weight_grp

ddply(Hlth1, "Air_Weight_grp",
      summarise , mean_water_wt= mean(Water_Weight))
?ddply



