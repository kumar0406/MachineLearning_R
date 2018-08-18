
##Sensors were expected to reduce power consumption ,
##because of its automatic switching off  feature

## in Q3 we dont have sensor, but in Q4 we have
## so if there is a drop of values, is it
## the seasonality or there is actually a drop

power <- read.csv(choose.files())
str(power)
summary(power)

## deseasonalise the value

power$Q3_Consumption <- power$Q3_Consumption/1.2
power$Q4_Consumption <- power$Q4_Consumption/0.9

## running T test
t.test(power$Q3_Consumption, power$Q4_Consumption, paired = TRUE)


##mean of the differences 
##5.127994 ; p-value < 2.2e-16
