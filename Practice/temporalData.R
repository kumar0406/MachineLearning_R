## Stacked chart, temporal data

Dob_data <- read.csv(choose.files())
head(Dob_data)
summary(Dob_data)
## create date of birth

Dob_data$Dob_text = trimws(paste((Dob_data$Year_of_DoB), "/",
                                  trimws(Dob_data$Month_of_DoB), "/",
                                  trimws(Dob_data$Day_of_DoB)
                                  
                                  ))
Dob_data$Dob_final = as.Date(Dob_data$Dob_text, "%Y /%m /%d")
str(Dob_data)


## age on 31 march 2015
## create the reference date and substract both tha dates

Dob_data$Dob_ref <- as.Date('2015/03/01', "%Y /%m /%d ")

Dob_data$Age_in_Days <- Dob_data$Dob_ref - Dob_data$Dob_final
Dob_data$age_in_years <- floor(Dob_data$Age_in_Days/365)


table(Dob_data$age_in_years, Dob_data$Gender)

library(ggplot2)
ggplot(Dob_data, aes(x=as.integer(age_in_years), fill = Gender))+ geom_histogram()




