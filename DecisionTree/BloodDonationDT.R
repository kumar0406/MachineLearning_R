## blood donation

Bdonation_data <- read.csv(choose.files())
str(Bdonation_data)

summary(Bdonation_data)
plot(Bdonation_data$Donated_in_Mar07)
x <- table(Bdonation_data$Donated_in_Mar07)
plot(x)

## change 1 and 0 to Y and N

Bdonation_data$Donat_March <- ifelse(Bdonation_data$Donated_in_Mar07>0, 'Y', 'N')
BD_data <- Bdonation_data[,-5]

##now a tree can be made

library(rpart)
library(party)
library(partykit)


set.seed(100)
BD_data_model <- rpart(Donat_March~., data = BD_data,
                       control = rpart.control(minsplit = 60,
                                               minbucket = 30,
                                               maxdepth = 4))
plot(as.party(BD_data_model))
print(BD_data_model)






