## stats for two class variable


sales <- read.csv(choose.files())
str(sales)
summary(sales)
## there is no NA

## we want region wise channel wise - avg milk, max milk sales
## avg grocery and SD of grocery

## here we will use aggrigate function

ddply(sales, .(Channel, Region),
      summarise,
      Avg_milk = mean(Milk),
      max_milk = max(Milk),
      Avg_Grocery = mean(Grocery),
      SD_Grocery= sd(Grocery)
      )
