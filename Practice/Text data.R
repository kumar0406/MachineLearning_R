## row wise max or min
## among diff variables 

Sales_data <- read.csv(choose.files(), header = TRUE)

## lets create a small data set
df <- data.frame(age= c(5,6,9), marks= c(1,2,7), story = c(2,9,1))
df

df$row_max <-  apply(df, 1 , max)

df$max_var <-  colnames(df)[apply(df,1,which.max)]
