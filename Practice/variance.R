## Calculate relative variance
## realtive variance means coefficient of variance

iris
## install plyr

ddply(iris, "Species",
      summarise,
      avg_petal_len= mean(Petal.Length),
      avg_petal_width = mean(Petal.Width),
      
      sd_petal_len = sd(Petal.Length),
      sd_petal_width = sd(Petal.Width),
      
      cov_petal_len =sd_petal_len/avg_petal_len,
      cov_petal_width =sd_petal_width/avg_petal_width )


##    cov_petal_len    cov_petal_width
##1    0.11878522       0.4283967
##2    0.11030774       0.1491348
##3    0.09940466       0.1355627

## here we see petal width varies more than petal length, as width is always higher 

#################################################################
## creating stacked chart