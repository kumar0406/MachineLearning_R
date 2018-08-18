## variable selection for PCA

data_pca <- read.csv(choose.files(), header = T)
head(data_pca)

## remove the last column

data_pca1 <- princomp(data_pca[,-20], cor= TRUE)

pca_result <- summary(data_pca1, loadings = TRUE)

first5_comps <- pca_result$loadings[,c(1:5)]
str(first5_comps)
typeof(first5_comps)
mode(first5_comps)
first5_comps_df <- as.data.frame(first5_comps)

str(first5_comps_df)
## find the highest no of diff columns
## 3rd highest no of PC1
comp1 <- abs(first5_comps_df)
head(comp1)
sortedValues1 <- first5_comps_df[order("Comp.1"),]

n <- length(first5_comps_df$Comp.1)
n
sort(first5_comps_df$Comp.1,partial=NULL, na.last = NA)
sort(first5_comps_df$Comp.2,partial=NULL, na.last = NA)
sort(first5_comps_df$Comp.2,partial=NULL, na.last = NA)[1]

first5_comps_df[match(-0.33705285, first5_comps_df$Comp.1),] 


comp1 <- first5_comps_df$Comp.1,
comp2 <- first5_comps_df$Comp.2
comp11 <- as.data.frame(comp1)
comp12 <- as.data.frame.raw(comp1)

