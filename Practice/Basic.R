## test

gifts_base <- read.csv(choose.files())

gifts1 <- gifts_base[1:30,]
gifts2 <- gifts_base[40:55,]

summary(gifts1)

# removing the $ sign from dataset gifts1
## using gsub

gifts1$TargetD <- as.numeric(gsub("\\$", "", gifts1$TargetD))
head(gifts1)
head(gifts_base)

gifts1 <- gifts1[,1:5]
head(gifts1)

## calculate the % of columns
## solution 1
str(gifts1)

gifts1$GiftCnt36_per <- gifts1$GiftCnt36/ sum(gifts1$GiftCnt36)
head(gifts1)

## another method is using dplyr
## solution 2, mutate()

gifts1 <- mutate(gifts1,
                 cnt36_per = GiftCnt36/ sum(GiftCnt36),
                 cntAll_per = GiftCntAll/ sum(GiftCntAll))
head(gifts1)
gifts1 <- gifts1[1:5]

## Another method with Loop
## for(), while(), sapply(), lapply()
## 1. sapply()

names(gifts1[-1])

sapply(gifts1[-3], mean)

sapply(names(gifts1[-3]), function(x){
  gifts1[paste0(x, "_per")] <<- gifts1[x]/ sum(gifts1[x])
})


## for loop

for(col in names(gifts1)[-3]) {
  gifts1[paste0(col, "_per")]=gifts1[col]/ sum(gifts1[col])
  print(head(gifts1))
}
g <- data.frame(g)
head(g)

#######################################################
###### column to text in R
## paste() for combining string

head(gifts1)
col1 <- gifts1$TargetB
length(gifts1$TargetB)

## colapse all of them to a string
string = paste(col1, collapse = ",")
string
length(string)

##Deduping that list
## colapse the col vector as string
string <- paste(unique(col1), collapse = ",")
string


## nth largest and smallest in group

head(gifts1)

group_by(gifts1, TargetB) %>%
  filter(GiftCnt36 == max(GiftCnt36))

## for target 0 maxis 6
## for target 1 ma is 7


group_by(gifts1, TargetB) %>%
  mutate((rank= rank(desc(GiftCnt36)))) %>%
  arrange(rank)
head(gifts1)


?mutate
mutate((rank= rank(desc(gifts1$GiftCnt36))))
mutate(newColumn, gifts1$TargetB>0)
#########################################################
## mid, right , left in R

a = "Chicago"
b = "New York City"
c = "Los Angeles"
a


## Left
substr(a,1,3)
substr(a,1,.)
substr(b,5,8)

left <- function(text, num_char){
  substr(text, 1, num_char)
}
































