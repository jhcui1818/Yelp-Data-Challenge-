library(dplyr)
library(tidyr)
## 1. Data Cleaning

# Using the csv file created by data manipulation
bars <- read.csv("bars.csv")
bars_reduced <- bars %>%
  filter(review_count >50,review_count<9000, store_review_count>200, store_review_count<3500)

# remove redundant information and reorder the columns to improve effiiency
bars.v2 <- bars_reduced[, c(-1,-3,-4)]
bars.v3 <- bars.v2[,c(1,8,3,4,5,6,7,9,11,12,13,10)]
bars.v3 <- bars.v3[order(bars.v3$makeup_id),]

bars.v3$is_open <- as.factor(bars.v3$is_open)
bars.v3$stars <- as.factor(bars.v3$stars)

bars.v3$useful_pct <- bars.v3$useful/bars.v3$review_count
bars.v3$review_count_c <- (bars.v3$review_count-mean(bars.v3$review_count))/sd(bars.v3$review_count)
bars.v3$fans_c <- (bars.v3$fans-mean(bars.v3$fans))/sd(bars.v3$fans)
bars.v3$store_review_count_c <- (bars.v3$store_review_count-mean(bars.v3$store_review_count))/sd(bars.v3$store_review_count)
bars.v3$useful_pct_c <- (bars.v3$useful_pct-mean(bars.v3$useful_pct))/sd(bars.v3$useful_pct)

sample_store_bars <- bars.v3 %>%
  filter(store_review_count <2500, store_review_count >2000, useful_pct <1.1)
sample_store_bars$stars <- as.numeric(sample_store_bars$stars)
sample_store_bars$stars <- as.factor(sample_store_bars$stars)

# our data set has too many different users thus we sample the users with a makeup_id smaller than 1000.
sample_user_bars <- bars.v3 %>%
  filter(review_count<7000, review_count>2000)
write.csv(sample_store_bars, "sample_store_bars.csv")
write.csv(sample_user_bars, "sample_user_bars.csv")
