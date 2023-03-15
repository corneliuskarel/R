
#import library
library(tidyverse)
library(corrplot)
library(tidyr)
library(ggplot2)

#import dataset
df <- read.csv("C://WA_Fn-UseC_-Telco-Customer-Churn.csv")
head(df)

#check the descriptive statistic
summary(df)

#check the structure
str(df)

#check dimension
dim(df)

#check if there are any missing value in the dataset
sumMisVal = function(x){
  temp <- sum(is.na(x))
  return(temp)
}
sapply(X = df,FUN = sumMisVal)

#drop missing value
df <- drop_na(df)

#make a corrplot to see each variable's correlation
corrplot(cor(select_if(df, is.numeric)),method = "color")

#make a scatterplot to compare tenure and total charges
plot(x=df$tenure, y=df$TotalCharges, xlab = "Tenure", ylab = "Total charges")


