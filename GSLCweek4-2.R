
#import library
library(corrplot)
library(plotly)

#import "USArrests" dataset and store it into "df"
df <- USArrests

#view the first 6 rows of "df" to grasp better understanding
head(df)

#check structure
str(df)

#check  dimension
dim(df)

#check dataset's descriptive statistic
summary(df)

#check if dataset contains missing value
sumMisVal = function(x){
  temp = sum(is.na(x))
  return(temp)
}
sapply(X = df,FUN = sumMisVal)

#check the correlation between variables
corrplot(cor(df), method = "color")

#check info about this particular dataset
help("USArrests")


#check the murder case distribution
ggplot(
  USArrests, 
  aes(x = Murder)) +
  geom_histogram(bins = 10, fill = "white", color = "red") +
  labs(title = "Murder distribution", x = "Cases", y = "Total")

