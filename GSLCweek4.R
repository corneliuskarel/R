#import library
library(tidyr)
library(dplyr)
library(ggplot2)

#EDA
#import dataset, replacing N/A value in dataset to NA, because R cannot read N/A as missing values.
df <- read.csv(file = "C:\\vgsales.csv", na.strings = c("N/A"))
head(df)

##check dataset's descriptive statistic
summary(df)

#check data dimension
dim(df)

#check structure
str(df)

#check missing values
sumMisVal = function(x){
  temp = sum(is.na(x))
  return(temp)
}
sapply(X = df,FUN = sumMisVal)

#drop missing values
df_test <- drop_na(df)

#drop unusable columns and remove rows that has missing values
df <- drop_na(df)
df <- subset(df,select = -c(Rank))

#scatter plot to compare yearly sales
plot(x=df$Year, y=df$Global_Sales, xlab = "Year", ylab = "Global Sales")


#filling missing data
df_test <- drop_na(df)
modeYear <- median(df_test$Year)
modeYear
df$Year[is.na(df$Year)] = 0
df$Year[df$Year == 0] <- modeYear
  
#creating a table of sorted platform by it's popularity
ListOfPlatform <- df %>% group_by(Platform) %>% count()
print(ListOfPlatform[order(ListOfPlatform$n, decreasing = TRUE), ])
ListOfPlatform <- ListOfPlatform[order(ListOfPlatform$n, decreasing = TRUE), ]
ListOfPlatform

#creating a table of sorted genre by it's popularity
ListOfGenre <- df %>% group_by(Genre) %>% count()
print(ListOfGenre[order(ListOfGenre$n, decreasing = TRUE), ])
ListOfGenre <- ListOfGenre[order(ListOfGenre$n, decreasing = TRUE), ]

#creating a plot based on the sorted genre
ggplot(
  ListOfGenre, 
  aes(x = ListOfGenre$n, y = ListOfGenre$Genre, colour = ListOfGenre$Genre)
) + geom_point()

#creating a barplot to compare each region's sales
salesSum <- c(sum(df$NA_Sales), sum(df$EU_Sales), sum(df$JP_Sales), sum(df$Other_Sales))
barplot(salesSum, names.arg = c("NA", "EU", "JP", "Other"))
title(ylab = "Global")


