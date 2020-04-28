library(openxlsx)
library(tidyr)
library(dplyr)
library(ggplot2)
library(arules)
library(arulesViz)

# Reading base dataset on loan and returns
POSData <- read.xlsx(xlsxFile = "D:\\Data Science and Big Data Analytics\\Assignment 2\\Data.xlsx", sheet = 1, colNames = TRUE)
nrow(POSData) # Checking number of rows
head(POSData)

LoyaltyData <- read.xlsx(xlsxFile = "D:\\Data Science and Big Data Analytics\\Assignment 2\\Data.xlsx", sheet = 2, colNames = TRUE)
nrow(LoyaltyData) # Checking number of rows
head(LoyaltyData)

BarcodeData <- read.xlsx(xlsxFile = "D:\\Data Science and Big Data Analytics\\Assignment 2\\Data.xlsx", sheet = 3, colNames = TRUE)
nrow(BarcodeData) # Checking number of rows
head(BarcodeData)

TaxonomyData <- read.xlsx(xlsxFile = "D:\\Data Science and Big Data Analytics\\Assignment 2\\Data.xlsx", sheet = 4, colNames = TRUE)
nrow(TaxonomyData) # Checking number of rows
head(TaxonomyData)


POSData <- filter(POSData, Sum_Units >= 0)
POSData <- filter(POSData, Sum_Value >= 0)
POSData$Date <- as.Date(POSData$Date, origin = "1899-12-30")
POSData <- POSData %>% drop_na(Card_ID)
POSData <- filter(POSData, Card_ID != 'N/A')
POSData <- mutate(POSData, Spend = Sum_Value)


GroupData <- POSData %>% group_by(Card_ID) %>% summarise(Spend = sum(Spend))

wss <- numeric(15)
for(k in 1:15)
  wss[k] <- sum(kmeans(GroupData[,2],centers=k,nstart=25)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares") 

cluster<- kmeans(GroupData[,2],4, nstart = 25)
ggplot(GroupData, aes(Card_ID, Spend)) + geom_point(color = cluster$cluster)



MergedData <- merge(GroupData, LoyaltyData, by.x = "Card_ID", by.y = "CardholderID", all.x = TRUE)
rggplot(GroupData, aes(Card_ID, Spend, color = MergedData$Gende)) + geom_point()
ggplot(GroupData, aes(Card_ID, Spend, color = MergedData$MaritalStatus)) + geom_point()
ggplot(GroupData, aes(Card_ID, Spend, color = MergedData$HouseholdSize)) + geom_point()


