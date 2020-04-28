library(openxlsx)
library(tidyr)
library(dplyr)
library(ggplot2)
library(arules)
library(arulesViz)
library(data.table)
library(dummies)


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
BarcodeData <- mutate(BarcodeData, DCS = paste0(CategoryA,"/",CategoryB,"/", CategoryC, "/", CategoryD))

TaxonomyData <- read.xlsx(xlsxFile = "D:\\Data Science and Big Data Analytics\\Assignment 2\\Data.xlsx", sheet = 4, colNames = TRUE)
nrow(TaxonomyData) # Checking number of rows
head(TaxonomyData)
TaxonomyData <- mutate(TaxonomyData, DCS = paste0(CategoryA,"/",CategoryB,"/", CategoryC, "/", CategoryD))


POSData <- filter(POSData, Sum_Units >= 0)
POSData <- filter(POSData, Sum_Value >= 0)
POSData$Date <- as.Date(POSData$Date, origin = "1899-12-30")
POSData <- POSData %>% drop_na(Card_ID)
POSData <- filter(POSData, Card_ID != 'N/A')

Merge1<- merge(POSData, BarcodeData, by.x = "Barcode", by.y = "Barcode", all.x = TRUE)
Merge1 <- Merge1 %>% drop_na(CategoryA)
head(Merge1)
Merge2 <- merge(Merge1, TaxonomyData, by.x = "DCS", by.y = "DCS", all.x = TRUE)
head(Merge2)
names(Merge2)
Merge2 <- select(Merge2, c(Card_ID, CategoryBDescription, Sum_Units))

Merge2 <- Merge2 %>% distinct()
Merge3 <- merge(Merge2, LoyaltyData, by.x = "Card_ID", by.y = "CardholderID", all.x = TRUE)
head(Merge3)
Merge2 <- Merge2 %>% drop_na(CategoryBDescription)
Dup <- Merge2

Merge3$Card_ID <- as.factor(Merge3$Card_ID)
Merge3$CategoryB <- as.factor(Merge3$CategoryB)
POSData_by_Cat <- split(Merge3$CategoryB, Merge3$Card_ID)
Basket <- as(POSData_by_Cat, "transactions")
itemsets <- apriori(Basket, parameter=list(minlen= 1, support=0.5, target="frequent itemsets"))
inspect(sort(itemsets, by ="support"))

Dup <- dummy.data.frame(Dup, names = c("CategoryBDescription"), sep = "_")
names(Dup)



Dup <- rename(Dup, BabiesClothing = "CategoryBDescription_babies clothing", Bakery = "CategoryBDescription_bakery", 
              BedKitchen = "CategoryBDescription_bed and kitchen ", 
              BookMusicPaperVideo = "CategoryBDescription_books-paper-music-video" ,
              Butchery = "CategoryBDescription_butchery" ,       
              Car = "CategoryBDescription_car",
              ColdCuts = "CategoryBDescription_COLD CUTS",
              Dairy = "CategoryBDescription_DAIRY PRODUCTS",
              DeliCannedFood = "CategoryBDescription_Deli and canned food", 
              Detergents = "CategoryBDescription_Detergents", 
              Drinks = "CategoryBDescription_DRINKS" ,
              Electronics = "CategoryBDescription_electronic devices",
              FreshPoultry = "CategoryBDescription_Fresh poultry",         
              Frozen = "CategoryBDescription_frozen", 
              FruitsVegetables = "CategoryBDescription_fruits and vegetables",     
              GamesEntertainement = "CategoryBDescription_GAME - ENTERTAINMENT ", 
              Gardening = "CategoryBDescription_GARDENING",                
              Gifts = "CategoryBDescription_gifts" , 
              GroceryFood = "CategoryBDescription_Grocery Food",             
              Hardware = "CategoryBDescription_hardware" ,
              HouseOrganized = "CategoryBDescription_house organized",          
              Household = "CategoryBDescription_household" , 
              IceCreams = "CategoryBDescription_ice creams",               
              manClothing = "CategoryBDescription_man clothing" ,
              PastryMaking = "CategoryBDescription_pastry making",            
              ProductsThatExpireSoon = "CategoryBDescription_products thatn expire soon", 
              SaltedFish = "CategoryBDescription_salted fish",              
              Shoes = "CategoryBDescription_shoes" ,
              Telephone = "CategoryBDescription_telephone ",               
              TraditionalButchery = "CategoryBDescription_traditional butchery" ,
              TraditionalFisherman = "CategoryBDescription_traditional fisherman",    
              HIFISound = "CategoryBDescription_TV - HIFI - sound", 
              Underwear = "CategoryBDescription_underwear")

Dup <- mutate(Dup, BabiesClothing = BabiesClothing * Sum_Units, Bakery = Bakery * Sum_Units, 
              BedKitchen = BedKitchen * Sum_Units, 
              BookMusicPaperVideo = BookMusicPaperVideo * Sum_Units ,
              Butchery = Butchery * Sum_Units ,       
              Car = Car * Sum_Units,
              ColdCuts = ColdCuts * Sum_Units,
              Dairy = Dairy * Sum_Units,
              DeliCannedFood = DeliCannedFood * Sum_Units, 
              Detergents = Detergents * Sum_Units, 
              Drinks = Drinks * Sum_Units ,
              Electronics = Electronics * Sum_Units,
              FreshPoultry = FreshPoultry * Sum_Units,         
              Frozen = Frozen * Sum_Units, 
              FruitsVegetables = FruitsVegetables * Sum_Units,     
              GamesEntertainement = GamesEntertainement * Sum_Units, 
              Gardening = Gardening * Sum_Units,                
              Gifts = Gifts * Sum_Units , 
              GroceryFood = GroceryFood * Sum_Units,             
              Hardware = Hardware * Sum_Units ,
              HouseOrganized = HouseOrganized * Sum_Units,          
              Household = Household * Sum_Units , 
              IceCreams = IceCreams * Sum_Units,               
              manClothing = manClothing * Sum_Units ,
              PastryMaking = PastryMaking * Sum_Units,            
              ProductsThatExpireSoon = ProductsThatExpireSoon * Sum_Units, 
              SaltedFish = SaltedFish * Sum_Units,              
              Shoes = Shoes * Sum_Units ,
              Telephone = Telephone * Sum_Units,               
              TraditionalButchery = TraditionalButchery * Sum_Units ,
              TraditionalFisherman = TraditionalFisherman * Sum_Units,    
              HIFISound = HIFISound * Sum_Units, 
              Underwear = Underwear * Sum_Units)

Dup <- Dup %>% group_by(Card_ID) %>% summarise(BabiesClothing = sum(BabiesClothing), Bakery = sum(Bakery),
                                               BedKitchen = sum(BedKitchen), BookMusicPaperVideo = sum(BookMusicPaperVideo),
                                               Butchery = sum(Butchery), Car = sum(Car), ColdCuts = sum(ColdCuts),
                                               Dairy = sum(Dairy), DeliCannedFood = sum(DeliCannedFood),
                                               Detergents = sum(Detergents), Drinks = sum(Drinks), 
                                               Electronics =  sum(Electronics), FreshPoultry = sum(FreshPoultry),
                                               Frozen = sum(Frozen), FruitsVegetables = sum(FruitsVegetables),
                                               GamesEntertainement = sum(GamesEntertainement), Gardening = sum(Gardening),
                                               Gifts = sum(Gifts), GroceryFood = sum(GroceryFood),
                                               Hardware = sum(Hardware), HouseOrganized = sum(HouseOrganized),
                                               Household = sum(Household), IceCreams = sum(IceCreams),
                                               manClothing = sum(manClothing), PastryMaking = sum(PastryMaking),
                                               ProductsThatExpireSoon = sum(ProductsThatExpireSoon),
                                               SaltedFish = sum(SaltedFish), Shoes = sum(Shoes), Telephone = sum(Telephone),
                                               TraditionalButchery = sum(TraditionalButchery), 
                                               TraditionalFisherman = sum(TraditionalFisherman), HIFISound = sum(HIFISound),
                                               Underwear = sum(Underwear))

wss <- numeric(30)
for(k in 1:30)
  wss[k] <- sum(kmeans(Dup[,2:34],centers=k,nstart=25)$withinss)
plot(1:30, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares") 


Dup <- merge(Dup, LoyaltyData, by.x = "Card_ID", by.y = "CardholderID", all.x = TRUE)
cluster1<- kmeans(Dup[,2:34],4, nstart = 25)
ggplot(Dup, aes(Card_ID, Gender)) + geom_point(color = cluster1$cluster)
write.csv(cluster1$centers, file = "NewSegments.csv")







