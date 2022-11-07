library("dplyr")
library("car")
library("rcompanion")

#View names of columns
names(Turo)
#Keep important columns
keep <- c( "list.vehicle.id", "list.vehicle.year", "list.vehicle.model", "list.vehicle.make", "list.rate.averageDailyPriceWithCurrency.amount", "list.rate.weekly", "list.rate.daily", "list.rate.monthly", "list.owner.allStarHost", "list.location.city", "list.turoGo", "list.renterTripsTaken", "list.rating", "list.rate.discountSavingsText", "list.vehicle.type")

# remove duplicates
Turo1 <- Turo[keep]

Turo1 %>%
  distinct(list.vehicle.id, .keep_all = TRUE)

#filter out listings with 0 trips

Turo2 <- filter(Turo1, list.renterTripsTaken >= '1')

table(Turo2$Make)

#Checking number of levels for variables
length(unique(Turo$list.vehicle.make))
length(unique(Turo$list.vehicle.model))
length(unique(Turo$list.rate.discountSavingsText))
length(unique(Turo$list.location.addressLines))
length(unique(Turo$searchLocation.airportCode))

library(Hmisc) #for correlation matrix

mat_dat = as.matrix(Turo2[sapply(Turo2, is.numeric)]); rcorr(mat_dat)
corr <- rcorr(mat_dat, type = c("spearman"))

names(Turo2)[names(Turo2) == 'list.vehicle.id'] <- 'id'
names(Turo2)[names(Turo2) == 'list.vehicle.year'] <- 'year'
names(Turo2)[names(Turo2) == 'list.vehicle.model'] <- 'model'
names(Turo2)[names(Turo2) == 'list.vehicle.make'] <- 'make'
names(Turo2)[names(Turo2) == 'list.rate.averageDailyPriceWithCurrency.amount'] <- 'AvgDailyPrice'
names(Turo2)[names(Turo2) == 'list.rate.weekly'] <- 'WeeklyRate'
names(Turo2)[names(Turo2) == 'list.rate.monthly'] <- 'MonthlyRate'
names(Turo2)[names(Turo2) == 'list.rate.daily'] <- 'DailyPrice'
names(Turo2)[names(Turo2) == 'list.owner.allStarHost']<- 'AllStarHost'
names(Turo2)[names(Turo2) == 'list.location.city'] <- 'city'
names(Turo2)[names(Turo2) == 'list.turoGo'] <- 'TuroGo'
names(Turo2)[names(Turo2) == 'list.renterTripsTaken']<- 'TripsTaken'
names(Turo2)[names(Turo2) == 'list.rate.discountSavingsText'] <- 'Discount'
names(Turo2)[names(Turo2) == 'list.vehicle.type'] <- 'VehicleType'
names(Turo2)[names(Turo2) == 'list.rating'] <- 'rating'

# Distribution of categorical variables

ggplot(data = Turo2) +
  geom_bar(mapping = aes(x = model))
ggplot(data = Turo2) +
  geom_bar(mapping = aes(x = make))
ggplot(data = Turo2) +
  geom_bar(mapping = aes(x = year))
ggplot(data = Turo2) +
  geom_bar(mapping = aes(x = VehicleType))
ggplot(data = Turo2) +
  geom_bar(mapping = aes(x = TripsTaken))
ggplot(data = Turo2) +
  geom_bar(mapping = aes(x = Discount))
ggplot(data = Turo2) +
  geom_bar(mapping = aes(x = city))
ggplot(data = Turo2) +
  geom_bar(mapping = aes(x = DailyPrice))

#I need to find the avg revenue of a car (make, model & year) and then look at 
#the highest performers
#Add a Revenue column
Turo3 <- Turo2 %>% mutate(revenue = AvgDailyPrice * TripsTaken)
#combine make model and year
Turo3$modelR <- paste(Turo3$year, Turo3$make, Turo3$model)
#delete make model and year
Turo3 = select(Turo3, -c(make, model, year))

describe(Turo3$revenue)
ggplot(data = Turo3) +
  geom_bar(mapping = aes(x = revenue))

# Boxplot of Revenue by model
boxplot(revenue~modelR,data=Turo3, main="Car Revenue",
        xlab="Model", ylab="Est Revenue")
RevenueCount = table(Turo3$revenue)
RevenueCount

#I want to look at all star host with 20 plus trips in Raleigh
#then do the same for philadelphia and compare

CityValueCount <- table(Turo3$city)
CityValueCount



#Filtering out only Raleigh and philadelphia listings

Turo4 <- Turo3 %>% filter(city == c('Raleigh', 'RALEIGH', 'Philadelphia')) 
Turo5 = select(Turo4, -revenue) # Dropping revenue col as there is too many assumptioins

#Going forward we will compare listings in Philadelphia and Raleigh
#We should also examine AllStar vs NonAllStar
#A High performing Listing will be an AllStar host with >20 trips
#We hypothesize that the Allstar host will have less discounts than the non AllStarHost
#We also want to examine all cars with more than 20 trips 
# AllStarHost have >10 trips and 5 stars

