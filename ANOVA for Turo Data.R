#Find the average daily price by state 
OG_Turo <- Turo
Keep <- c("list.location.state", "list.rate.daily", "list.owner.id")
price_and_state <- Turo[Keep]
price_and_state$list.location.state
library("dplyr")
colnames(price_and_state)[1] ="state"
colnames(price_and_state)[2] ="price"
agg_tbl <- price_and_state %>% group_by(state) %>%
  summarise(mean_price=mean(price),
            .groups = 'drop')
price_and_stateDE <- price_and_state %>% filter(state == 'DE')
price_and_stateNC <- price_and_state %>% filter(state == 'NC')
price_and_stateNJ <- price_and_state %>% filter(state == 'NJ')
price_and_stateNY <- price_and_state %>% filter(state == 'NY')
price_and_statePA <- price_and_state %>% filter(state == 'PA')
#independent t test comparing daily price of cars PA vs. NC
t_ind <- t.test(price_and_stateNC$price, price_and_statePA$price, alternative="two.sided", var.equal=FALSE)
print(t_ind)

#Pvalue is .45 . There is no sig difference in avg price between NC and PA


#Does the average number of trips differ by location?
#Run MANOVA

#Check sample size for MANOVA
table(OG_Turo['list.location.city'])
#subset data for city (cary,durham,morrisville, philly, raleigh, and wake forest)
variables <- c("list.location.city", "list.renterTripsTaken")
cityANDtrips <- OG_Turo[variables]
data.frame(cityANDtrips)
cityANDtrips %>% 
  rename("city" = "list.location.city")
cityANDtrips = cityANDtrips %>% 
  filter(list.location.city== 'Cary' | list.location.city== 'Durham' | list.location.city== 'Philadelpia'| list.location.city== 'Morrisville' | list.location.city== 'Wake Forest')
#set up for MANOVA

#change to variables to numeric
class(cityANDtrips$list.location.city)
cityANDtrips$list.location.city[cityANDtrips$list.location.city=="Cary"] <- "1"
cityANDtrips$list.location.city[cityANDtrips$list.location.city=="Durham"] <- "2"
cityANDtrips$list.location.city[cityANDtrips$list.location.city=="Raleigh"] <- "3"
cityANDtrips$list.location.city[cityANDtrips$list.location.city=="Morrisville"] <- "4"
cityANDtrips$list.location.city[cityANDtrips$list.location.city=="Philadelphia"] <- "5"
cityANDtrips$list.location.city[cityANDtrips$list.location.city=="Wake Forest"] <- "6"
cityANDtrips
class(cityANDtrips$list.location.city) #is chr
cityANDtrips$list.location.city <- as.numeric(cityANDtrips$list.location.city)
class(cityANDtrips$list.location.city)
library("mvnormtest")
library("car")
library("dplyr")
library("rcompanion")

#Format as dataframe
cityANDtrips<- as.data.frame(cityANDtrips)
class(cityANDtrips)

#drop nulls

cityANDtrips <- na.omit(cityANDtrips)

#Check for normality 
plotNormalHistogram(cityANDtrips$list.renterTripsTaken)

#Normalize 
cityANDtrips$list.renterTripsTakenSQRT <- sqrt(cityANDtrips$list.renterTripsTaken)
plotNormalHistogram(cityANDtrips$list.renterTripsTakenSQRT)
cityANDtrips$list.renterTripsTakenCube <- cityANDtrips$list.renterTripsTaken ^ 3
plotNormalHistogram(cityANDtrips$list.renterTripsTakenCube)

#Not much improvement will continue with orginal data since there is a large data

#Test for Homogeneity of Variance

fligner.test(list.location.city~list.renterTripsTaken, data =  cityANDtrips)
#meets the assumption with p value < .05

cityANOVA <- aov(cityANDtrips$list.renterTripsTaken ~ cityANDtrips$list.location.city)
summary(cityANOVA)

#Conculsion : There is a difference in trips taken between the cities

#Post HOC

pairwise.t.test(cityANDtrips$list.renterTripsTaken, cityANDtrips$list.location.city, p.adjust="bonferroni")

#Look at differences in means

cityMeans <- cityANDtrips %>% group_by(list.location.city) %>% summarize(Mean = mean(list.renterTripsTaken))


#Does being an allstar Host influence trips taken and price
keep1 <- c("list.rate.averageDailyPrice", "list.owner.allStarHost", "list.renterTripsTaken")

allStar <- Turo[keep1]

colnames(allStar)[1] ="DailyPrice"
colnames(allStar)[2] ="allStar"
colnames(allStar)[3] ="TripsTaken"

allStar$DailyPrice <- as.numeric(allStar$DailyPrice)
allStar$TripsTaken <- as.numeric(allStar$TripsTaken)

allStarMatrix <- as.matrix(allStar)

library("mvnormtest")

mshapiro.test(t(allStarMatrix)) #violates assumption of normality
