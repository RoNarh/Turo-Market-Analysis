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
cityANDtrips$list.renterTripsTaken <- as.numeric(cityANDtrips$list.renterTripsTaken)
dplyr::recode_factor(cityANDtrips$list.location.city, 1 = "Cary", 2 = "Durham", 3 = "Philadelphia", 4 = "Morrisville", 5 = "Wake Forest")