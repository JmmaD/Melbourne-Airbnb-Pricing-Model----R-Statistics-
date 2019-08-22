install.packages("mice")
install.packages("corrplot")
install.packages("leaflet")

library(tidyverse)
library(dslabs)
library(dplyr)
library(psych)
library(ggplot2)
library(stringr)
library(MASS)
library(mice)
library(corrplot)
library(readr)
library(leaflet)

# Read the file
data = read.csv("cleansed_listings_dec18.csv")
names(data)
head(data)



##Plotting & Basic data cleaning##

#Initial histogram of price
ggplot(data, aes(x=price)) +
  xlim(0, 1500) +
  geom_histogram(binwidth = 50) +
  labs(title = "Initial price histogram")

#Price histogram < $1000###
ggplot(data_cleaned, aes(x = price)) +
  geom_histogram(binwidth = 20) +
  labs(title = "Cleaned price histogram")

# Only include propertyies with price < 1000
clean_data <- data.frame(data) %>%
  filter(price<1000)


# Only included the variables that are possibly related to prices.
data_cleaned <- clean_data %>%
  dplyr::select(property_type, name, host_response_rate, space, summary, notes, description, access, 
                transit, cancellation_policy, first_review, last_review, neighborhood, zipcode, neighborhood_overview, 
                amenities,suburb, bathrooms, bedrooms, beds, price, review_scores_rating, reviews_per_month, guests_included, state,
                latitude, longitude, access, room_type, bed_type, accommodates, smart_location)

head(data_cleaned)

str(data_cleaned)
data_cleaned$host_response_rate <- as.numeric(data_cleaned$host_response_rate)
#Found that the host response rate has a wrong data type

#Change zipcode to numeric values
data_cleaned$converted_zipcode <- as.numeric(as.character(data_cleaned$zipcode))
#data_filtered_zip <- data_cleaned %>% filter(converted_zipcode >= 3000 & converted_zipcode <= 3051)
#summary(data_cleaned)
#summary(data_filtered_zip)

# Create a new column to group zipcodes within and out of Melbourn 
data_cleaned$filtered_zipcode <- ifelse(data_cleaned$converted_zipcode >= 3000 & data_cleaned$converted_zipcode <= 3051, TRUE, FALSE)

lm = lm(price ~ filtered_zipcode, data_cleaned)
summary(lm)



###Distribution of Records on map visualization###
data_cleaned = read.csv('/Users/sophie/Desktop/data_cleaned.csv')
data_maps<-data_cleaned %>%
  dplyr::select(price,latitude,longitude)
getColor <- function(data_maps_500) {
  sapply(data_maps_500$price, function(price) {
    if(price < 100) {
      "blue"
    } else if(price < 200) {
      "orange"
    } else {
      "red"
    } })
}
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(data_maps_500)
)
leaflet(data_maps_500) %>% addTiles() %>%
  addAwesomeMarkers(~longitude, ~latitude, icon=icons, label=~as.character(price),clusterOptions = markerClusterOptions())




#Room type exploration#
ggplot(data_cleaned %>% filter(price<1000), aes(x = price, fill = room_type, alpha = 0.8)) +
  geom_histogram() +
  labs(title = 'Prices by room type histogram')

ggplot(data_cleaned %>% filter(price<1000), aes(x = as.factor(room_type), y = price, fill = room_type)) +
  geom_boxplot() +
  labs(title = "Price vs. Room Type") +
  xlab("Room type")


#Property type Exploration
data_cleaned %>%
  filter(property_type == 'House' | property_type == 'Apartment' | property_type == 'Condominium' | property_type == 'Loft')%>%
  ggplot(aes(y = price, fill = property_type)) + 
  geom_boxplot() +
  labs(title = 'Property type boxplot')

data_cleaned %>% 
  filter(property_type == 'House' | property_type == 'Apartment' | property_type == 'Condominium' | property_type == 'Loft')%>%
  filter(price < 1000) %>%
  ggplot(aes(x = price, fill = property_type, alpha =.5)) + 
  geom_histogram()




###Boxplot of price vs bedrooms###
ggplot(data_cleaned, aes(x = as.factor(data_cleaned$bedrooms), y = price, fill = as.factor(data_cleaned$bedrooms))) + 
  geom_boxplot() + 
  xlab("Bedrooms")

# The reason that the prices goes down for '8 bedrooms' property is because there's not enough observations for 8 bedrooms.
eight_beds <- data_cleaned %>%
  filter(data_cleaned$bedrooms == 8)
summary(eight_beds)



###Boxplot of price vs bathrooms###
ggplot(data_cleaned, aes(x = as.factor(data_cleaned$bathrooms), y = price, fill = as.factor(data_cleaned$bathrooms))) + 
  geom_boxplot() + 
  xlab("Bathrooms")

fourplus_baths <- data_cleaned %>%
  filter(data_cleaned$bathrooms > 3.5)
summary(fourplus_baths)
str(fourplus_baths)
# There're not enough data points for over 3.5 bathrooms

ggplot(data_cleaned, aes(x = review_scores_rating, y = price, alpha = 0.5)) + 
  geom_point() + 
  xlab("Rating")





###AMENITIES EXPLORATION###

#Pool#
pool <- list(grepl("Pool|pool", data_cleaned$amenities))
data_cleaned["pool"] <-  pool

class(data_cleaned$amenities)

ggplot(data_cleaned, aes(x = price, fill = pool)) + 
  geom_histogram()

ggplot(data_cleaned, aes(x = as.factor(pool), y = price, fill = as.factor(pool))) + 
  geom_boxplot()


#Family Friendly#
family_friendly <- list(grepl("Family|family|family freindly|kids|Kids|family/kids|family/kid|kid friendly", data_cleaned$description))
data_cleaned["family_friendly"] <-  family_friendly

ggplot(data_cleaned, aes(x = as.factor(family_friendly), y = price, fill = as.factor(family_friendly))) + 
  geom_boxplot()



###DESCRIPTION EXPLORATION###

#beautiful#
beautiful <- list(grepl("Beautiful|beautiful", data_cleaned$description))
data_cleaned["beautiful"] <-  beautiful

ggplot(data_cleaned, aes(x = as.factor(beautiful), y = price, fill = as.factor(beautiful))) + 
  geom_boxplot()

#quiet#
quiet <- list(grepl("Quiet|quiet", data_cleaned$description))
data_cleaned["quiet"] <-  quiet

ggplot(data_cleaned, aes(x = as.factor(quiet), y = price, fill = as.factor(quiet))) + 
  geom_boxplot() +
  ylim(0, 1000)



#modern/contemporary#
modern <- list(grepl("Modern|modern|contemporary|Contemporary", data_cleaned$description))
data_cleaned["modern"] <-  modern

ggplot(data_cleaned, aes(x = as.factor(modern), y = price, fill = as.factor(modern))) + 
  geom_boxplot()



#majestsic/prestige#
majestic <- list(grepl("Majestic|majestic|prestige|prestigious|Prestigious", data_cleaned$description))
data_cleaned["majestic"] <- majestic

ggplot(data_cleaned, aes(x = as.factor(majestic), y = price, fill = as.factor(majestic))) + 
  geom_boxplot()



##Beach##
beach <- list(grepl("Beach|beach|ocean|Ocean", data_cleaned$description))
data_cleaned["beach"] <- beach

ggplot(data_cleaned, aes(x = as.factor(beach), y = price, fill = as.factor(beach))) + 
  geom_boxplot() +
  ylim(0,1000)




### Data Cleaning ###

# How many nas are there in each variable
sapply(data_cleaned, function(x) sum(is.na(x)))

#There are only few nas in beds/bedrooms/bathrooms/host_response_rate/suburb, so we just delete those rows.
data_cleaned <- data_cleaned %>%
  filter(!is.na(beds), !is.na(bedrooms), !is.na(bathrooms), !is.na(host_response_rate),!is.na(suburb))

str(data_cleaned)
data_cleaned$host_response_rate <- as.numeric(data_cleaned$host_response_rate)


#Only include the most commmon property_types
data_cleaned <- data_cleaned %>%
  filter(property_type == "Apartment" | property_type == "House" | property_type == "Loft" | property_type == "Condominium")



# There are thousands of missing values in review_scores_rating and reviews_per_month
# Use mice function to fill in the nas with expected mean of the entire column.
need_fill <- data_cleaned %>% dplyr::select(review_scores_rating,reviews_per_month)
head(data_cleaned)
class(need_fill)
fill_cplt <- mice(need_fill, method = "pmm")

cplt <- complete(fill_cplt)

data_cleaned$review_scores_rating <- cplt$review_scores_rating
data_cleaned$reviews_per_month <- cplt$reviews_per_month


### Correlation Matrix ###
data_cleaned_num <- data_cleaned %>% dplyr::select(price,
                                              host_response_rate,
                                              bathrooms,
                                              bedrooms,
                                              beds,
                                              review_scores_rating,
                                              reviews_per_month,
                                              guests_included,
                                              accommodates,
                                              beach,
                                              pool,
                                              beautiful,
                                              quiet)
data_cleaned_cor = cor(data_cleaned_num)
corrplot(data_cleaned_cor, addCoef.col = 'black')



#Interaction Analysis

ggplot(data_cleaned, aes(x = beds, y = price, color = room_type)) +
  geom_point()+
  geom_smooth(method = 'lm')

ggplot(data_cleaned, aes(x = reviews_per_month, y = price, color = property_type)) +
  geom_point()+
  geom_smooth(method = 'lm')

ggplot(data_cleaned, aes(x = review_scores_rating, y = price, color = property_type)) +
  geom_point()+
  geom_smooth(method = 'lm')

ggplot(data_cleaned, aes(x = review_scores_rating, y = price, color = room_type)) +
  geom_point()+
  geom_smooth(method = 'lm')

ggplot(data_cleaned, aes(x = review_scores_rating, y = price, color = pool)) +
  geom_point()+
  geom_smooth(method = 'lm')

ggplot(data_cleaned, aes(x = review_scores_rating, y = price, color = beautiful)) +
  geom_point()+
  geom_smooth(method = 'lm')






melbourne_housing_market <- read_csv("melbourne-housing-market.zip")

#Merging Airbnb and housing datasets on zipcode#
melb_housing <- melbourne_housing_market %>%
  group_by(Postcode) %>%
  summarize(m_price = mean(Price, na.rm = TRUE))

colnames(melb_housing)[colnames(melb_housing) == 'Postcode'] <- 'zipcode'


data_cleaned$zipcode <- as.numeric(as.character(data_cleaned$zipcode))
class(prices_1$zipcode)

prices_ultimate <- merge(data_cleaned, melb_housing, by = "zipcode", copy = FALSE)


prices_ultimate$USD_m_price <- prices_ultimate$m_price * .68

housing_prices_lm <- lm(price~USD_m_price, data=prices_ultimate)
summary(housing_prices_lm)


##Initial model##

lm_initial= lm(price ~ room_type +
          property_type + 
          bathrooms +
          beds +
          review_scores_rating   +
          reviews_per_month +   
          guests_included +
          as.factor(pool)+
          as.factor(beautiful)+
          as.factor(quiet) +
          as.factor(beach)+ 
          USD_m_price, data = prices_ultimate
)

summary(lm_initial)





#forward & backward selection
FitAll = lm(price ~ property_type + 
              host_response_rate + 
              cancellation_policy +
              bathrooms +
              beds +
              review_scores_rating +
              reviews_per_month +
              guests_included +
              room_type +
              bed_type +
              beach +
              pool + 
              beautiful +
              USD_m_price +
              quiet, data = prices_ultimate
            ) 

summary(FitAll)

#forward selection
fit.nothing=lm(price ~1, data=prices_ultimate)
forward_result <- step(fit.nothing,direction='forward', scope=formula('FitAll'))
summary(forward_result)
#backward selection
back_result <- step(FitAll, direction = "backward")
summary(back_result)

#write.csv(data_cleaned,'data_cleaned')






lm1= lm(price ~ room_type +
          property_type + 
          as.factor(cancellation_policy) +
          bathrooms +
          beds +
          review_scores_rating   +
          reviews_per_month +   
          guests_included +
          as.factor(pool)+
          as.factor(beautiful)+
          as.factor(quiet) +
          as.factor(beach)+ 
          USD_m_price +
          room_type*beds +
          as.factor(pool)*review_scores_rating, data = prices_ultimate
)

summary(lm1)


