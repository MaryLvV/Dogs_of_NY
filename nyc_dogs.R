setwd("C:/R_and_PowerBI")
library(tidyverse)
library(ggmap)
library(leaflet)
library(caret)


#https://fusiontables.google.com/data?docid=1pKcxc8kzJbBVzLu_kgzoAMzqYhZyUhtScXjB0BQ#rows:id=1
#retrieved on September 4, 2017
NYC_dogs <- read_csv("data/Dogs_NYC.csv", na = c("", 'n/a'))

#make names consistently cased
NYC_dogs$upper_name <- toupper(NYC_dogs$dog_name)


#create a date from the birth variable
NYC_dogs$birth_date <- as.Date(gsub('-', '01', NYC_dogs$birth), format = '%b%d%y')
NYC_dogs$age <- as.numeric((as.Date('2013-02-01') - NYC_dogs$birth_date)/365)

#create factors for visualzing
NYC_dogs$gender <- as.factor(NYC_dogs$gender)
NYC_dogs$spayed_or_neutered <- as.factor(NYC_dogs$spayed_or_neutered)
NYC_dogs$guard_or_trained <- as.factor(NYC_dogs$guard_or_trained)
NYC_dogs$borough <- as.factor(NYC_dogs$borough)
NYC_dogs$breed <- as.factor(NYC_dogs$breed)
NYC_dogs$dominant_color <- as.factor(NYC_dogs$dominant_color)

#categorical variables
par(mfrow = c(3,1))
plot(NYC_dogs$gender, main = "NYC Dogs Gender", col = "darkgreen")
plot(NYC_dogs$spayed_or_neutered, main = "NYC Dogs Spayed or Neutered", col = "darkgreen")
plot(NYC_dogs$guard_or_trained, main = "NYC Dogs Trained", col = "darkgreen")
#plot(NYC_dogs$borough, main = "NYC Dogs by Borough", col = "darkgreen")
par(mfrow = c(1, 1))
plot(as.factor(NYC_dogs$dominant_color), las = 3, cex.names = .75, main = "NYC Dogs Dominant Color", col = "cadetblue4")

#continuous variables
boxplot(NYC_dogs$age)

#clean up age outliers
NYC_dogs$age <- ifelse(NYC_dogs$age < 0, median(NYC_dogs$age), NYC_dogs$age)
NYC_dogs$age <- ifelse(NYC_dogs$age > 30, median(NYC_dogs$age), NYC_dogs$age)

#how many dogs are trained as guard dogs?
sum(NYC_dogs$guard_or_trained == 'Yes') #166

missing_gender <- NYC_dogs %>% filter(is.na(gender))
View(missing_gender)

#we could take a guess at gender based on name...but let's just remove them for our first pass
NYC_dogs <- NYC_dogs %>% filter(!is.na(gender))
#filter our numeric variables for missing values
NYC_dogs <- NYC_dogs %>% filter(!is.na(zip_code))
NYC_dogs <- NYC_dogs %>% filter(!is.na(age))



#summary by breed
by_breed <- NYC_dogs %>%
  group_by(breed, spayed_or_neutered) %>%
  summarise(count= n()) 

ggplot(by_breed, aes(breed, count)) + 
  geom_col(aes(fill = spayed_or_neutered)) +
  ggtitle("Dogs of NYC - Spayed or Neutered by Breed") +
  theme(axis.text.x = element_text(angle = 90, size = 8,
                                   hjust = 1))


#summary by borough
by_borough <- NYC_dogs %>%
  group_by(borough, spayed_or_neutered) %>%
  summarise(count = n()) 

ggplot(by_borough, aes(borough, count)) + 
  geom_col(aes(fill = spayed_or_neutered)) +
  ggtitle("Dogs of NYC - Spayed or Neutered by Borough")

#summary by age
by_age <- NYC_dogs %>%
  group_by(age = round(age), spayed_or_neutered) %>%
  summarise(count = n()) 

ggplot(by_age, aes(age, count)) + 
  geom_col(aes(fill = spayed_or_neutered)) +
  ggtitle("Dogs of NYC - Spayed or Neutered by Age") +
  scale_x_continuous(breaks = c(seq(1, 25, by = 1)))


#summary by name where more than 250 dogs have a particular name
by_name <- NYC_dogs %>%
  group_by(dog_name, spayed_or_neutered) %>%
  summarise(count= n()) %>%
  filter(count >250) %>%
  filter(!is.na(dog_name))


#create a subset of NYC_dogs where dog_name is shared by more than 250 dogs
pop_names <- NYC_dogs %>%
  subset(dog_name %in% by_name$dog_name) %>%
  group_by(dog_name, spayed_or_neutered) %>%
  summarise(count = n())

ggplot(pop_names, aes(dog_name, count)) + 
  geom_col(aes(fill = spayed_or_neutered)) +
  ggtitle("Dogs of NYC - Spayed or Neutered by Popular Name") +
  theme(axis.text.x = element_text(angle = 90, size = 10,
                                   hjust = 1))


#uncomment the code in lines 116 - 132 the first time through to geocode zipcodes, 
#bind them to the zipcode summary data, and then merge lat/lon pairs with the
#main dataset; save these as .rda files so you can just load them as needed (lines 135-136)

#summary by zipcode
# by_zip <- NYC_dogs %>%
#   group_by(zip_code) %>%
#   summarise(count= n())

#google limits geocode requests to 2500 per day
#we look up each unique zipcode to get lat/lon and bind the results to our zipcode summary table

# locations <-  geocode(as.character(by_zip$zip_code))
# by_zip <- cbind(by_zip, locations)    #33 (out of 225) zipcodes not geocoded (15%)
 
#merge lon/lat pairs with our original data
# merge_loc <- by_zip %>% select(zip_code, lon, lat)
# dogs_loc <- merge(NYC_dogs, merge_loc)

#since geocoding takes a little time, we save these objects as .rda files
# save(dogs_loc, file = 'NYC_dogs_with_location.rda')
# save(by_zip, file = 'zipcode_summary.rda')

#load the dogs_loc and by_zip data
load('NYC_dogs_with_location.rda')
load('zipcode_summary.rda')

#take a quick look at these on a map
library(leaflet)
dog_map <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = by_zip$lon, lat = by_zip$lat)

dog_map

#you must un-comment and run the code in lines 151 - 175 the first time 
#to build your model - then comment them out again and use the 3 load commands

#set a seed value so we can repeat our random selection
#divide the data randomly - 80% for training
# set.seed(42)
# 
# #random selection rows of data for our training and test datasets
# train <- sample(1:81462, 81462 * .80)
# test <- setdiff(1:81462, train)

# #use these selections to subset the data
# train_data <- subset(dogs_loc[train, ])
# test_data <- subset(dogs_loc[test, ])

#pull 5000 rows for our (fabricated) 'new' cases
# new <- sample(1:16295, 5000)
# new_cases <- test_data[new, ]
# test_data <- test_data[-new, ]
# 
# new_cases <- new_cases %>% select(-spayed_or_neutered)
# save(new_cases, file = 'new_cases.rda')
# 
# #fit our model
# model_small <- glm(spayed_or_neutered ~ gender + breed + age + dominant_color + zip_code,
#               family = binomial(link = 'logit'), 
#               data = train_data)
#  
# save(model_small, file = 'model_small.rda')
# save(test_data, file = 'test_data.rda') 

load('model_small.rda')
load('new_cases.rda')
load('test_data.rda')

summary(model_small)


predicted_small <- predict(model_small, test_data, type = 'response')

#change numeric values to Yes and No
predicted_small2 <- as.factor(ifelse(predicted_small > 0.5, 'Yes', 'No'))


#confusion matrix
confusionMatrix(test_data$spayed_or_neutered, predicted_small2)



