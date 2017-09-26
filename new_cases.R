setwd("C:/R_and_PowerBI")

#load our model and the new cases
load('model_small.rda')
load('new_cases.rda')


#make predictions for our new cases
new_predictions <- predict(model_small, new_cases, type = 'response')
new_cases$spayed_or_neutered <- as.factor(ifelse(new_predictions > 0.5, 'Yes', 'No'))



#summary by borough
new_by_borough <- new_cases %>%
  group_by(borough, spayed_or_neutered) %>%
  summarise(count= n())

ggplot(new_by_borough, aes(borough, count)) + 
  geom_col(aes(fill = spayed_or_neutered)) +
  ggtitle("Dogs of NYC - New Cases by Borough") +
  scale_fill_discrete(name = 'predicted spayed/neutered')
