##----0.1 Questions ----
#1.	What musical feature most impacts a song’s popularity?
#2.	What features distinguish high popularity genres compared to low popularity genres
#3.	Can track popularity be predicted?
  
##----0.2 Packages ----
#Packages I will be using throughout the code in order to work efficiently

install.packages("tidyverse") #GGplot for graphing and visualization, dplyr for data manipulation, and tidyr for data tidying
install.packages("ggeasy") #Additional GGplot functions to enhance readability and customization
install.packages("ggstatsplot") #Additional GGplot function enhancements to allow for inbuilt statistical analysis
install.packages("car")#Package that allows for multicolinearity testing with ease
install.packages("corrplot")#Provides me with a correlation plot between n variables
install.packages("factoextra")#allows me to create a scree plot to assess required dimensionality 
install.packages(("RColorBrewer"))#Allows me to customise graph colourings
install.packages("treemapify")#Provides a treemap extension to GGplot
install.packages("ggalt")


library(tidyverse)
library(ggeasy)
library(ggstatsplot)
library(dplyr)
library(car)
library(corrplot)
library(factoextra)
library(broom)
library(RColorBrewer)
library(treemapify)
library(ggalt)

#----1.1 External file manipulation ------

#Reading the files and creation of Variables to work with

music_data<-read.csv("Music_Data.csv", header = TRUE) #Function allows me to import the Spotify songs data set from my folders


##----1.2 Main dataset reformatting ----

#I convert explicit (T/F) into 0/1 to facilitate analysis
expl_convert <- as.integer(as.logical(music_data$explicit))
music_data$explicit_converted <- expl_convert
music_data$explicit <- music_data$explicit_converted
music_data <- music_data %>% select(-explicit)

#The code below rearranges the data in a more favourable format
music_data$duration_sec <- music_data$duration_ms/1000 #removing ms time measure and replacing it with second ~ gets me lower numbers thus easier to interpret
music_data <- music_data %>% select(X, track_id, artists, album_name, track_name, 
                                    track_genre,explicit_converted, popularity, duration_sec,
                                    danceability, energy, loudness, speechiness, acousticness,
                                    instrumentalness, liveness, valence, tempo, time_signature, key, mode)


###----1.21 Descriptive Statistics for unstandardised dataset ----

#Descriptive statistics regarding variables used

#Popularity
summary(music_data$popularity)
sd(music_data$popularity)

#Danceability
summary(music_data$danceability)
sd(music_data$danceability)

#Energy
summary(music_data$energy)
sd(music_data$energy)

#loudness
summary(music_data$loudness)
sd(music_data$loudness)

#Speechiness
summary(music_data$speechiness)
sd(music_data$speechiness)

#Acousticness
summary(music_data$acousticness)
sd(music_data$acousticness)

#Instrumentalness
summary(music_data$instrumentalness)
sd(music_data$instrumentalness)

#Liveness
summary(music_data$liveness)
sd(music_data$liveness)


#Valence
summary(music_data$valence)
sd(music_data$valence)

#Tempo
summary(music_data$tempo)
sd(music_data$tempo)


#----2.0 Standardization of data ----


#Standardising all the numerical variables i intend to use allow me to perform accurate linear regressions
music_data_stds <- as.data.frame(scale(music_data[, c("danceability", "energy", "loudness", 
                                                      "speechiness", "acousticness", "instrumentalness",
                                                      "liveness", "valence", "tempo")]))

#I then include the missing columns that provide additional information, which do not need standardization
music_data_stds <- cbind(music_data_stds, music_data[, c("artists", "album_name","track_name", 
                                                         "track_genre", "popularity" ,"explicit_converted", "duration_sec",
                                                         "time_signature", "key", "mode")])

#I then reorganise the columns to create a better structure
music_data_stds <- music_data_stds %>% select("artists", "album_name", "track_name", "track_genre", "duration_sec" , 
                                              "popularity", "explicit_converted" ,everything()) %>% view()



#After standardisation, i create a training and test dataset with a 70/30 split in order to assess the true, training predicted, and test predicted popularity
set.seed(345)
music_data_stds_shuffled <- music_data_stds[sample(1:nrow(music_data_stds)),] #shuffles data in random order

train_size = 0.7 #Training dataset size is 70% of original dataset.

music_data_stds_train <- music_data_stds_shuffled[1:(train_size*nrow(music_data_stds_shuffled)),]
music_data_stds_test <- music_data_stds_shuffled[(nrow(music_data_stds_train) +1):nrow(music_data_stds_shuffled),]




##----2.1 Descriptive statistics for standardised data -------

#This displays all the data with standardised values
clean_music_data_stds <- music_data_stds %>% select (track_genre, 6:16) %>% select(,-explicit_converted)
summary(clean_music_data_stds)

clean_train_data <- music_data_stds_train %>% select (track_genre, 6:16, -explicit_converted)

clean_test_data <- music_data_stds_test %>% select (track_genre, 6:16, -explicit_converted)
clean.F_test_data <- clean_test_data %>% 
  filter(track_genre %in% c("pop-film", "k-pop","chill","iranian", "latin", "romance",
                            "cantopop", "edm", "children", "rock-n-roll"))

organised_music_data_stds <- clean_music_data_stds %>% arrange(track_genre)
organised_music_data_stds.filter <- organised_music_data_stds %>% filter(track_genre %in% c("pop-film", "k-pop","chill",
                                                                                     "iranian", "latin", "romance",
                                                                                     "cantopop", "edm", "children",
                                                                                     "rock-n-roll"))


##----2.2 Average song across genre --------

#average song across each genre
grouped_music_data_stds <- music_data_stds %>% group_by(track_genre) %>% 
  summarise(across(c(popularity,danceability, energy, loudness, speechiness, 
                     acousticness, instrumentalness,
                     liveness, valence, tempo),mean))

summary(grouped_music_data_stds)

condensed <- grouped_music_data_stds %>% 
  filter(track_genre %in% c("pop-film", "k-pop","chill", "iranian", 
                            "latin", "romance","cantopop", "edm", "children", "rock-n-roll"))

write.table(condensed, "Full DS observed avg.csv", row.names = TRUE, sep = ",")


#The most popular genres were pop-film, k-pop, and chill
#the least popular were romance, iranian, and latin
#The median popularity genre was scored 34.89, or 35
#-> the median 4 genres were cantopop, edm, children, and rock-n-roll



##----2.3 Popularity box-plot for filtered genre data ----

summary(grouped_music_data_stds$popularity)
summary(music_data_stds$popularity)
#Difference is noted, therefore good to create boxplots for comparison

pop.boxplot_filtered_music_data <- organised_music_data_stds.filter %>% 
  select(track_genre, popularity)

ggplot(pop.boxplot_filtered_music_data, aes(x = track_genre, y = popularity, fill = track_genre)) +
  geom_boxplot() +
  labs(title = "Popularity Distribution by Genre",
       subtitle = "Box-plot of filtered genres",
       y = "Popularity", 
       x = "Genre",
       caption = "Dataset origin: music_data_stds, filtered by genre and reformatted.",
       fill = "Genre") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##----2.4 Correlation plot between variables in full dataset and grouped genre ----


#This plots correlations of the full dataset
correlation_plot_grouped_music.data <-  organised_music_data_stds %>% 
  select(-track_genre) #This selects all tracks in the relevant genres, and removes categorical columns

corr_grouped_matrix <- cor(correlation_plot_grouped_music.data)
summary(corr_grouped_matrix)
corr_grouped_matrix.pop <-corr_grouped_matrix["popularity", -1]
corrplot(corr_grouped_matrix, 
         method = "color",
         mar = c(1, 1, 1, 1),
         tl.cex = 0.8,
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.6)
title("Correlation plot of musical features in the full dataset", line = 0.75, cex.main = 1.5)
mtext("Dataset: music_data_stds", side = 1, line = 3, adj = 1, cex = 0.8)


#This plots correlations of test data used for MVR
corr_plot_test_data <- organised_music_data_stds.filter %>% 
  select(-track_genre)

corr_test_matrix <- cor(corr_plot_test_data)
corrplot(corr_test_matrix,method = "color",
         mar = c(1, 1, 1, 1),
         tl.cex = 0.8,
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.6)
title("Correlation plot of musical features in the genre-filtered dataset", line = 0.75, cex.main = 1.5)
mtext("Dataset: music_data_stds, filtered by genre", side = 1, line = 3, adj = 1, cex = 0.8)

##----2.5 Feature Coefficient Analysis (FCA) ----

#To identify feature coefficients, we can perform 2 sets of regressions, Multivariate, and individual 

#Descriptive Multivariate model using limited genre dataset
Desc.filter_multivar_model <- lm(popularity ~ danceability + energy + 
                                   loudness + speechiness + acousticness + 
                                   instrumentalness + liveness + valence + tempo, 
                                 data = organised_music_data_stds.filter)
summary(Desc.filter_multivar_model)

#This code adds the coefficients into a dataframe
summary_DescF.multivar <- as.data.frame(Desc.filter_multivar_model$coefficients)


#This outputs the prior dataframe into a csv file
write.table(summary_DescF.multivar, "Filtered MVR Model output.csv", row.names = TRUE, sep = ",")

#Descriptive multivariate model using full dataset
Desc_multivar_model <- lm(popularity ~ danceability + energy + 
                            loudness + speechiness + acousticness + 
                            instrumentalness + liveness + valence + tempo, 
                          data = clean_music_data_stds)

summary_Desc.multivar <- as.data.frame(Desc_multivar_model$coefficients)

write.table(summary_Desc.multivar, "Full MVR Model output.csv", row.names = TRUE, sep = ",")

test_multivar.model <- lm(popularity ~ danceability + energy + 
                            loudness + speechiness + acousticness + 
                            instrumentalness + liveness + valence + tempo, 
                          data = clean_test_data)
summary(test_multivar.model)

summary.Multivar_Test <- as.data.frame(test_multivar.model$coefficients)
write.table(summary.Multivar_Test, "Filtered Test MVR data.csv", row.names = TRUE, sep = ",")


##----2.6 Reformatting of Train/Test data ----
#Musical features are grouped and averaged from the training data
grouped_genre_stds_train <- music_data_stds_train %>% group_by(track_genre) %>% summarise(across(c(popularity,danceability, energy, loudness, speechiness, 
                                                                                                   acousticness, instrumentalness,
                                                                                                   liveness, valence, tempo),mean))

#top/bot 3 and 4 median genres are selected
control_group_stds_train <- grouped_genre_stds_train %>% group_by(track_genre) %>%
  filter(track_genre %in% c("pop-film", "k-pop", 
                            "chill", "iranian", "latin", "romance",
                            "cantopop", "edm", "children", "rock-n-roll"))

#This converts my data into a long format
control_group_stds_train_long <- control_group_stds_train %>% 
  pivot_longer(cols = c(danceability, energy, liveness, loudness, speechiness,  
                        acousticness, instrumentalness, valence, tempo), 
               names_to = "feature", 
               values_to = "value")

#This gives me data organised by feature, which is inconsistent with the arrangement of my other data
#To fix this, i create a variable that dictates the order i want the data to be in

feature_order <- c("danceability", "energy", "liveness", "loudness", 
                   "speechiness", "acousticness", "instrumentalness", 
                   "valence", "tempo")


#Following the training data, the Test data is restructured

#Restructuring of test data to remove the track name, album name, and additional info that does not benefit analysis
structured_music_data_stds_test <- music_data_stds_test %>% 
  select (track_genre, popularity, danceability, energy, liveness, loudness,
          speechiness, acousticness, instrumentalness, valence, tempo )

#This is to create average genre-grouped music data 
grouped_test_avg <-structured_music_data_stds_test %>% group_by(track_genre) %>% 
  summarise(across(c(popularity,danceability, energy, loudness, speechiness, 
                     acousticness, instrumentalness, 
                     liveness, valence, tempo),mean))

#I then transform it into a long format to match the training data
grouped_test_long <- grouped_test_avg %>%
  pivot_longer(
    cols = c(danceability, energy, liveness, loudness, speechiness,  
             acousticness, instrumentalness, valence, tempo),
    names_to = "feature",
    values_to = "value"
  )
#This ensures the test pivoted table has the correct features, matching the training dataset


#i then create a control_group with the pivoted test table and the genres i've selected
grouped_test_long <- grouped_test_long %>% select (track_genre, feature,value, popularity)

control_group_stds_test_long <- grouped_test_long %>% 
  filter(track_genre %in% c("pop-film", "k-pop", 
                            "chill", "iranian", "latin",
                            "romance","cantopop",
                            "edm", "children", "rock-n-roll"))


#----3.0 Individual Feature modelling for popularity ----

#In order to measure the training model on the test data, i create a loop that iterates the features and model  
#-> the goal is to create a predicted popularity using the test data, and compare it to the training prediction, as well as the averaged full-dataset prediction

#I first create an empty list in order to add data to it
predictions_list <- list()

#this creates a loop through each unique feature present in the training data (danceability, valence, etc)
for (feature_name in unique(control_group_stds_train_long$feature)) {
  
  # This creates a subset for the current feature in the loop
  train_data_subset <- control_group_stds_train_long %>% filter(feature == feature_name)
  
  #Then i fit the model onto the training subset
  model <- lm(popularity ~ value, data = train_data_subset)
  
  #I repeat the subset creation, but using the test data and the same feature
  test_data_subset <- control_group_stds_test_long %>% filter(feature == feature_name)
  
  #Using predict() i generate a predicted value of popularity based on the feature in the loop
  predill <- predict(model, newdata = test_data_subset)
  
  #This entry is then stored onto the column for predicted test values
  test_data_subset <- test_data_subset %>%
    mutate(predicted_test_pop = predill, data_type = "test-predicted")
  
  # The loop then ends by storing the resulting data into a list
  predictions_list[[feature_name]] <- test_data_subset
}

#following the iterations, the final list is attached to the final predictions df
final_predictions <- bind_rows(predictions_list)

# View the final predictions
head(final_predictions)

#Add the "final predictions" lines to "ordered_combined_data" matching genre and feature.
final_predictions <- final_predictions %>% select(-predicted)

##----3.1 Reformatting of indiv. model data ----
#After gathering all the data required, I format the data in order to combine it with the ordered combined data previously
final_predictions_rev <- final_predictions %>% 
  rename(predicted_popularity = predicted_test_pop) #renames variable for better comprehension

new_rows <- final_predictions_rev %>% mutate(data_type = "test_predicted") #creates the new rows to add to the prior ordered data 


#for troubleshooting purposes, i create a copy of the dataset i want to add the rows to
ordered_combined_data2 <- bind_rows (ordered_combined_data, new_rows) %>% 
  arrange(track_genre, factor(feature, levels = feature_order), factor(data_type, levels = c("observed"), "predicted", "test-predicted"))
#As the data seems correct, i replace the original dataset with the new, and remove the old one
ordered_combined_data <- ordered_combined_data2
rm(ordered_combined_data2)


#After creating the dataset, i change the previous "observed" values to the full dataset "observed" to ensure accuracy
fullset_grouped_music_data_stds_long <- grouped_music_data_stds%>% filter(track_genre %in% c("pop-film", "k-pop",
                                                                                             "chill", "iranian", "latin", "romance",
                                                                                             "cantopop", "edm", "children", "rock-n-roll")) %>%
  pivot_longer(cols = c(danceability, energy, liveness, loudness, speechiness,  
                        acousticness, instrumentalness, valence, tempo), 
               names_to = "feature", 
               values_to = "value"
  )
               
#Adds a column with all "full-observed" entries
fullset_grouped_music_data_stds_long <- fullset_grouped_music_data_stds_long %>% mutate(data_type = "full_observed")

#Creates a filtered dataset with only full_observed entried
observed_replacement <- fullset_grouped_music_data_stds_long %>% filter(data_type == "full_observed")#dataframe with the correct data

remaining_data <- ordered_combined_data %>% filter(data_type != "observed") %>% view()#filter removes "observed" values

full_combined_data <- bind_rows(remaining_data, observed_replacement) #adds full_observed rows to the filtered dataframe  

#I then update all values for predicted_popularity in the "full_observed" categories with the test predicted values
full_combined_data <- full_combined_data %>%
  group_by(track_genre, feature) %>% mutate(predicted_popularity = ifelse(data_type == "full_observed",
                                                                          predicted_popularity[data_type == "test_predicted"],
                                                                          predicted_popularity)) %>%
  ungroup()
#This rearranges the data in a more readable format
full_combined_data <- full_combined_data %>% 
  arrange(track_genre, factor(feature, levels = feature_order), factor(data_type, levels = c("full_observed", "predicted", "test-predicted")))

#Popularity is the average popularity score of the genre in the training data
#test_predicted shows the predicted popularity value on the test data based on the training data

#predicted_popularity shows the predicted popularity value based on the song features of the training data
#-> this was not used as it did not help in the overall analysis

###----3.12 Plotting of observed and test data for indiv. models ----

filtered_ordered_data <- full_combined_data %>% 
  filter(data_type != "predicted")

#This plots the observed data, with the predicted test values of individual musical features

ggplot(filtered_ordered_data, aes(x = value, y = popularity, color = track_genre, shape = data_type)) + 
  geom_point(size = 1.5, alpha = 0.8) + #These are the data point features
  facet_wrap(~ feature, scales = "free_x") +
  geom_smooth(data = filter(filtered_ordered_data, data_type == "full_observed"),
              method = "lm", colour = 'black') +
  scale_shape_manual(values =c(16, 17, 18)) + 
  theme_bw() +
  labs(x = "feature intensity" ,
       y = "popularity",
       title = "Faceted graph of popularity and numeric musical features in tracks", 
       subtitle = "average observed popularity in the full dataset, and predicted popularity based on individual training models",
       color = "Track Genre",
       shape = "Data Type",
       caption = "Dataset: filtered_ordered_data, derived from full_combined_data"
       )

###----3.13 Residuals for indiv. models ----
filtered_ordered_data$residuals <- filtered_ordered_data$popularity - filtered_ordered_data$predicted_popularity

residuals_summary_indiv_regression <- filtered_ordered_data %>% 
  group_by(feature) %>%
  summarise(
    mean_residual = mean(residuals),
    median_residual = median(residuals, na.rm = TRUE),
    sd_residual = sd(residuals, na.rm = TRUE),
    min_residual = min(residuals, na.rm = TRUE),
    max_residual = max(residuals, na.rm = TRUE),
    IQR_residual = IQR(residuals, na.rm = TRUE)
  )

write.table(residuals_summary_indiv_regression, "indiv regression residuals.csv", row.names = TRUE, sep = ",")

###----3.14 Descriptive statistics for genres, separated by popularity categories ----

#High popularity genres and their individual feature data
filtered_high <- filtered_ordered_data %>% 
  filter(track_genre %in% c("chill", "k-pop", "pop-film"))

filtered_high_data <- filtered_high %>% 
  group_by(feature) %>%
  summarise(
    mean_feature = mean(value),
    sd_feature = sd(value),
    median_feature = median(value, na.rm = TRUE),
    min_feature = min(value),
    max_feature = max(value)
) #Gives me the descriptive statistics

#Creates a csv file for easy importing into word
write.table(filtered_high_data, "high popularity feature data.csv", row.names = TRUE, sep = ",")


#Median popularity genres
filtered_median <- filtered_ordered_data %>% 
  filter(track_genre %in% c("children", "edm", "cantopop", "rock-n-roll"))

filtered_median_data <- filtered_median %>% 
  group_by(feature) %>%
  summarise(
    mean_feature = mean(value),
    sd_feature = sd(value),
    median_feature = median(value, na.rm = TRUE),
    min_feature = min(value),
    max_feature = max(value)
  ) #Repeated process

write.table(filtered_median_data, "median popularity feature data.csv", row.names = TRUE, sep = ",")


#low popularity genres
filtered_low <- filtered_ordered_data %>% 
  filter(track_genre %in% c("romance", "latin", "iranian"))

filtered_low_data <- filtered_low %>% 
  group_by(feature) %>%
  summarise(
    mean_feature = mean(value),
    sd_feature = sd(value),
    median_feature = median(value, na.rm = TRUE),
    min_feature = min(value),
    max_feature = max(value)
  ) #Repeated process

write.table(filtered_low_data, "low popularity feature data.csv", row.names = TRUE, sep = ",")


#Test plot to visualise relationship of high pop genres and features
#This is for high popularity genres only
ggplot(filtered_high, aes(x = value, y = popularity, color = track_genre, shape = data_type)) + 
  geom_point(size = 1.5, alpha = 0.8) + #These are the data point features
  facet_wrap(~ feature, scales = "free_x") +
  geom_smooth(data = filter(filtered_ordered_data, data_type == "full_observed"),
              method = "lm", colour = 'black') +
  scale_shape_manual(values =c(16, 17, 18)) + 
  theme_bw() +
  labs(x = "feature intensity" ,
       y = "popularity",
       title = "Faceted graph of popularity and numeric musical features in tracks", 
       subtitle = "average observed popularity in the full dataset, and predicted popularity based on individual training models",
       color = "Track Genre",
       shape = "Data Type",
       caption = "Dataset: filtered_ordered_data, derived from full_combined_data"
  )


#----4.0 Multivariate model for popularity ----


#I first create an empty list in order to add data to it
multivar_pred_list <- list()

#this creates a loop through each unique feature present in the training data (danceability, valence, etc)
for (genre_name in unique(music_data_stds_train$track_genre)) {
  
  # This creates a subset for the current feature in the loop
  multivar_train_subset <- music_data_stds_train %>% filter(track_genre == genre_name)
  
  #Then i fit the model onto the training subset
  multivar_model <- lm(popularity ~ danceability + energy + 
                         loudness + speechiness + acousticness + 
                         instrumentalness + liveness + valence + tempo, 
                       data = multivar_train_subset)
  
  #I repeat the subset creation, but using the test data and the same genre
  multivar_test_subset <- music_data_stds_test %>% filter(track_genre == genre_name)
  
  #Using predict() i generate a predicted value of popularity based on the genre in the loop
  pred_multivar_pop <- predict(multivar_model, newdata = multivar_test_subset)
  
  #This entry is then stored onto the column for predicted test values
  multivar_test_subset <- multivar_test_subset %>%
    mutate(pred_test_pop = pred_multivar_pop, data_type = "test-predicted")
  
  # The loop then ends by storing the resulting data into a list
  multivar_pred_list[[genre_name]] <- multivar_test_subset
}

#following the iterations, the final list is attached to the final predictions df
multivar_final_predictions <- bind_rows(multivar_pred_list)

#
multivar_final_predictions <- multivar_final_predictions %>% 
  select(track_genre, popularity,pred_test_pop, data_type, danceability, energy, liveness, 
         loudness, speechiness, acousticness, instrumentalness, valence, tempo )

multivar_df <- multivar_final_predictions %>% 
  filter(track_genre %in% c("pop-film", "k-pop","chill", "iranian", "latin",
                            "romance","cantopop","edm", "children", "rock-n-roll"))#This filters the data with the genres i want to analyse

#This groups the tracks by feature, averages the data and allows me to get an average predicted popularity per genre
multivar_df <- multivar_df %>% group_by(track_genre) %>% 
  summarise(across(c(popularity,danceability, energy, loudness, speechiness, 
                     acousticness, instrumentalness,
                     liveness, valence, tempo,pred_test_pop),mean)) %>%
  mutate(data_type = "test predicted")


##----4.1 VIF table for colinearity assessment ----
vif_multivar_model <- vif(multivar_model)
print(vif_multivar_model)

write.table(vif_multivar_model, "vif scores for training data.csv", row.names = TRUE, sep = ",")


##----4.2 Residual Calculations for MVR model ----

multivar_df_residuals <- multivar_final_predictions %>% 
  filter(track_genre %in% c("pop-film", "k-pop","chill", "iranian", "latin",
                            "romance","cantopop","edm", "children", "rock-n-roll"))

multivar_df_residuals$residuals <- multivar_df_residuals$popularity - multivar_df_residuals$pred_test_pop

residuals_summary_testing <- multivar_df_residuals %>% 
  group_by(track_genre) %>%
  summarise(
    mean_residual = mean(residuals),
    sd_residual = sd(residuals),
    median_residual = median(residuals),
    min_residual = min(residuals),
    max_residual = max(residuals)
  )
write.table(residuals_summary_testing, "MVR test residuals(test ver).csv", row.names = T, sep = ",")






#Tree map of genre popularity and genre
ggplot(condensed, aes(area = popularity, fill = track_genre, label = paste(track_genre, round(popularity, 1), sep = "\n"))) + 
  geom_treemap() + 
  geom_treemap_text(color = "white", place = "center") +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Average popularity score of genre",
    subtitle = "(Popularity ranging from 0-100)",
    caption = "Spotify Music Database"
         )

write.table(multivar_df, "Test-dataset predicted Avg.csv", row.names = TRUE, sep = ",")







##----4.3 Multivariate linear regression visualisation ----

#convert the multivariate model data into long format for visualisation
multivar_df_long <- multivar_df %>% pivot_longer(cols = c(danceability, energy, loudness, speechiness,
                                                     acousticness,instrumentalness, liveness, valence, tempo),
                                            names_to = "feature",
                                            values_to = "value")

multivar_predicted <- multivar_df_long
multivar_predicted$popularity <- multivar_predicted$pred_test_pop
multivar_predicted$data_type <- "observed"

multivar_full <- rbind(multivar_df_long, multivar_predicted) %>% arrange(track_genre, feature, data_type)


#Used to create a faceted plot of observed and MVR predicted test data
ggplot(multivar_full, aes(x = value, y = popularity, color = track_genre, shape = data_type)) + 
  geom_point(alpha = 0.8, size = 2, position = position_jitter(width = 0, height = 0.2)) + #These are the data point features
  facet_wrap(~ feature, scales = "free_x") +
  geom_smooth(fullrange = TRUE, data = filter(multivar_full, data_type == "test predicted"),
              method = "lm", colour = 'black', ) +
  scale_shape_manual(values =c(16, 17, 18)) + 
  labs(x = "feature intensity" ,
       y = "popularity",
       title = "Faceted graph of popularity and Multi-Variate Regression prediction based on genre", 
       subtitle = "average observed popularity in the full dataset, and predicted popularity based on MVR model",
       tag = expression(italic("Line and Confidence Interval are based on predicted test values")),
       color = "Track Genre",
       shape = "Data Type",
       caption = "Dataset: Multivariate dataset, derived from MVR modelling ") +
  theme_bw() +
  theme(plot.tag.position = c(0.15,0.03), 
        plot.tag = element_text(size = 10))


#----**UNUSED DATA** ---- 
##----Unused Plot for categories of genre and popularity ----
ggplot(multivar_full, aes(x = value, y = popularity, color = track_genre, shape = data_type)) + 
  geom_point(width = 0.2, height = 0, alpha = 0.8) +
  facet_wrap(~ category, scales = "free_x")
  geom_smooth(data = filter(multivar_full, data_type == "test predicted"),
              method = "lm", colour = 'black') +
  scale_shape_manual(values =c(16, 17, 18)) + 
  theme_minimal() +
  labs(x = "feature intensity" ,
       y = "popularity",
       title = "Faceted graph of popularity and Multi_variate Regression prediction based on genre", 
       subtitle = "average observed popularity in the full dataset, and predicted popularity based on MVR model",
       color = "Track Genre",
       shape = "Data Type",
       caption = "Dataset: Multivariate dataset, derived from MVR modelling "
  )

#This adds a column to the dataset distinguishing the groups 
multivar_full <- multivar_full %>%
  mutate(category = case_when (
    track_genre %in% c("pop-film", "k-pop", "chill") ~ "popular genres",
    track_genre %in% c("cantopop", "children", "edm", "rock-n-roll") ~ "median popularity genres",
    track_genre %in% c("latin", "romance", "iranian") ~ "unpopular genres",
    TRUE ~ "other"
  ))
multivar_full <- multivar_full %>% select(-7:13)


multivar_df_long$residuals <- multivar_df_long$popularity - multivar_df_long$pred_test_pop

write.table(multivar_df, "multivariate regression results.csv", row.names = TRUE, sep = ",")






##----Unused Graph for popularity ----

summary.data <- organised_music_data_stds.filter %>% summarise(
  min_feature = min(danceability),
  max_feature = max(danceability),
  pop_at_min = popularity[which.min(danceability)],
  pop_at_amx = popularity[which.max(danceability)]
)
view(summary.data)

summary_list <- list()

#This collects the column names, and excludes the first column as it is categorical
feature_columns <- colnames(organised_music_data_stds.filter)[2:ncol(organised_music_data_stds.filter)]

for (feature in feature_columns) { #This allows me to iterate through the loop and store the results every loop
  summary_data <- organised_music_data_stds.filter %>%
    summarise(
      feature_name = feature,
      min_feature = min(.data[[feature]], na.rm = TRUE),
      max_feature = max(.data[[feature]], na.rm = TRUE),
      pop_at_min = popularity[which.min(.data[[feature]])],
      pop_at_max = popularity[which.max(.data[[feature]])]
    )
  
  #This adds the iterated data to the list
  summary_list[[feature]] <- summary_data
}

#Combines all summaries into a single dataframe
final_summary_df <- do.call(rbind, summary_list)
final_summary_df <- final_summary_df [-1,]

#Point and line plot of popularity range based on feature
ggplot(final_summary_df, aes(x = feature_name)) +
  geom_errorbar(aes(ymin = pop_at_min, ymax = pop_at_max), width = 0.2) +
  geom_point(aes(y = pop_at_min), color = "blue", size = 3) +
  geom_point(aes(y = pop_at_max), color = "red", size = 3) +
  geom_text(aes(y = pop_at_min, label = round(pop_at_min, 1)), hjust = 2, size = 3.5, color = "black") +
  geom_text(aes(y = pop_at_max, label = round(pop_at_max, 1)), hjust = -2, size = 3.5, color = "black") +
  labs(
    x = "Musical Features",
    y = "Popularity",
    title = "Popularity Range for Musical Features",
    subtitle = "Red points: Maximum Popularity, Blue points: Minimum Popularity",
    caption = "Based on Spotify music dataset"
  ) +
  theme_minimal() +
  theme( axis.text.x = element_text(angle = 45, hjust = 1))





























##----Unused Heatmap of feature intensity and genre------

#This Heatmap indicates the test-predicted popularity of genres and their feature-values
ggplot(new_rows, aes(x = feature, y = track_genre, fill = value)) +
  geom_tile() + 
  scale_fill_gradientn(
    colors = brewer.pal(9, "Spectral"),
    name = "intensity of feature") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(
    x = "Musical Features within tracks", y = "Track Genre",
    title = "Heatmap of genre and musical feature intensity",
    subtitle = "The relationship between genres and certain musical features",
    caption = "Spotify music Dataset"
  )


