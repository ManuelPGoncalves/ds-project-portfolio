library(readr)

# Representative sample of music from the 2010's
a2010 = read.csv('/Users/manuelgoncalves/Desktop/Nova IMS/Statistics for DS/Project/2010.csv')

# Metadata 
# top genre: genre of the song
# year: year of the song (due to re-releases, the year might not correspond to the release year of the original song)
# bpm(beats per minute): beats per minute
# nrgy(energy): energy of a song, the higher the value the more energetic the song is
# dnce(danceability): the higher the value, the easier it is to dance to this song.
# dB(loudness): the higher the value, the louder the song.
# live(liveness): the higher the value, the more likely the song is a live recording.
# val(valence): the higher the value, the more positive mood for the song.
# dur(duration): the duration of the song.
# acous(acousticness): the higher the value the more acoustic the song is.
# spch(speechiness): the higher the value the more spoken word the song contains.
# pop(popularity): the higher the value the more popular the song is.

# Exploratory Data Analysis (EDA)
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(dplyr)

# Summary of data set
summary(a2010)

# Check for missing values in the data set
print("Missing values in 2010's:")
print(sum(is.na(a2010)))

# Correlation matrix heat map
ggcorrplot(cor(select(a2010,bpm,nrgy,dnce,dB,live,val,dur,acous,spch,pop)),
           title=' Correlation Matrix 2010’s',lab=TRUE)

# Create scatter plots for each variable against popularity
plot(a2010$bpm, a2010$pop, xlab="Beats Per Minute", ylab="Popularity", main="Popularity vs BPM", pch=19, col="blue")
plot(a2010$nrgy, a2010$pop, xlab="Energy", ylab="Popularity", main="Popularity vs Energy", pch=19, col="green")
plot(a2010$dnce, a2010$pop, xlab="Danceability", ylab="Popularity", main="Popularity vs Danceability", pch=19, col="red")
plot(a2010$dB, a2010$pop, xlab="Loudness", ylab="Popularity", main="Popularity vs Loudness", pch=19, col="purple")
plot(a2010$live, a2010$pop, xlab="Liveness", ylab="Popularity", main="Popularity vs Liveness", pch=19, col="orange")
plot(a2010$val, a2010$pop, xlab="Valence", ylab="Popularity", main="Popularity vs Valence", pch=19, col="pink")
plot(a2010$dur, a2010$pop, xlab="Duration", ylab="Popularity", main="Popularity vs Duration", pch=19, col="brown")
plot(a2010$acous, a2010$pop, xlab="Acousticness", ylab="Popularity", main="Popularity vs Acousticness", pch=19, col="gray")
plot(a2010$spch, a2010$pop, xlab="Speechiness", ylab="Popularity", main="Popularity vs Speechiness", pch=19, col="cyan")


# There are 26 different unique genres of music
# We need to find a way to group these into similar genres
# In order to be able to use meaningful dummy variables
length(unique(a2010$top.genre))


# Define logical genre categories
genre_categories = list(
  "Pop" = c("dance pop", "electropop", "pop", "neo mellow", "canadian pop", "irish pop", "aussietronica","alternative r&b","boy band",'australian pop'),
  "Hip Hop/Rap" = c("dfw rap", "emo rap", "canadian hip hop", "conscious hip hop", "atl hip hop",'country rap','canadian contemporary r&b'),
  "Rock" = c("modern rock", "celtic rock", "indie poptimism"),
  "Electronic" = c("brostep", "big room", "electronic trap", "electro house", "complextro", "edm"))

# Drop 'dB' (Loudness) feature since it contains negative, non-sensical values
a2010$dB = NULL

# Function to apply map 'genre_categories' to music genres
map_genre_to_category <- function(genre) {
  for (category in names(genre_categories)) {
    if (genre %in% genre_categories[[category]]) {
      return(category)
    }
  }
}

# Apply this function to the top.genre column
a2010$top.genre <- sapply(a2010$top.genre, map_genre_to_category)

# Rename the 'year' column to 'decade' to be in conformity with analysis
a2010 <- rename(a2010, decade = year)

# Update all values in the 'decade' column to '2010s'
a2010$decade = '2010s'

# Create dummy variables for genre
pop_genre = as.integer(a2010$top.genre=='Pop')
rap_genre = as.integer(a2010$top.genre=='Hip Hop/Rap')
rock_genre = as.integer(a2010$top.genre=='Rock')
electronic_genre = as.integer(a2010$top.genre=='Electronic')

# Using aggregate to calculate mean popularity for each genre
med_popularity_by_genre <- aggregate(pop ~ top.genre, data = a2010, median)

# Define a vector of colors
colors <- c("blue", "green", "red", "yellow")

# Creating a bar plot
barplot(med_popularity_by_genre$pop, names.arg = med_popularity_by_genre$top.genre, 
        main = "Median Popularity by Genre", xlab = "Genre", ylab = "Median Popularity",col=colors)


# Regression with popularity as target variable (using dummies)
model_dummies = lm(pop~bpm+nrgy+dnce+live+val+dur+acous+spch+
                     pop_genre+rap_genre+rock_genre,data=a2010)

# R omits the dummy 'pop' to avoid the dummy variable trap
# Dummy model summary
summary(model_dummies)

# Regression with popularity as target variable (no dummies)
model_no_dummies = lm(pop~bpm+nrgy+dnce+live+val+dur+acous+spch,data=a2010)

# No dummies model summary
summary(model_no_dummies)


# Check the distribution of the independent variables given popularity
# Under a log-log model 
plot(log(a2010$bpm), log(a2010$pop), xlab="Beats Per Minute", ylab="Popularity", main="Popularity vs BPM", pch=19, col="blue")
plot(log(a2010$nrgy), log(a2010$pop), xlab="Energy", ylab="Popularity", main="Popularity vs Energy", pch=19, col="green")
plot(log(a2010$dnce), log(a2010$pop), xlab="Danceability", ylab="Popularity", main="Popularity vs Danceability", pch=19, col="red")
plot(log(a2010$live), log(a2010$pop), xlab="Liveness", ylab="Popularity", main="Popularity vs Liveness", pch=19, col="orange")
plot(log(a2010$val), log(a2010$pop), xlab="Valence", ylab="Popularity", main="Popularity vs Valence", pch=19, col="pink")
plot(log(a2010$dur), log(a2010$pop), xlab="Duration", ylab="Popularity", main="Popularity vs Duration", pch=19, col="brown")
plot(log(a2010$acous), log(a2010$pop), xlab="Acousticness", ylab="Popularity", main="Popularity vs Acousticness", pch=19, col="gray")
plot(log(a2010$spch), log(a2010$pop), xlab="Speechiness", ylab="Popularity", main="Popularity vs Speechiness", pch=19, col="cyan")

# Can't log() accousticness because it contains 0 as a value

# log-log model with dummies
model_dummies_log = lm(log(pop) ~ log(bpm) + log(nrgy) + log(dnce) + log(live) + 
                         log(val) + log(dur) + acous + log(spch) + pop_genre
                       + rap_genre + rock_genre, data = a2010)

# Summary of the log-log model with dummies
summary(model_dummies_log)

# log-log model no dummies
model_no_dummies_log = lm(log(pop) ~ log(bpm) + log(nrgy) + log(dnce) + log(live) + 
                            log(val) + log(dur) + acous + log(spch), data = a2010)

# Summary of the log-log model without dummies
summary(model_no_dummies_log)

library(lmtest)
# 'Best' Model (highest explanatory power)
model_dummies = lm(pop~bpm+nrgy+dnce+live+val+dur+acous+spch+
                     pop_genre+rap_genre+rock_genre,data=a2010)

# Dummy model summary
summary(model_dummies)

# Testing for Heteroskedasticity

# The Breusch-Pagan Test
bptest(model_dummies)

# BP-Test -> p-value (0.4052) is greater than 0.05, it suggests that there
# there is not enough evidence to reject the null hypothesis of homoskedasticity.
# In other words, the test does not provide strong statistical evidence 
# of heteroskedasticity in the model.
# It's important to note that while this test does not indicate 
# heteroskedasticity, it does not necessarily prove homoskedasticity.

# The White Test (modified)
bptest(model_dummies, ~ fitted(model_dummies) + I(fitted(model_dummies)^2))

# Modified White Test -> p-value (0.1164) do not have sufficient evidence 
# to reject the null hypothesis of homoskedasticity

# Testing for Model Misspecification 
# The Ramsey’s RESET test
resettest(model_dummies) 

# Since the p-value in the test result is 0.6494, which is much higher than 
# the common significance level (0.05), it suggests that there is no 
# statistical evidence to reject the null hypothesis. In other words, the test 
# does not indicate the presence of specification errors such as omitted 
# variables or an incorrect functional form in your model.


# Let's compare our 4 main models 
# Level - level -> Dummies & No Dummies
# Log - log -> Dummies & No Dummies

# Level - level

# No Dummies
# The Breusch-Pagan Test
bptest(model_no_dummies)

# The White Test (modified)
bptest(model_no_dummies, ~ fitted(model_no_dummies) 
       + I(fitted(model_no_dummies)^2))

# Testing for Model Misspecification 
# The Ramsey’s RESET test
resettest(model_no_dummies) 


# Log - log
# Dummies

# The Breusch-Pagan Test
bptest(model_dummies_log)

# The White Test (modified)
bptest(model_dummies_log, ~ fitted(model_dummies_log) 
       + I(fitted(model_dummies_log)^2))

# Testing for Model Misspecification 
# The Ramsey’s RESET test
resettest(model_dummies_log)

# No Dummies

# Testing for Heteroskedasticity

# The Breusch-Pagan Test
bptest(model_no_dummies_log)

# The White Test (modified)
bptest(model_no_dummies_log, ~ fitted(model_no_dummies_log)
       + I(fitted(model_no_dummies_log)^2))

# Testing for Model Misspecification 
# The Ramsey’s RESET test
resettest(model_no_dummies_log) 
