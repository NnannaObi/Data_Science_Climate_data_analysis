
#DATA SCIENCE ASSIGNMENT 
# OBIABUCHI MARTIN NNANNA

#install.packages('tidyverse')
#if (!require(scatterplot3d)) install.packages("scatterplot3d")
#install.packages(c("maps", "rnaturalearth", "rnaturalearthdata", "ggplot2", "sf", "leaflet"))

# Load the library
library('tidyverse')
library(ggplot2)
library(sf)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(scatterplot3d)
library(broom)


#Loading the data
clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep =";")
clim  <-as_tibble(clim )
View(clim)
dim(clim)
str(clim)


# Exploration Data Analysis-EDA
# Check for missing values
any(is.na(clim))

# check for duplicate values by columnwise
sum(duplicated(clim))

#Data cleaning
unique(clim$altitude)
unique(clim$p_mean)
#There is a problem with the variables "altitude" and "p_mean", they are in string fromat and contain comma special character. 

# Remove commas and convert to numeric
clim$altitude<- as.numeric(gsub(",", "", clim$altitude))
clim$p_mean <- as.numeric(gsub(",", "", clim$p_mean))
View(clim)
# Check for missing values
sum(is.na(clim$altitude))
sum(is.na(clim$lat))
sum(is.na(clim$lon))
sum(is.na(clim$p_mean))
#There is NO missing values on the key variable that will be used for this analysis and modelling


# Reset graphics device
dev.off()
# Set proper margins
par(mar = c(5, 4, 4, 2))


# Plot the map of France
map("world", regions = "France", col = "lightblue", fill = TRUE)

# Overlay climate data points
points(clim$lon, clim$lat, col = "red", pch = 19, cex = 1.5)

# Add labels for the points
text(clim$lon, clim$lat, labels = clim$station, pos = 4, cex = 0.8, col = "blue")
# Add a title
title("Map of Climate Stations in France with Temperature Attributes")

#EXERCISE 1

# Scatter plot with regression line
#NO. 1A
ggplot(clim, aes(x = altitude, y = t_mean)) +
  geom_point(color = "blue", size = 2) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Regression line
  labs(
    title = "Mean_Temperature vs Altitude",
    x = "Altitude",
    y = "Temperature"
  ) +theme_minimal()  # A clean theme for the plot
#The plot of mean temperature against altitude show the negative trend and drop in the temperature as the atitude increase, 
#I can closely see that  the  lower atitudes result to increase in temperature.

#NO. 1B
ggplot(clim, aes(x = lat, y = t_mean)) +
  geom_point(color = "blue", size = 2) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Regression line
  labs(
    title = "Mean_Temperature vs latitude",
    x = "Longitude",
    y = "Temperature"
  ) +theme_minimal()  # A clean theme for the plot

#In the latitudinal direction, there is slight negative trend in the temperature pattern

#NO. 1C
ggplot(clim, aes(x = lon, y = t_mean)) +
  geom_point(color = "blue", size = 2) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Regression line
  labs(
    title = "Mean_Temperature vs Longitude",
    x = "Longitude",
    y = "Temperature"
  ) +theme_minimal()  # A clean theme for the plot

# For the longitudianl  direction, there is slightly positive trend in the temperature pattern

#EXERCISE 1 Excludimg the two high mountains
Climfrar <- clim[1:34,]
Climfrar

Model_frar <- lm(t_mean~altitude + lat + lon, data=Climfrar)
summary(Model_frar)
tidy(Model_frar)

# A tibble: 4 × 5
#  term        estimate std.error statistic  p.value
#  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
#1 (Intercept) 37.3      2.62        14.2   7.29e-15
#2 altitude    -0.00641  0.000869    -7.38  3.17e- 8
#3 lat         -0.534    0.0558      -9.58  1.24e-10
#4 lon          0.0321   0.0396       0.811 4.24e- 1



# Extract R-squared and Adjusted R-squared
r_squared <- summary(Model_frar)$r.squared
r_squared
#r_squared is  0.8329 or 83.29%
adjusted_r_squared <- summary(Model_frar)$adj.r.squared
adjusted_r_squared 
#adjusted_r_squared is 0.8162 or 81.62


#Exercise 2; excluding the non-significant term, 'longtitude' from the model 
Model_frar1 <- lm(t_mean~altitude + lat, data=Climfrar)
summary(Model_frar1)
tidy(Model_frar1)

# A tibble: 3 × 5
#  term        estimate std.error statistic  p.value
#  <chr>          <dbl>     <dbl>     <dbl>    <dbl>
#1 (Intercept) 37.9      2.48         15.3  5.68e-16
#2 altitude    -0.00626  0.000844     -7.42 2.34e- 8
#3 lat         -0.547    0.0533      -10.3  1.72e-11


# Extract R-squared and Adjusted R-squared
r_squared <- summary(Model_frar1)$r.squared
#r_squared is  0.8292 or 82.92%
adjusted_r_squared <- summary(Model_frar1)$adj.r.squared
#adjusted_r_squared is 0.8182 or 81.82


#Extracting the values of the values
#For Mont-Ventoux
clim$altitude[clim$station=="Mont-Ventoux"]
clim$lat[clim$station=="Mont-Ventoux"]
clim$lon[clim$station=="Mont-Ventoux"]

#For Pic-du-Midi
clim$altitude[clim$station=="Pic-du-Midi"]
clim$lat[clim$station=="Pic-du-Midi"]
clim$lon[clim$station=="Pic-du-Midi"]

# Let the model without the high mountains and non-significant variable predict for Mont-Ventoux
predicted_mont_ventoux <- predict(
Model_frar1, newdata = list(
altitude=clim$altitude[clim$station=="Mont-Ventoux"],
lat= clim$lat[clim$station=="Mont-Ventoux"]
))
predicted_mont_ventoux

#For station Mont-Ventoux,  the model without the high mountains and non-significant variable,  predicted mean temperature is 6.081139 degrees centigrade, which is not equal to original value of 3.6 degrees centigrade

# Let the model without the high mountains and non-significant variable predict for Pic-du-Midi

clim$t_mean[clim$station=="Mont-Ventoux"]
clim$t_mean[clim$station=="Mont-Ventoux"]==6.187574
# The model without the high mountains and non-significant variable predicted FALSE value when compared with the original value of  3.6 degrees centigrade

# Let the model without the high mountains predict for Pic-du-Midi
predicted_pic_du_midi <- predict(Model_frar1, newdata = list(
altitude=clim$altitude[clim$station=="Pic-du-Midi"],
lat= clim$lat[clim$station=="Pic-du-Midi"]
))
predicted_pic_du_midi 
# Let the model without the high mountains and non-significant variable predict for Pic-du-Midi
clim$t_mean[clim$station=="Pic-du-Midi"]
clim$t_mean[clim$station=="Pic-du-Midi"]== -3.463727
# For station Pic-du-Midi, the model without the high mountains and non-significant variable, predicted mean temperature is -3.46 degrees centigrade which  is not equal to the original value of -1.2 degrees centigrade

#EXERCISE 3: Evaluation the model result.

# 3D Scatterplot
scatter_3d <- with(climfrar, scatterplot3d(altitude, lat, t_mean,
  pch = 16, highlight.3d = TRUE,
  angle = 45, xlab = "Altitude",
  ylab = "Latitude",
  zlab = "Temperature",
  main = "3D Scatterplot of Temperature by Latitude and Altitude"
))
scatter_3d$plane3d(Model_frar1)




