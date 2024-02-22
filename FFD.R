rm(list=ls())
setwd(dir="/users/faker/desktop")
library(lme4)
library(ggplot2)
library(dplyr)
require(lmtest)

# Import data
phenology <- read.table("Observational_phenology.txt", header = TRUE, sep = "\t")
temperature <- read.table("Temperature.txt", header = TRUE, sep = "\t")

# Data preprocessing
# Convert Ordinal_day to Date
temperature$Date <- as.Date(temperature$Ordinal_day, format="%j", origin=paste(temperature$Year-1, "-01-01", sep=""))

# Calculate WST
calculate_wst <- function(year, temp_data) {
  # Filter previous year's Nov and Dec
  prev_year <- filter(temp_data, Year == year - 1 & format(Date, "%m") %in% c("11", "12"))
  # Filter current year's Jan, Feb, Mar
  current_year <- filter(temp_data, Year == year & format(Date, "%m") %in% c("01", "02", "03"))
  # Calculate mean
  mean_temp <- mean(c(prev_year$Ave_Temp_C, current_year$Ave_Temp_C), na.rm = TRUE)
  return(mean_temp)
}

# Apply WST calculation
phenology$WST <- sapply(phenology$Year, calculate_wst, temp_data = temperature)

# Check and process missing values
combined_data <- na.omit(phenology)
combined_data <- combined_data%>% 
  filter(Year >= 1975 & Year<=2010 & Year != 1995)

plot(Ordinal_date_of_flower~Year,data=combined_data, pch=19, cex=0.5)

# First, the average time of first flowering for each species in each year was calculated
species_yearly_avg <- combined_data %>%
  group_by(Year, Species) %>%
  summarise(Avg_Flowering_Time = mean(Ordinal_date_of_flower, na.rm = TRUE)) %>%
  ungroup()

#The average time of first flowering for all species in each year was then calculated
yearly_avg <- species_yearly_avg %>%
  group_by(Year) %>%
  summarise(Avg_Flowering_Time = mean(Avg_Flowering_Time, na.rm = TRUE))

par(mfrow = c(1, 2))

# 1. Plot of year vs. WST
ggplot(combined_data, aes(x = Year, y = WST)) +
  geom_point() +
  geom_line(group = 1) +
  theme_minimal() +
  labs(title = "Year vs. Winter-Spring Temperature (WST)",
       x = "Year",
       y = "WST")

# 2. Plot of year vs. first flowering time
ggplot(yearly_avg, aes(x = Year, y = Avg_Flowering_Time)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Average First Flowering Time Across All Species by Year",
       x = "Year",
       y = "Average First Flowering Time (Ordinal Date)")

# 3. Plot showing first flowering time of different species over years
ggplot(species_yearly_avg, aes(x = Year, y = Avg_Flowering_Time, color = Species)) +
  geom_point() +
  geom_line(aes(group = Species)) +
  theme_minimal() +
  labs(title = "First Flowering Time of Different Species Over Years",
       x = "Year",
       y = "First Flowering Time (Ordinal Date)")

# 4. Fit lines of first flowering time of different species over years
fits <- lapply(split(combined_data, combined_data$Species), function(df) {
  lm(Ordinal_date_of_flower ~ Year, data = df)
})

# Extract predicted values and confidence intervals from the fit
fits_data <- lapply(names(fits), function(species) {
  data.frame(Species = species,
             Year = combined_data$Year[combined_data$Species == species],
             Fit = predict(fits[[species]], newdata = combined_data[combined_data$Species == species,]))
})

# Convert a list to a data frame
fits_data <- do.call(rbind, fits_data)

# Draw raw data points and fit lines
ggplot(combined_data, aes(x = Year, y = Ordinal_date_of_flower, color = Species)) +
  geom_point() +
  geom_line(data = fits_data, aes(x = Year, y = Fit, group = Species)) +
  theme_minimal() +
  labs(title = "First Flowering Date Over Years for Different Species",
       x = "Year",
       y = "First Flowering Date")

# Output the processed data for inspection
head(combined_data)

# Construct a linear mixed-effect model
# Take the effect of average annual temperature on the first flowering time of plants
model1 <- lmer(Ordinal_date_of_flower ~ WST + (1|Species), data=combined_data)
model2 <- lmer(Ordinal_date_of_flower ~ WST + (1|Year) + (1|Species), data=combined_data)
model3 <- lmer(Ordinal_date_of_flower ~ WST + (1|Year) + (1|Species) +(1|Plot), data=combined_data)

plot(model3)
summary(model3)

# Model comparison
lrtest(model1,model2)
lrtest(model2,model3)

#Computational repeatability
mR1<-lmer(Ordinal_date_of_flower ~ 1+(1|Species), data=combined_data)
summary(mR1)
repeatability_s<-(307.2/(307.2+122.3))
repeatability_s

mR2<-lmer(Ordinal_date_of_flower ~ 1+(1|Year), data=combined_data)
summary(mR2)
repeatability_y<-(99/(99+1325.3))
repeatability_y

mR3<-lmer(Ordinal_date_of_flower ~ 1+(1|Plot), data=combined_data)
summary(mR3)
repeatability_p<-(3.9/(3.9+413.5))
repeatability_p







