# load packages
# install.packages(c("lattice", "nortest", "car", "lmtest", "plm", "sandwich", "caret", "tidyverse"))
# use the above line ^^ if no package is installed

library(nortest)
library(car)
library(lmtest)
library(plm)
library(lattice)
library(sandwich)
library(caret)
library(tidyverse)

set.seed(123)

# didn't set the directory because was working in .Rproj the whole time
# however here's how I would set the directory:
# setwd('pathToProjectFolder/')


# load dataset
songs <- read_csv("spotify.csv")

summary(songs)

# data filtering
songs <- songs %>%
  mutate(duration_min = (duration_ms/1000)/60,
         genre = as.factor(genre),
         key = as.factor(key),
         mode = as.factor(mode),
         popularity = popularity/100) %>%
  filter(duration_min > 1.2 & duration_min < 6, popularity >= 0 & popularity <= 100) %>%
  select(genre, key, mode, danceability, energy, duration_min, popularity)

View(head(songs, 10))

# model
model <- lm(popularity ~ energy + duration_min + mode + danceability + genre, data = songs)

# summarize model
summary(model)

# residuals
residuals <- residuals(model)
#songs$residuals <- residuals(model)
# fitted values
fitted_values <- fitted(model)
resi_fit <- data.frame(residuals = residuals, fitted_values = fitted_values)

# Variables
hist(songs$popularity)
hist(songs$energy)
hist(songs$duration_min)
hist(songs$danceability)


### Assumptions ----

## 1. Assumption of Linearity (MET) ----

  # To check linearity for all continuous variables
  # We made a function that takes in a predictor variable and response variable
  # and returns a scatter plot with a line of best fit
  check_linearity <- function(predictor, response) {
    plot(predictor, response)
    abline(lm(response ~ predictor), col = "red")
    title(paste("Linearity:", deparse(substitute(predictor)), "vs", deparse(substitute(response))))
    xlab(deparse(substitute(predictor)))
    ylab(deparse(substitute(response)))
  }
  
  # 1) Proof 1 - Visualization
  # We applied the function to all continuous variables
  check_linearity(songs$danceability, songs$popularity)
  check_linearity(songs$energy, songs$popularity)
  check_linearity(songs$duration_min, songs$popularity)
  
  # Proof 2 - Q-Q Plot
  qqnorm(residuals)
  qqline(residuals)
  
  # Proof 3 - Random Binning
  hist(residuals,
       breaks = "FD",
       main = "Histogram of residuals with breaks = FD",
       xlab = "Residuals",
       ylab = "Frequency")

  
## 2. Assumption of Multicollinearity (MET) ----
  
  # VIF values should be less than 5. They are therefore assumption is met
  vif(model)
  
## 3. Assumption of Normality ----
  
  # Q-Q Plot for Normal Distribution
  qqnorm(residuals)
  qqline(residuals)
  
  # therefore, ad test (too much data for Shapiro)
  ad.test(residuals)
  
## 4. Assumption of Heteroscedasticity ----
  
  plot(model)
  
  ggplot(model, aes(x = fitted(model), y = residuals(model))) +
    geom_point() + 
    ggtitle("Scatter Plot of Residuals vs Fitted Values")
  
  # create a scale-location plot
  ggplot(model, aes(x = log(fitted(model)), y = residuals(model))) +
    geom_point() + 
    ggtitle("Scale-Location Plot")
  
  # Breusch-Pagan test
  bptest(model)
  
  # White test
  ncvTest(model)

## 6. Cook's Distance ----
    
    cooks_distance <- cooks.distance(model)
    cooks_distance
    
    songs$cooks_distance <- cooks_distance
    
    songs %>%
      filter(cooks_distance < 0.004) %>%
      ggplot(aes(residuals, cooks_distance)) +
        geom_point() + 
        geom_hline(yintercept = 4/nrow(songs), linetype = "dotted") +
        xlab("Observations") + 
        ylab("Cook's Distance")
    
    
# Model Summary ----
    # Unstandardized Coefficients
    coef(model)

    