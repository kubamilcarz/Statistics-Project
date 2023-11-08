library(tidyverse)
library(psych)

fraud = read_csv("dataset/fraud.csv")

fraud = fraud %>%
  select(!c(trans_date_trans_time, cc_num, amt, first, last, street, city, lat, long, trans_num, unix_time)) %>%
  mutate(merchant = as.factor(merchant)) %>%
  mutate(is_fraud = as.logical(is_fraud)) %>%
  mutate(gender = as.factor(gender))

summary(fraud)

describe(fraud)
names(fraud)

fraud %>%
  ggplot(aes(dob, is_fraud)) +
  geom_jitter() +
  geom_boxplot() +
  geom_smooth(method='lm', se = FALSE)

















tickets = read_csv("dataset/ticketPrices.csv")

tickets = tickets %>%
  mutate(airline = as.factor(airline)) %>%
  mutate(departure_time = as.factor(departure_time)) %>%
  mutate(arrival_time = as.factor(arrival_time)) %>%
  mutate(class = as.factor(class)) %>%
  mutate(stops = as.factor(stops)) %>%
  
  filter(price < 65000 & duration < 32)

summary(tickets)

head(tickets)

names(tickets)

lm(days_left~price, data = tickets)

boxplot(tickets$days_left)

levels(tickets$stops) = factor(c("1", "2", "0"))

levels(tickets$departure_time) = factor(c("1", "2", "3", "4", "5", "6"))
levels(tickets$arrival_time) = factor(c("1", "2", "3", "4", "5", "6"))

tickets %>%
  mutate(stops = as.numeric(stops)) %>%
  mutate(departure_time = as.numeric(departure_time)) %>%
  mutate(arrival_time = as.numeric(arrival_time))

summary(tickets)

tickets %>%
  ggplot(aes(stops, price)) +
  geom_jitter() +
  geom_boxplot() +
  geom_smooth(method='lm', se = FALSE)









#######################

fat = read_csv('dataset/bodyfat.csv')

summary(fat)

describe(fat)

names(fat)

fat %>%
  ggplot(mapping = aes(Wrist, BodyFat)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")


t.test(age~stroke, data = strokes, var.equal = FALSE)












#######################

stroke = read_csv('dataset/strokes.csv')



strokes = stroke %>%
  mutate(gender = as.factor(gender)) %>%
  mutate(hypertension = as.factor(hypertension)) %>%
  mutate(heart_disease = as.factor(heart_disease)) %>%
  mutate(ever_married = as.factor(ever_married)) %>%
  mutate(work_type = as.factor(work_type)) %>%
  mutate(Residence_type = as.factor(Residence_type)) %>%
  mutate(smoking_status = as.factor(smoking_status)) %>%
  mutate(stroke = as.factor(stroke)) %>%
  mutate(bmi = as.double(bmi))

summary(strokes)
names(strokes)

strokes %>%
  ggplot(mapping = aes(smoking_status, stroke)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

strokes %>%
  ggplot(mapping = aes(bmi, stroke)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

strokes %>%
  ggplot(mapping = aes(gender, stroke)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

strokes %>%
  ggplot(mapping = aes(ever_married, stroke)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")











######

names(wine) = c("fixed_acid", "volatile_acid", "citric_acid", "residual_sugar", "chlorides", "free_sulfur", "total_sulfur", "density", "pH", "sulphates", "alcohol", "quality", "id")

names(wine)

wine %>%
  ggplot(mapping = aes(chlorides, quality)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")















#####################

diamonds = read_csv('dataset/diamonds.csv')
summary(diamonds)

names(diamonds)

names(diamonds) = c("id", 'carat', 'cut', 'color', 'clarity', 'depth', 'table', 'price', 'x', 'y', 'z')

diamonds = diamonds %>%
  mutate(cut = as.factor(cut)) %>%
  mutate(color = as.factor(color)) %>%
  mutate(clarity = as.factor(clarity)) %>%
  mutate(volume = x*y*z) %>%
  drop_na() %>%
  filter(volume < 1000) %>%
  filter(price < 20000)


summary(diamonds)
names(diamonds)

diamonds %>%
  ggplot(mapping = aes(clarity, price)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

diamonds %>%
  ggplot(mapping = aes(cut, price)) +
  geom_boxplot()
















#####################

insurance = read_csv('dataset/medical.csv', col_names = c('age', 'sex', 'bmi', 'children', 'smoker', 'region', 'charges'))

names(insurance)

summary(insurance)

insurance = insurance %>%
  mutate(age = as.integer(age)) %>%
  mutate(sex = as.factor(sex)) %>%
  mutate(bmi = as.double(bmi)) %>%
  mutate(children = as.integer(children)) %>%
  mutate(smoker = as.factor(smoker)) %>%
  mutate(charges = as.double(charges)) %>%
  drop_na()

summary(insurance)

insurance %>%
  ggplot(mapping = aes(bmi, charges)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

insurance %>%
  ggplot(mapping = aes(children, charges)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

insurance %>%
  ggplot(mapping = aes(smoker, charges)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

insurance %>%
  ggplot(mapping = aes(age, charges)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")








#########################

amsterdam = read_csv('dataset/amsterdam.csv')
names(amsterdam) = c('id', 'address', 'zip', 'price', 'area', 'room_count', 'lon', 'lat')

summary(amsterdam)

amsterdam %>%
  ggplot(mapping = aes(room_count, price)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")











#########################

flights = read_csv("dataset/flights.csv")
names(flights)

flights = flights %>%
  select(!c(TAIL_NUMBER, TAXI_OUT, WHEELS_OFF, WHEELS_ON, TAXI_IN, CANCELLATION_REASON)) %>%
  select(!c(WEATHER_DELAY, LATE_AIRCRAFT_DELAY, AIRLINE_DELAY, YEAR, MONTH, DAY, DEPARTURE_TIME, ARRIVAL_TIME)) %>%
  select(!c(ELAPSED_TIME, AIR_TIME, SCHEDULED_ARRIVAL, AIR_SYSTEM_DELAY, SECURITY_DELAY, DIVERTED, CANCELLED))

flights = flights %>%
  mutate(AIRLINE = as.factor(AIRLINE)) %>%
  mutate(DAY_OF_WEEK = as.factor(DAY_OF_WEEK))
  

names(flights)
summary(flights)

flights %>%
  ggplot(mapping = aes(DAY_OF_WEEK, ARRIVAL_DELAY)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")





#########################

estate = read_csv("dataset/estate.csv")

names(estate) = c("index", "date", "house_age", "distance_to_MRT", "convenience_store", "lat", "long", "price")

estate %>%
  summarize(
    correlation = cor(convenience_store, price)
  )

names(estate)

estate %>%
  ggplot(mapping = aes(convenience_store, price)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

estate %>%
  ggplot(mapping = aes(house_age, price)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

estate %>%
  ggplot(mapping = aes(distance_to_MRT, price)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")








#####################

wine = read_csv2("dataset/wine.csv")

names(wine) <- c(
  "fixed_acidity", "volatile_acidity", "citric_acid", 
  "residual_sugar", "chlorides", "free_sulfur_dioxide", 
  "total_sulfur_dioxide", "density", "pH", "sulphates", "alcohol", "quality"
  )

wine = wine %>%
  mutate(volatile_acidity = as.double(volatile_acidity)) %>%
  mutate(citric_acid = as.double(citric_acid)) %>%
  mutate(chlorides = as.double(chlorides)) %>%
  mutate(density = as.double(density)) %>%
  mutate(sulphates = as.double(sulphates)) %>%
  filter(residual_sugar < 200)

summary(wine)

names(wine)

wine %>%
  summarize(
    correlation = cor(citric_acid, quality)
  )
  
  #ggplot(mapping = aes(alcohol, quality)) +
  #geom_jitter() +
  #geom_smooth(method = "lm", se = FALSE, color="orange")







########################33

# load dataset
housing = read_csv("dataset/realtor.csv")
housing = housing %>%
  drop_na() %>%
  select(!c("full_address", "street", "status", "city"))

housing = housing %>%
  mutate(state = as.factor(state)) %>% # factor
  filter(price < 250000) %>%
  filter(bed < 10) %>% # focus on houses with less than 10 bedrooms
  filter(bath < 6) %>% # less than 6 bathrooms
  filter(house_size < 10000) %>%
  filter(acre_lot < 50)


names(housing)

housing %>%
  ggplot(mapping = aes(bed, price)) +
  geom_boxplot()

# number of bathrooms vs price
housing %>%
  ggplot(mapping = aes(bath, price)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

# number of bedrooms vs price
housing %>%
  ggplot(mapping = aes(bed, price)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

# zip_code vs price
housing %>%
  ggplot(mapping = aes(zip_code, price)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

names(housing)

# house_size vs price
housing %>%
  ggplot(mapping = aes(house_size, price)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

# acre_lot vs price
housing %>%
  ggplot(mapping = aes(acre_lot, price)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

# state vs price
housing %>%
  ggplot(mapping = aes( price)) +
  geom_boxplot() +
  facet_wrap(.~state)

housing %>%
  summarise(
    cor = cor(house_size, price)
  )


#############################

names(delays)  <- c("date", "time", "day", "station", "code", "delay", "gap", "bound", "line", "vehicle")

delays = delays %>%
  drop_na() %>%
  mutate(bound = as.factor(bound)) %>%
  filter(bound %in% c("E", "N", "S", "W")) %>%
  mutate(line = as.factor(line)) %>%
  filter(line %in% c("BD", "SHP", "SRT", "YU")) %>%
  mutate(day = str_replace(day, 'Monday', '1')) %>%
  mutate(day = str_replace(day, 'Tuesday', '2')) %>%
  mutate(day = str_replace(day, 'Wednesday', '3')) %>%
  mutate(day = str_replace(day, 'Thursday', '4')) %>%
  mutate(day = str_replace(day, 'Friday', '5')) %>%
  mutate(day = str_replace(day, 'Saturday', '6')) %>%
  mutate(day = str_replace(day, 'Sunday', '7')) %>%
  mutate(day = as.integer(day))
  
names(delays)

summary(delays)

delays %>%
  summarize(
    corr = cor(day, delay)
  )

delays %>%
  filter(delay > 0 & delay <= 90) %>%
  ggplot(mapping = aes(date, delay)) +
  geom_jitter() +
  geom_smooth() +
  facet_wrap(.~line)

############################

names(delays)
summary(delays)

delays = delays %>%
  drop_na() %>%
  mutate(carrier = as.factor(carrier)) %>% # carrier to factor
  mutate(delay = str_replace(delay, ' min', '')) %>% # remove 'min'
  mutate(delay = as.integer(delay)) # delay as int

delays

delays %>%
  summarize(
    corr = cor(carrier, delay)
  )

delays %>%
  ggplot(mapping = aes(carrier, delay)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color="orange")


### Songs ----

library(tidyverse)
library(psych)

# set working directory
setwd("~/Users/kubamilcarz/My Drive/College/Sophomore/Statistics/Final Project")

#Set random seed
set.seed(10)

# load training data set
songs = read_csv("dataset/spotify.csv")
songs

# EDA ----
dim(songs)
glimpse(songs)

head(songs)

names(songs)

#Univariate EDA ----
summary(songs)

#Genre - categorical data ----
summary(songs$genre)

IQR(songs$popularity)
boxplot(songs$popularity)
describe(songs$popularity)

install.packages("beanplot")
library(beanplot)

beanplot(songs$popularity ~ songs$genre, las = 2, 
         what = c(1, 1, 1, 0),
         col = c("transparent"))

songs = songs %>%
  mutate(genre = as.factor(genre)) %>%
  mutate(key = as.factor(key)) %>%
  mutate(mode = as.factor(mode)) %>%
  mutate(tempo = as.factor(tempo)) %>%
  mutate(duration_min = (duration_ms/1000)/60) %>%
  filter(duration_min > 1.2 & duration_min < 6) %>%
  mutate(popularity = popularity/100) %>%
  filter(popularity >= 0 & popularity <= 100) %>%
  select(!c("track_name", "artist_name", "duration_ms"))

colnames(songs)

songs %>%
  summarize(
    corr = cor(duration_min, popularity)
  )

describe(songs)

hist(songs$valence)

library(car)
# Model ---
fit_lm <- lm(popularity ~ danceability + duration_min + energy * genre,
              data = songs)
fit_ancova <- aov(popularity ~ danceability + energy + Error(genre) + genre, data = songs)

TukeyHSD(fit_lm, p.adjust="fdr")

?ancova

summary(fit_lm)
anova(fit_lm)
summary(fit_anova)
anova(fit_lm, fit_anova)

install.packages('car')
install.packages('lmtest')
library(car)
library(lmtest)

# Durbin Watson Test
durbinWatsonTest(fit_lm)
durbinWatsonTest(fit_ancova)

par(mfrow = c(2, 2))
plot(fit_lm)
plot(fit_anova)

plot(fitted(fit_lm), resid(fit_lm), xlab='Fitted Values', ylab='Residuals')
abline(0,0)

# Residuals
residuals <- residuals(fit_lm)
qqnorm(residuals)
qqline(residuals)

# Residuals: Shapiro Test
sample_residuals = head(residuals, 5000)
shapiro.test(sample_residuals)

# Cook's Distance
cook = cooks.distance(fit_lm)
summary(cook)
plot(fit_lm, which = 4)

# Breusch-Pagan test
bptest(fit_lm)

# Heteroscedasticity assumption
ncvTest(fit_lm)

# normal distribution of residuals
hist(fit_lm$residuals)

# linearity test
plot(fit_lm, 1)

fit_lm %>%
  ggplot(aes(valence, popularity)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = valence, yend = .fitted), color = "red", size = 0.3)


install.packages('lsr')
library(lsr)
cross <- table(songs$genre, songs$popularity)
chisq.test(cross)

cramersV(fit_lm) #strong dependency







fit_lm %>% 
  ggplot(aes(genre, popularity)) +
  geom_bar(stat="identity")

fit_anova %>% 
  ggplot(aes(genre, popularity)) +
  geom_bar(stat="identity")


# ANOVA ---
install.packages('haven')
library(haven)
library(stats)

anova(fit)
summary(fit)








data_log = songs %>% mutate(valence = log(valence), popularity = log(popularity))

data_log %>%
  ggplot(aes(x = duration_min, y = popularity)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE)

# danceability vs popularity
songs %>%
  filter(duration_ms > 120000 & duration_ms < 240000) %>%
  ggplot(aes(danceability, popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

# acousticness vs popularity
songs %>%
  filter(duration_ms > 120000 & duration_ms < 240000) %>%
  ggplot(mapping = aes(acousticness, popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

# duration_ms vs popularity
songs %>%
  filter(song_duration_ms > 120000 & song_duration_ms < 240000) %>%
  ggplot(mapping = aes(song_duration_ms, song_popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

# instrumentalness vs popularity
songs %>%
  filter(song_duration_ms > 120000 & song_duration_ms < 240000) %>%
  ggplot(mapping = aes(instrumentalness, song_popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

# liveness vs popularity
songs %>%
  filter(song_duration_ms > 120000 & song_duration_ms < 240000) %>%
  ggplot(mapping = aes(liveness, song_popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="orange")
  # studio recorded are MUCH more popular than live

# loudness vs popularity
songs %>%
  filter(song_duration_ms > 120000 & song_duration_ms < 240000) %>%
  ggplot(mapping = aes(loudness, song_popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="orange")

# speechiness vs popularity
songs %>%
  filter(duration_ms > 120000 & duration_ms < 240000) %>%
  ggplot(mapping = aes(speechiness, popularity)) +
  geom_bin_2d(bins=30) +
  scale_fill_continuous(type="viridis", trans="log") +
  geom_smooth(method = "lm", se = FALSE, color="orange")

# tempo vs popularity
songs %>%
  filter(duration_ms > 120000 & duration_ms < 240000) %>%
  ggplot(mapping = aes(tempo, popularity)) +
  geom_bin_2d(bins=30) +
  scale_fill_continuous(type="viridis", trans="log") +
  geom_smooth(method = "lm", se = FALSE, color="orange") +
  geom_violin(alpha = 1/4)
  # around 140-150 bps

# time_signature vs popularity
songs %>%
  filter(duration_ms > 120000 & duration_ms < 240000) %>%
  ggplot(mapping = aes(time_signature, popularity)) +
  geom_bin_2d(bins=30) +
  scale_fill_continuous(type="viridis", trans="log") +
  geom_smooth(method = "lm", se = FALSE, color="orange")
  # 4/4 is the one

# valence vs popularity
songs %>%
  filter(duration_ms > 120000 & duration_ms < 240000) %>%
  ggplot(mapping = aes(valence, popularity)) +
  geom_bin_2d(bins=30) +
  scale_fill_continuous(type="viridis", trans="log") +
  geom_smooth(method = "lm", se = FALSE, color="orange")

# release_date vs popularity
songs %>%
  filter(duration_ms > 120000 & duration_ms < 240000) %>%
  ggplot(mapping = aes(release_date, popularity)) +
  geom_bin_2d(bins=30) +
  scale_fill_continuous(type="viridis", trans="log") +
  geom_smooth(method = "lm", se = FALSE, color="orange")

# release_date vs popularity
songs %>%
  filter(duration_ms > 120000 & duration_ms < 240000) %>%
  ggplot(mapping = aes(release_date, popularity)) +
  geom_bin_2d(bins=30) +
  scale_fill_continuous(type="viridis", trans="log") +
  geom_smooth(method = "lm", se = FALSE, color="orange")

names(songs)

# visualization
songs %>%
  group_by(genre) %>%
  summarise(
    avg_popularity = mean(popularity), 
    median_popularity = median(popularity),
    min_pop = min(popularity),
    max_pop = max(popularity),
    n = n()) %>%
  arrange(desc(avg_popularity)) %>%
  print(n= Inf)
  
songs %>%
  ggplot(aes(genre, popularity)) +
  geom_jitter(alpha = 1/20) +
  geom_boxplot(color = "blue", alpha=1/5) +
  coord_flip()

?geom_smooth

