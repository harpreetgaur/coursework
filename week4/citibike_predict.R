library(dplyr)
library(tidyverse)
library(modelr)
library(lubridate)

#loading tsv file
load_data <- read_tsv("~/coursework/week4/trips_per_day.tsv")

shuffled_predict <- sample_frac(load_data)



#Is it a weekday
shuffled_predict <- shuffled_predict %>% mutate(wday=as.factor(weekdays(ymd))) 
shuffled_predict

#Did it rain?
rain <- shuffled_predict %>% group_by(ymd) %>% 
  summarize(count = n(), prcp = mean(prcp), tmin = mean(tmin)) %>%
  mutate(rain = prcp>=mean(prcp)+ 2 * sd(prcp))


cb_data <- left_join(shuffled_predict, rain, by=c("tmin"="tmin","ymd"="ymd" , "prcp"="prcp"))

#splitting the shuffled data
train<-cb_data[1:292,]
test<-cb_data[293:365,]

#seeing if the split data is unique
intersect(train$date,test$date)

View(cb_data)

form <- as.formula(num_trips~poly(tmin, 2, raw = T)+rain+snow+wday)


#training the model
train_fit <- lm(form, data = train)
summary(train_fit)


#Adding predictions
trained_data <- add_predictions(train, train_fit)

validation_data <- add_predictions(test, train_fit)


ggplot() + geom_line(data = validation_data, mapping = aes(x=ymd, y=pred), 
                     color = "red") + geom_point(data = validation_data, 
                                                 mapping=aes(x=ymd, y=num_trips), color = "green") +
  geom_line(data = trained_data, mapping =
              aes(x=ymd, y=pred), color = "orange") + geom_point(data = trained_data, 
             mapping=aes(x=ymd, y=num_trips), color = "blue")



#Calculating RMSE and r2 for train and validation data

rmse_train <- rmse(train_fit, trained_data)
rmse_train


#Trying for another model by adding snow, is weekday and snowday (taking into account)
form2 <- as.formula(num_trips~poly(tmin, 5, raw = T)+tmax+rain+prcp+snow+snwd+wday)


#training the model
train_fit_2 <- lm(form2, data = train)
summary(train_fit_2)


#Adding predictions
trained_data_2 <- add_predictions(train, train_fit)

validation_data_2 <- add_predictions(test, train_fit)


ggplot() + geom_line(data = validation_data_2, mapping = aes(x=ymd, y=pred), 
                     color = "red") + geom_point(data = validation_data_2, 
                                                 mapping=aes(x=ymd, y=num_trips), color = "green") +
  geom_line(data = trained_data_2, mapping =
              aes(x=ymd, y=pred), color = "orange") + geom_point(data = trained_data_2, 
                                                                 mapping=aes(x=ymd, y=num_trips), color = "blue")



#Calculating RMSE and r2 for train and validation data

rmse_train_2 <- rmse(train_fit_2, trained_data_2)
rmse_train_2




rmse_validate_2 <- rmse(train_fit_2, validation_data_2)
rmse_validate_2

r2_train_2 <- rsquare(train_fit_2, trained_data_2)
r2_train_2

r2_validate_2 <- rsquare(train_fit_2, validation_data_2)
r2_validate_2





#Doing a cross validation
K <- 1:8

# creating vectors to store rmse and r2 for train and validation
v_rmse_train <- rep(NA, 8)
v_rmse_validate <- rep(NA, 8)

v_r2_train <- rep(NA, 8)
v_r2_validate <- rep(NA, 8)



for (k in K){
  form_k <- as.formula(num_trips~poly(tmin, k, raw = T)+tmax+rain+snow+snwd+wday+prcp)
  
  train_fit_k<- lm(form_k, data = train)
  
  trained_data_k <- add_predictions(train, train_fit_k)
  
  validation_data_k <- add_predictions(test, train_fit_k)
  
  
  #Calculating RMSE and R square
  
  v_rmse_train[k] <- rmse(train_fit_k, trained_data_k)
  v_rmse_validate[k] <- rmse(train_fit_k, validation_data_k)
  
  
  v_r2_train[k] <- rsquare(train_fit_k, trained_data_k)
  v_r2_validate[k] <- rsquare(train_fit_k, validation_data_k)
  
  
}

ggplot() + geom_line(data = data.frame(cbind(K, v_rmse_train, v_r2_train)), 
                     mapping = aes(x=K, y=v_r2_train),color = "red") +
  geom_line(data = data.frame(cbind(K, v_rmse_validate, v_r2_validate)), 
            mapping = aes(x=K, y=v_r2_validate),color = "blue")

ggplot() + geom_line(data = data.frame(cbind(K, v_rmse_train, v_r2_train)), 
                     mapping = aes(x=K, y=v_rmse_train),color = "red") +
  geom_line(data = data.frame(cbind(K, v_rmse_validate, v_r2_validate)), 
            mapping = aes(x=K, y=v_rmse_validate),color = "blue")


best_model <- which(v_rmse_validate == min(v_rmse_validate))
K[best_model]



#Working with optimal K value found above (in my case poly isnt doing well)

train_fit_best <- lm(num_trips~tmin+tmax+rain+snow+snwd+wday+prcp, data = train)


#adding predictions
trained_data_best <- add_predictions(train, train_fit_best)

validation_data_best <- add_predictions(test, train_fit_best)


# plotting the validation model
ggplot() + geom_line(data = validation_data_best, mapping = aes(x=ymd, y=pred), 
                     color = "red") + geom_point(data = validation_data_best, 
                                                 mapping=aes(x=ymd, y=num_trips), color = "green") +
  geom_line(data = trained_data_best, mapping =
              aes(x=ymd, y=pred), color = "black") +
  geom_point(data = trained_data_best, 
             mapping=aes(x=ymd, y=num_trips), color = "blue")


#Calculating RMSE and R2 values for degree 2

rmse_train <- rmse(train_fit_best, trained_data_best)
rmse_train

rmse_validate <- rmse(train_fit_best, validation_data_best)
rmse_validate

r2_train <- rsquare(train_fit_best, trained_data_best)
r2_train

r2_validate <- rsquare(train_fit_best, validation_data_best)
r2_validate


#plot for Predicted Vs Actual number of trips
ggplot() + geom_point(data = validation_data_best, mapping=aes(x=pred, y=num_trips), color = "green") +  geom_point(data = trained_data_best, 
             mapping=aes(x=pred, y=num_trips), color = "blue")












