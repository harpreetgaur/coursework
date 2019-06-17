########################################
# load libraries
########################################

# load some packages that we'll need
library(tidyverse)
library(scales)
library(lubridate)
# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides
trips %>% 
  filter(tripduration/60<1000) %>%
  ggplot(mapping = aes(x=tripduration/60)) + 
  geom_histogram() + scale_y_continuous(label = comma) +
  scale_x_log10() + xlab(label = "Trip duration in mins")

# plot the distribution of trip times by rider type
trips %>% 
  filter(tripduration/60<1000) %>%
  ggplot(mapping = aes(x=tripduration/60)) + 
  geom_histogram() + facet_wrap(~usertype, scale = "free_y") +
  scale_y_continuous(label = comma) +
  scale_x_log10() + xlab(label = "Trip duration in mins") 

# plot the total number of trips over each day
trips %>% group_by(ymd) %>% count(ymd) %>%
  ggplot(mapping = aes(x = ymd, y = n)) + geom_line()+
  xlab(label = "Date") +ylab(label = "Number of trips")


# plot the total number of trips (on the y axis) by age (on the x axis) and gender (indicated with color)
trips %>% mutate(age = (year(ymd)-birth_year)) %>% 
  group_by(age,gender)%>% summarize(count = n()) %>% 
  ggplot(mapping = aes(x=age, y = count, color=gender)) + 
  scale_y_continuous(lim = c(0,250000)) + geom_point()+
  scale_color_brewer(type = "qualitative", palette = "Pastel1", direction = -1)

# plot the ratio of male to female trips (on the y axis) by age (on the x axis)
# hint: use the spread() function to reshape things to make it easier to compute this ratio
trips %>% 
  mutate(age = (year(ymd)-birth_year)) %>% 
  group_by(age,gender) %>% summarize(count = n()) %>% 
  spread(gender, count) %>% mutate(ratio = Male/Female) %>%
  ggplot(mapping = aes(x = age, y= ratio))+geom_line() + scale_x_continuous(lim = c(15,90))
  

########################################
# plot weather data
########################################
# plot the minimum temperature (on the y axis) over each day (on the x axis)
weather %>% 
  ggplot(mapping = aes(x=ymd, y=tmin)) + geom_line()

# plot the minimum temperature and maximum temperature (on the y axis, with different colors) over each day (on the x axis)
# hint: try using the gather() function for this to reshape things before plotting
weather %>%
  gather("min_max","temperature",tmin,tmax) %>%
  ggplot(mapping = aes(x = ymd, y = temperature, color=min_max)) + geom_line()

########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this (its already joined)
View(trips_with_weather)

trips_with_weather %>% 
  group_by(ymd, tmin) %>% summarize(count=n()) %>% 
  ggplot(mapping = aes(x=tmin, y=count)) + geom_point()
  

# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this
trips_with_weather %>% 
  group_by(ymd) %>%
  summarize(count = n(), prcp = mean(prcp), tmin = mean(tmin)) %>%
  mutate(rain = prcp>=mean(prcp)+ 2 * sd(prcp)) %>%
  ggplot(mapping = aes(x = tmin, y = count, color = rain))+ 
  geom_point() + geom_line()

# add a smoothed fit on top of the previous plot, using geom_smooth

trips_with_weather %>% 
  group_by(ymd) %>%
  summarize(count = n(), prcp = mean(prcp), tmin = mean(tmin)) %>%
  mutate(rain = prcp>=mean(prcp)+ 2 * sd(prcp)) %>%
  ggplot(mapping = aes(x = tmin, y = count, color = rain)) + 
  geom_point() + geom_smooth()




# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package
trips %>% mutate(hour = hour(starttime)) %>% 
  group_by(ymd,hour) %>%
  summarize(count = n()) %>% group_by(hour) %>% 
  summarize(avg = mean(count), sd = sd(count))




# plot the above
trips %>% mutate(hour = hour(starttime)) %>% group_by(ymd,hour) %>%
  summarize(count = n()) %>% group_by(hour) %>% 
  summarize(avg = mean(count), sd = sd(count)) %>%
  ggplot(mapping = aes(x = hour, y = avg)) +
  geom_pointrange(aes(ymin = avg - sd, ymax = avg + sd))



# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package
trips %>% mutate(day = wday(starttime, label = TRUE)) %>% 
  group_by(ymd,day) %>%
  summarize(count = n()) %>% group_by(day) %>% 
  summarize(avg = mean(count), sd = sd(count)) %>%
  ggplot(mapping = aes(x = day, y = avg)) +
  geom_pointrange(aes(ymin = avg - sd, ymax = avg + sd))







