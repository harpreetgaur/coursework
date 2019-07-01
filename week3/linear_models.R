library(scales)
library(broom)
library(modelr)
library(tidyverse)

options(na.action = na.warn)

theme_set(theme_bw())
options(repr.plot.width=4, repr.plot.height=3)

#Reading file
users <- read_tsv(gzfile("~/coursework/week3/users.tsv.gz"))

head(users)

# histogram of the label/regressor variable:
ggplot(users, aes(x = daily.views)) +
  geom_histogram(bins = 50) +
  scale_x_log10(label=comma, breaks=10^(0:ceiling(log10(max(users$daily.views))))) +
  scale_y_continuous(label = comma) +
  xlab('Daily pageviews') +
  ylab('')


#Plotting every user as a point by age
ggplot(data = users, aes(x = age, y = daily.views)) +
  geom_point() +
  facet_wrap(~ gender) +
  xlab('Age') +
  ylab('Daily pageviews')








