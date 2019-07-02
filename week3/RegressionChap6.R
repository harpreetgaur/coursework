library(dplyr)

data <- read.table("~/coursework/week3/babyweights.txt")
View(data)

summary(data)


#Table 1
lm_fit <- lm(bwt~smoke, data = data)

summary(lm_fit)


#Table 2
lm_fit2 <- lm(bwt~parity, data = data)

summary(lm_fit2)



#Table 3


lm_fit3 <- lm(bwt~gestation+parity+age+height+weight+smoke, data = data)
summary(lm_fit3)

add_predictions(data, lm_fit3)



