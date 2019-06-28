library(dplyr)

data <- read.table("~/coursework/week2/data.txt",
                   colClasses = c(rep("NULL",22), rep("double",3)), header=FALSE)
colnames(data) <- c("weight", "height", "gender")

#why is it giving an error?

new_data <- data %>% select(weight, height)
lm_fit <- lm(weight~height, data = new_data)

summary(lm_fit)

View(new_data)

confint(lm_fit)
