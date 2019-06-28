#What is the sample average of the change in score between the patient's
#rating before the application of the device and the rating after the application?
magnets <- read.csv("magnets.csv")
summary(magnets)

#Is the variable "active" a factor or a numeric variable?

#it is a factor
levels(magnets$active)

#Compute the average value of the variable "change" for the patients that
#received and active magnet and average value for those that received an
#inactive placebo. 

mean(magnets$change[1:29])
mean(magnets$change[30:50])


# Compute the sample standard deviation of the variable "change" for the
#patients that received and active magnet and the sample standard deviation for those that received an inactive placebo.
sd(magnets$change[1:29])
sd(magnets$change[30:50])

#Produce a boxplot of the variable "change" for the patients that received
#and active magnet and for patients that received an inactive placebo.
#What is the number of outliers in each subsequence?
x<- boxplot(magnets$change[1:29],magnets$change[30:50])

table(magnets$change[30:50])





adult <- rnorm(10000,70,6)
kids <- rnorm(1000,60,6)


mean(sample(height,100))
hist(x)


df <- data.frame(height = c(adult,kids))

ggplot(df, aes(x = height)) + geom_histogram()






