#importing the csv
pop.1 <- read.csv("pop1.csv")

#Creating the sample
X.samp <- sample(pop.1$height,100)
X.samp

#mean of sample
mean(X.samp)

#truth value (mean)
mean(pop.1$height)

X.samp <- sample(pop.1$height,100)
X.bar <- mean(X.samp)
X.bar

#Creating a function rep that creates sequence
X.bar <- rep(0,10^5)
for(i in 1:10^5)
{
  X.samp <- sample(pop.1$height, 100)
  X.bar[i] <- mean(X.samp)
  
 
}

#creating histogram of the samples
hist(X.bar)


X.bar <- rep(0.10^5)
for(i in 1:10^5){
  X.samp <-rbinom(64,10,0.5)
  X.bar[i] <- mean(X.samp)
  
}

mean(X.bar)

sd(X.bar)

quantile(X.bar, c(0.25,0.975))

qnorm(c(0.025, 0.975), mean(X.bar), sd(X.bar))

#simulation of 3 samples
unif.10 <- rep(0.10^5)
unif.100 <- rep(0,10^5)
unif.1000 <- rep(0,10^5)

for(i in 1:10^5){
  
  X.samp.10 <- runif(10,3,7)
  unif.10[i] <- mean (X.samp,10)
  X.samp.100 <- runif(100,3,7)
  unif.100[i] <- mean (X.samp,100)
  X.samp.1000 <- runif(1000,3,7)
  unif.1000[i] <- mean (X.samp,1000)
 
  
}

#mean of the samples
mean(unif.10)
mean(unif.100)
mean(unif.1000)

#variance of the samples
var(unif.10)
var(unif.100)
var(unif.1000)



#Exercise 7.1
#1
pop.2 <- read.csv(file="pop2.csv")
mean(pop.2$bmi)

#2
sd(pop.2$bmi)

#3
X.bar <- rep(0,10^5)
for( i in 1:10^5)
{
 X.samp <- sample(pop.2$bmi,150)
 X.bar[i] <- mean(X.samp)
  
}

mean(X.bar)


#4
sd(X.bar)

#5
quantile(X.bar, c(0.1,0.9))

#6
qnorm(c(0.1,0.9), mean(X.bar))





