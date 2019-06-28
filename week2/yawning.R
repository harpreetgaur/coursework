indexcards=c(rep('yawn', 14), rep('noyawn',36)) 


shufflefunc <- function(indexcards) {
  
  shuffled<- sample(indexcards)

  treatment <- mean(shuffled[1:34]=='yawn')

  control <- mean(shuffled[35:50]=='yawn')
  
  return(treatment-control)

}

p_diff <- replicate(10000,shufflefunc(indexcards))

difference <- (10/34)-(4/16)
difference

ggplot(data.frame(p_diff), aes(x=p_diff))+
  geom_histogram(binwidth = 0.01) + geom_vline(xintercept = difference, lty=2)




diff_df <- data.frame(p_diff)

diff_vals <- diff_df %>% filter(diff_df>=difference) %>%
        summarize(count=n())
      
total_vals <- diff_df %>% summarize(count=n())

p_value <- diff_vals/total_vals
p_value


power.prop.test(n=25,p1=10/34, p2=4/16, sig.level = 0.05, alternative="one.sided")



power.prop.test(p1=10/34, p2=4/16, power=0.8,sig.level = 0.05, alternative="one.sided")



power.prop.test(p1=0.4, p2=0.5, sig.level = 1/10, power=4/5, alternative="one.sided")


#power.prop.test(p1=0.4, p2=0.40001, power=4/5,sig.level = 1/10, alternative="two.sided")
