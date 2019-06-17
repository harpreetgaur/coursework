library(tidyverse)


table <- table2 %>% 
  spread(type, count) 
  

table %>% 
  group_by(country) %>%
  count(year, wt=cases) %>% View

table %>%
  group_by(country) %>% 
  count(year, wt=cases, population) 
  

table %>% 
  mutate(rate = cases/population*10000)

table4c <- inner_join(table4a, table4b, by = c("country" = "country")) %>%
  mutate(rate1999 = `1999.x`/`1999.y`*10000, rate2000 = `2000.x`/`2000.y`/10000)

View(table4c)

 