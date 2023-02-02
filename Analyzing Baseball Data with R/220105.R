library(dplyr)
library(tidyverse)
people <-read_csv('baseballdatabank-master/core/people.csv')
people
people %>% slice(1:3) %>% select(1:4)
people %>% slice(1:3) %>% select(birthDay, birthCity)
people %>%
  summarize(LO = min(birthDay),
            QL = quantile(birthDay, .25),
            QU = quantile(birthDay, .75),
            M = median(birthDay), HI = max(birthDay))
