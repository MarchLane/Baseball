library(tidyverse)
library(Lahman)

S1960s = Teams %>%
  filter(yearID >= 1961) %>%
  filter(yearID <= 1970) %>%
  select(teamID, yearID, lgID, G, W, L, R, RA)
S1960s

S1960s = S1960s %>%
  mutate(RD = R - RA, Wpct = W / (W + L))

linfit = lm(Wpct ~ RD, data = S1960s)
linfit

library(broom)
S1960s_aug = augment(linfit, data = S1960s)

S1960s

S1960s = S1960s %>%
  mutate(Wpct_pyt = R ^ 2 / (R ^ 2 + RA ^ 2))

S1960s = S1960s %>%
  mutate(residuals_pyt = Wpct - Wpct_pyt)

S1960s
