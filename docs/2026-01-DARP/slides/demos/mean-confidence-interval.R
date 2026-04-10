
## confidence intervals

t_confint <- function(x, alpha = 0.2) {
  x <- na.omit(x)
  n <- length(x)
  xbar <- mean(x)
  s <- sd(x)
  ## how to calculate c from alpha?
  c <- -qt(alpha / 2, df = n-1)
  lower <- xbar - c * s / sqrt(n)
  upper <- xbar + c * s / sqrt(n)
  data.frame(n = n, estimate = xbar, 
             lower = lower, upper = upper)
}

library(NHANES)
library(dplyr)
library(ggplot2)

ht_summary <- 
  NHANES |>
  filter(Age >= 20) |>
  select(Height, Weight, Gender, Race1) |> 
  group_by(Gender, Race1) |>
  summarise(t_confint(Height, alpha = 0.01))

ht_summary

p <- 
  ggplot(ht_summary, 
         mapping = aes(x = estimate, 
                       y = reorder(Race1, estimate), 
                       color = Gender))

p + geom_point(aes()) + 
  geom_errorbar(aes(xmin = lower, xmax = upper))

wt_summary <- 
  NHANES |>
  filter(Age >= 20) |>
  select(Height, Weight, Gender, Race1) |> 
  group_by(Gender, Race1) |>
  summarise(t_confint(Weight, alpha = 0.05))

wt_summary

p <- 
  ggplot(wt_summary, 
         mapping = aes(x = estimate, 
                       y = reorder(Race1, estimate), 
                       color = Gender))

p + geom_point(aes()) + 
  geom_errorbar(aes(xmin = lower, xmax = upper))


## Link with t.test

xx <- rnorm(50, mean = 25, sd = 3)
plot(xx)

t_confint(xx, alpha = 0.2)
t_confint(xx, alpha = 0.05)

## compare with
t.test(xx, conf.level = 0.8)$conf.int
t.test(xx, conf.level = 0.95)$conf.int


