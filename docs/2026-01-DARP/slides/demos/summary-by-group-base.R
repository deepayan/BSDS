

## data preparation and summarization

library(NHANES)

## Task 1: Subset / filter

nhsub <- subset(NHANES, Age >= 20)
nhsub <- nhsub[c("Race1", "Age", "Gender", "Height", "Weight")]

## The above is equivalent to:

nhsub <- subset(NHANES, 
                subset = Age >= 21, 
                select = c("Race1", "Age", "Gender", "Height", "Weight"))


## split-apply-combine / map-reduce

## split() and tapply()

s <- with(nhsub, split(Weight, list(Gender, Race1)))
str(s)

lapply(s, mean)

vapply(s, function(x) mean(x, na.rm = TRUE), FUN.VALUE = numeric(1))
vapply(s, fivenum, FUN.VALUE = numeric(5))

vapply(s, mean, na.rm = TRUE, FUN.VALUE = numeric(1))

lapply(s, fivenum)


## split data frame

dlist <- split(nhsub, list(nhsub$Gender, nhsub$Race1))
str(dlist)

dlist <- split(nhsub, ~ Gender + Race1)
str(dlist)

## cor(Weight, Height) within each subgroup

vapply(dlist, function(d) cor(d$Weight, d$Height, use = "pairwise.complete"), FUN.VALUE = numeric(1)) |> sort() |> dotchart()

vapply(dlist, 
       function(d) cor(d$Weight, d$Height, use = "pairwise.complete"), 
       FUN.VALUE = numeric(1))

vapply(dlist, 
       function(d) with(d, cor(Weight, Height, use = "pairwise.complete")), 
       FUN.VALUE = numeric(1))

vapply(dlist, 
       with, cor(Weight, Height, use = "pairwise.complete"), 
       FUN.VALUE = numeric(1))

vapply(dlist, 
       with, mean(Weight, na.rm = TRUE), 
       FUN.VALUE = numeric(1))

## tapply()

with(nhsub, tapply(Weight, list(Gender = Gender, Race = Race1), mean, na.rm = TRUE)) |> array2DF()

## working with data frame

tapply(nhsub, ~ Gender + Race1, function(d) mean(d$Weight, na.rm = TRUE))

tapply(nhsub, ~ Gender + Race1, function(d) with(d, cor(Height, Weight, use = "pairwise")))

tapply(nhsub, ~ Gender + Race1, with, cor(Height, Weight, use = "pairwise"))


ci_mean <- function(x, level = 0.95) {
  t.test(x, level = level)$conf.int
}

ci_by_group <- tapply(nhsub, ~ Gender + Race1, with, ci_mean(Height))
array2DF(ci_by_group)

ci_mean_df <- function(x, level = 0.95) {
  ci <- t.test(x, level = level)$conf.int
  data.frame(mean_lower = ci[[1]], mean_upper = ci[[2]])
}

ci_by_group_df <- 
  tapply(nhsub, ~ Gender + Race1, 
         with, ci_mean_df(Height))

ci_df <- array2DF(ci_by_group_df)


ci_median_df <- function(x, level = 0.95, alpha = 1 - level) {
  x <- na.omit(x)
  n <- length(x)
  k <- floor(0.5 * n + 0.5 * (qnorm(alpha / 2)) * sqrt(n))
  s <- sort(x)
  data.frame(med_lower = s[k], med_upper = s[n-k+1])
}

ci_by_group_df <- 
  tapply(nhsub, ~ Gender + Race1, 
         with, cbind(ci_mean_df(Height), 
                     ci_median_df(Height)))

ci_df <- array2DF(ci_by_group_df)


library(ggplot2)

p1 <- 
  ggplot(ci_df, 
         mapping = aes(y = reorder(Race1, mean_lower + mean_upper), 
                       color = Gender)) + 
  geom_errorbar(aes(xmin = mean_lower, xmax = mean_upper))


p2 <- 
  ggplot(ci_df, 
         mapping = aes(y = reorder(Race1, mean_lower + mean_upper), 
                       color = Gender)) + 
  geom_errorbar(aes(xmin = med_lower, xmax = med_upper))


p1
p2

p1 + geom_errorbar(aes(xmin = med_lower, xmax = med_upper))

## 'tidy approach': one row for each interval

confint_df <- function(x, type = c("mean", "median"), level = 0.95)
{
  type <- match.arg(type)
  d <- 
    switch(type, 
           mean = ci_mean_df(x),
           median = ci_median_df(x))
  colnames(d) <- c("lower", "upper")
  cbind(type = type, d)
}


ci_by_group_df_long <- 
  tapply(nhsub, ~ Gender + Race1, 
         with, 
         rbind(confint_df(Height, type = "mean"),
               confint_df(Height, type = "median")))



ci_df_long <- array2DF(ci_by_group_df_long)

ggplot(ci_df_long, 
       mapping = aes(y = reorder(Race1, lower + upper), 
                     color = type)) + 
  facet_wrap(~ Gender, ncol = 1) + 
  geom_errorbar(aes(xmin = lower, xmax = upper))


