

str(ToothGrowth)

library(lattice)

xyplot(len ~ dose | supp, data = ToothGrowth)
xyplot(len ~ supp | factor(dose), data = ToothGrowth)
qq(supp ~ len | factor(dose), ToothGrowth)

len_1_VC <- subset(ToothGrowth, 
                   dose == 1 & supp == "VC")$len
tt <- t.test(len_1_VC)
tt$conf.int

## Alternative using lm(response ~ predictors)

fm1 <- lm(len_1_VC ~ 1)
fm1
summary(fm1)

fm2 <- lm(len ~ 1, data = ToothGrowth, 
          subset = (dose == 1 & supp == "VC"))
summary(fm2)
predict(fm2, newdata = data.frame(dose = 1, supp = "VC"))

predict(fm2, 
        newdata = data.frame(dose = 1, supp = "VC"),
        interval = "confidence")
## compare with
tt$conf.int 

predict(fm2, 
        newdata = data.frame(dose = 1, supp = "VC"),
        interval = "predict")

fm3 <- lm(len ~ 1 + dose, data = ToothGrowth, 
          subset = (supp == "VC"))
summary(fm3)

dd3 <- data.frame(dose = c(0.5, 1, 2), supp = "VC")

dd3 <- 
  cbind(dd3, 
        predict(fm3, 
                newdata = dd3,
                interval = "confidence"))

library(ggplot2)

ebars <- 
  ggplot(dd3) + geom_point(aes(x = dose, y = fit)) + 
    geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), width = 0.2)

ebars

## overlay on top of plot of data

ebars + 
  geom_point(aes(x = dose, y = len), 
             data = subset(ToothGrowth, supp == "VC"),
             col = "orange")

## separate mean for all combinations

fm4 <- lm(len ~ 0 + supp, data = ToothGrowth)

getCI <- function(y) {
  fm <- lm(y ~ 1)
  predict(fm, newdata = data.frame(z = 1), 
          interval = "confidence") |> as.data.frame()
}

res <- tapply(ToothGrowth, ~ supp + dose, function(d) getCI(d$len))
df_ci <- array2DF(res)
df_ci <- dplyr::mutate(df_ci, dose = as.numeric(dose))

ggplot(df_ci) + facet_wrap(~ supp) + 
  geom_point(aes(x = dose, y = fit)) + 
  geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), width = 0.2) + 
  geom_point(aes(x = dose, y = len), 
             data = ToothGrowth,
             col = "orange")

fm5 <- lm(len ~ 0 + interaction(dose, supp), data = ToothGrowth)
fm5$coefficients 

dd5 <- expand.grid(dose = c(0.5, 1, 2), supp = c("OJ", "VC"))

dd5 <- 
  cbind(dd5, 
        predict(fm5, 
                newdata = dd5,
                interval = "confidence"))

ggplot(dd5) + facet_wrap(~ supp) + 
  geom_point(aes(x = dose, y = fit)) + 
  geom_line(aes(x = dose, y = fit)) + 
  geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), width = 0.2) + 
  geom_point(aes(x = dose, y = len), 
             data = ToothGrowth,
             col = "orange")



## linear regression by subgroup

fm6 <- lm(len ~ 1 + dose * supp, data = ToothGrowth)
dd6 <- expand.grid(dose = c(0.5, 1, 2), supp = c("OJ", "VC"))

dd6 <- 
  cbind(dd6, 
        predict(fm6, 
                newdata = dd6,
                interval = "confidence"))

ggplot(dd6) + facet_wrap(~ supp) + 
  geom_point(aes(x = dose, y = fit)) + 
  geom_line(aes(x = dose, y = fit)) + 
  geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), width = 0.2) + 
  geom_point(aes(x = dose, y = len), 
             data = ToothGrowth,
             col = "orange")


## model with transoformed predictor

fm7 <- lm(len ~ 1 + log(dose) * supp, data = ToothGrowth)
dd7 <- expand.grid(dose = seq(0, 3, by = 0.1), supp = c("OJ", "VC"))

dd7 <- 
  cbind(dd7, 
        predict(fm7, 
                newdata = dd7,
                interval = "confidence"))

ggplot(dd7) + facet_wrap(~ supp) + 
  geom_point(aes(x = (dose), y = fit)) + 
  geom_line(aes(x = (dose), y = fit)) + 
  geom_errorbar(aes(x = (dose), ymin = lwr, ymax = upr), width = 0.2) + 
  geom_point(aes(x = (dose), y = len), 
             data = ToothGrowth,
             col = "orange")


## A similar plot using lattice

library(lattice)
library(latticeExtra)

segplot(dose ~ lwr + upr | supp, data = dd7, horizontal = FALSE, center = fit) + 
  xyplot(len ~ dose | supp, ToothGrowth, grid = TRUE, jitter.x = TRUE, pch = 16, col = "orange")





