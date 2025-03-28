
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

fm3 <- lm(len ~ dose, data = ToothGrowth, 
          subset = (supp == "VC"))
summary(fm3)

dd3 <- data.frame(dose = c(0, 0.5, 1, 2), supp = "VC")

dd3 <- 
  cbind(dd3, 
        predict(fm3, 
                newdata = dd3,
                interval = "confidence"))

library(ggplot2)

ebars <- 
  ggplot(dd3) + geom_point(aes(x = dose, fit)) + 
    geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), width = 0.2)

ebars

## overlay on top of plot of data

ebars + 
  geom_point(aes(x = dose, y = len), 
             data = subset(ToothGrowth, supp == "VC"),
             col = "blue")




