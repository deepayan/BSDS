
## Confidence intervals via lm()

library(NHANES)

## Subset / filter

nhsub <- subset(NHANES, Age >= 21)
nhsub <- nhsub[c("Race1", "Age", "Gender", "Height", "Weight")]

fm <- lm(Weight ~ Age, data = nhsub, 
         subset = (Gender == "female" & Race1 == "White"))

nh_white <- subset(nhsub, Race1 == "White")
nh_white <- within(nh_white, Gender01 <- as.numeric(Gender == "male"))

fm_gender <- lm(Weight ~ Gender01, data = nh_white)

predict(fm_gender, newdata = data.frame(Gender01 = c(0, 1)),
        interval = "prediction")

predict(fm_gender, newdata = data.frame(Gender01 = c(0, 1)),
        interval = "confidence")

ci_mean <- function(x, level = 0.95) {
  t.test(x, level = level)$conf.int
}

with(nh_white, tapply(Weight, Gender01, ci_mean))


## separate means for each (gender, race) combination
fm_gender_race <- lm(Weight ~ 0 + Gender:Race1, data = nhsub)
coef(fm_gender_race)

## compare with
with(nhsub, tapply(Weight, list(Gender, Race1), mean, na.rm = TRUE))

X <- model.matrix(Weight ~ 0 + Gender:Race1, data = nhsub)
params = coef(fm_gender_race)
(X %*% params)[1:20, , drop = FALSE]

newdf <- unique(nhsub[c("Gender", "Race1")])


newdf <- cbind(newdf, 
               predict(fm_gender_race, newdata = newdf, 
                       interval = "confidence"))
library(ggplot2)

ggplot(newdf, 
       mapping = aes(y = reorder(Race1, lwr + upr), 
                     color = Gender)) + 
  geom_errorbar(aes(xmin = lwr, xmax = upr))




