
library(NHANES)
library(lattice)

nh_adults <- subset(NHANES, Age >= 20)

bwplot(Race1 ~ Height | Gender, data = nh_adults)

xyplot(Weight ~ Height | Gender, data = nh_adults)

## The tapply() function

mean(nh_adults$Height, na.rm = TRUE)

with(nh_adults,
     tapply(Height, list(Race1, Gender), 
            FUN = function(x) mean(x, na.rm = TRUE)))

msummary <- 
   with(nh_adults,
        tapply(Height, list(Race = Race1, Gender = Gender), 
               FUN = mean, na.rm = TRUE))

dfsummary <- array2DF(msummary)
dfsummary

dotplot(Race ~ Value, data = dfsummary, 
        xlab = "Mean Height",
        groups = Gender, auto.key = TRUE)


## Summarize by 25%-trimmed mean

msummary <- 
  with(nh_adults,
       tapply(Height, list(Race = Race1, Gender = Gender), 
              FUN = mean, na.rm = TRUE, trim = 0.25))

dfsummary <- array2DF(msummary)
dfsummary

dotplot(Race ~ Value, data = dfsummary, 
        xlab = "Mean Height",
        groups = Gender, auto.key = TRUE)


## Do something similar for a continuous variable: 
## Given two variables X and Y, we want to find 
## a function f(x) which represents the average value 
## of Y when X = x.

## Let's start with Age

mage <- 
  with(NHANES,
       tapply(Height, list(Age = Age, Gender = Gender), 
              FUN = mean, na.rm = TRUE, trim = 0.25))

dfage <- array2DF(mage)
dfage$Age <- as.numeric(dfage$Age)


xyplot(Value ~ Age | Gender, 
       data = dfage, grid = TRUE,
       subset = Age >= 20)

xyplot(Height ~ Age | Gender, 
       data = NHANES, grid = TRUE,
       subset = Age >= 20)



mage.race <- 
  with(NHANES,
       tapply(Height, list(Age = Age, Gender = Gender, Race = Race1), 
              FUN = mean, na.rm = TRUE))

dfage.race <- array2DF(mage.race)
dfage.race$Age <- as.numeric(dfage.race$Age)

xyplot(Value ~ Age | Race, 
       data = dfage.race, grid = TRUE, groups = Gender,
       subset = Age >= 20)

## Repeat this with Weight

wtage.race <- 
  with(NHANES,
       tapply(Weight, list(Age = Age, Gender = Gender, Race = Race1), 
              FUN = mean, na.rm = TRUE))

dfage.race <- array2DF(wtage.race, responseName = "Mean.Wt")
dfage.race$Age <- as.numeric(dfage.race$Age)

xyplot(Mean.Wt ~ Age | Race, 
       data = dfage.race, grid = TRUE, groups = Gender,
       subset = Age >= 20)


## Repeat this with BPSysAve (average systolic blood pressure)

bpage.race <- 
  with(NHANES,
       tapply(BPSysAve, list(Age = Age, Gender = Gender, Race = Race1), 
              FUN = mean, na.rm = TRUE))

dfage.race <- array2DF(bpage.race, responseName = "Mean.SysBP")
dfage.race$Age <- as.numeric(dfage.race$Age)

xyplot(Mean.SysBP ~ Age | Race, 
       data = dfage.race, grid = TRUE, groups = Gender,
       subset = Age >= 20, auto.key = TRUE)

xyplot(BPSysAve ~ Age | Race1, 
       data = NHANES, grid = TRUE, groups = Gender,
       subset = Age >= 20, auto.key = TRUE)

xyplot(BPSysAve ~ Age | Gender, 
       data = NHANES, grid = TRUE, 
       subset = Race1 == "White" & Age >= 20)


## Regression examples (Nov 5, 2024)

library(carData)

xyplot(weight ~ height, data = Davis, grid = TRUE, 
       groups = sex, auto.key = TRUE)

xyplot(pctUrban ~ ppgdp, data = UN, grid = TRUE)
xyplot(pctUrban ~ log(ppgdp), data = UN, grid = TRUE)


## function to calculate a-hat and b-hat

s2 <- function(x) {
  m <- mean(x)
  mean((x - m)^2)
}

cxy <- function(x, y) {
  mx <- mean(x)
  my <- mean(y)
  mean((x - mx) * (y - my))
}

lsfit <- function(x, y) {
  if (length(x) != length(y)) stop("Lengths and x and y do not match")
  skip <- is.na(x) | is.na(y)
  x <- x[!skip]
  y <- y[!skip]
  mx <- mean(x)
  my <- mean(y)
  s2x <- s2(x)
  covxy <- cxy(x, y)
  b_hat <- covxy / s2x
  a_hat <- my - b_hat * mx
  c(a = a_hat, b = b_hat)
}



abDavis <- with(Davis, lsfit(x = height, y = weight))

abDavis

## Model weight = a * 1 + b * height + error
coef(lm(weight ~ 1 + height, data = Davis))

xyplot(weight ~ height, data = Davis, grid = TRUE, 
       groups = sex, auto.key = TRUE,
       abline = abDavis)

xyplot(weight ~ height, data = Davis, grid = TRUE, 
       groups = sex, auto.key = TRUE, 
       type = c("p", "r"),
       abline = abDavis)



abUN <- with(UN, lsfit(x = log(ppgdp), y = pctUrban))

xyplot(pctUrban ~ log(ppgdp), data = UN, grid = TRUE,
       abline = abUN)


nhSubM <- subset(NHANES, Race1 == "White" & Age >= 20 & Gender == "male")
nhSubF <- subset(NHANES, Race1 == "White" & Age >= 20 & Gender == "female")

abNH.M <- with(nhSubM, lsfit(x = Age, y = BPSysAve))
xyplot(BPSysAve ~ Age, data = nhSubM, grid = TRUE, abline = abNH.M)

abNH.F <- with(nhSubF, lsfit(x = Age, y = BPSysAve))
xyplot(BPSysAve ~ Age, data = nhSubF, grid = TRUE, abline = abNH.F)


## How can we do Least Absolute Error

ladfit <- function(x, y) {
  if (length(x) != length(y)) stop("Lengths and x and y do not match")
  skip <- is.na(x) | is.na(y)
  x <- x[!skip]
  y <- y[!skip]
  lambda <- function(par) {
    a <- par[1]
    b <- par[2]
    sum(abs(y - a - b * x))
  }
  opt <- optim(par = c(0, 0), fn = lambda)
  opt$par
}

abDavisLAD <- with(Davis, ladfit(x = height, y = weight))

xyplot(weight ~ height, data = Davis, grid = TRUE, 
       groups = sex, auto.key = TRUE, 
       type = c("p", "r"),
       abline = abDavisLAD)


abUNLAD <- with(UN, ladfit(x = log(ppgdp), y = pctUrban))

xyplot(pctUrban ~ log(ppgdp), data = UN, grid = TRUE,
       abline = abUNLAD, type = c("p", "r"))

xyplot(pctUrban ~ log(ppgdp), data = UN, grid = TRUE,
       abline = abUNLAD, type = c("p", "smooth"), col.line = "red")


abNH.MLAD <- with(nhSubM, ladfit(x = Age, y = BPSysAve))
xyplot(BPSysAve ~ Age, data = nhSubM, grid = TRUE, 
       type = c("p", "r"), abline = abNH.MLAD)

abNH.FLAD <- with(nhSubF, ladfit(x = Age, y = BPSysAve))
xyplot(BPSysAve ~ Age, data = nhSubF, grid = TRUE, 
       type = c("p", "r"), abline = abNH.FLAD)

xyplot(BPSysAve ~ Age, data = nhSubF, grid = TRUE, 
       type = c("p", "smooth"), abline = abNH.FLAD, col.line = "red")


## Is R^2 == r^2

R2 <- function(x, y) {
  if (length(x) != length(y)) stop("Lengths and x and y do not match")
  skip <- is.na(x) | is.na(y)
  x <- x[!skip]
  y <- y[!skip]
  ab <- lsfit(x, y)
  r <- cor(x, y)
  T2 <- sum((y - mean(y))^2)
  S2 <- sum((y - ab[1] - ab[2] * x)^2)
  R2 <- 1 - S2 / T2
  c(r2 = r^2, R2 = R2)
}

R2(runif(20), runif(20))
R2(runif(20), rnorm(20))



