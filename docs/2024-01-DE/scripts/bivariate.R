
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






