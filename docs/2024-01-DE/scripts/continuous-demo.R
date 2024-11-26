
## demos: continuous distributions

## density histograms / average shifted histogram

## Distribution of sample mean for uniform(0, 1)


library(NHANES)
library(lattice)

histogram(~ BPSysAve | Race1, data = NHANES, 
          subset = Gender == "male" & Age > 20)


histogram(~ BPSysAve | Race1, data = NHANES, 
          subset = Gender == "male" & Age > 20,
          type = "count")

histogram(~ BPSysAve | Race1, data = NHANES, 
          subset = Gender == "male" & Age > 20,
          type = "density")

nhsub <- subset(NHANES, Gender == "male" & Age > 20 & Race1 == "White")

hist(nhsub$BPSysAve)

h <- hist(nhsub$BPSysAve, plot = FALSE)
str(h)

xyplot(density ~ mids, data = h, type = "l")

densityplot(~ BPSysAve | Race1, data = NHANES, 
            subset = Gender == "male" & Age > 20,
            kernel = "rect", plot.points = FALSE)

## point-wise histogram

densityplot(~ BPSysAve | Race1, data = NHANES, 
            subset = Gender == "male" & Age > 20,
            kernel = "rect", plot.points = FALSE,
            bw = 10)

## average-shifted histogram

densityplot(~ BPSysAve | Race1, data = NHANES, 
            subset = Gender == "male" & Age > 20,
            kernel = "triangular", plot.points = FALSE,
            bw = 10)


library(latticeExtra)

hbp <- histogram(~ BPSysAve | Race1, data = NHANES, 
                 subset = Gender == "male" & Age > 20,
                 type = "density", nint = 20, border = NA)

dbp <- densityplot(~ BPSysAve | Race1, data = NHANES, 
                   subset = Gender == "male" & Age > 20,
                   kernel = "triangular", plot.points = FALSE,
                   bw = 5, col = "black")

hbp + dbp

## mean of U(0, 1)

u <- runif(10000, -1, 1)
ecdfplot(~ u)
qqmath(~ u)
qqmath(~ u, distribution = qunif)
histogram(~u, type = "density")
densityplot(~u, kernel = "tri", plot.points = FALSE)

x <- replicate(100000, mean(runif(2, min = -1, max = 1)))
ecdfplot(~ x)
qqmath(~ x)
qqmath(~ x, distribution = qunif)
histogram(~ x, type = "density")
densityplot(~ x, kernel = "tri", plot.points = FALSE)

x <- replicate(100000, mean(runif(5, min = -1, max = 1)))
ecdfplot(~ x)
qqmath(~ x, pch = ".", cex = 3, grid = TRUE)
histogram(~ x, type = "density")
densityplot(~ x, kernel = "tri", plot.points = FALSE)

x <- replicate(100000, sum(runif(25, min = -1, max = 1)))
ecdfplot(~ x)
qqmath(~ x, pch = ".", cex = 3, grid = TRUE)
histogram(~ x, type = "density")
densityplot(~ x, kernel = "tri", plot.points = FALSE)

x <- replicate(100000, median(runif(25, min = -1, max = 1)))
ecdfplot(~ x)
qqmath(~ x, pch = ".", cex = 3, grid = TRUE)
histogram(~ x, type = "density")
densityplot(~ x, kernel = "tri", plot.points = FALSE)

## Finite population

S <- seq(-1, 1, length.out = 100)

x <- replicate(100000, mean(sample(S, 25, replace = TRUE)))
ecdfplot(~ x)
qqmath(~ x, pch = ".", cex = 3, grid = TRUE)
histogram(~ x, type = "density")
densityplot(~ x, kernel = "tri", plot.points = FALSE)

## Finite population with non-uniform probablities

S <- seq(-1, 1, length.out = 100)
P <- seq(0, 1, length.out = 100)
P <- P / sum(P)

sample(S, 10000, prob = P, replace = TRUE) |> histogram(type = "density")

x <- replicate(100000, mean(sample(S, 25, prob = P, replace = TRUE)))
ecdfplot(~ x)
qqmath(~ x, pch = ".", cex = 3, grid = TRUE)
histogram(~ x, type = "density")
densityplot(~ x, kernel = "tri", plot.points = FALSE)

probs <- seq(0.0001, 1 - 0.0001, length = 501)
q <- quantile(x, probs, names = FALSE)

plot(probs, q)

plot(qnorm(probs), q)

## p.d.f. / p.m.f. --- dnorm() / dbinom()
## CDF F(x) = P(X <= x) --- pnorm() / pbinom()
## quantile function - inverse of F(x) 
## Q(p) = {q such that F(q) = p} --- qnorm() / qbinom()


y <- rnorm(10000, mean = 1, sd = 0.1)
densityplot(~ x, plot.points = FALSE)
densityplot(~ c(x, y), plot.points = FALSE)
















