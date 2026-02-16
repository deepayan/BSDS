

## n randomly chosen individuals
## X = number of distinct birthdays

## What is the distribution of X?

sample(1:10, 15)
sample(1:10, 15, replace = TRUE)
sample(1:10, 8, replace = TRUE)

n <- 30
s <- sample(1:365, size = n, replace = TRUE)
X <- length(unique(s))

## Let A be an event, and we want to calculate P(A).
## If we simulate the experiment independently a 
## large number of times, then P(A) can be approximated 
## by the proportion of experiments in which A happens.
## (This is the Law of Large Numbers)

do_simulation <- function(n) {
  s <- sample(1:365, size = n, replace = TRUE)
  X <- length(unique(s))
  X
}

## option 1: for loop

X = c()
for (i in 1:10) X[i] <- do_simulation(30)
X

## option 2: replicate

NREP <- 100000 # number of replications
X <- replicate(NREP, do_simulation(30))

plot(X)
table(X)
## plot(table(X))
barplot(table(X) / NREP)

## A = { X = 4 }, P(A) = P(X = 4)

X <- replicate(NREP, do_simulation(22))
barplot(table(X) / NREP); abline(h = 0.5)

X <- replicate(NREP, do_simulation(30))
barplot(table(X) / NREP)

X <- replicate(NREP, do_simulation(50))
barplot(table(X) / NREP)

X <- replicate(NREP, do_simulation(100))
barplot(table(X) / NREP)

mean(X)

X <- replicate(NREP, do_simulation(500))
barplot(table(X) / NREP)

## Do similarly for X ~ Poisson(50)

## In this case, we know true probabilities, so we can compare

dpois(25:75, lambda = 50) |> barplot()

X <- rpois(NREP, lambda = 20)
plot(table(X) / NREP)

true.p <- dpois(0:40, lambda = 20)
lines(0:40, true.p, col = 2)


X <- rpois(NREP, lambda = 5)
plot(table(X) / NREP)

true.p <- dpois(0:20, lambda = 5)
lines(0:20, true.p, col = 2)
