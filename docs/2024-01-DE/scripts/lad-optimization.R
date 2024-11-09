
## optimization for LAD regression

lad_loss_function <- function(x, y) {
  if (length(x) != length(y)) stop("Lengths and x and y do not match")
  skip <- is.na(x) | is.na(y)
  x <- x[!skip]
  y <- y[!skip]
  lambda <- function(a, b) {
    if (length(a) != 1) stop("a must be scalar")
    if (length(b) != 1) stop("b must be scalar")
    e <- abs(y - a - b * x)
    sum(e)
  }
  lambda
}


DavisLoss <- with(Davis, lad_loss_function(x = height, y = weight))

DavisLoss
DavisLoss(2, 4)

## scope: x and y are found (in scope) because x and y are available 
## in the function that defined this function. This kind of scoping 
## is known as "lexical scoping".

## Question: How to find (a, b) which minimizes DavisLoss(a, b)?

## fix a=0, and vary b

bseq <- seq(0, 10, length.out = 21)

## loops (for vs sapply)

f <- numeric(length(bseq))
for (i in seq_along(f)) {
  f[i] <- DavisLoss(a = 0, b = bseq[i])
} 

## alternative using sapply() / vapply()


sapply(bseq, function(b) DavisLoss(a = 0, b = b))

sapply(bseq, DavisLoss, a = 0)

## safer version of sapply where expected result type is specified
vapply(bseq, DavisLoss, a = 0, FUN.VALUE = numeric(1))


bseq <- seq(0, 2, length.out = 201)
f <- vapply(bseq, DavisLoss, a = 0, FUN.VALUE = numeric(1))

plot(f ~ bseq, type = "l")

g <- 
  expand.grid(a = seq(-500, 500, length.out = 200),
              b = seq(-3, 3, length.out = 200))

g$loss <- 0

for (i in seq_along(g$loss)) {
  g$loss[i] <- DavisLoss(g$a[i], g$b[i])
}

g[which.min(g$loss), ]

## contour plots / image plots or level plots

library(lattice)

contourplot(loss ~ a + b, data = g)

levelplot(loss ~ a + b, data = g)

wireframe(loss ~ a + b, data = g, shade = TRUE)

plot(weight ~ height, Davis)
abline(abDavisLAD)
abline(-113.0653, 1.040201, col = "red")




