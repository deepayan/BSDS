

choose1 <- function(u, a, b) {
    if (u < 0.5) a else b
}

choose1(0, sqrt(2), sqrt(-2))
choose1(1, sqrt(2), sqrt(-2))


sqrt(-2) # produces a warning every time it is evaluated



## Quoting expressions

a < b

e1 <- quote(a < b)
e2 <- expression(a < b)

class(e1)
length(e1)
str(e1)

class(e2)
length(e2)
str(e2)

## compare

str(e1)
str(e2[[1]])

## The eval() function

e <- quote(sqrt(x))
eval(e)
x <- 10
eval(e)


eval(e1)
d <- list(a = rnorm(10), b = rexp(10))
eval(e1, envir = d)


## lazy evaluation + substitute ==> quote without evaluation

getExpression <- function(x) {
    substitute(x)
}
getExpression(sqrt(10))
getExpression(a + b)

withCall <- function(e)
{
    ans <- eval(e)
    attr(ans, "call") <- substitute(e)
    ans
}
withCall(sqrt(10))
withCall(rnorm(5) < rexp(5))

## examples made possible due to this feature

x <- seq(-10, 10, length.out = 201)
plot(x = x, y = x * sin(x), type = "l")
curve(x * sin(x), from = -15, to = 15)


    
