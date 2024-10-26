---
layout: default
title: 'Income Quiz - solution'
mathjax: true
---




The following functions provide two implementations that simulate the
random outcome for one student investing in scheme B. The first one
uses a for loop, while the second one uses vectorized computation.


```r
schemeBloop <- function(steps = 10, initial = 100) {
    money <- initial
    for (year in 1:steps) {
        multiplier = sample(c(2, 1/2), size = 1, prob = c(0.5, 0.5))
        money = money * multiplier
    }
    return(money)
}
schemeBvec <- function(steps = 10, initial = 100)
{
    multipliers <- sample(c(2, 1/2), steps, replace = TRUE)
    initial * prod(multipliers)
}
```

Both of these approaches are fine for this problem. Vectorized
approaches are usually faster in R, as can be seen below. 


```r
system.time(sim10000 <- replicate(10000, schemeBloop()))
```

```
   user  system elapsed 
  0.540   0.004   0.544 
```

```r
system.time(sim10000 <- replicate(10000, schemeBvec()))
```

```
   user  system elapsed 
  0.081   0.000   0.082 
```

However, the loop approach is more general. For example, suppose we
have a hybrid approach where a student chooses scheme A if their
current investment is less than 200, but scheme B otherwise. Such a
scheme can be easily incorporated into the loop approach, but not so
easily in the vectorized approach.


We can now use these simulation results to obtain answers to our
questions.


```r
## 1 (b) - average amount per student
mean(sim10000)
```

```
[1] 900.6568
```

```r
## 2 (b) - P(amount >= 100)
mean(sim10000 >= 100)
```

```
[1] 0.6288
```

```r
## 3 (b) - P(amount >= 931.32)
mean(sim10000 >= 931.32)
```

```
[1] 0.1709
```

For the last question, we need to find the total of the top 10% of the
amounts, which we will interpret as the top 1000 amounts in our
simulation (ignoring possible ties at the corresponding value).


```r
sim10000 <- sort(sim10000, decreasing = TRUE)
total <- sum(sim10000)
top10 <- sum(head(sim10000, 1000))
round(100 * top10 / total)
```

```
[1] 75
```

Becuase this is such a simple scheme, it is also possible to compute
answers to these questions without doing any simulation. Note that the
amount of money after 10 years for a particular student is

$$
M = 100 \cdot 2^X \cdot (1/2)^{10-X} = 2^{2X} \cdot 100 \cdot 2^{-10}
$$

where $X ~\sim \text{Bin}(10, \frac12)$. In other words,

$$
\log_2 M = 2X - 10 +  \log_2 100
$$

This means that we can calculate the exact probabilities asked in
questions 2 and 3 in terms of Binomial probabilities using the
`pbinom()` function, which gives the cumulative distribution function
of the Binomial distribution.

$$
P(M \geq a) = P(\log_2 M \geq \log_2 a) 
= P\left( X \geq \frac12 (\log_2 a + 10 - \log_2 100)  \right)
$$


```r
## 2 (b) - P(amount >= 100)
1 - pbinom(0.5 * (log2(100) + 10 - log2(100)) - 0.5,
           size = 10, prob = 0.5)
```

```
[1] 0.6230469
```

```r
## 3 (b) - P(amount >= 931.32)
1 - pbinom(0.5 * (log2(931.32) + 10 - log2(100)) - 0.5,
           size = 10, prob = 0.5)
```

```
[1] 0.171875
```

Read the help page for `pbinom()` to understand why the offset of
`-0.5` is required, and what happens if it is not included.




