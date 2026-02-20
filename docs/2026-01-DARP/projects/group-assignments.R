
ALL <-
    c(BG = "BSDBG2501 BSDBG2502 BSDBG2503 BSDBG2504 BSDBG2505 BSDBG2506 BSDBG2507 BSDBG2508 BSDBG2509 BSDBG2510 BSDBG2511 BSDBG2512 BSDBG2513 BSDBG2514 BSDBG2515 BSDBG2516 BSDBG2517 BSDBG2519 BSDBG2520 BSDBG2522 BSDBG2523 BSDBG2524 BSDDH2513 BSDDH2425",
      DH = "BSDDH2501 BSDDH2502 BSDDH2503 BSDDH2504 BSDDH2505 BSDDH2506 BSDDH2507 BSDDH2508 BSDDH2509 BSDDH2510 BSDDH2511 BSDDH2512 BSDDH2515 BSDDH2516 BSDDH2517 BSDDH2518 BSDDH2519 BSDDH2520 BSDDH2521 BSDDH2522 BSDDH2523 BSDBG2521 BSDDH2411 BSDDH2421",
      CC = "BSDCC2501 BSDCC2502 BSDCC2503 BSDCC2504 BSDCC2505 BSDCC2506 BSDCC2507 BSDCC2508 BSDCC2509 BSDCC2510 BSDCC2511 BSDCC2512 BSDCC2513 BSDCC2514 BSDCC2515 BSDCC2516 BSDCC2517 BSDCC2518 BSDCC2519 BSDCC2520 BSDCC2521 BSDCC2522 BSDCC2523") |> strsplit(split = " ")

sapply(ALL, length)

(with(ALL, cbind(BG, DH, c(CC, NA))) |> t() |> as.vector())

## randomize the order within location

set.seed(20260220)

(ROLLNUM <- with(ALL, cbind(sample(BG), sample(DH), c(sample(CC), NA))) |> t() |> as.vector())

length(ROLLNUM)

## Now easy to make 12 groups of 6 each (with one group actually of 5)

(II <- diag(12))

(G1 <- II[rep(1:12, each = 6), ])



## This is a reasonable assignment for everyone's first group. In
## principle, we can repeat exactly the same group for the second set
## of groups:

G <- cbind(G1, G1)

rowSums(G) # each student (row) is in two groups
colSums(G) # each group (column) has 6 students

## But this is not "good" because groups are heavily "correlated" -
## group 1 and group 13 are exactly the same.

## This can quantified using pairwise sum-of-products of G:

t(G) %*% G

image(crossprod(G))

lattice::levelplot(crossprod(G), main = "Number of common students in group i and group j")

## Ideally we would want the off-diagonal entries to be as small as possible.

## This is basically an optimization problem. We can try to do this
## systematically, but instead we will do a random search.

permute_rows <- function(X) {
    i <- sample(seq(1, nrow(X)))
    X[i, ]
}

t(G1) %*% G1 # not desirable

t(G1) %*% permute_rows(G1)
lattice::levelplot(t(G1) %*% permute_rows(G1))


max(t(G1) %*% permute_rows(G1)) # Want this to be as small as possible


replicate(100, max(t(G1) %*% permute_rows(G1)))

replicate(10000, max(t(G1) %*% permute_rows(G1))) |> table()

## So max = 1 seems difficult but max = 2 is easy. What else could we
## want to optimize?

count_ones <- function(X) {
    if (max(X) == 2) sum(X == 1) else 0
}

replicate(10000, count_ones(t(G1) %*% permute_rows(G1))) |> table()

## search by simulating and keep track of best

set.seed(20260220)

best_G2 <- G1
best_count <- 0

for (i in 1:50000) {
    G2 <- permute_rows(G1)
    X <- t(G1) %*% G2
    if (max(X) < 3) {
        m <- count_ones(X)
        if (m > best_count) {
            best_count <- m
            best_G2 <- G2
        }
    }
}

lattice::levelplot(t(G1) %*% best_G2)

## make sure all conditions are satisfied

G <- cbind(G1, best_G2)

rowSums(G) # each student (row) is in two groups
colSums(G) # each group (column) has 6 students
lattice::levelplot(crossprod(G), main = "Number of common students in group i and group j")


## Final groups (each column of G is a group)

for (j in seq(1, ncol(G))) {
    cat("- **Group ", j, "** : ", sep = "")
    cat(ROLLNUM[G[, j] == 1], sep = ", ")
    cat("\n\n")
}



