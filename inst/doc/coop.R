## ---- echo=FALSE---------------------------------------------------------
library(memuse)
matmemsize <- function(n) capture.output(memuse::mu(8*2*(n+1)*n))

## ------------------------------------------------------------------------
cov2 <- function(x)
{
  1/(NROW(x)-1) * crossprod(scale(x, TRUE, FALSE))
}

## ---- cache=TRUE---------------------------------------------------------
m <- 500
n <- 100
x <- matrix(rnorm(m*n), m, n)

rbenchmark::benchmark(cov(x), cov2(x), columns=c("test", "replications", "elapsed", "relative"))

## ------------------------------------------------------------------------
cor2 <- function(x)
{
  1/(NROW(x)-1) * crossprod(scale(x, TRUE, TRUE))
}

## ------------------------------------------------------------------------
cosine <- function(x, y)
{
  cp <- t(x) %*% y
  normx <- sqrt(t(x) %*% x)
  normy <- sqrt(t(y) %*% y)
  cp / normx / normy
}

## ------------------------------------------------------------------------
cosine <- function(x)
{
  cp <- crossprod(x)
  rtdg <- sqrt(diag(cp))
  cos <- cp / tcrossprod(rtdg)
  return(cos)
}

## ---- cache=TRUE---------------------------------------------------------
n <- 500
x <- matrix(rnorm(n*n), n, n)

mb <- microbenchmark::microbenchmark(cp1 <- t(x) %*% x, cp2 <- crossprod(x), times=20)
boxplot(mb)

## ------------------------------------------------------------------------
all.equal(cp1, cp2)

