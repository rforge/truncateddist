rtbeta <- function(n, alpha, beta, a=0, b=1)
{
  stopifnot(n > 0 & all(beta > 0) & all(alpha > 0))
  x <- runif(n)
  Fa <- pbeta(a, alpha, beta)
  Fb <- pbeta(b, alpha, beta)
  y <- (1-x)*Fa + x*Fb
  return(qbeta(y, alpha, beta))
}

dtbeta <- function(x, alpha, beta, a=0, b=1)
{
  stopifnot( all(alpha > 0) & all(beta > 0) )
  Fa <- pbeta(a, alpha, beta)
  Fb <- pbeta(b, alpha, beta)
  y <- dbeta(x, alpha, beta)
  inda <- which(x < a)
  indb <- which(x > b)
  if(length(inda) > 0) y[inda] <- 0
  if(length(indb) > 0) y[indb] <- 0
  return(y/(Fb-Fa))
}

ptbeta <- function(q, alpha, beta, a=0, b=1)
{
  stopifnot( all( alpha > 0 ) & all(beta > 0) )
  Fa <- pbeta(a, alpha, beta)
  Fb <- pbeta(b, alpha, beta)
  p <- ( pbeta(q, alpha, beta) - Fa ) / ( Fb - Fa )
  inda <- which(q < a)
  indb <- which(q > b)
  if(length(inda) > 0) p[inda] <- 0
  if(length(indb) > 0) p[indb] <- 1
  return(p)
}

qtbeta <- function(p, alpha, beta, a=0, b=1)
{
  stopifnot( all(p >= 0 & p <= 1) &  all( alpha > 0 ) & all(beta > 0) )
  Fa <- pbeta(a, alpha, beta)
  Fb <- pbeta(b, alpha, beta)
  pNew <- p * (Fb - Fa) + Fa
  x <- qbeta(pNew, alpha, beta)
  return(x)
}
