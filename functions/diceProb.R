diceProb <- function(n,r,p){
  nCr(n,r) * p^r * (1-p)^(n-r)
}