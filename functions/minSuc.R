minSuc <- function(p, n, r){
  cProb <- 0
  for(i in r:n){
    cProb <- cProb + diceProb(n,i,p)
  }
  return(cProb)
}