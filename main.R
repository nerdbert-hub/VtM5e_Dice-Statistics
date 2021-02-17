# # formulars 
# P(X=r) = nCr * p^r * (1-p)^(n-r)
# nCr = n! / ((n-r)! * r!)

# functions in R
nCr <- function(n,r) {
  factorial(n) / ( factorial(r) * factorial(n-r))
}

diceProb <- function(n,r,p){
  nCr(n,r) * p^r * (1-p)^(n-r)
}

minSuc <- function(p, n, r){
  cProb <- 0
  for(i in r:n){
    cProb <- cProb + diceProb(n,i,p)
  }
  return(cProb)
}


# parameters
p <- 0.5
n <- 7
r <- 2

for(n in 1:10){
  prob <- minSuc(p,n,r)
  cat("Willpower*", n, "at Diff", r, "=", prob, "\n")
}




# Monte Carlo Simulation
diff <- 2
nMC <- 20000
wins <- 0
sux <- 0
crit <- 0
fail <- 0
beast <- 0
nHunger <- 3
nDice <- 7

# Monte Carlo Simulation

V5stats <- function(nDice, nHunger, diff, nMC = 20000){
  # initiate 
  wins <- 0
  sux <- 0
  crit <- 0
  fail <- 0
  beast <- 0
  messy <- 0
  vSuc <- rep(-1, nMC)
  #check the number of hunger dice, adjust if necessary
  if (nHunger >= nDice){
    nHunger <- nDice
  }
  # loop
  for(r in 1:nMC){
    # reset values
    sux <- 0
    # diceRoll
    dResult <- floor(runif(nDice) * 10) + 1
    # number of successes
    sux <- sux + sum(dResult >= 6)
    sux <- sux + floor(sum(dResult == 10) / 2) * 2
    # store number of successes
    vSuc[r] <- sux
    # counting wins
    if(sux >= diff){
      wins <- wins + 1
    }
    # counting crits
    if(sum(dResult == 10) / 2 >= 1 && sux >= diff){
      crit <- crit + 1
    }
    # counting messy
    if(sum(dResult == 10) / 2 >= 1 && 
       sux >= diff && 
       any(tail(dResult, nHunger) == 10)){
      messy <- messy + 1
    }
    # counting fails
    if(sux < diff){
      fail <- fail + 1
    }
    # counting fails
    if(sux < diff && any(tail(dResult, nHunger) == 1)){
      beast <- beast + 1
    }
  }
  # calculate results
  l <- list(
    AvgSuc = mean(vSuc),
    sdSuc = sd(vSuc),
    Prob_Win = wins / nMC * 100,
    Prob_Fail = fail / nMC * 100,
    Prob_Crit = crit / nMC * 100,
    Prob_Messy = messy / nMC * 100,
    Prob_Beast = beast / nMC * 100
  )
  return(l)
}

V5stats(5,1,2, nMC = 1000000)

