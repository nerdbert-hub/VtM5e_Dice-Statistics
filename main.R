# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #
#                                                                             #
#                             The PMT SILAC Analyzer Script                   #                                                                        
#                                                                             #
# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #


# =========================================================================== #
#                                                                             #
#########                   LOADING FUNCTIONS                         #########
#                                                                             #
# =========================================================================== #

### Load all functions

# List files in "R/"
fnFunc <- list.files(path = "functions/", recursive=T, full.names = TRUE)
# Source all functions
for(i in 1:length(fnFunc)){
  source(fnFunc[i])
}

# =========================================================================== #
#                                                                             #
#########                TESTING THE FUNCTIONS                        #########
#                                                                             #
# =========================================================================== #

#### Bionomincal Distribution ####

# # formulars 
# P(X=r) = nCr * p^r * (1-p)^(n-r)
# nCr = n! / ((n-r)! * r!)

# parameters
p <- 0.5
n <- 7
r <- 2

for(n in 1:10){
  prob <- minSuc(p,n,r)
  cat("Dice Pool of", n, "at Difficulty", r, "=", prob, "\n")
}


#### Monte Carlo Simulation ####

V5stats(7,2,2, nMC = 20000)

