#a
set.seed(0) 
runif(1)
save(.Random.seed, file = 'tmp.Rda')
runif(1)
load('tmp.Rda') 
.Random.seed[2]
runif(1)

library(pryr)
tmp <- function() { 
  print("Location of Random seed 1: ")
  print(where(".Random.seed"))
  cat("position on global env:", .Random.seed[2], "\n")
  load('tmp.Rda') 
  print("Location of Random seed 2: ")
  print(where(".Random.seed"))
  cat("position on local env:", .Random.seed[2], "\n")
  a = runif(1)
  cat("position on local env after generating a random number:", .Random.seed[2], "\n")
  return(a)
} 
tmp()

#b

tmp <- function() { 
  print("Location of Random seed 1: ")
  print(where(".Random.seed"))
  cat("position on global env:", .Random.seed[2], "\n")
  load('tmp.Rda', env=.GlobalEnv) 
  print("Location of Random seed 2: ")
  print(where(".Random.seed"))
  cat("position on local env:", .Random.seed[2], "\n")
  a = runif(1)
  cat("position on local env after generating a random number:", .Random.seed[2], "\n")
  return(a)
} 
tmp()
