IncluProbLv22 <- function(popsize,sampsize,set,rank){
  require(doParallel)
  require(foreach)
  cl <- makeCluster(7)
  registerDoParallel(cl)

  secondorder  <- foreach (i = 1:popsize, .combine = rbind) %:%
    #Looping over all id > i (adjusted indice)
    foreach (id = 1:popsize) %dopar% {
  #Looping over all i < id (adjusted indice)
       #Skip unncessary indices
      if (i < id) {
        #Initiate intermediate matrix
        int <- matrix(0, nrow = sampsize, ncol = sampsize)
        #Running down the rows
        for (j in 1:sampsize) {
          #Running down the columns
          for (jd in 1:sampsize) {
            #Miss the diagonal
            if (j != jd) {
              #lambda limits
              start = 0
              end = (id - i - 1)
              #Logical gate for a < b
              if (start <= end) {
                #This is where the problem is
                L <- start:end
                a <- choose(i - 1, rank[j] - 1)
                b <- choose(id - i - 1, L)
                c <- choose(popsize - id, set - L - rank[j])
                d <- choose(id - 1 - rank[j] - L, rank[jd] - 1)
                e <- choose(popsize - id - set + L + rank[j], set - rank[jd])
                f <- choose(popsize, set)
                g <- choose(popsize - set, set)
                #Calculate intermediate quantitiy
                int[jd, j] <- sum(a * b * c * d * e / (f * g))
              }
              else{
                int[jd, j] <- 0
              }
            }
          }
        }
        #Sum matrix for second order inclusion probability
        sum(int)
      }
    }
  
  return(secondorder)
  stopCluster(cl)
}
