

#==============RNG Set Up===============
RNGversion(min(as.character(getRversion()),"3.5.3"))

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )


#==============Brute Force Function=======
brute_force_knapsack <- function(x,W){
  
  if(any(colnames(x) != c("v", "w"))){stop()} # Check if the names of each column of dataframe are correct
  if(is.data.frame(x)==FALSE ){stop()}
  if( any(x) < 0){stop()}
  
  
  
  
}