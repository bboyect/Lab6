

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
  
  # if(any(colnames(x) != c("v", "w"))){stop()} # Check if the names of each column of dataframe are correct
  # if(is.data.frame(x)==FALSE ){stop()}
  # if( any(x) < 0){stop()}

}
#=============End Brute Force Function=======



#=============Dynamic programming=======
  
  #=====pseudo codes======
  # array m[0..n, 0..W];
  # for j from 0 to W do:
  #   m[0, j] := 0
  # for i from 1 to n do:
  #   m[i, 0] := 0
  # 
  # for i from 1 to n do:
  #   for j from 0 to W do:
  #   if w[i] > j then:
  #   m[i, j] := m[i-1, j]
  # else:
  #   m[i, j] := max(m[i-1, j], m[i-1, j-w[i]] + v[i])
  #=====End pseudo codes========

knapsack_dynamic <- function(x, W){
  
  m <- matrix(0,W+1,length(x[,1]+1))
   
    for(i in 1:length(x[,1]) ){
      for(j in 0:W){
        if(x$w[i] > j){m[i+1, j+1] <- m[i, j+1]}
        
        else{m[i+1, j+1] <- max(m[i, j+1], m[i, j+1-x$w[i]] + x$v[i])}
        print(m)
        
      }
        
   return(m)     
        
    }
  
  
  
  
}




#=============End Dynamic programming=======

