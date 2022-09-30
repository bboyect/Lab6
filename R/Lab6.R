

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
  
  
  
}

  
#=============End Brute Force Function=======



#=============Dynamic programming=======

knapsack_dynamic <- function(x, W){
  
  table <- matrix(0,length(x[,1])+1,W+1)
  
  for(i in 2:length(x[,1])){
    for(j in 1:(W+1)){
      if(x$w[i] > j){
        table[i,j]<-table[i-1,j]
      }else{
        table[i,j]<-max((x$v[i]+table[i-1,j-x$w[i]]),table[i-1,j])
      }
    }
  }
  maxvalue <- round(max(table[,W+1]))
  
  #====find the elements=====
  elements <- c()
  j <- W
  
  for (i in length(x$w):2){
    
    if (table[i,  j] != table[i-1, j]){
      elements[i] <-  i 
      j <- j - x$w[i]
      
      next
    }
  }
  
  elements <- elements[!is.na(elements)]
  output_list <- list("value"=maxvalue, "elements"=c(sort(elements, decreasing = FALSE)))
  
  return(output_list)
  
}














#=============End Dynamic programming=======

