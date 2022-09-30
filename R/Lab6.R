

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
  
    
    element1 <- which.max(table[,W+1])
    table_calculate <-table[-element1:-length(x$w),] #need to delete the rows below the one have already chooesn 
    #============This is the woriking part if only two elements are inside, Comment out if you want to try===========
    # element2 <- which.max(table_calculate[,W-x$w[element1]])
    # elements <- c(element1,element2)
    # output_list <- list("value"=maxvalue, "elements"=c(sort(elements, decreasing = FALSE)))
    # 
    # return(output_list)
    #============End of the woriking part if only two elements are inside===========
    
    #============Below are the testing I am doing
    elements <- c(element1)
     for(i in 2:length(x$w)){
       table_calculate <-table_calculate[-elements[i-1],]
       element[i+1] <- which.max(table_calculate[,W-x$w[elements[i]]])


     }
    return(table_calculate)
  
}
  

     


  
  
  
  





#=============End Dynamic programming=======

