

#==============RNG Set Up===============
RNGversion(min(as.character(getRversion()),"3.5.3"))

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

get_indexes<- function(vector){
  indexes <- which(vector == 1)
  sum <- 0
  if (exists("indexes"))
  {
      for (i in indexes){
       sum <- sum + knapsack_objects[i, 1]
      }
  }
  return(sum)
}

get_value <- function(vector){
  indexes <- which(vector == 1)
  sum <- 0
  if (exists("indexes"))
  {
    for (i in indexes){
      sum <- sum + knapsack_objects[i, 2]
    }
  }
  return(sum)
}

#==============Brute Force Function=======
brute_force_knapsack <- function(x,W){
  
 n <- nrow(x)
 l <- rep(list(0:1), n)
 multiple_combinations <- expand.grid(l)
 total_weights <- list()
 total_values <- list()
 
 for (i in 1:nrow(multiple_combinations)){
   total_weights <- append(total_weights, get_indexes(multiple_combinations[i, ]))
   total_values <- append(total_values, get_value(multiple_combinations[i, ]))
   
 }
 
 indexes_to_look_for <- which(total_weights <= W)
 value_to_return <- max(unlist(total_values[indexes_to_look_for]))
 index_to_return <- which(total_values == value_to_return)
 
 
 

  return(list(value = value_to_return , elements =  which(multiple_combinations[index_to_return, ] == 1)))
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
  
  m <- matrix(NA,W+1,length(x[,1]+1))
   
    for(i in 1:length(x[,1]) ){
      for(j in 0:W){
        if(x$w[i] > j){m[i+1, j+1] <- m[i, j+1]}
        
        else{m[i+1, j+1] <- max(m[i, j+1], m[i, j+1-x$w[i]] + x$v[i])}
        
        
      }
        
   return(m)     
        
    }
  
  
  
  
}

# brute_force_knapsack <- function(x, w){
#   total_value <- 0
#   combinations <- list()
#   for (i in seq_along(x[,1])){
#     weight1 <- x[i, 1]
#     value1 <- x[i, 2]
#     element1 <- i
#     for (j in seq_along(x[, 1])){
#       weight2 <- x[j, 1]
#       value2 <- x[j, 2]
#       element2 <- j
#       print(j)
#       print(i)
#       
#       total_value <- value1 + value2
#       total_weight <- weight2 + weight1
#       
#       combinations <- append(combinations, total_value)
#       
#       
#     }
#     
#   }


#=============End Dynamic programming=======

