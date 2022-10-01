

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
  
  if(any(colnames(x) != c("w", "v"))){stop()} # Check if the names of each column of dataframe are correct
  if(is.data.frame(x)==FALSE ){stop()} # Check if the firs input is data.frame
  if( any(x < 0)){stop()} #Check if every value in dataframe is positive
  if( W < 0){stop()}
  
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
 
 
 

  return(list(value = round(value_to_return) , elements =  which(multiple_combinations[index_to_return, ] == 1)))
 }

#=============End Brute Force Function=======



#=============Dynamic programming=======

knapsack_dynamic <- function(x, W){
  
  if(any(colnames(x) != c("w", "v"))){stop()} # Check if the names of each column of dataframe are correct
  if(is.data.frame(x)==FALSE ){stop()} # Check if the firs input is data.frame
  if( any(x < 0)){stop()} #Check if every value in dataframe is positive
  if( W < 0){stop()}
  
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


#============Gredy
greedy_knapsack <- function(x, W){
  
  if(any(colnames(x) != c("w", "v"))){stop()} # Check if the names of each column of dataframe are correct
  if(is.data.frame(x)==FALSE ){stop()} # Check if the firs input is data.frame
  if( any(x < 0)){stop()} #Check if every value in dataframe is positive
  if( W < 0){stop()}
  
  
  x$ratio = 0
  for (i in 1:length(x[, 1, 1])){
    x$ratio[i] <- x$v[i] / x$w[i]
  }
  sorted_dataframe <-x[order(x$ratio),]
  elements <- list()
  sum <- 0
  j <- 1
  value <- 0
  for (i in (length(sorted_dataframe[, 1, 1])):1){
    if ((sum + sorted_dataframe$w[i] ) < W){
      sorted_dataframe$w[i]
      sum <- sum + sorted_dataframe$w[i]
      value <- value + sorted_dataframe$v[i]
      elements <- append(elements, i)
      j <- j+1
      
    }
    else{
      break
    }
  }
  output_elements<- as.numeric(row.names(tail(sorted_dataframe, n=j-1)))
  output_list <- list("value"=round(value), "elements"=c(rev(output_elements)))
  
  return(output_list)
}
