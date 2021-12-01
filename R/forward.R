#' @title Forward Algorithm
#'
#' @description Creates a vector of forward probabilities which is used in most of the functions in this package.
#' 
#' @details Does recursive computation of the likelihood which plays a key role in the likelihood evaluation
#' and thus parameter estimation. It is also used in forecasting, decoding and model checking. The recursive nature of
#' the forward algorithm is much more computationally inexpensive than brute-force summation over all possible state sequences.
#' 
#' @usage forward(HM,X)
#' 
#' @include HMMclass.R
#'
#' @param HM A  HMM class object 
#' @param X  Data
#' @param log_sum True or False
#' @return alpha_matrix A matrix of forward probabilities 
#' @export
#' 
#'
forward <- function(HM,X,log_sum=F){
  if (!is.null(HM) ){
    trans = HM$transmision
    delta = HM$stationary_dist
    param = HM$param
    emisf = HM$emission_func
  }
  if (class(HM)[1] !="HMM") {stop("The HM has been build wrong")}
  m = dim(trans)[1]
  t = length(X)
  if (log_sum==T){
    alpha_matrix = matrix(c(0),m,t)
    for (i in c(1:m)){
      alpha_matrix[i,1] = log(delta[i]) + log(do.call(emisf[[i]],c(list(x=X[1]),param[[i]])))
    }
    for (i in c(2:t)){
      at = max(alpha_matrix[,i-1])
      for (j in c(1:m)){
        alpha_matrix[j,i] =log(do.call(emisf[[j]],c(list(x=X[i]),param[[j]]))) + log(sum(exp(alpha_matrix[,i-1]-at) * trans[,j]))+at
      }
    }
  } else{
  alpha_matrix = matrix(c(0),m,t)
  for (i in c(1:m)){
    alpha_matrix [i,1] = delta[i] * do.call(emisf[[i]],c(list(x=X[1]),param[[i]]))
  }
  for (i in c(2:t)){
    for (j in c(1:m)){
      alpha_matrix [j,i] =do.call(emisf[[j]],c(list(x=X[i]),param[[j]])) *sum(alpha_matrix[,i-1] * trans[,j])
    }
  }}
  
  return(alpha_matrix)
}
