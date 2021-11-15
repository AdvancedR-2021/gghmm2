#' @title Forward Algorithm
#'
#' @description Creates a vector of forward probabilities which is used in most of the functions in this package.
#' 
#' @details Does recursive computation the likelihood which plays a key role in the likelihood evaluation
#' and thus parameter estimation. It is also used in forecasting, decoding and model checking. The recursive nature of
#' the forward algorithm is much more computationally inexpensive than brute-force summation over all possible state sequences.
#' 
#' 
#' 
#' @usage forward(HM,X)
#' 
#' @include HMMclass.R
#'
#' @param HMM A  HMM class object 
#' @param X  Data
#' @return 
#' @export
#' 
#'
forward <- function(HM,X){
  if (!is.null(HM) ){
    trans = HM$transmision
    delta = HM$stationary_dist
    param = HM$param
    emisf = HM$emission_func
  }
  m = dim(trans)[1]
  t = length(X)
  alpha_matrix = matrix(c(0),m,t)
  for (i in c(1:m)){
    alpha_matrix [i,1] = delta[i] * do.call(emisf[[i]],c(list(x=X[1]),param[[i]]))
  }
  for (i in c(2:t)){
    for (j in c(1:m)){
      alpha_matrix [j,i] =do.call(emisf[[j]],c(list(x=X[i]),param[[j]])) *sum(alpha_matrix[,i-1] * trans[,j])
    }
  }
  return(alpha_matrix)
}
