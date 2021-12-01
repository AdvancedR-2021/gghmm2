#' @title Backward Algorithm 
#'
#' @description Creates a vector of conditional probabilities which is uded in most of the functions in this package.
#' 
#' 
#' @details Creates through a backward pass through the data a evaluation in in form of conditional probabilities 
#' of the observations being x_{t+1},...,x_t given that the Markov chain is in state i at time t.
#'  
#' @usage backward(HM,X)
#' 
#' @include HMMclass.R
#' 
#' @param HM A  HMM class object 
#' @param X  Data
#' @return beta_matrix A matrix of backward probabilities 
#' 
#' @include HMMclass.R 
#' @export
#' 
backward <- function(HM,X){
  if (!is.null(HM) ){
    trans = HM$transmision
    delta = HM$stationary_dist
    param = HM$param
    emisf = HM$emission_func
  }
  if (class(HM)[1] !="HMM") {stop("The HM has been build wrong")}
  m = dim(trans)[1]
  t = length(X)
  beta_matrix= matrix(1,m,t)
  for (i in rev(c(1:(t-1)))){
    for (j in c(1:m)){
      empty_vec = c(1:m)
      for (a in c(1:m)){
        empty_vec[a] = do.call(emisf[[a]],c(list(x=X[i+1]),param[[a]]))
      }
      beta_matrix[j,i] = sum(empty_vec*  beta_matrix[,i+1] * trans[j,])
    }
  }
  return( beta_matrix)
}
