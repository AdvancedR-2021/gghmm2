#' @title backward
#'
#' @description backward
#' 
#' @usage backward(HM,X)
#' 
#' @include HMMclass.R
#'
#' @param HMM A HMM object
#' @param X  Data
#' @include HMMclass.R 
#' @return 
#' @export
#' 
#'

backward <- function(HM,x){
  if (!is.null(HM) ){
    trans = HM$transmision
    delta = HM$stationary_dist
    param = HM$param
    emisf = HM$emission_func
  }
  m = dim(trans)[1]
  t = length(X)
  beta_matrix= matrix(1,m,t)
  for (i in rev(c(1:(t-1)))){
    for (j in c(1:m)){
      empty_vec = c(1:m)
      for (a in c(1:m)){
        empty_vec[a] = do.call(emisf[[a]],c(x=x[i+1],param[[a]]))
      }
      beta_matrix[j,i] = sum(empty_vec*  beta_matrix[,i+1] * trans[j,])
    }
  }
  return( beta_matrix)
}
