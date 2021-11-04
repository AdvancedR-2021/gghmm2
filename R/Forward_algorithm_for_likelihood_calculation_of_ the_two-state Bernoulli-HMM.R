
#' Title
#'
#' @param delta 
#' @param trans 
#' @param emision 
#' @param X 
#' @param f 
#'
#' @return
#' @export
#'
#' @examples
#' tpm = matrix(c(0.5,0.25,0.5,0.75),2,2)
#' del = c(1/3 , 2/3)
#' XX = c(1,1,1) 
#' emis = matrix(c(0.5,0,0.5,1),2,2) 
#' fbinom = function(x){x+1}
#' forward(delta = del,trans = tpm, emision = emis,X = XX,f=fbinom)
 

forward<- function(delta,trans,emision,X,f){
  m = dim(trans)[1]
  t = length(X)
  Im = matrix(c(0),m,t)
  for (i in c(1:m)){
    Im[i,1] = delta[i] * emision[i,f(X[1])]
  }

  for (i in c(2:t)){
    for (j in c(1:m)){
      Im[j,i] = emision[j,f(X[i])] *sum(Im[,i-1] * trans[,j])

    }
  }
  return(Im)
}
