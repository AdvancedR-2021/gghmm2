#Temporary - needs generalisation of dpois

backward <- function(trans,X,param){
  m = dim(trans)[1]
  t = length(X)
  beta_matrix= matrix(1,m,t)
  for (i in rev(c(1:(t-1)))){
    for (j in c(1:m)){
      beta_matrix[j,i] = sum(dpois(X[i+1],lambda = param) *  beta_matrix[,i+1] * trans[j,])
      
    }
  }
  return( beta_matrix)
}
