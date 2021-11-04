#mangler generalisering af dpois

forward <- function(delta,trans,X,param){
  m = dim(trans)[1]
  t = length(X)
  alpha_matrix = matrix(c(0),m,t)
  for (i in c(1:m)){
    alpha_matrix [i,1] = delta[i] * dpois(X[1],lambda = param[i])
  }
  
  for (i in c(2:t)){
    for (j in c(1:m)){
      alpha_matrix [j,i] =dpois(X[i],lambda = param[j])  *sum(alpha_matrix[,i-1] * trans[,j])
      
    }
  }
  return(alpha_matrix)
}
