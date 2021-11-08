#' @title Viterbialgo
#'
#' @description 
#' 
#' @usage Viterbialgo(HMM,X)
#'
#' @param HMM A HMM object
#' @param X  Data
#'
#' @return 
#' @export
#'
#'

viterbi = function(X,delta,trans,param){
 
  
  
   t <- length(X)
  m <- length(param)
  epsilon = matrix(0,m,t)
  maxindex = matrix(0,m,t)
  for (i in c(1:m)){
    epsilon[i,1] = delta[i] * dpois(X[1],lambda = param[i])
      
      
      #dpois(X[1],lambda = param[i])
    
    
    #do.call(emisf[[1]],c(list(x=x[i],param[[i]])))
    
  }
  for (i in c(2:t)){
    for (j in c(1:m)){
      temp = epsilon[,i-1] * trans[,j]
      maxindex[j,i] = which.max(temp)
      epsilon[j,i] =dpois(X[i],lambda = param[j])*max(temp)
    }
  }
  path = numeric(t)
  path[t] = which.max(epsilon[,t])
  for (i in c((t-1):1)){
    path[i] = maxindex[path[i+1],i+1]
  }
  return(list(path=path,path_prob=max(epsilon[,t]),maxindex=maxindex))
}

 X = read.table("http://www.hmms-for-time-series.de/second/data/earthquakes.txt")[,2]
delta = c(0.5,0.5)
lambdaL=c(10,30)
trans=matrix(c(0.9,0.1,0.1,0.9),2,2)

Result2 = viterbi(X,delta = delta,trans = trans,param = lambdaL)
Result2$path
Result2$path_prob
Result2$maxindex
