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
''
viterbi = function(HM,X){
  if (!is.null(HM) ){
    trans = HM$transmision
    delta = HM$stationary_dist
    param = HM$param
    emisf = HM$emission_func
  }
  t <- length(X)
  m <- length(param)
  epsilon = matrix(0,m,t)
  maxindex = matrix(0,m,t)
  x=X
  for (i in c(1:m)){
    
    epsilon[i,1] = delta[i] * do.call(emisf[[i]],c(list(x=x[1],param[[i]])))
      
      
      #dpois(X[1],lambda = param[i])
    
    
    #do.call(emisf[[1]],c(list(x=x[i],param[[i]])))
    
  }
  for (i in c(2:t)){
    for (j in c(1:m)){
      temp = epsilon[,i-1] * trans[,j]
      maxindex[j,i] = which.max(temp)
      epsilon[j,i] =do.call(emisf[[j]],c(list(x=x[i],param[[j]])))
      
      
      #dpois(X[i],lambda = param[j])*max(temp)
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

hm = HM(delta,trans,emission_function_names=c("dpois","dpois"),parameters=list(list(lambda =10),list(lambda =30)))


Result2= viterbi(hm,X)

Result2 = viterbi(X,delta = delta,trans = trans,param = lambdaL)
Result2$path
Result2$path_prob
Result2$maxindex
