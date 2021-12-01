#' @title  Expectation–Maximization (EM) Algorithm 
#'
#' @description The EM algorithm finds the maximum-likelihood estimates of a HMM. 
#' 
#' @details Is an iterative method to find a local maximum likelihood or a maximum poosteriori estimates of parameters in a 
#' statistical model, where the model depends on hidden variables. The EM alternates between performing an expectation step, 
#' which creates a function for the expectation of the log-likelihood evaluated using the curretn estimate for the parameters, 
#' and a maximation step, which computes the parameters, which maximize the expected log-likelihood found in the E step. This 
#' is then iterated.
#' 
#' @usage em(HM,X)
#' 
#' @param HM A HMM object
#' @param X  Data
#' @param tol tolerance of percentage change between likelihoods from different iterations
#' @param maxiter the maximum number of iteration the algorithm will run 
#'  
#' @include HMMclass.R forward.R backward.R 
#' @return HMM Returns an updated HMM object
#' @import stats
#' @export
#' 
#' @examples 
#'  X <- earthquakes$n
#' delta = c(0.5,0.5)
#' trans=matrix(c(0.9,0.1,0.1,0.9),2,2)
#' HM = HMM(stationary_dist = delta,transmission = trans,  
#'         emission_function_names = c("dpois","dpois"),parameters = list(list(lambda=10),list(lambda=30)) )
#' em(HM=HM,X=X)
#'

em <- function(HM,X,tol = 0.001 , maxiter = 100){
  if (!is.null(HM) ){
    trans = HM$transmision
    delta = HM$stationary_dist
    param = HM$param
    emisf = HM$emission_func
  }
  
  neglikeli = function(newpar, parloc, HM, X){
    name <- names(HM$param[[parloc]])
    HM$param[[parloc]]= as.list(newpar)
    names(HM$param[[parloc]]) <- name
    FA = forward(HM,X)
    t = length(X)
    likelihood =  sum(FA[,t])
    
    return(-log(likelihood))
  }
  
  m = dim(trans)[1]
  t = length(X)
  
  FA = forward(HM,X)
  BA =backward(HM,X)
  likelihood =  sum(FA[,t])
  L= c(likelihood*0.1,likelihood)
  u=0
  HM1=HM
  while(maxiter>u & (L[length(L)]- L[length(L)-1])/L[length(L)-1]>tol){
    
  Statetrans = matrix(1,m,m)
  for (i in c(1:m)){
    for (j in c(1:m)){
      temp=0
      for (l in c(2:t)){
    temp = temp+ FA[i,(l-1)]*trans[i,j]* do.call(emisf[[j]],c(list(x=X[l]),param[[j]]))*BA[j,l]/likelihood }
      Statetrans[i,j] = temp
    }
  }
  delta =   FA[,1]*BA[,1]/likelihood
  col_statetrans = colSums(Statetrans)
  trans= t(t(Statetrans)/colSums(Statetrans))
  
  
  for (k in c(1:m)){
    if (emisf[k]=="dpois"){
      param[[k]] = sum(X*FA[k,]*BA[k,]/likelihood)/ sum(FA[k,]*BA[k,]/likelihood)
    }
    else if (emisf[k]=="dnorm"){
      param[[k]]$mean =  sum(X*FA[k,]*BA[rfk,]/likelihood)/ sum(FA[k,]*BA[k,]/likelihood)
      param[[k]]$var =  sum((X-param[[k]]$mean)*FA[k,]*BA[k,]/likelihood)/ sum(FA[k,]*BA[k,]/likelihood)
    }
    
    else{
      name <- names(param[[k]])
      param[[k]] = list(stats::nlm(f = neglikeli,p = unlist(param[[k]]),parloc=k,HM=HM,X=X)$estimate)
      names(param[[k]]) <- name
    }
    
  }
  
  HM1$transmision = trans 
  HM1$stationary_dist = delta
  HM1$param = param
  
  FA = forward(HM1,X)
  BA =backward(HM1,X)
  likelihood =  sum(FA[,t])
  L= c(L,likelihood)
  LL = length(L)
  if (L[LL] > L[LL-1]){
    HM$transmision = trans 
    HM$stationary_dist = delta
    HM$param = param
    u=u+1
  } else {
    if (u>0){
      print(paste("EM algoprithm has stopped after" ,u, "iterations as the likelihood did not increase in the" ,u+1, "iteration"))
      return(HM)
    } else{
      stop("EM algoprithm cannot run as the likelihood is not increasing.
        Consider other starting values.")
    }
  }

  }
  #print(L)
  print(paste("Number of iterations:" ,u))
  return(HM)
}



