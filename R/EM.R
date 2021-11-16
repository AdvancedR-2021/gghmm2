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
#' @usage em(HMM,X)
#' 
#' @param HMM A HMM object
#' @param X  Data
#' @include HMMclass.R forward.R backward.R 
#' @return HMM Returns an updated HMM object
#' @export
#' 
#'

em <- function(HM,X){
  if (!is.null(HM) ){
    trans = HM$transmision
    delta = HM$stationary_dist
    param = HM$param
    emisf = HM$emission_func
  }
  
  m = dim(trans)[1]
  t = length(X)

  for (k in c(1:100)){
  FA = forward(HM,X)
  BA =backward(HM,X)
  likelihood =  sum(FA[,t])
  
  Statetrans = matrix(1,m,m)
  for (i in c(1:m)){
    for (j in c(1:m)){
      temp=0
      for (l in c(2:t)){
    temp = temp+ FA[i,(l-1)]*trans[i,j]* do.call(emisf[[l]],c(list(x=X[l]),param[[j]]))*BA[j,l]/likelihood }
      Statetrans[i,j] = temp
    }
  }
  delta =   FA[,1]*BA[,1]/likelihood
  col_statetrans = colSums(Statetrans)
  trans= t(t(Statetrans)/colSums(Statetrans))
  
  
  neglikeli = function(newpar, parloc, HM, X){
    name <- names(HM$param[[parloc]])
    HM$param[[parloc]]= as.list(newpar)
    names(HM$param[[parloc]]) <- name
    FA = forward(HM,X)
    t = length(X)
    likelihood =  sum(FA[,t])
    
    return(-log(likelihood))
  }
  
  for (k in c(1:m)){
    if (emisf[k]=="dpois"){
      param[[k]] = sum(X*FA[k,]*BA[k,]/likelihood)/ sum(FA[k,]*BA[k,]/likelihood)
    }
    else{
      name <- names(param[[k]])
      param[[k]] = list(nlm(f = neglikeli,p = unlist(param[[k]]),parloc=k,HM=HM,X=X)$estimate)
      names(param[[k]]) <- name
    }
    
  }
  

  }
  HM$transmision = trans 
  HM$stationary_dist = delta
  HM$param = param
  return(HM)
}
