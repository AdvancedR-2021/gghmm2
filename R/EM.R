#' @title  Expectation–Maximization (EM) Algorithm 
#'
#' @description The EM algorithm finds the maximum-likelihood estimates of a HMM. 
#' 
#' @details Is an iterative method to find a local maximum likelihood or a maximum poosteriori estimates of parametersin a 
#' statistical model, where the model depends on hidden variables. The EM alternates between performing an expectation step, 
#' which creates a function for the expectation of the log-likelihood evaluated using the current estimate for the parameters, 
#' and a maximization step, which computes the parameters, which maximize the expected log-likelihood found in the E step. This 
#' is then iterated. The numbers of iteration the user want to do, are dependent on the tol and maxiter parameters. Maxiter is the maximum
#' number of iterations the user will allow the algorithm to run and tol is minimal percentage change in likelihood the algorithm need to see
#' before running another iterations. By using these two parameters, the user can control how long the algorithm should run. 
#' 
#' Note that if the user are using a distribution other then normal or poisson in the model, then the initial values need to be picked carefully. As 
#' numerical maximization is used to fine the optimal parameter, which can have problem with ill chosen starting parameters.  
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
#' HM = HMM(initial_dist = delta,transmission = trans,  
#'         emis_names = c("dpois","dpois"),parameterlist = list(list(lambda=10),list(lambda=30)) )
#' em(HM=HM,X=X)
#'

em <- function(HM,X,tol = 10^-9 , maxiter = 100){
  if (!is.null(HM) ){
    trans = HM$transmision
    delta = HM$initial_dist
    param = HM$param
    emisf = HM$emission_func
    emisfname = HM$emis_names
    
  }
  
  neglikeli = function(newpar, parloc, HM, X){
    name <- names(HM$param[[parloc]])
    HM$param[[parloc]]= as.list(newpar)
    names(HM$param[[parloc]]) <- name
    FA = forward(HM,X,log_sum=T)
    t = length(X)
    likelihood =log(sum(exp(FA[,ncol(FA)]- max(FA[,ncol(FA)]))))+ max(FA[,ncol(FA)])
    
    return(-likelihood)
  }
  
  m = dim(trans)[1]
  t = length(X)
  
  FA = forward(HM,X,log_sum=T)
  BA = backward(HM,X,log_sum=T)
  likelihood =log(sum(exp(FA[,ncol(FA)]- max(FA[,ncol(FA)]))))+ max(FA[,ncol(FA)])
  L= c(likelihood*10,likelihood)
  u=0
  HM1=HM
  while(maxiter>u & abs(L[length(L)]- L[length(L)-1])/abs(L[length(L)-1])>tol){
    
  Statetrans = matrix(1,m,m)
  for (i in c(1:m)){
    for (j in c(1:m)){
      temp=0
      for (l in c(2:t)){
    temp = temp+ exp(FA[i,(l-1)] + log(trans[i,j]) + log(do.call(emisf[[j]],c(list(x=X[l]),param[[j]]))) + BA[j,l]- likelihood) }
      Statetrans[i,j] = temp
    }
  }
  delta =   exp(FA[,1]+BA[,1]-likelihood)
  col_statetrans = colSums(Statetrans)
  trans= t(t(Statetrans)/colSums(Statetrans))
  
  
  for (k in c(1:m)){
    if (emisfname[k]=="dpois"){
      param[[k]] = list(lambda= sum(X*exp(FA[k,]+BA[k,]-likelihood))/ sum(exp(FA[k,]+BA[k,]-likelihood)))
    }
    else if (emisfname[k]=="dnorm"){
      param[[k]][1] = list(mean= sum(X*exp(FA[k,]+BA[k,]-likelihood))/ sum(exp(FA[k,]+BA[k,]-likelihood)))
      mu = param[[k]]$mean
      param[[k]][2] =list(sd =sqrt(sum(abs(X-mu)*exp(FA[k,]+BA[k,]-likelihood))/ sum(exp(FA[k,]+BA[k,]-likelihood))) )
    }
    
    else{
      name <- names(param[[k]])
      param[[k]] = list(stats::nlm(f = neglikeli,p = unlist(param[[k]]),parloc=k,HM=HM,X=X)$estimate)
      names(param[[k]]) <- name
    }
    
  }
  
  HM1$transmision = trans 
  HM1$initial_dist = delta
  HM1$param = param
  
  FA = forward(HM1,X,log_sum=T)
  BA = backward(HM1,X,log_sum=T)
  likelihood =log(sum(exp(FA[,ncol(FA)]- max(FA[,ncol(FA)]))))+ max(FA[,ncol(FA)])
  L= c(L,likelihood)
  LL = length(L)
  if (L[LL] > L[LL-1]){
    HM$transmision = trans 
    HM$initial_dist = delta
    NN = names(HM$param)
    HM$param = param
    u=u+1
  } else {
    if (u>0){
      warning(paste("EM algoprithm has stopped after" ,u, "iterations as the likelihood did not increase in the" ,u+1, "iteration",
                    "Consider trying other starting values, as the algorithm may have fallen into a local maximum"))
      print(u)
      return(HM)
    } else{
      stop("EM algoprithm cannot run as the likelihood is not increasing.
        Consider other starting values.")
    }
  }

  }
  print(paste("Number of iterations:" ,u))
  return(HM)
}
