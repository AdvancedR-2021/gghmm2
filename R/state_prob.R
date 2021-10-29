#' @title state_prob
#'
#' @description 
#' 
#' @usage state_prob(state,state_time,HMM,X)
#'
#' @param state 
#' @param state_time 
#' @param HM A HMM object
#' @param X  Data
#'
#' @return 
#' @export
#'
#'
state_prob = function(state,state_time,HM,X){
  #delta,trans,param
  FA = forwardalgopois(delta,trans,X,param)
  BA = backwardalgopois(trans,X,param)
  
  likelihood =  sum(BA[,1]*delta*dpois(X[1],lambda = param))
  return(FA[state,state_time]*BA[state,state_time]/likelihood)
}