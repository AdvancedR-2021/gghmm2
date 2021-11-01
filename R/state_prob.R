#' @title state_prob
#'
#' @description 
#' 
#' @usage state_prob(state,state_time,HM,data)
#'
#' @param state A Natural Numbers from 1 up to dim(HMM$transmision).
#' @param state_time A Natural Numbers from 1 up to length(data).
#' @param HM A HMM object.
#' @param Data  vector of observations.
#'
#' @return A numerical value between 0 and 1, which is the probability for having state 
#' at time state_time given our HMM and data. 
#' @export
#'
#'
state_prob = function(state,state_time,HM,data){
  if (!is.null(HMM) ){
    trans = HMM$transmision
    delta = HMM$stationary_dist
    Param = HMM$param
    emisf = HMM$emission_func
  }
  #delta,trans,param
  FA = forward(data = data, HM = HM)
  BA = backward(data = data, HM = HM)
  likelihood =  sum(FA[,length(data)])
  return(FA[state,state_time]*BA[state,state_time]/likelihood)
}
