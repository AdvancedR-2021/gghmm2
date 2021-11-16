#' @title State probability
#'
#' @description state_prob will calculate the probability of being in state at state_time based on the HMM and the data. 
#' 
#' @usage state_prob(state,state_time,HM,data)
#'
#' @param state A Natural Numbers from 1 up to number of hidden states.
#' @param state_time A Natural Numbers from 1 up to number of ob.
#' @param HM A HMM object.
#' @param Data  vector of observations.
#' 
#' @include HMMclass.R forward.R backward.R
#'
#' @return A numerical value between 0 and 1, which is the probability for having state 
#' at time state_time given the model and data. 
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
