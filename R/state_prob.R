#' @title State probability
#'
#' @description state_prob will calculate the probability of being in state at state_time based on the HMM and the data. 
#' 
#' @usage state_prob(state,state_time,HM,X)
#'
#' @param state A Natural Numbers from 1 up to number of hidden states.
#' @param state_time A Natural Numbers from 1 up to number of ob.
#' @param HM A HMM object.
#' @param X  vector of observations.
#' 
#' @include HMMclass.R forward.R backward.R
#'
#' @return A numerical value between 0 and 1, which is the probability for having state 
#' at time state_time given the model and data. 
#' @export
#'
#'
state_prob = function(state,state_time,HM,X){
  if (class(HM)[1] !="HMM") {stop("The HM has been build wrong")}
  n = length(X)
  FA = forward(X = X, HM = HM)
  BA = backward(X = X, HM = HM)
  likelihood =  sum(FA[,n])
  Result = FA[state,state_time]*BA[state,state_time]/likelihood
  return(Result)
}
