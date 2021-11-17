#' @title Local decoder
#'
#' @description Local_decoder calculate the states that are most probable for time frame that the data covers.
#' 
#' @usage local_decoder(HMM,X)
#' 
#' @include HMMclass.R
#' 
#' @details  The function uses the state_prob function to calculate which state would be most likely to yield the data. 
#' Note that this is the   local decoder will give the state most probable for each separate time, and will not yield the 
#' sequence of state that is most probable to emit the data. That sequence is returned by the Viterbi function. 
#'
#' @param HMM A HMM object
#' @param X Data
#' 
#' @include HMMclass.R state_prob.R 
#' @return A vector containing the  states that are most probable given our model and data. 
#' @export
#' 
#'


local_decoder = function(X,HMM){
  if (!is.null(HMM) ){
    trans = HMM$transmision
    delta = HMM$stationary_dist
    Param = HMM$param
    emisf = HMM$emission_func
  }
  n = length(X)
  m = length(delta)
  Result = c(1:n)
  for (i in c(1:n)){
    prob = c(1:m)
    for (j in c(1:m)){
      prob[j] = state_prob(state = j,state_time = i,HM=HMM,X=X)
    }
    Result[i] = which.max(prob)
  }
  return(Result)
}
