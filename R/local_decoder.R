#' @title local_decoder
#'
#' @description 
#' 
#' @usage local_decoder(HMM,X)
#'
#' @param HMM A HMM object
#' @param X  Data
#'
#' @return 
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
      prob[j] = state_prob(state = j,state_time = i,HM=HMM,data=X)
    }
    Result[i] = which.max(prob)
  }
  return(Result)
}
