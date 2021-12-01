#' @title Forecast
#'
#' @description The function will give the probability of observing pred_obs at time 
#' pred_time. 
#' 
#' @usage forecast(pred_obs,pred_time,HM,X)
#'
#' @param pred_obs The observation we would like to predict
#' @param pred_time The time at which the observation should occur
#' @param HM A HMM object
#' @param X The vector containing the data, which the model should be based on.
#' 
#' @details Note that the time we are prediction on, is not the time after the model has started,
#' but after the observed data has been observed. So if there is n observation in the data,
#' and we are looking t into the future we are n+t away form the first hidden state in the model. 
#' 
#' @include HMMclass.R forward.R
#'
#' @return The probability of observing then observation at time T+n
#' @export
#'
forecast <- function(pred_obs,pred_time,HM,X){
  if (!is.null(HM) ){
    trans = HM$transmision
    delta = HM$stationary_dist
    Param = HM$param
    emisf = HM$emission_func
  }
  if (class(HM)[1] !="HMM") {stop("The HM has been build wrong")}
  matrix_m = function(QQ,q){
    if (q<2){return(QQ)} else
    {A = QQ %*% QQ
    return(matrix_m(A,q-1))}
  }
  
  new_trans = matrix_m(trans,pred_time)
  alpha = forward(X = X, HM = HM)
  n = length(X)
  theta = alpha[,length(X)]/sum(alpha[,length(X)])
  emision = c(1:length(delta))
  for (i in c(1:length(delta))){
    emision[i] = do.call(emisf[[i]], c(list(x=pred_obs), Param[[i]])) 
  }
  return(sum(new_trans %*% theta *emision)[1])
}

