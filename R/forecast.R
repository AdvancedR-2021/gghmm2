#' @title forecast
#'
#' @description forecast
#' 
#' @usage forecast(pred_obs,pred_time,data)
#'
#' @param pred_obs 
#' @param pred_time
#' @param HMM A HMM object
#' @param data Data
#' 
#' @include HMMclass.R forward.R
#'
#' @return 
#' @export
#'
forecast <- function(pred_obs,pred_time,HM=NULL,data){
  if (!is.null(HM) ){
    trans = HM$transmision
    delta = HM$stationary_dist
    Param = HM$param
    emisf = HM$emission_func
  }
  matrix_m = function(QQ,q){
    if (q<2){return(QQ)} else
    {A = QQ %*% QQ
    return(matrix_m(A,q-1))}
  }
  
  new_trans = matrix_m(trans,pred_time)
  alpha = forward(data = data, HM = HM)
  theta = alpha[,length(data)]/sum(alpha[,length(data)])
  emision = c(1:length(delta))
  for (i in c(1:length(delta))){
    emision[i] = do.call(emisf[[i]], c(x=pred_ob, Param[[i]])) 
  }
  return(sum(new_trans %*% theta *emision)[1])
}

