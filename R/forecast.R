#' @title forecast
#'
#' @description 
#' 
#' @usage forecast(HMM,X)
#'
#' @param pred_obs 
#' @param pred_time
#' @param HMM A HMM object
#' @param X  Data
#'
#' @return 
#' @export
#'
forecast <- function(pred_obs,pred_time,HMM=NULL,X ){
  if (!is.null(HMM) ){
    trans = HMM$transmision
    delta = HMM$stationary_dist
    Param = HMM$param
    emisf = HMM$emission_func
  }
  matrix_m = function(QQ,q){
    if (q<2){return(QQ)} else
    {A = QQ %*% QQ
    return(matrix_m(A,q-1))}
  }
  
  new_trans = matrix_m(trans,pred_time)
  alpha = forwardalgo(X,HM)
  theta = alpha[,length(data)]/sum(alpha[,length(data)])
  emision = c(1:length(delta))
  for (i in c(1:length(delta))){
    emision[i] = do.call(emisf[[i]], c(x=pred_ob, PP[[i]])) 
  }
  return(sum(new_trans %*% theta *emision)[1])
}

