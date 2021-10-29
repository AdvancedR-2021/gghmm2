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
forecast <- function(pred_obs,pred_time,HMM,X ){
  
  matrix_m = function(QQ,q){
    if (q<2){return(QQ)} else
    {A = QQ %*% QQ
    return(matrix_m(A,q-1))}
  }
  

  new_trans = matrix_m(trans,pred_time)
  alpha = forwardalgopois(delta=delta,trans=trans,x=data,param=param)
  theta = alpha[,length(data)]/sum(alpha[,length(data)])
  return(sum(new_trans %*% theta * dpois(x =pred_obs,lambda = param) )[1])
}

