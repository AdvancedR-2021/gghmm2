#' @title Viterbi algorithm
#'
#' @description Finds through dynamic programming the most likely sequence of states
#' 
#' 
#' @details Does Global decoding through dynamic programming. It is not feasible to maximize over all possible states
#' because this would run in O(m^t). Thus Viterbi is used as an alternative to this approach. 
#' 
#' 
#' @usage viterbi(HM,X)
#'
#' @param HM A HMM object
#' @param X  Data
#'
#' @return list() Returns a list of the most likely sequence of states.
#' @export
#' @examples 
#' #X = read.table("http://www.hmms-for-time-series.de/second/data/earthquakes.txt")[,2]
#' delta = c(0.5,0.5)
#' lambdaL=c(10,30)
#' trans=matrix(c(0.9,0.1,0.1,0.9),2,2)
#' hm = HMM(initial_dist = delta,transmission = trans,emis_names = c("dpois","dpois"),parameterslist = list(list(lambda =10),list(lambda =30)))
#' viterbi(hm,X)

viterbi = function(HM,X){
  if (!is.null(HM) ){
    trans = HM$transmision
    delta = HM$initial_dist
    param = HM$param
    emisf = HM$emission_func
  }
  if (class(HM)[1] !="HMM") {stop("The HM has been build wrong")}
  t <- length(X)
  m <- length(param)
  epsilon = matrix(0,m,t)
  maxindex = matrix(0,m,t)
  x=X
  for (i in c(1:m)){
    epsilon[i,1] = delta[i] * do.call(emisf[[i]],c(list(x=x[1]),param[[i]]))
  }
  for (i in c(2:t)){
    for (j in c(1:m)){
      temp = epsilon[,i-1] * trans[,j]
      maxindex[j,i] = which.max(temp)
      epsilon[j,i] =do.call(emisf[[j]],c(list(x=x[i]),param[[j]]))
    }
  }
  path = numeric(t)
  path[t] = which.max(epsilon[,t])
  for (i in c((t-1):1)){
    path[i] = maxindex[path[i+1],i+1]
  }
  return(list(path=path,path_prob=max(epsilon[,t])))
}


