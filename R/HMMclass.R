#' @title HMM
#'
#' @description Will make a object containing the parameters of a hidden markov model. 
#'
#' @usage HMM(stationary_dist,transmission , emission_function, parameters)
#'
#' @param stationary_dist  numerical vector of the stationary distribution
#' @param transmision  numerical matrix containing the transmission probabilities 
#' @param emission_function vector of emission functions 
#' @param parameters  list of list, where each list containing the parameters need in the emision functions
#' @param state_names vector of names for each state 
#'
#' @return A  HMM class object 
#' @export
#'
#'
HMM <- function(stationary_dist=c(0.5,0.5),transmision = matrix(0.5,2,2),
                emission_function=c(dnorm,dnorm),
                parameters =list(list(mean=5,sd=3),list(mean=5,sd=3)) ,state_names = NULL,
                ...){
  lisst <- tibble(stationary_dist=stationary_dist,transmision = transmision ,
                  emission_func=emission_function, parm = parameters , state_names = state_names)
  class(lisst ) <- c("HMM","tbl_df")
  return(lisst ) 
}

#params <- tibble(param= list(list(mean=5, sd=4),list(mean=3, sd=2)))
#PP = params$param
#do.call(dnorm, c(x=1, PP[[1]]))

