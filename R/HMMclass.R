#' @title The hidden markov model
#'
#' @description A S3 class that can be used to define a hidden markov model, which can then be used by the ther functions in this package.
#' 
#' @details 
#' The user will have to define parameters behind this model. This would be the stationary distribution, transmission matrix and the distribution 
#' of each hidden state with its parameters. The distribution will come as a vector 
#' of strings, where each string is the name of the density function to be used 
#' for that hidden state. The parameters for each hidden state are kept in a list 
#' of list, where each list contain the named parameters for the relevant distribution. 
#' It is important that they are named, as this allow the do.call function to use parameters 
#' in the distribution function. The class has its own print function, which will 
#' print out the parameter for the HHM. The user can use their own density function, 
#' if R does not have  then one they want to use. However then it will be required 
#' that the function is defined in the current environment as the function "get", 
#' is used to get the function. Here it is important to note that if the user specify their own
#' distribution function, then it is necessary for the quantile parameter to be called x. 
#'
#' @usage HMM(stationary_dist,transmision , emission_function_names, parameters)
#'
#' @param stationary_dist  numerical vector of the stationary distribution
#' @param transmision  numerical matrix containing the transmission probabilities 
#' @param emission_function_names vector of names for emission functions 
#' @param parameters  list of list, where each list containing the parameters need in the emission functions
#' @param state_names vector of names for each state 
#'
#' @return A HMM class object 
#' @import tibble
#' @export
#' @examples 
#' X <- earthquakes$n
#' delta = c(0.5,0.5)
#' lambdaL=c(10,30)
#' trans=matrix(c(0.9,0.1,0.1,0.9),2,2)
#' HM = HMM(stationary_dist = delta,transmision = trans,  
#' emission_function_names = c("dpois","dpois"),parameters = list(list(lambda=10),
#' list(lambda=30)) )
#' HM

HMM <- function(stationary_dist,transmision,
                emission_function_names,parameters  ,state_names = NULL,...){
  if (is.null(state_names)){
    state_names = c(1:length(stationary_dist))
  }
  emission_function = sapply(emission_function_names,get)
  lisst <- tibble::tibble(stationary_dist=stationary_dist,transmision = transmision ,
                  emission_func=emission_function,
                  emission_function_names=emission_function_names, 
                  param = parameters , state_names = state_names)
  class(lisst ) <- c("HMM","tbl_df")
  return(lisst ) 
}

#' @title print.HMM
#'
#' @description This will print the descibtion of the model defined in the HMM object. 
#'
#' @usage print(HMM)
#'
#' @param HMM A HMM class object.
#'
#' @return A  HMM class object 
#' @import tibble
#' @export
print.HMM <- function(HMM){
  if (!is.null(HMM) ){
    trans = HMM$transmision
    delta = HMM$stationary_dist
    Param = HMM$param
    emisf = HMM$emission_func
    emisnames = HMM$emission_function_names
    name_of_state = HMM$state_names
  }
  
  print(paste("This is a Hidden Markov model with", length(delta), "hidden states."))
  print("It has the Statinonary distribution of:")
  print(delta)
  print("It has the transmision matrix:")
  print(trans)
  for (i in c(1:length(delta))){
    PP = unlist(strsplit( paste(Param[i]),"[()]"))[2]
    print(paste( "State" ,name_of_state[i]," has the emission function", emisnames[i],"with parameters", PP))
  } 
  
  
}
