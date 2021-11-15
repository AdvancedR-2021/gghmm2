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
#' @usage HMM(stationary_dist,transmission , emission_function_names, parameters)
#'
#' @param stationary_dist  numerical vector of the stationary distribution
#' @param transmision  numerical matrix containing the transmission probabilities 
#' @param emission_function_names vector of names for emission functions 
#' @param parameters  list of list, where each list containing the parameters need in the emision functions
#' @param state_names vector of names for each state 
#'
#' @return A  HMM class object 
#' @import tibble
#' @export
#' @examples 
#' ddnorm = function(x,mean,sd){return(x+mean+sd)}
#' delta = c(0.3,0.7)
#' MM= matrix(c(1,2,3,4),2,2)
#' nn= c("ddnorm","dnorm")
#' parameters = list(list(mean=5,sd=2),list(mean=4,sd=3))
#' HM = HMM(stationary_dist=delta,transmision =MM,emission_function_names=nn,parameters =parameters)
#' params <- tibble(param= list(list(mean=5, sd=4),list(mean=3, sd=2)))
#' PP = HM$param
#' func= HM$emission_func
#' do.call(func[[1]], c(x=1, PP[[1]]))
#' do.call(func[[2]], c(x=1, PP[[2]]))

HMM <- function(stationary_dist=c(0.5,0.5),transmision = matrix(0.5,2,2),
                emission_function_names=c("dnorm","dnorm"),
                parameters =list(list(mean=5,sd=3),list(mean=5,sd=3)) ,state_names = NULL,
                en,...){
  if (is.null(state_names)){
    state_names = c(1:length(stationary_dist))
  }
  emission_function = sapply(emission_function_names,get)
  lisst <- tibble::tibble(stationary_dist=stationary_dist,transmision = transmision ,
                  emission_func=emission_function,emission_function_names=emission_function_names, param = parameters , state_names = state_names)
  class(lisst ) <- c("HMM","tbl_df")
  return(lisst ) 
}

#' @title print.HMM
#'
#' @description This will print the descibtion of the model defined in the HMM object. 
#'
#' @usage print(HMM())
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
