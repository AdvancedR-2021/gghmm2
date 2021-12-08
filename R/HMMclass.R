#' @title The hidden markov model
#'
#' @description A S3 class that can be used to define a hidden markov model, which can then be used by the ther functions in this package.
#' 
#' @details 
#' The user will have to define parameters behind this model. This would be the initial distribution, transmission matrix and the distribution 
#' of each hidden state with its parameters. The distribution will come as a vector 
#' of strings, where each string is the name of the density function to be used 
#' for that hidden state. If only one string is defined, then that one will be used as the distribtuion for all the hidden states.
#' 
#' The user has two ways to define what parameters the density functions will use. If the density only need up 
#' to two different parameters, then the variables parm1 and parm2 can be used. They can either be a vector where each value will be a parameter
#' in the corresponding distribution or if the same value should be a parameter in each of the distribution then a single value will work.  
#' 
#' Alternativly the user can also give the parameters in a list of lists, where the internal list each contain all the parameters needed for the corresponding distribution.
#' 
#' 
#' The class has its own print function, which will 
#' print out the parameter for the HHM. The user can use their own density function, 
#' if R does not have then one they want to use. However then it will be required 
#' that the function is defined in the current environment. To ensure this it may 
#' be necessary for the users custom function to be defined in the same 
#' environment as the HMM function. 
#'  
#' Here it is important to note that if the user specify their own
#' distribution function, then it is necessary for the quantile parameter to be called x. 
#'
#' 
#'
#' @usage HMM(initial_dist,transmission , emis_names, parameterlist)
#'
#' @param initial_dist  numerical vector of the stationary distribution
#' @param transmission  numerical matrix containing the transmission probabilities 
#' @param emis_names vector of names for emission functions 
#' @param parameterlist  list of list, where each list containing the parameters needed in the corresponding emission function
#' @param state_names vector of names for each state 
#' @param parm1 vector of parameters
#' @param parm2 vector of parameters 
#'
#' @return A HMM class object 
#' @import tibble
#' @export
#' @examples 
#' X <- earthquakes$n
#' delta = c(0.5,0.5)
#' trans=matrix(c(0.9,0.1,0.1,0.9),2,2)
#' HM = HMM(initial_dist = delta,transmission = trans,  
#' emis_names = "dpois",parameterslist = list(list(lambda=10),
#' list(lambda=30)) )
#' HM

HMM <- function(initial_dist, 
                transmission,
                emis_names,parameterlist=NULL, nparams=NULL,state_names = NULL,
                parm1 = NULL,parm2=NULL,...){
  if (is.null(state_names)){
    state_names = c(1:length(initial_dist))
  }
  if (!is.vector(initial_dist) | sum(initial_dist)!= 1) {stop("The initial distribution needs to be a vector and sum to 1")}
  if (!is.matrix(transmission)) {stop("The transmission matrix needs to be a matrix")}
  if (is.null(parameterlist) & is.null(parm1) & is.null(parm2) ){ stop("The distributions needs parameterlist to work")
    }
  d1 = length(initial_dist)
  d2= dim(transmission)
  d3 = length(emis_names)
  if (d2[1] != d2[2]) {stop("The transmission matrix need to be a Square matrix")}
  if (d1 != d2[2]) {stop("The transmission matrix and the vector of initial distribution need to have the same dimensions")}
  if (d3==1 & d1>1){
    
    emis_names= rep(emis_names,d1)
  }
  d3 = length(emis_names)
  if (d1 != d3) {stop("The number of emission function need to be the same as the number of initial distributions " )}
 
  if (!is.null(parameterlist)){
    param= parameterlist
  } else{
    if (!is.null(parm1) & length(parm1)==1){
      parm1= rep(parm1,d1)
    }
    if (!is.null(parm2) & length(parm2)==1){
      parm1= rep(parm2,d1)
    }
    if (!is.null(parm1) & d1 != length(parm1)) {stop("There need to be as many parameters in parm1 as there are distributions.
                                                    If you want to use different distributions with diferent numbers of paramets
                                                   you will need to use the parameterlist argument." )}
    
    param= list()
    if (!is.null(parm1) & !is.null(parm2)){
     for (i in c(1:d1)){
      param[[i]] = list(parm1[i],parm2[i]) 
    }
    } else if (!is.null(parm1) & is.null(parm2)){
      for (i in c(1:d1)){
        param[[i]] = list(parm1[i]) 
      } 
      
    } else{
      for (i in c(1:d1)){
        param[[i]] = list(parm2[i]) 
      } 
    }
  }
  d4 = length(param)
  emission_function = sapply(emis_names,get)
  lisst <- tibble::tibble(initial_dist=initial_dist,transmision = transmission ,
                  emission_func=emission_function,
                  emis_names=emis_names, 
                  param = param , state_names = state_names)
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
    delta = HMM$initial_dist
    Param = HMM$param
    emisf = HMM$emission_func
    emisnames = HMM$emis_names
    name_of_state = HMM$state_names
  }
  
  print(paste("This is a Hidden Markov model with", length(delta), "hidden states."))
  print("It has the initial distirbution of:")
  print(delta)
  print("It has the transmision matrix:")
  print(trans)
  for (i in c(1:length(delta))){
    PP = unlist(strsplit( paste(Param[i]),"[()]"))[2]
    print(paste( "State" ,name_of_state[i]," has the emission function", emisnames[i],"with parameters", PP))
  } 
  
  
}
