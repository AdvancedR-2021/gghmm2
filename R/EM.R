
forwardalgo<- function(delta,trans,emision,X,f){
  m = dim(trans)[1]
  t = length(X)
  alpha_matrix = matrix(c(0),m,t)
  for (i in c(1:m)){
    alpha_matrix [i,1] = delta[i] * emision[i,f(X[1])]
  }

  for (i in c(2:t)){
    for (j in c(1:m)){
      alpha_matrix [j,i] = emision[j,f(X[i])] *sum(alpha_matrix[,i-1] * trans[,j])

    }
  }
  return(alpha_matrix)
}


backwardalgo<- function(trans,emision,X,f){
  m = dim(trans)[1]
  t = length(X)
  beta_matrix= matrix(1,m,t)
  for (i in rev(c(1:(t-1)))){
    for (j in c(1:m)){
      beta_matrix[j,i] = sum(emision[,f(X[i+1])] *  beta_matrix[,i+1] * trans[j,])

    }
  }
  return( beta_matrix)
}


tpm = matrix(c(0.5,0.25,0.5,0.75),2,2)

del = c(1/3 , 2/3)

XX = c(1,0,0)

emis = matrix(c(0.5,0,0.5,1),2,2)
fbinom = function(x){x+1}

FA = forwardalgo(delta = del,trans = tpm, emision = emis,X = XX,f=fbinom)
BA= backwardalgo(trans = tpm, emision = emis,X = XX,f=fbinom)
FA
BA
colSums(FA)
sum(BA[,1]*del*emis[,fbinom(XX[1])] )








matrix_m = function(X,q){
  if (q<2){return(X)} else
  {A = X %*% X
  return(matrix_m(A,q-1))}
}

#####################################################
poisEM <- function(delta,trans,lambdaL,X){
  m = dim(trans)[1]
  t = length(X)

  forwardalgopois<- function(delta,trans,X,param){
    m = dim(trans)[1]
    t = length(X)
    alpha_matrix = matrix(c(0),m,t)
    for (i in c(1:m)){
      alpha_matrix [i,1] = delta[i] * dpois(X[1],lambda = param[i])
    }

    for (i in c(2:t)){
      for (j in c(1:m)){
        alpha_matrix [j,i] =dpois(X[i],lambda = param[j])  *sum(alpha_matrix[,i-1] * trans[,j])

      }
    }
    return(alpha_matrix)
  }




  backwardalgopois<- function(trans,X,param){
    m = dim(trans)[1]
    t = length(X)
    beta_matrix= matrix(1,m,t)
    for (i in rev(c(1:(t-1)))){
      for (j in c(1:m)){
        beta_matrix[j,i] = sum(dpois(X[i+1],lambda = param) *  beta_matrix[,i+1] * trans[j,])

      }
    }
    return( beta_matrix)
  }



  for (k in c(1:100)){
  #print(k)


  FA = forwardalgopois(delta,trans,X,param=lambdaL)

  BA =backwardalgopois(trans,X,param=lambdaL)

  #sum(BA[,1]*delta*dpois(X[1],lambda = lambdaL))
  #sum(FA[,t])

  likelihood =  sum(BA[,1]*delta*dpois(X[1],lambda = lambdaL))

  Statetrans = matrix(1,m,m)

  for (i in c(1:m)){
    for (j in c(1:m)){
      temp=0
      for (l in c(2:t)){
    temp = temp+ FA[i,(l-1)]*trans[i,j]*dpois(X[l],lambda = lambdaL[j])*BA[j,l]/likelihood }
      Statetrans[i,j] = temp
    }
  }

  delta =   FA[,1]*BA[,1]/likelihood
  col_statetrans = colSums(Statetrans)
  trans= t(t(Statetrans)/colSums(Statetrans))
  lambdaL = c(1:m)
  for (i in c(1:m)){
    lambdaL[i] = sum(X*FA[i,]*BA[i,]/likelihood)/ sum(FA[i,]*BA[i,]/likelihood)
  }
  }

  return(list(delta=delta , trans=trans,lambdaL=lambdaL))
}

X <- read.table("earthquakes.txt")[,2]
delta = c(0.5,0.5)
lambdaL=c(10,30)
trans=matrix(c(0.9,0.1,0.1,0.9),2,2)

Result = poisEM(delta,trans,lambdaL,X)
Result$delta
Result$trans
Result$lambdaL


##################### 3 states #####################################

delta3 = c(1,1,1)/3
lambdaL3=c(10,20,30)
trans3=matrix(c(0.8,0.1,0.1,0.1,0.8,0.1,0.1,0.1,0.8),3,3)

Result = poisEM(delta3,trans3,lambdaL3,X)
Result$delta
Result$trans
Result$lambdaL
