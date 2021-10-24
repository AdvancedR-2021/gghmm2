

forwardalgo<- function(delta,trans,emision,X,f){
  m = dim(trans)[1]
  t = length(X)
  Im = matrix(c(0),m,t)
  for (i in c(1:m)){
    Im[i,1] = delta[i] * emision[i,f(X[1])]
  }

  for (i in c(2:t)){
    for (j in c(1:m)){
      Im[j,i] = emision[j,f(X[i])] *sum(Im[,i-1] * trans[,j])

    }
  }
  return(Im)
}

tpm = matrix(c(0.5,0.25,0.5,0.75),2,2)

del = c(1/3 , 2/3)

XX = c(1,1,1)

emis = matrix(c(0.5,0,0.5,1),2,2)
fbinom = function(x){x+1}

AA = forwardalgo(delta = del,trans = tpm, emision = emis,X = XX,f=fbinom)




#Forward_vector ----
forwardalgo<- function(delta,tpm,emision,x,f){
  m = dim(tpm)[1]
  t = length(x)
  mm = matrix(c(0),m,t)
  print(dim(mm))
  for (i in c(1:m)){
    mm[i,1] = delta[i] * emision[i,f(x[1])]
  }

  for (i in c(2:t)){
    for (j in c(1:m)){
      mm[j,i] = emision[j,f(x[i])] *sum(mm[,i-1] * tpm[,j])

    }
  }
  return(mm)
}



forwardalgo<- function(delta,trans,lambda,X){
  m = dim(trans)[1]
  t = length(X)
  Im = matrix(c(0),m,t)
  for (i in c(1:m)){
    Im[i,1] = log(delta[i] * dpois(X[1],lambda = lambda[i]))
  }

  for (i in c(2:t)){
    for (j in c(1:m)){
      Im[j,i] =dpois(X[i],lambda = lambda[j])  *sum(Im[,i-1] * trans[,j])

    }
  }
  return(sum(Im[,ncol(Im)]))
}




tpm = matrix(c(0.5,0.25,0.5,0.75),2,2)

delta = c(1/3 , 2/3)

XX = c(1,1,1)


emis = matrix(c(0.5,0,0.5,1),2,2)
fbinom = function(x){x+1}
AA = forwardalgo(delta = delta,trans = tpm, emision = emis,X = XX,f=fbinom)
print(AA)

#Forward_matrix ----
forward <- function(delta,TPM,param,obs) {
  delta_length = length(delta)
  #print((delta_length))
  obs_length = length(obs)
  #print((obs_length))
  prob_matrix = matrix(1:(delta_length*obs_length),delta_length,obs_length)
  #print(dim(prob_matrix))
  for (i in 1:obs_length) {
    prob_matrix[,i] = dpois(obs[i],param )
  }
  alpha = matrix(1:(delta_length*obs_length),delta_length,obs_length)
  alpha[,1] = delta*prob_matrix[,1]
  # print(dim(prob_matrix))
  for (j in 2:obs_length ) {
    alpha[,j] = prob_matrix[,j] *alpha[,j-1]%*% TPM
  }
  print(alpha)
}





TPM = matrix(c(1/2,1/4,1/2,3/4),2,2)
delta = c(1/2,1/2)
param = c(1,5)
obs = c(1,4,2)
test = forward(delta,TPM,param,obs)

sum(test)
29/48
