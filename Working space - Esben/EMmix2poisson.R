Y <- read.table("earthquakes.txt")[,2]

delta = 0.4

lambda1 = 5

lambda2=15
n = length(Y)

deltaold = 1000

lambda1old = 1000

lambda2old = 1000
i = 0

while (T) {
 cond = (delta*dpois(Y,lambda1))/(delta*dpois(Y,lambda1)+(1-delta)*dpois(Y,lambda2))

delta = sum(cond)/n

lambda1 = sum(cond*Y)/sum(cond)

lambda2 = sum(Y*(1-cond))/sum(1-cond)
if (abs(delta-deltaold)/deltaold <0.01 && abs(lambda1old - lambda1)/lambda1old <0.01 && abs(lambda2old-lambda2)/lambda2old<0.01){break}
deltaold = delta

lambda1old = lambda1

lambda2old = lambda2
i = i+1
}

print(c(delta, lambda1, lambda2,i))

