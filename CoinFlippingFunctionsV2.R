randomprob = runif(1)
#Coin flipping function
flipper = function(nflips,headprob){
  #creates a logical list nflips long with a probability of generating a 1 of headprob
  return(sample(c(0,1),nflips,replace=TRUE,prob=c((1-headprob),headprob)))
  vec
}
flips = flipper(20000,randomprob)
#finding the likelyhoods of each possibility
likefun = function(flips,thetas){
  return(thetas^(sum(flips))*((1-thetas)^(sum(!flips))))
}

#estimating the value of headprob
coinprob = function(flips,thetas,prior){
  #preallocating data structures
  nflips = length(flips)
  lval = length(thetas)
  prior = rep(1/lval,lval)
  #likelyhood  = matrix(data=NA,nrow = lval,ncol=1)
  likelyhood = matrix(data=NA,nrow = lval,ncol = nflips)
  posterior = matrix(data=NA,nrow = lval,ncol = nflips)

  for (n in 1:nflips){
    
    z = sum(flips[1:n])
      
    likelyhood[1:lval,n] = likefun(flips[1:n],thetas)
     
    if (n == 1){
      posterior[1:lval,n] = likelyhood[1:lval,n]*prior
    }else{
      posterior[1:lval,n] = likelyhood[1:lval,n]*posterior[1:lval,n-1]
    }                                 
    
    posterior[1:lval,n] = posterior[1:lval,n]/sum(posterior[1:lval,n])
    bestguess = thetas[which(posterior[1:lval,n]==max(posterior[1:lval,n]))[[1]]]
    print(paste0("After ", n, " flips, the most likely value of P(heads) is ", bestguess))
    
  }
  bias = vals[which(posterior[1:lval,nflips]==max(posterior[1:lval,nflips]))[[1]]]
}

stepsize = 0.01
thetas = seq(0,1,stepsize)
lval = length(thetas)
prior = rep(1/lval,lval)
bias = coinprob(flips,thetas,prior)
print(bias, digits=5)
