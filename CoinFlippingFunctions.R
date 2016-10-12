flipper = function(nflips,headprob){
  vec = sample(c(0,1),nflips,replace=TRUE,prob=c((1-headprob),headprob))
  vec
}
flips = flipper(20000,.7769)

coinprob = function(flips,stepsize){
  
  nflips = length(flips)
  vals = seq(0,1,stepsize)
  lval = length(vals)
  prior = rep(1/lval,lval)
  likelyhood  = matrix(data=NA,nrow = lval,ncol = length(flips))
  posterior = likelyhood
  
  for (n in 1:nflips){
    
    z = sum(flips[1:n])
    
    for (i in 1:lval){
      
    likelyhood[i,n] = (vals[i]^(z))*(1-vals[i])^(n-z)
    if (n == 1)
      posterior[i,n] = likelyhood[i,n]*prior[i]
    else
      posterior[i,n] = likelyhood[i,n]*posterior[i,n-1]
                                      
    }
    
    posterior[1:lval,n] = posterior[1:lval,n]/sum(posterior[1:lval,n])
    bestguess = vals[which(posterior[1:lval,n]==max(posterior[1:lval,n]))[[1]]]
    print(paste0("After ", n, " flips, the most likely value of P(heads) is ", bestguess))
    
  }
  bias = vals[which(posterior[1:lval,nflips]==max(posterior[1:lval,nflips]))[[1]]]
}

bias = coinprob(flips,0.0001)
print(bias, digits=5)
