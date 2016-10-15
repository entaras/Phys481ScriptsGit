## Problem 6.1 ##

## Part a)

source("DBDA2E-utilities.R") # Load definitions of graphics functions etc.
source("BernBeta.R") # Load the definition of the BernBeta function
# Specify the prior:
t = 0.5 # Specify the prior mode.
n = 8 # Specify the effective prior sample size.
a = t*(n-2) + 1 # Convert to beta shape parameter a.
b = (1-t)*(n-2) + 1 # Convert to beta shape parameter b.
Prior = c(a,b) # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 1 # The total number of flips.
z = 1 # The number of heads.
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0's and 1's.
posterior = BernBeta( priorBetaAB=Prior, Data=Data )
print(paste0("Posterior beta distribution has values a = ", posterior[[1]],",b = ", posterior[[2]]))

## Part b) 

Prior = posterior
N = 1 # The total number of flips.
z = 1 # The number of heads.
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0's and 1's.
posterior = BernBeta( priorBetaAB=Prior, Data=Data )
print(paste0("Posterior beta distribution has values a = ", posterior[[1]],",b = ", posterior[[2]]))

## Part c)

Prior = posterior
N = 1 # The total number of flips.
z = 0 # The number of heads.
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0's and 1's.
posterior = BernBeta( priorBetaAB=Prior, Data=Data )
print(paste0("Posterior beta distribution has values a = ", posterior[[1]],",b = ", posterior[[2]]))

postrun1 = posterior

## Part d)
t = 0.5 # Specify the prior mode.
n = 8 # Specify the effective prior sample size.
a = t*(n-2) + 1 # Convert to beta shape parameter a.
b = (1-t)*(n-2) + 1 # Convert to beta shape parameter b.
Prior = c(a,b) # Specify Prior as vector with the two shape parameters.
N = 1 # The total number of flips.
z = 0 # The number of heads.
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0's and 1's.
posterior = BernBeta( priorBetaAB=Prior, Data=Data )
print(paste0("Posterior beta distribution has values a = ", posterior[[1]],",b = ", posterior[[2]]))

Prior = posterior
N = 1 # The total number of flips.
z = 1 # The number of heads.
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0's and 1's.
posterior = BernBeta( priorBetaAB=Prior, Data=Data)
print(paste0("Posterior beta distribution has values a = ", posterior[[1]],",b = ", posterior[[2]]))

Prior = posterior
N = 1 # The total number of flips.
z = 1 # The number of heads.
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0's and 1's.
posterior = BernBeta( priorBetaAB=Prior, Data=Data )
print(paste0("Posterior beta distribution has values a = ", posterior[[1]],",b = ", posterior[[2]]))

dif = postrun1-posterior

# The posteriors appear to be order invariant.

## Problem 6.2 ##

# For this test candidate A is a "success".

## Part a)

t = 0.5 # Specify the prior mode.
n = 2 # Specify the effective prior sample size.
a = t*(n-2) + 1 # Convert to beta shape parameter a.
b = (1-t)*(n-2) + 1 # Convert to beta shape parameter b.
Prior = c(a,b) # Specify Prior as vector with the two shape parameters.
N = 100
z = 58
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0's and 1's.
posterior = BernBeta( priorBetaAB=Prior, Data=Data, plotType="Bars" ,
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE)


#I feel like there should be a way to automate printing the HDI, but as far as I can tell from searching chapters 
#5 and 6, and from looking at his function, he only ever overlays it on his plots, and I don't really feel motivated to edit 
#the author's function.

#The HDI on the plot is 0.483 to 0.673

## Part b)

Prior = posterior
N = 100
z = 57
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0's and 1's.
posterior = BernBeta( priorBetaAB=Prior, Data=Data, plotType="Bars" ,
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE)

#The HDI on the plot is 0.506 to 0.642

## Problem 6.3 ##

#For this problem I'm calling F a "success".


t = 0.5 # Specify the prior mode.
n = 2 # Specify the effective prior sample size.
a = t*(n-2) + 1 # Convert to beta shape parameter a.
b = (1-t)*(n-2) + 1 # Convert to beta shape parameter b.
Prior = c(a,b) # Specify Prior as vector with the two shape parameters.
N = 50
z = 40
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0's and 1's.
posterior = BernBeta( priorBetaAB=Prior, Data=Data, plotType="Bars" ,
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE)
# The HDI runs from 0.677 to 0.893, so a clear preference.


t = 0.5 # Specify the prior mode.
n = 2 # Specify the effective prior sample size.
a = t*(n-2) + 1 # Convert to beta shape parameter a.
b = (1-t)*(n-2) + 1 # Convert to beta shape parameter b.
Prior = c(a,b) # Specify Prior as vector with the two shape parameters.
N = 50
z = 15
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0's and 1's.
posterior = BernBeta( priorBetaAB=Prior, Data=Data, plotType="Bars" ,
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE)

#The HDI runs from 0.187 to 0.433, still significant, but less pronounced.

## Problem 6.4 ##


a = 0.1 # Convert to beta shape parameter a.
b = 0.1 # Convert to beta shape parameter b.
Prior = c(a,b) # Specify Prior as vector with the two shape parameters.
N = 5
z = 4
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0's and 1's.
posterior = BernBeta( priorBetaAB=Prior, Data=Data, plotType="Bars" ,
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE)
print(paste0("Posterior beta distribution has values a = ", posterior[[1]],",b = ", posterior[[2]]))



## Problem 6.5 ##


## Part a)

#The problem specifies a strong prior peaked at 0.5, so large equal values of a and b seemed appropriate
a = 10 # Convert to beta shape parameter a.
b = 10 # Convert to beta shape parameter b.
Prior = c(a,b) # Specify Prior as vector with the two shape parameters.
N = 10
z = 9
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0's and 1's.
posterior = BernBeta( priorBetaAB=Prior, Data=Data, plotType="Bars" ,
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE)
#Here I'm taking my new probability of heads to be the mode of the posterior distribution after my ten flips,
#since the mode is the most likely value.
tprime = (posterior[[1]]-1)/(sum(posterior)-2)

print(paste0("The probability of getting heads on the next flip is ",tprime))

## Part b)


#The book specifically tells you to use this prior, right after it asks you to justify your prior...
#We're just assuming that it is a super not fair coin.
a = 0.1 # Convert to beta shape parameter a.
b = 0.1 # Convert to beta shape parameter b.
Prior = c(a,b) # Specify Prior as vector with the two shape parameters.
N = 10
z = 9
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0's and 1's.
posterior = BernBeta( priorBetaAB=Prior, Data=Data, plotType="Bars" ,
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE)

tprime = (posterior[[1]]-1)/(sum(posterior)-2)

print(paste0("The probability of getting heads on the next flip is ",tprime))
