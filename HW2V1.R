## Problem 4.1 ##

#Displays the built in data
show( HairEyeColor ) # Show data
#basically adds the male and female tables together to get the joint occurences 
#of each set of properties
EyeHairFreq = apply( HairEyeColor, c("Eye","Hair"), sum ) # Sum across sex
#finds proportion of individuals with each set of properties
EyeHairProp = EyeHairFreq / sum( EyeHairFreq ) # joint proportions, Table 4.1
#displays the above proportions, rounded to two decimal places
show( round( EyeHairProp , 2 ) )
#computes total occurence of each hair color
HairFreq = apply( HairEyeColor , c("Hair") , sum ) # Sum across sex and eye
#computes proportion of total individuals with each hair color
HairProp = HairFreq / sum( HairFreq ) # marginal proportions, Table 4.1
#displays the above proportion, rounded to two decimal places
show( round( HairProp , 2 ) )
#computes total occurence of each eye color
EyeFreq = apply( HairEyeColor , c("Eye") , sum ) # Sum across sex and eye
#computes proportion of total individuals with each eye color
EyeProp = EyeFreq / sum( EyeFreq ) # marginal proportions, Table 4.1
#displays the above proportion, rounded to two decimal places
show( round( EyeProp , 2 ) )
#computes the probabilities that an individual would have a specific hair color given they
#have blue eyes
EyeHairProp["Blue",] / EyeProp["Blue"] # conditional prob, Table 4.2
#same as above, but given brown eyes
EyeHairProp["Brown",] / EyeProp["Brown"]
#Eye color probabilities given Brown hair
EyeHairProp[,"Brown"] / HairProp["Brown"]

## Problem 4.2 ##

N = 500 # Specify the total number of flips, denoted N.
#Edit one: pHeads from 0.5 to 0.8
pHeads = 0.8 # Specify underlying probability of heads.
# Generate a random sample of N flips (heads=1, tails=0):
flipSequence = sample( x=c(0,1), prob=c(1-pHeads,pHeads), size=N, replace=TRUE)
# Compute the running proportion of heads:
r = cumsum( flipSequence ) # Cumulative sum: Number of heads at each step.
n = 1:N # Number of flips at each step.
runProp = r / n # Component by component division.
# Graph the running proportion:
plot( n , runProp , type="o" , log="x" , col="skyblue" ,
      xlim=c(1,N) , ylim=c(0.0,1.0) , cex.axis=1.5 ,
      xlab="Flip Number" , ylab="Proportion Heads" , cex.lab=1.5 ,
      main="Running Proportion of Heads" , cex.main=1.5 )
#Reference line is set to sit on pHeads by default, no change
# Plot a dotted horizontal reference line:
abline( h=pHeads , lty="dotted" )
# Display the beginning of the flip sequence:
flipLetters = paste( c("T","H")[flipSequence[1:10]+1] , collapse="" )
displayString = paste0( "Flip Sequence = " , flipLetters , "..." )
text( N , .9 , displayString , adj=c(1,0.5) , cex=1.3 )
# Display the relative frequency at the end of the sequence.
text( N , .8 , paste("End Proportion =",runProp[N]) , adj=c(1,0.5) , cex=1.3 )


## Problem 4.3 ##

#total cards
Ntot = 48
#total # possible vals
nvals = 6
#number of suits
nsuits = 4
#occurence of each card
ntimes = 2
#sanity check to make sure my math isn't terrible
if (nvals*nsuits*ntimes != Ntot)
  show("Yer a retard, 'arry!")
#occurence of any given number(or face)
nnum = nsuits*ntimes
#probability of getting any given number or face
pnum = nnum/Ntot
#printing results
print(paste0("The probability of getting any given number, including ten, is ", pnum))
print(paste0("The probability of getting a ten or a jack (or any two different numbers is ) ", 2*pnum))

## Problem 4.4 ##

## Part a)
#Had to google the name of the file given in the book, got it off author's U of Indiana website

# Graph of normal probability density function, with comb of intervals.
#meanval = 0.0             # Specify mean of distribution.
#sdval = 0.2               # Specify standard deviation of distribution.

#New Max and Min
xlow  = 0 # Specify low end of x-axis.
xhigh = 1 # Specify high end of x-axis.
#running a smaller dx because of small interval
dx = 0.005                 # Specify interval width on x-axis
# Specify comb points along the x axis:
x = seq( from = xlow , to = xhigh , by = dx )
# Compute y values, i.e., probability density at each value of x:
y = 6*x*(1-x)
# Plot the function. "plot" draws the intervals. "lines" draws the bell curve.
plot( x , y , type="h" , lwd=1 , cex.axis=1.5
      , xlab="x" , ylab="p(x)" , cex.lab=1.5
      , main="Updated Function" , cex.main=1.5 ) #changed axis label
lines( x , y )
# Approximate the integral as the sum of width * height for each interval.
area = sum( dx * y )
# Display info in the graph.
text( -sdval , .9*max(y) , bquote( paste(mu ," = " ,.(meanval)) )
      , adj=c(1,.5) )
text( -sdval , .8*max(y) , bquote( paste(sigma ," = " ,.(sdval)) )
      , adj=c(1,.5) )
text( sdval , .9*max(y) , bquote( paste(Delta , "x = " ,.(dx)) )
      , adj=c(0,.5) )
text( sdval , .8*max(y) ,
      bquote(
        paste( sum(,x,) , " " , Delta , "x p(x) = " , .(signif(area,3)) )
      ) , adj=c(0,.5) )

## Part b)
#I don't know how else to write this succinctly so this will roughly be mathematica script
# <x> = Integrate[x*P(x), {x,0,1}] where P(x) is given as 6x(1-x)

## Part c)
#Yes, the integral of P(x) computes to 1 on the interval of 0<x<1

## Part d)
# Looks like 1.5


## Problem 4.5 ##

## Part a)

# Graph of normal probability density function, with comb of intervals.
meanval = 0.0             # Specify mean of distribution.
sdval = 0.2               # Specify standard deviation of distribution.
xlow  = meanval - sdval # Specify low end of x-axis.
xhigh = meanval + sdval # Specify high end of x-axis.
dx = 0.02                 # Specify interval width on x-axis
# Specify comb points along the x axis:
x = seq( from = xlow , to = xhigh , by = dx )
# Compute y values, i.e., probability density at each value of x:
y = ( 1/(sdval*sqrt(2*pi)) ) * exp( -.5 * ((x-meanval)/sdval)^2 )
# Plot the function. "plot" draws the intervals. "lines" draws the bell curve.
plot( x , y , type="h" , lwd=1 , cex.axis=1.5
      , xlab="x" , ylab="p(x)" , cex.lab=1.5
      , main="Normal Probability Density" , cex.main=1.5 )
lines( x , y )
# Approximate the integral as the sum of width * height for each interval.
area = sum( dx * y )
# Display info in the graph.
text( -sdval , .9*max(y) , bquote( paste(mu ," = " ,.(meanval)) )
      , adj=c(0.5,.5) )
text( -sdval , .8*max(y) , bquote( paste(sigma ," = " ,.(sdval)) )
      , adj=c(0.5,.5) )
text( sdval , .9*max(y) , bquote( paste(Delta , "x = " ,.(dx)) )
      , adj=c(0,.5) )
text( sdval , .8*max(y) ,
      bquote(
        paste( sum(,x,) , " " , Delta , "x p(x) = " , .(signif(area,3)) )
      ) , adj=c(0,.5) )


## Part b) 

# The mean is obviously 162, and roughly two thirds of the area under a normal curve is within one
#standard deviation of the mean, so sigma should be about 162-147 = 15


## Problem 4.6 ##
#write in data as matrix with correct dimensions
grade <- matrix(c(0.2,0.2,0.6),ncol=3,byrow=TRUE)
#label columns and rows
colnames(grade) <- c("1st","6th", "11th")
rownames(grade) <- c("Sample%")
#Convert from matrix to table
grade <- as.table(grade)
#display
grade

#second verse, same as the first
food <- matrix(c(0.3,0.6,0.1,0.6,0.3,0.1,0.3,0.1,0.6),ncol=3,byrow=TRUE)
colnames(food) <- c("Ice Cream","Fruit","French Fries")
rownames(food) <- c("1st","6th", "11th")
food <- as.table(food)
food
#each entry in the joint table is the probability of being in that grade times the probability of 
#having that food as your favorite given you are in that grade
joint <- matrix(c(grade*food["1st",],grade*food["6th",],grade*food["11th",]),ncol=3,byrow=TRUE)
colnames(joint) <- c("Ice Cream","Fruit","French Fries")
rownames(joint) <- c("1st","6th", "11th")
joint <- as.table(joint)
joint
#total is the total probability of being in that grade (it doesn't quite sum to one because precision)
total = colSums(joint)
total
#dif is the diferrence between the the overall probability of having an item as a favorite food
#and the probability of having it as a favorite given a specific grade
#this being nonzero shows that favorite food and grade are not independent
dif <- matrix(c(total-food["1st",],total-food["6th",],total-food["11th",]),ncol=3,byrow=TRUE)
colnames(dif) <- c("Ice Cream","Fruit","French Fries")
rownames(dif) <- c("1st","6th", "11th")
dif <- as.table(dif)
dif

## Problem 5.1 ##

#new prior
psad = 0.019
#negative given sad
pneggsad = 0.01
#sum of ways you get a negative test
sumpneg = (1-0.99)*0.019 + 0.95*(1-0.019)
#Plug it in
posterior = (pneggsad*psad)/sumpneg
posterior

## Problem 5.2 ##

#Part a)

#defining parameters
hit = 0.99
fneg = 0.05
psad = 0.001
population = 100000

#defining relations
pposgsad = hit
pneggsad = (1-hit)
pposghappy = fneg
pnegghappy = (1-fneg)

#doing the math
sadpos = pposgsad*psad
sadneg = pneggsad*psad
happypos = pposghappy*(1-psad)
happyneg = pnegghappy*(1-psad)

#making the table of probabilities
table1 = matrix(c(sadpos,happypos,sadneg,happyneg,psad,(1-psad)),ncol=2,byrow=TRUE)
colnames(table1) <- c("=(","=)")
rownames(table1) <- c("positive","negative","marginal")
table1 = as.table(table1)
table1
#probabilities times population
table2 = table1*population
table2

#Part b)

#supposed to guess first... Guess about 1/50
#take number of true positives over total number of positives
trueposrat = table2[1,1]/(table2[1,1]+table2[1,2])
trueposrat

#Part c) 
#numbers from problem
nsick = 10000
nhealthy = 9990000
#conditionals all hinge on the reliability numbers from the test
npossick = nsick*hit
nnegsick = npossick*(1-hit)
nposhealthy = nhealthy*(fneg)
nneghealthy = nposhealthy*(1-fneg)

table3 = matrix(c(npossick,nposhealthy,nnegsick,nneghealthy),ncol=2,byrow=TRUE)
colnames(table3) <- c("=(","=)")
rownames(table3) <- c("positive","negative")
table3 = as.table(table3)
table3

#they called this a Markov representation, is there a way to do this with matrices?

## Part d)

proportion = table3[2,1]/(table3[2,1]+table3[2,2])
proportion
#comes out to almost the answer from 5.1, I assume the error is from machine precision

## Problem 5.3 ##

## Part a)
#parameters
psad = 0.001
hit = 0.99
fneg = 0.05
#Bayes theorem
psickgneg = (1-hit)*psad/((1-hit)*psad+(1-fneg)*(1-psad))
psickgneg

## Part b)
#pretty much the same, just carrying the result from the previous test through
psad2 = psickgneg
psickgneg2 = (hit)*psad2/((hit)*psad2+(fneg)*(1-psad2))
psickgneg2
#Hey look, answer to 5.1 again

## Problem 5.4 ##


graphics.off()
source("openGraphSaveGraph.R") # for openGraph() function, used below.
source("BernGrid.R")
#I wrote this in after looking at his blog, his graph opening code needs it.
source("DBDA2E-utilities.R")

# For Figure 6.4:
# Specify theta values.
thetagrid = seq(0,1,length=1001)
# Specify probability mass at each theta value.
relprob = sin( 2*pi*thetagrid )^6
prior = relprob / sum(relprob) # probability mass at each theta
# Specify the data vector.
datavec = c( rep(1,2) , rep(0,1) ) 
# Open a window.
openGraph(width=7,height=10,mag=0.7)
# Call the function.
posterior = BernGrid( Theta=thetagrid , pTheta=prior , Data=datavec )
saveGraph(file="Fig.6.4",type="jpg")

# For Figure 6.5:
pTheta = c( 50:1 , rep(1,50) , 1:50 , 50:1 , rep(1,50) , 1:50 )
pTheta = pTheta / sum( pTheta )
width = 1 / length(pTheta)
Theta = seq( from = width/2 , to = 1-width/2 , by = width )
dataVec = c( rep(1,3) , rep(0,1) )
openGraph(width=7,height=10,mag=0.7)
posterior = BernGrid( Theta=Theta , pTheta=pTheta , Data=dataVec )
saveGraph(file="Fig.6.5left",type="jpg")

dataVec = c( rep(1,12) , rep(0,4) )
openGraph(width=7,height=10,mag=0.7)
posterior = BernGrid( Theta=Theta , pTheta=posterior , Data=dataVec )
saveGraph(file="Fig.6.5right",type="jpg")


#So I guess I'm just supposed to talk about these?
#In each case I'm given a prior distribution, a distribution based on data, and the prior distribution
#The intent seems to be to demonstrate that data has different effects on the prior distribution
#depending on where it sits and how good (how big and noisy) the data is.
#I'm not super sure what else to say about it.



