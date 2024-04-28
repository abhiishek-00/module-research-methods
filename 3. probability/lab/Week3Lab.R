install.packages("gtools")
library(gtools)

#Question 1
#Generate values between 100 and 200cm - randomly sample 500 of them
vals <-rnorm(500,163,9)
print(vals)
heightVals <- sample(seq(from=100,to=200,length.out=1000),500)
plot(heightVals,dnorm(heightVals,mean=163,sd=9))

#Generate values for bone mineral density
#This gets 1000 values at increments from -4.0 to 4.0
boneVals <-seq(from=-4,to=4,length.out=1000)
#No mean/sd parameters needed for standard normal distribution
plot(boneVals,dnorm(boneVals),type="l") 
                    
#Bus waiting times - generate some data from -10 to 25
timeVals <-seq(from=-10,to=25,length.out=1000)
print(timeVals)
#Plot a uniform distribution where everything from 0-15 has the same probability
plot(timeVals,dunif(timeVals,min=0,max=15),lwd=3,ylim=c(0,.1),type="l")

#Coin toss values using a binomial distribution
barplot(height=dbinom(0:20,size=20,p=0.5),names.arg=0:20, xlab="X",ylab="Probability")

#Find probability that the standard normal distribution is >-1.00
#(pnorm calculates probability that the variable is LESS than the given value)
goodDensity <- 1- pnorm(-1.00)
print(sprintf("Probability of good bone density is %f",goodDensity))

#Find probability of osteopenia (between two values)
upperLimit <- pnorm(-1.00)
lowerLimit <- pnorm(-2.50)
print(sprintf("Probability of osteopenia is %f",upperLimit-lowerLimit))

#Find probability of osteoporosis (< -2.50)
print(sprintf("Probability of osteoporosis is %f",lowerLimit))

#95th percentile
ninetyFifth <- qnorm(0.95)
print(sprintf("95th percentile is %f",ninetyFifth))

#Value that 80% is over (i.e., 20th percentile_
twentieth <- qnorm(0.2)
print(sprintf("4/5ths of people have bone density higher than %f",twentieth))

#Question 3
#a) Returns the number of threes in a vector
getThrees <- function(n){
  length(n[n == 3])
}

#Permutations here gets all the ways in which we can arrange 6 different
#values (i.e., dice rolls) that have 6 possible values. We say repeats are allowed
#can come up more than once, or never at all.
combos <-permutations(6,6,repeats.allowed=TRUE)
threes <- apply(combos,1,getThrees)
print(threes)
print(sprintf("Mean number of threes is %f",mean(threes)))
print(sprintf("Max is %f",max(threes)))
print(sprintf("Variance is %f",sd(threes)^2))
print(sprintf("SD is %f",sd(threes)))

#Dice roll value probability plot
table <- table(threes) #frequency table
prob_table <- table / sum(table) #probability table
barplot(prob_table)

#Binomial distribution - can just use this line instead!
barplot(height=dbinom(0:6,size=6,p=0.16),names.arg=0:6, xlab="X",ylab="Probability")
