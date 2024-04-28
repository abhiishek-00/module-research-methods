
#An R script that performs the functions from the Week 2 lab sheet
nileData <- Nile

#`a) Histograms
hist(nileData,
     breaks=5, #with 5 columns
     col='blue') 
hist(nileData,
     breaks=10, #with 10 columns
     col='blue')

#1b) Boxplot. (For some reason, range has to be set to be less than 1.5
#    to show the outliers. 
boxplot(nileData,col="green",range=1)

#1c) Descriptive statistics
print("Min, Max, Mean and Standard Deviation for Nile Data:")
print(paste(min(nileData),max(nileData), mean(nileData), sd(nileData), sep=", "))

#1d) Random sampling the 'efficient' way

#A function that takes a value n and returns a random sample
#from the Nile dataset of size n
getSample <- function(n) {
  sample(nileData,n)
}

#A vector of the sample sizes we're interested in
sizes <- c(5,10,20,50)

#Use 'sapply' to get a list containing our 4 random samples of different sizes
mySamples <- sapply(sizes,getSample)

#Now repeat with this new list to get a list of means, medians, and SDs
means <-sapply(mySamples,mean)
medians <-sapply(mySamples,median)
sds <- sapply(mySamples,sd)

#Finally, combine these into a single dataframe
randomSamples<- data.frame(means,medians,sds)

#1e) The same process but using the 'getFirst' function to return the first
#    n values of each sample. Note there is repetition here
getFirst <- function(n){
  nileData[1:n]
}
firstSamples <-sapply(sizes,getFirst)

means <-sapply(firstSamples,mean)
medians <-sapply(firstSamples,median)
sds <- sapply(firstSamples,sd)

firstData<- data.frame(means,medians,sds)

#1f) A function to calculate outliers given a value n and its dataset. 
#   This uses the IQR function to get the interquartile range, and then
#   the five-number summary (using fivenum) to get Q1 and Q3 for calculating
#   whether n is an outlier
isAnOutlier <- function(n,dataset){
  outlierDiff <- 1.5*IQR(dataset)

  #Value 2 of fivenum is Q1; Value 4 of fivenum is Q3
  if(n < fivenum(dataset)[2] - outlierDiff || n > fivenum(dataset)[4] + outlierDiff)
    print(sprintf("%d is an outlier!",n))
  else
    print(sprintf("%d is NOT an outlier",n))
}

#2a) Reading in the football dataset. Make sure the file is in the same
#    location as this script!
football <- read.csv("2021-2022.csv")

#2b) Making vectors containing corner kicks and fouls in each match
corners <- football$HC + football$AC
fouls <- football$HF + football$AF

#2c) Histograms
hist(corners,col="magenta")
hist(fouls,col="red")

#2e) Using the 'isAnOutlier' function to check if there are outliers
print(sprintf("Lowest number of corners: %i. Highest number of corners: %i",min(corners),max(corners)))
print(sprintf("Lowest number of fouls: %i. Highest number of fouls: %i",min(fouls),max(fouls)))
isAnOutlier(min(corners),corners)
isAnOutlier(max(corners),corners)
isAnOutlier(min(fouls),fouls)
isAnOutlier(max(fouls),fouls)

#Get the row numbers corresponding to the lowest and highest number of fouls
#The 'which' function finds the row of a dataframe that meets the condition (in this case,
#that the home+away fouls add up to the minimum or maximum values)
minRowNum = which(football$HF + football$AF == min(fouls))
maxRowNum = which(football$HF + football$AF == max(fouls))

#There may be more than 1 row with a minimum or maximum, so it's useful to make this a vector
rowNums <- c(minRowNum,maxRowNum)

#A function to print things nicely
printTeams = function(n){
  print(paste("Home: ",football[n,"HomeTeam"],". Fouls: ",football[n,"HF"],". Away: ",football[n,"AwayTeam"], ". Fouls: ", football[n,"AF"],sep=""))
}
sapply(rowNums,printTeams)