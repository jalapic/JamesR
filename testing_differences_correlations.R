

### bootstrapping correlation coefficients
#boot package method
# other method

#### Differences phi-correlation.....



### Differences between correlations.....
# Hollister's test...

#remember to look up

#steiger, williams, hollister tests.



#-------Differences between independent rs-----

zdifference<-function(r1, r2, n1, n2)
{zd<-(atanh(r1)-atanh(r2))/sqrt(1/(n1-3)+1/(n2-3))
p <-1 - pnorm(abs(zd))
print(paste("Z Difference: ", zd))
print(paste("One-Tailed P-Value: ", p))
}

zdifference(-0.506, -0.381, 52, 51)

#-------Differences between dependent rs-----

tdifference<-function(rxy, rxz, rzy, n) 
{	df<-n-3
td<-(rxy-rzy)*sqrt((df*(1 + rxz))/(2*(1-rxy^2-rxz^2-rzy^2+(2*rxy*rxz*rzy))))
p <-pt(td, df)
print(paste("t Difference: ", td))
print(paste("One-Tailed P-Value: ", p))
}

tdifference(-0.441, -0.709, 0.397, 103)



#Smart Alex Task 2-------------

#load the data:

chickFlick = read.delim("ChickFlick.dat", header = TRUE)

#conduct two point-biserial correlations:

cor.test(chickFlick$gender, chickFlick$arousal)

cor.test(chickFlick$film, chickFlick$arousal)

#Smart Alex Task 3--------

#load in the data

gradesData = read.csv("grades.csv", header = TRUE)

#Conduct a Spearman correlation:
cor.test(gradesData$gcse, gradesData$stats, alternative = "greater", method = "spearman")

#conduct a Kendall correlation:
cor.test(gradesData$gcse, gradesData$stats, alternative = "greater", method = "kendall")


#-------R Souls Tip Writing Functions----

nameofFunction<-function(inputObject1, inputObject2, etc.)
{
  a set of commands that do things to the input objects
  a set of commands that specify the output of the function
}


meanOfVariable<-function(variable)
{
  mean<-sum(variable)/length(variable)
  cat("Mean = ", mean)
  
}

meanOfVariable<-function(HarryTheHungyHippo)
{
  mean<-sum(HarryTheHungyHippo)/length(HarryTheHungyHippo)
  cat("Mean = ", mean)
}


lecturerFriends = c(1,2,3,3,4)

meanOfVariable(lecturerFriends)

