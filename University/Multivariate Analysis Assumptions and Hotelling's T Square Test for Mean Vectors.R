#Testing the 3 Assumptions

#Cleaning the Dataset
attach(attitude)

attitude = data.frame(attitude$complaints, attitude$learning, attitude$raises)

#1. Testing the Normality Assumption
#Shapiro-Wilk Test for normality
install.packages("mvnormtest") #mvnormtest = multivariatenormaltest
library(mvnormtest) #Access the functions in the package
transpose_attitude = t(attitude)
mshapiro.test(transpose_attitude) #H0: All three variables (complaints,learning,raises) are Normally Distributed


#2. Testing Positive Determinant of Variance-Covariance Matrix Assumption
CovAttitude = cov(attitude)
det(CovAttitude)

#3. Testing Equality of Variance-Covariance Matrices of Groups (Gender)

group = rep(c("male", "female"), c(15, 15)) #Creating a vector in which the first 15 are males and the last 15 are females
factor(group)
group

install.packages("biotools")
library(biotools)
boxM(attitude, group) #Testing if the males and the females in the 2 groups have relatevely similar Variance-Covariance matrices

#Hoteling T2 Test
#Compare 2 Independent Group Means vectors


#Graph the means of the 3 variables for males and females
install.packages("gplots")
library(gplots)

plotmeans(attitude.complaints ~ group, data=attitude, ylim=c(0,100), xlab="Groups", 
          legends=c("Males", "Females"), main="Attitude", connect=FALSE, mean.labels=TRUE, col=NULL,
          p=1.0)

plotmeans(attitude.learning ~ group, data=attitude, ylim=c(0,100), xlab="Groups", 
          legends=c("Males", "Females"), main="Attitude", connect=FALSE, mean.labels=TRUE, col=NULL,
          p=1.0)

plotmeans(attitude.raises ~ group, data=attitude, ylim=c(0,100), xlab="Groups", 
          legends=c("Males", "Females"), main="Attitude", connect=FALSE, mean.labels=TRUE, col=NULL,
          p=1.0)

#Perform Hotteling T2 Test
install.packages("ICSNP")
library(ICSNP)
HotellingsT2(attitude[1:15,], attitude[16:30,]) 

#H0: Males and Females have the same attitude towards their jobs 
#based on the measurements of their [raises,learning,complains]
