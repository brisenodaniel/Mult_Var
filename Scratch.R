d = read.table('T1-8.DAT')
names(d) = c('r1','r2','h1','h2','u1','u2')
head(d)
d$r1
#library(MVN)

mvn(data=d, mvnTest='hz',multivariateOutlierMethod='adj')
dim(d)
# T2=n*t(xbar-mu0) %*%solve(S)%*%(xbar-mu0) ~p(n-1)/(n-p)F(p,n-p)
mu0 = c(0.9,0.9,2,2,0.8,0.8)
n=25
p=6
xbar=colMeans(d)
xbar



S= var(d)
 T2=n*t(xbar-mu0) %*%solve(S)%*%(xbar-mu0) 
T2 
p*(n-1)/(n-p)*qf(0.95,p,n-p)
#we need (1-0.03/6)*100% CIs for all components of the mean vector
(1-0.03/6)*100
#(xi_bar-qt((1-0,03/(2*6)),n-1)*sd(xbar),(xi_bar+qt((1-0.03/(2*6))n-1) *sd(xbar))
c(xbar[1]-qt((1-0.03)/(2*6),n-1)*sqrt(S[1,1]/n),xbar[1]+qt((1-0.03/(2*6)),n-1)*sqrt(S[1,1]/n))
for(i in 1:6){
  print(c(xbar[i]-qt((1-0.03)/(2*6),n-1)*sqrt(S[i,i]/n),xbar[i]+qt((1-0.03/(2*6)),n-1)*sqrt(S[i,i]/n)))
}
qt()

qt((1-0.01)/(4),129)





# HW2

library(MVN)
d = read.table("T4-6.DAT")

#Mardia Skewness and Mardia Kurtosis tests
mvn(d, mvnTest="mardia")

#
mvn(d, mvnTest="hz")


mvn(d, mvnTest="royston")


#Conclusion, the data is not multivariate normal, since 





#Midterm

#Problem 4
library(MVN)
d = read.table('T4-6 (2).DAT')
head(d)

names(d) = c('Ind', 'Sup', 'Ben','Con', 'Led','C_1', 'C_2')
d = cbind(d$Ben, d$Con)
colnames(d) = c('Ben','Con')

#Tests for normality
mvn(d,univariateTest = "CVM")

mvn(d, mvnTest = 'hz')
mvn(d, mvnTest = 'royston')
tail(d) colnames(d)
mvn(d, mvnTest = "hz", multivariateOutlierMethod='adj' )

#no outliers detected, additonally, the linearity of the Q-Q plot futher confirms our assumpion of multivariate normality
dim(d)
#Using Null hypothesis mu0= (17,22)
#Reject H0 if : n(xbar - mu0) %*% solve(S) %*% (xbar - mu0) > (n-1)p/(n-p) F(0.05,p,n-p)
#Define vars:
n = 130
xbar = colMeans(d)
mu0 = c(17,22)
S = var(d)
p = 2
T2 = n*(xbar - mu0) %*% solve(S) %*% (xbar - mu0)
T2
((n-1)*p/(n-p))*qf(0.95,2,130-2)
#So the null hypothesis is rejected


#99% confidence intervals:

#xbar[i] - qt(0.99/2m,n-1)*sqrt(S[i,i]/n), xbar[i] + qt(0.99/2m,n-1)*sqrt(S[i,i]/n)
xbar = c(xbar[1],xbar[2])
m=2 
for( i in 1:2){
 print( c(xbar[i] - qt(0.99/(2*m),n-1)*sqrt(S[i,i]/n), xbar[i] + qt(0.99/(2*m),n-1)*sqrt(S[i,i]/n)))
}