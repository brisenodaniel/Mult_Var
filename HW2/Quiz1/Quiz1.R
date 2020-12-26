



library(MVN)
d = read.table('T4-6.DAT')
d = cbind(d[,3],d[,4])
colnames(d) = c('Ben','Con')

#Problem a
mvn(d,univariateTest = "CVM")

mvn(d, mvnTest = 'hz')

#Conclusion, we can assume both variables are univariate normal, since both 
# univariate normality tests concluded that benevolence and conformity are normally distributed (univariate)


#Problem b

mvn(d, mvnTest = 'hz')
mvn(d, mvnTest = 'royston')

#conclusion: we can assume that the two variables are bivariate normal

#Problem c

#mvn(d, multivariateOutlierMethod='adj') Commented out due to errors with knittr, but plot is included

# conclusion: no outliers detected, additonally, the linearity of the Q-Q plot futher confirms our assumpion of multivariate normality


#Problem d

#Using Null hypothesis mu0= (17,22)
#Reject H0 if : n(xbar - mu0) %*% solve(S) %*% (xbar - mu0) > (n-1)p/(n-p) F(0.05,p,n-p)
#Define vars:
n = 130
xbar = colMeans(d)
mu0 = c(17,22)
S = var(d)
p = 2
T2 = n*t(xbar - mu0) %*% solve(S) %*% (xbar - mu0)
T2
c2 =  ((n-1)*p/(n-p))*qf(0.95,2,128)
c2
T2 <= c2
#So the null hypothesis is rejected in favor of the alternative hypothesis. mu != t(17,22)


#Problem e
#99% confidence intervals:

#xbar[i] - qt(0.99/2m,n-1)*sqrt(S[i,i]/n), xbar[i] + qt(0.99/2m,n-1)*sqrt(S[i,i]/n)
xbar = c(xbar[1],xbar[2])
m=2 
for( i in 1:2){
  print( c(xbar[i] - qt(0.99/(2*m),n-1)*sqrt(S[i,i]/n), xbar[i] + qt(0.99/(2*m),n-1)*sqrt(S[i,i]/n)))
}
d

#Thus our confidence intervals are :
# 18.46 <= mu_ben <= 19.11
# 15.15 <= mu_con <= 15.84