#homework 5 Due:Feb20,2014
#1.127 Find more proportions
#a, Z>1.65
pnorm(1.65,lower.tail=F)# probability when Z score> 1.65

#b, Z<1.65
pnorm(1.65,lower.tail=T)# probability when Z score< 1.65

#c,Z>-0.76
pnorm(.76,lower.tail=F)# probability when Z score> 0.76

#d, -0.76<Z<1.65
#lib 003 probability between Z score 
left=-.76  #left limit 
right=1.65 #right limit
pnorm(right)-pnorm(left,lower.tail=T)#probability betweenn left and right limit
#END lib 003 probability between Z score

#END 1.127

#1.128 Find some values of z.
#a 22% below z
qnorm(.22,lower.tail=T)
#z score
#b 40% above z
qnorm(.40,lower.tail=F)
#z score
#END 1.128

#1.142 "Good cholesterol"
#a persentage have low cholesterol level
mean.HDL=mh=55 #mean value of HDL
sd.HDL=sh=15.5 #standard deviation of HDL
Threshold.low.HDL=th.low=40 #threshold defination for "Low Cholesterol"
zscore.low.HDL=z.low=(th.low-mh)/sh 
persentage.low.HDL=pl=pnorm(z.low,lower.tail=T)
pl*100#persentage of women have low valuse of cholesterol

#b persentage have pertective cholesterol level
threshold.protective.HDL=th.pro=60#threshold defination for "Protective Cholesterol"
zscore.protective.HDL=z.pro=(th.pro-mh)/sh #z-score for protective cholesterol level
persentage.protective.HDL=pp=pnorm(z.pro,lower.tail=F)
pp*100
#persentatge women at 20s have protective level of cholesterol
#c
#lib 003 probability between Z score 
left=z.low  #left limit 
right=z.pro #right limit
persentage.between=pb=pnorm(right)-pnorm(left)#probability betweenn left and right limit
#END lib 003 probability between Z score
pb*100 #persentage between 40mg and 60mg
#END 1.142

#5.12 Songs on an iPad
#a standard deviation of SRS of 10 songs
standard.deviation.pupolation=sd.p=280
n=10 #SRS taken
standard.deviation.sample=sd.s=sd.p/(sqrt(n))
#Page 301 Mean and Standard Deviation of Sample mean
sd.s#standard deviation of 10 samples
#b
n.b=n*(sd.s/15)^2#songs need to acheive SD of sample mean= 15
sd.p/(sqrt(n.b))# verifiy SD of sample did = 15
n.b #numbers of songs needed
#END 5.12

#5.14
#a)
#lib 004 density curve 
x=rnorm(10000,350,sd=280)
h<-hist(x, breaks=24, col="red", xlab="Length of song", 
        main="Play times for songs on an iPad(single sample)") 
xfit<-seq(min(x),max(x),length=10000) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:3])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
#Lib 004 END

#lib 004 density curve 
x=rnorm(10000,350,sd=280/sqrt(10))
h<-hist(x, breaks=24, col="red", xlab="Length of song",
        main="Play times for songs on an iPad(10 samples)") 
xfit<-seq(min(x),max(x),length=10000) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
#Lib 004 END

#b
zscore=z=19/280
#lib 003 probability between Z score 
left=-z  #left limit 
right=z #right limit
persentage.between=pb=pnorm(right)-pnorm(left)#probability betweenn left and right limit
#END lib 003 probability between Z score
1-pb
#probability differ more than 19 seconds(1 song)

#c
zscore=z=19/(280/sqrt(10))
#lib 003 probability between Z score 
left=-z  #left limit 
right=z #right limit
persentage.between=pb=pnorm(right)-pnorm(left)#probability betweenn left and right limit
#END lib 003 probability between Z score
1-pb
#probability differ more than 19 seconds(SRS 10 songs)
#END 5.14

#5.20 Grades in an Engl course
#a
name=c("A","B","C","D","F")
scale=c(4,3,2,1,0)
Percentage=p=c(.33,.24,.18,.16,.9)
engl.matrix=en=cbind(scale,p)
rownames(en)=name
X=en
en
n=4
#lib001 starting of lib for mean,Var,Std
mean=0;Std=0;Var=0
for(i in (1:n))
{
  mean=mean+X[i,1]*X[i,2]
}

for (i in seq(1:n))
{
  Var=Var+((X[i,1]-mean)^2)*X[i,2]
}
Std=sqrt(Var)
X
ne=X
mean#mean 
Var#Variance
Std#standard deviation 

#b
mean#sample mean of 50 samples 
n=50#numbers of sample
ssd=Std/sqrt(n)# formular on P301
ssd
#sample SD
#c
pnorm(3,mean,sd=Std,lower.tail=F)
#probability P(X>=3) for singel sample
pnorm(3,mean,sd=ssd,lower.tail=F)
#probability P(x>=3) for SRS 50
#END 5.20

#5.22
#a
mean=500*0.001
mean
#mean
std=sqrt(((0-mean)^2*0.999)+((500-mean)^2)*0.001)
std
#standard deviation

#b
mean*3
#average payoff
#c
#the distribution should close to a normal distribution
#d
p=pnorm(1,mean,std,lower.tail=F)
#probability he get more than $1 for single day
p^365
#probability he still have more than $1 for a whole year(365days) 
#END 5.22

#5.24
mean=1.3
std=1.5
n=200#number of sample size
ssd=std/sqrt(n)#sample mean
pnorm(2,mean,ssd,lower.tail=F)
#the probability of numbers of flaws exceed 2