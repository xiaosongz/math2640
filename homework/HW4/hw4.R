#HW4 math 2640
#4.54
p=c(.8507,.1448,.0045) #data input
#a
total=sum(p)
total #verify sum of probabilities
#b
ph=0
for(i in seq(1:8507))
{
  ph[i]=0.1
}
for(i in seq(1:1448))
{
  ph[i+8507]=1.1
}
for(i in seq(1:45))
{
  ph[i+8507+1448]=2.1
}
ph
hist(ph,breaks=2,col="red",main="Histogram of probability of #4.54")
#c
.1448+.0045# No.1 
1-.8507#No.2
#4.54 
#END

#4.59
dice8=seq(1:8)
dice6=seq(1:6)
#dice package
#dice simulation
d6=getTotalProbs(ndicePerRoll = 1, #using dice package to simulate dice
              nsidesPerDie = 6,
              perDieModifier = 0)

d8=getTotalProbs(ndicePerRoll = 1,
              nsidesPerDie = 8,
              perDieModifier = 0)
pd=0
d8t=d8$probabilities[,1]#retrieve dice number
d8t
d6t=d6$probabilities[,1]#retrieve probability for number above
d6t
d8p=d8$probabilities[,2]
d8p
d6p=d6$probabilities[,2]
d6p
  df = data.frame(matrix(vector(), 0, 2, dimnames=list(c(), c("Sum", "Prob"))), stringsAsFactors=F)
#creat empty data frame with two cols
# prepare for double loop below

df
for(i in seq(1:8)) #loop to calculate standard deviation(error) and confidence interval of the dataset
{
  for(k in seq(1:6))
  {
    df[d8t[i]+d6t[k],1]=d8t[i]+d6t[k]
    df[d8t[i]+d6t[k],2]=df[d8t[i]+d6t[k],2]+d8p[i]*d6p[k]
  }
}
df[,2]=0
for(i in seq(1:8)) #loop to calculate standard deviation(error) and confidence interval of the dataset
{
  for(k in seq(1:6))
  {
    df[d8t[i]+d6t[k],1]=d8t[i]+d6t[k]
    df[d8t[i]+d6t[k],2]=df[d8t[i]+d6t[k],2]+d8p[i]*d6p[k]
  }
}
df# probability distribution
sum(df[,2])# recheck the whole loop see if it follow the basic stat rules
plot(df[,1],df[,2],main="probability distribution of X",xlab="X",ylab="Probability of X")

#4.73 Servings of fruits and vegetables
SF = data.frame(matrix(vector(), 0, 2, dimnames=list(c(), c("Num", "Prob"))))#data frame for serving food
for(i in seq(1:6))
{
  SF[i,1]=i-1
}
SF[,2]=c(.3,.1,.1,.2,.1,.2)
SF
mean.Servings=ms=0
for(i in (1:6))
{
  mean.Servings=ms=ms+SF[i,1]*SF[i,2]
}
mean.Servings#mean of fruit servings
Std.Servings=ss=0
Variance.servings=vs=0
for (i in seq(1:6))
{
  vs=vs+((SF[i,1]-ms)^2)*SF[i,2]
}
vs#variance of fruit servings
Std.Servings=ss=sqrt(vs)
ss#standard deviation of fruit servings

#4.76¡¡
nonword.error=ne = data.frame(matrix(vector(), 0, 2, dimnames=list(c(), c("Errors", "Prob"))))
X=ne
n=5
for(i in seq(1:n))
{
  X[i,1]=i-1
}
X[,2]=c(.1,.3,.3,.2,.1)
#lib001 starting of lib for mean,Var,Std
mean=0
for(i in (1:n))
{
  mean=mean+X[i,1]*X[i,2]
}
Std=0
Var=0
for (i in seq(1:n))
{
  Var=Var+((X[i,1]-mean)^2)*X[i,2]
}
Std=sqrt(Var)
X
ne=X
mean#mean for non-word error
Var#for non-word error
Varn=Var
Std#standard deviation for non-word error
Stdn=Std
#lib001 end of lib for mean,Var,Std

word.error=we = data.frame(matrix(vector(), 0, 2, dimnames=list(c(), c("Errors", "Prob"))))
X=we
n=4
for(i in seq(1:n))
{
  X[i,1]=i-1
}
X[,2]=c(.4,.3,.2,.1)
#lib001 starting of lib for mean,Var,Std
mean=0
for(i in (1:n))
{
  mean=mean+X[i,1]*X[i,2]
}
Std=0
Var=0
for (i in seq(1:n))
{
  Var=Var+((X[i,1]-mean)^2)*X[i,2]
}
Std=sqrt(Var)
X
we=X
mean#mean for word error
Var#for word error
Varw=Var
Std#standard deviation forword error
Stdw=Std
#lib001 end of lib for mean,Var,Std
#END#4.76

#4.82
#a

df = data.frame(matrix(vector(), 0, 2, dimnames=list(c(), c("Error", "Prob"))), stringsAsFactors=F)
#lib 002 factors(x1,x2;n1,n2)
x1=ne
n1=5
x2=we
n2=4
for(i in seq(1:n1)) #loop to calculate standard deviation(error) and confidence interval of the dataset
{
  for(k in seq(1:n2))
  {

    df[x1[i,1]+x2[k,1]+1,1]=x1[i,1]+x2[k,1]
    df[x1[i,1]+x2[k,1]+1,2]=df[x1[i,1]+x2[k,1]+1,2]+x1[i,2]*x2[k,2]
  }
}

df[,2]=0
for(i in seq(1:n1)) #loop to calculate standard deviation(error) and confidence interval of the dataset
{
  for(k in seq(1:n2))
  {
    
    df[x1[i,1]+x2[k,1]+1,1]=x1[i,1]+x2[k,1]
    df[x1[i,1]+x2[k,1]+1,2]=df[x1[i,1]+x2[k,1]+1,2]+x1[i,2]*x2[k,2]
  }
}
df# probability distribution
#lib002 END

X=df
n=8

#lib001 starting of lib for mean,Var,Std
mean=0
for(i in (1:n))
{
  mean=mean+X[i,1]*X[i,2]
}
Std=0
Var=0
for (i in seq(1:n))
{
  Var=Var+((X[i,1]-mean)^2)*X[i,2]
}
Std=sqrt(Var)
X
mean#mean for word error
Var#for word error
Std#standard deviation forword error
#lib001 end of lib for mean,Var,Std

#correlation 
correlation=co=0.5
Varc=Varn^2+Varw^2+2*co*Stdn*Stdw #p271 Rule 3
Stdc=sqrt(Varc)
Varc# Variance under influence of correlation coefficiency 0.5
Stdc# Standard Deviation under influence of correlation coefficiency 0.5

#4.130
Aa=2/3
AA=1/3
#a
pa=Aa*1*1/2
pa

#4.134 Work with a transformation
vx= data.frame(matrix(vector(), 0, 2, dimnames=list(c(), c("Value of X", "Prob"))))
X=vx
n=2
for(i in seq(1:n))
{
  X[i,1]=i
}
X[,2]=c(.4,0.6)
#lib001 starting of lib for mean,Var,Std
mean=0
for(i in (1:n))
{
  mean=mean+X[i,1]*X[i,2]
}
Std=0
Var=0
for (i in seq(1:n))
{
  Var=Var+((X[i,1]-mean)^2)*X[i,2]
}
Std=sqrt(Var)
X
mean#mean 
Var#Variance
Std#standard deviation 

#lib001 end of lib for mean,Var,Std

Y=X
for(i in seq(1:2))
{
  Y[i,1]=X[i,1]*3-2
}
Y
X=Y
#lib001 starting of lib for mean,Var,Std
mean=0
for(i in (1:n))
{
  mean=mean+X[i,1]*X[i,2]
}
Std=0
Var=0
for (i in seq(1:n))
{
  Var=Var+((X[i,1]-mean)^2)*X[i,2]
}
Std=sqrt(Var)
X
mean#mean 
Var#Variance
Std#standard deviation 

#lib001 end of lib for mean,Var,Std