#homework 2
#1.74
CD= read.table("co2.txt",header=T) #data input
attach(CD)
CO2 # check
summary(CO2)# get a summary of CO2
stem(CO2) # stem plot created
outlier(CO2)

#1.80
ST=read.table("studytime.txt", header= T) # data input 
attach(ST)
StudyTime
st1=subset(ST,GroupNum==1) #seperate data from different group
st2=subset(ST,GroupNum==2)#seperate data from different group#
attach(st1)
wo=StudyTime
attach(st2)
men=StudyTime
stem.leaf.backback(wo,men)
stem(wo)#stem plot for Female study time
outlier(wo)# test outlier from Female study time
stem(men)
outlier(men) #test outlier from male data set
summary(wo)
summary(men)
F=rm.outlier(wo)# remove outlier from Female study time
summary(F) #6 digits summary for female after remove the outlier
M=rm.outlier(men)# remove outlier from male study time
summary(M)#6 digits summary for male after remove the outlier

#1.86 Hammingbirds and flowers
hf=read.table("heliconia.txt",header=T)
hf1=subset(hf,GroupNum==1)
hf2=subset(hf,GroupNum==2)
hf3=subset(hf,GroupNum==3)
attach(hf1)#split data into individual section
Hb=Length#summary of beak length of H.bihai
attach(hf2)
Hcr=Length#summary of beak length of H.caribaea red
attach(hf3)
Hcy=Length beak length of H.caribaea yellow

boxplot(Length~Variety,data=hf,main="Hummingbirds and flowers",ylab="Length(mm)",xlab="Species")
summary(Hb)#summary of beak length of H.bihai
summary(Hcr)#summary of beak length of H.caribaea red
summary(Hcy)#summary of beak length of H.caribaea yellow

#1.88 Imputaion
Im=read.table("imputation.txt",header=T)
attach(Im)
summary(x)
sd(x)#standard deviation of original x data set
mean(x)
xi=rnorm(mean=mean(x),sd=sd(x),10)

xh=c(x,xi)#imputed x serious
summary(xh)
sd(xh)#standard deviation of imputed X serious

#2.24/2.48 Relationship between first test and final exam
R2=read.table("statcourse8.txt",header=T)
attach(R2)
plot(First,Final) #scatterplot 
cor(R2)#correlation between exams

#2.77
x=5
y=12+6*x
y

#2.78 Progress in math score
nmath=nm=read.table("naepmath.txt",header=T)
plot(nm,main=("Progress in math score over time"))
attach(nm)
mean(Year)#mean of year
mean(Score)#mean of score
sd(Year)
sd(Score)#standard deviation of Score

res=lm(Score~Year)#linear regression of the Score~Year
res #showing coefficients
abline(res)# draw a regression line
