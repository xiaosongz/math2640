# problem 1.24/1.25 Garbage
Material = c("Food","Glass","Metal","Paper","Plastic","Rubber","Wood","Trimming","Other")
Weight = c(31.7,13.6,20.8,83.0,30.7,19.4,14.2,32.6,8.2)
Percent = c(2.6,23.7,34.8,54.5,6.8,14.7,9.3,64.1,0)
weight = sort(Weight,decreasing=T)
percent = sort(Percent,decreasing=T)
material = Material[order(Percent,decreasing=T)]

names(Percent) = Material
totalWeight=ttw=sum(Weight)
ttw #total weight of Garbage
barplot(Percent,space=1.5,col="blue",xlab="Material",ylab="Percent recycled")
abline(h=0)
names(percent) = material
barplot(percent,space=1.5,col="blue",xlab="Material",ylab="Percent recycled")
abline(h=0)
#pie chart! 
slices =sort(Weight)#slices with sorted weight
lbls = material 
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Pie Chart of Garbage")

#problem 1.33/34
gl=glucose=read.table("glucose.txt",header=T)#input data from glucose.txt 
gl #glucose data from both group
Gl1=subset(glucose,GroupNum==1) #Gl1== the data which have a GroupNum==1
attach(Gl1) 
gl1=Glucose #let gl1 to be the data under colum name="Glucose"
Gl2=subset(glucose,GroupNum==2)#Gl1== the data which have a GroupNum==1
attach(Gl2)
gl2=Glucose#let gl2 to be the data under colum name="Glucose"
gl1 #glucose data from group 1
stem(gl1) # stem plot created
outlier(gl1) # test outlier of the data
stem(gl2)

#problem 1.38 Acidity of rainwater
Ar=read.table("acidrain.txt",header=T)
Ar
attach(Ar) # attach the data in  table Ar under their NAME
hist(pH,breaks= seq(4.2,7,.2)) #histogram with breaks = 4.2 to 7.0 step = 0.2
hist(pH,breaks=seq(4.14,6.94,.2))#histogram with breaks = 4.14 to 6.94 step = 0.2
hist(pH)

#1.42 density of earth
ED= read.table("earthdensity.txt",header=T)
attach(ED)
Density
stem(Density)
hist(Density,col=rainbow(Density),breaks=seq(4.8,6,.2))
summary(Density)

#1.44 Describe the IQ scores.
IQT=read.table("seventhgrade.txt",header=T)
attach(IQT)
summary(IQ)
hist(IQ)
# Add a Normal Curve 
x=IQ 
h<-hist(x, breaks=10, col="red", xlab="IQ", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=78) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)
outlier(IQ)
#1.46 Boston Marathon Women
Marathon=Mara=read.table("marathon.txt",header=T)
attach(Mara)
plot(Year,Time,main="Boston Marathon")
lines(lowess(Time,Year), col="red") # regression line (y~x)
