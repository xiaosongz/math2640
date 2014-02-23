#Home work 3
#3.32 Smoking marijuana and willingness to work
name
name1
## generate a random ordering
set.seed(5) ## make reproducible here, but not if generating many random samples
rand <- sample(ncol(name1))#get a random number seq from 1:20
rand
namer=name1[,rand]#rearrange the name using the random seq
namer

#3.59 Interview residents of apartment complexes
set.seed=(359)## make reproducible here, but not if generating many random samples
rand5=sample(seq(1:33),size=5)#randomly choose 5 out of 33 objects
rand5

#4.26 distributuion of blood types
ubl=c(.42,.11,.03,.44)#distributuion of blood type of USA 
sum(ubl)
ibl=c(.35,.1,.03,.52)#distributuion of blood type of Ireland
sum(ibl)
OO=ubl[4]*ibl[4]#probability of both have type O
OO

same=ubl*ibl#probability of both have same type
sums=sum(same)
sums

#4.28 French and English in Canada
Language.p=lp=c(.59,1-.59-.07-.11,0.07,.11)
lp #
lp[2]#probability of French
not.eng=ne=1-lp[1]
ne# probability of non-english 
