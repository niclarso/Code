---
title: "Research Census Data"
author: "Nick Larson"
output:
  word_document: default
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Creating vector for field of study and a for loop to find which variables coincide with eath person.
```{r}
library(readr)
library(dplyr)
```

```{r}
ss15pwi <- read.csv("C:/Users/Larso/Documents/Research/ss15pwi.csv")
renamefos <- read.csv("~/Desktop/UWEC/research/rename.csv")
RenameOCP <- read.csv("C:/Users/Larso/Documents/Research/RenameOCP.csv")

```

```{r}
filterPwi=ss15pwi%>%
  filter(SCHL == 21) %>%
  filter(!is.na(FOD1P)) %>%
  filter(!is.na(OCCP)) 
```

```{r}
newfilterPwi = matrix(,nc = 0,nr = dim(filterPwi)[1])
nathresh=2000
newfiltercolname = c()
for(i in 1:dim(filterPwi)[2]){
  numna = length(which(is.na(filterPwi[,i])))
  if (numna<nathresh){
    newfilterPwi = cbind(newfilterPwi,filterPwi[,i])
    newfiltercolname = c(newfiltercolname,colnames(filterPwi)[i]) 
    
  }
}
colnames(newfilterPwi) = newfiltercolname
newfilterPwi = na.omit(newfilterPwi)
dim(newfilterPwi)

 
```
```{r}
#INTP into factor
newfilterPwi = as.data.frame(newfilterPwi)
for(i in 1 : length(newfilterPwi$INTP)){
  if (newfilterPwi$INTP[i] == 0){
    newfilterPwi$INTP[i] = 0
  }
  else{
    newfilterPwi$INTP[i] = 1
  }
}

for(i in 1:length(newfilterPwi$OIP)){
  if (newfilterPwi$OIP[i] == 0){
    newfilterPwi$OIP[i] = 0
  }
  else{
    newfilterPwi$OIP[i] = 1
  }
}

for(i in 1:length(newfilterPwi$SEMP)){
  if (newfilterPwi$SEMP[i] == 0){
    newfilterPwi$SEMP[i] = 0
  }
  else{
    newfilterPwi$SEMP[i] = 1
  }
}

#Factor Variables
newfilterPwi = as.data.frame(newfilterPwi)
newfilterPwi$PUMA = as.factor(newfilterPwi$PUMA)
newfilterPwi$COW = as.factor(newfilterPwi$COW)
newfilterPwi$DOUT = as.factor(newfilterPwi$DOUT)
newfilterPwi$HINS1 = as.factor(newfilterPwi$HINS1)
newfilterPwi$HINS2 = as.factor(newfilterPwi$HINS2)
newfilterPwi$MAR = as.factor(newfilterPwi$MAR)
newfilterPwi$LANX = as.factor(newfilterPwi$LANX)
newfilterPwi$MARHT = as.factor(newfilterPwi$MARHT)
newfilterPwi$GCL = as.factor(newfilterPwi$GCL)
newfilterPwi$MIL = as.factor(newfilterPwi$MIL)
newfilterPwi$MIG = as.factor(newfilterPwi$MIG)
newfilterPwi$OIP = as.factor(newfilterPwi$OIP)
newfilterPwi$SEMP = as.factor(newfilterPwi$SEMP)
newfilterPwi$SEX = as.factor(newfilterPwi$SEX)
newfilterPwi$WKW = as.factor(newfilterPwi$WKW)
newfilterPwi$ANC1P = as.factor(newfilterPwi$ANC1P)
newfilterPwi$DIS = as.factor(newfilterPwi$DIS)
newfilterPwi$JWAP = as.factor(newfilterPwi$JWAP)
newfilterPwi$MSP = as.factor(newfilterPwi$MSP)
newfilterPwi$NATIVITY = as.factor(newfilterPwi$NATIVITY)
newfilterPwi$POBP = as.factor(newfilterPwi$POBP)
newfilterPwi$INTP = as.factor(newfilterPwi$INTP)

#Scale VAriables
newfilterPwi$JWMNP=scale(newfilterPwi$JWMNP)    
newfilterPwi$PERNP=scale(newfilterPwi$PERNP)  
newfilterPwi$AGEP=scale(newfilterPwi$AGEP)
newfilterPwi$PWGTP=scale(newfilterPwi$PWGTP)



```

```{r}
#Clustering
test = matrix(nr = dim(newfilterPwi)[1],nc = 0)
#Age was removed from test
colsToUse = c("COW",
              "DOUT","HINS1",
              "LANX","MIL","MIG","SEMP","SEX","WKW","DIS","JWAP","MSP",
              "NATIVITY","INTP","JWMNP","PERNP")
for(mycol in colsToUse){
  mycolnum = which(colnames(newfilterPwi)== mycol)
  test = data.frame(test,newfilterPwi[,mycolnum])
}
colnames(test) = colsToUse

hc.complete = hclust(dist(test), method = "complete")
plot(hc.complete,main="Complete Linkage",xlab = "",sub="",cex=.1)
clusterNums = cutree(hc.complete,10)
TestPlusClust = data.frame(clusterNums,test,AGEP = newfilterPwi$AGEP)


```

```{r}
jobs = data.frame(newfilterPwi$OCCP,clusterNums)
colnames(jobs) = c("Num","clusterNums")
jobs = left_join(jobs,RenameOCP)


jobCount = jobs %>% group_by(.dots = c("clusterNums","Num","Type")) %>%
  summarise(n=n())

tclo = aggregate(n ~ clusterNums,jobCount,sum)
#clusttotal = c(0:(length(jobCount$clusterNums) - 1))
clusttotal = numeric(dim(jobCount)[1])

for(i in 1:length(tclo$clusterNums)){
  index = which( jobCount$clusterNums==tclo$clusterNums[i] )
  clusttotal[index] = tclo$n[i]

}
jobCount = data.frame(jobCount,clusttotal)

p = c(0:(length(jobCount$clusterNums) - 1))
for(i in 1:length(jobCount$clusterNums)){
  p[i] = jobCount$n[i] / jobCount$clusttotal[i]
}
jobCount = data.frame(jobCount,p)
jobCount = inner_join(jobCount,RenameOCP[,1:3])
#install.packages("ggformula")
library(ggformula)

gf_col(p ~ clusterNums, fill= ~ Type,data = jobCount)
```


```{r}

install.packages("ggplot2")
library(ggplot2)

#Grouping Variables 
#%in% test if value is in vector   COW %in% c (3,4,5), "govt", NA
library(readr)
library(dplyr)
TestPlusClust$JWAP = parse_number(TestPlusClust$JWAP)

JWAPcond = rep(NA, length(TestPlusClust$JWAP))
JWAPcond[which(TestPlusClust$JWAP <= 57 | TestPlusClust$JWAP >=215)] = "Night"

JWAPcond[which(TestPlusClust$JWAP >= 131 & TestPlusClust$JWAP <=214)] = "Middle"

JWAPcond[which(TestPlusClust$JWAP >= 58 & TestPlusClust$JWAP <=130)] = "Morning"

TestPlusClust = data.frame(TestPlusClust,JWAPcond)




TestPlusClust = TestPlusClust %>%
  mutate(COWcond = ifelse(COW ==1,1,2))

TestPlusClust = TestPlusClust %>%
  mutate(MILcond = ifelse(MIL ==4,1,2))

TestPlusClust = TestPlusClust %>%
  mutate(MIGcond = ifelse(MIG ==1,1,2))

TestPlusClust = TestPlusClust %>%
  mutate(WKWcond = ifelse(WKW ==1,1,2))

TestPlusClust = TestPlusClust %>%
  mutate(MSPcond = ifelse(WKW ==1,1,2))




sd(TestPlusClust$JWMNP[which(TestPlusClust$clusterNums == 2)])
sd(TestPlusClust$AGEP[which(TestPlusClust$clusterNums == 2)])
sd(TestPlusClust$PERNP[which(TestPlusClust$clusterNums == 2)])
for(i in 2:2){
JwapcondMatrix = matrix(c(table(TestPlusClust$JWAPcond),table(TestPlusClust$JWAPcond[which(TestPlusClust$clusterNums == i)])),nc = 2)

COWcondMatrix = matrix(c(table(TestPlusClust$COWcond),table(TestPlusClust$COWcond[which(TestPlusClust$clusterNums == i)])),nc = 2)

DOUTMatrix = matrix(c(table(TestPlusClust$DOUT),table(TestPlusClust$DOUT[which(TestPlusClust$clusterNums == i)])),nc = 2)

HINS1Matrix = matrix(c(table(TestPlusClust$HINS1),table(TestPlusClust$HINS1[which(TestPlusClust$clusterNums == i)])),nc = 2)

LANXMatrix = matrix(c(table(TestPlusClust$LANX),table(TestPlusClust$LANX[which(TestPlusClust$clusterNums == i)])),nc = 2)

MILcondMatrix = matrix(c(table(TestPlusClust$MILcond),table(TestPlusClust$MILcond[which(TestPlusClust$clusterNums == i)])),nc = 2)

MIGcondMatrix = matrix(c(table(TestPlusClust$MIGcond),table(TestPlusClust$MIGcond[which(TestPlusClust$clusterNums == i)])),nc = 2)

SEMPMatrix = matrix(c(table(TestPlusClust$SEMP),table(TestPlusClust$SEMP[which(TestPlusClust$clusterNums == i)])),nc = 2)

SEXMatrix = matrix(c(table(TestPlusClust$SEX),table(TestPlusClust$SEX[which(TestPlusClust$clusterNums == i)])),nc = 2)

WKWcondMatrix = matrix(c(table(TestPlusClust$WKWcond),table(TestPlusClust$WKWcond[which(TestPlusClust$clusterNums == i)])),nc = 2)


DISMatrix = matrix(c(table(TestPlusClust$DIS),table(TestPlusClust$DIS[which(TestPlusClust$clusterNums == i)])),nc = 2)

MSPcondMatrix = matrix(c(table(TestPlusClust$MSPcond),table(TestPlusClust$MSPcond[which(TestPlusClust$clusterNums == i)])),nc = 2)

NATIVITYMatrix = matrix(c(table(TestPlusClust$NATIVITY),table(TestPlusClust$NATIVITY[which(TestPlusClust$clusterNums == i)])),nc = 2)

INTPMatrix = matrix(c(table(TestPlusClust$INTP),table(TestPlusClust$INTP[which(TestPlusClust$clusterNums == i)])),nc = 2)

}
chisq.test(JwapcondMatrix)
chisq.test(COWcondMatrix)
chisq.test(HINS1Matrix)
chisq.test(LANXMatrix)
chisq.test(MILcondMatrix)
chisq.test(MIGcondMatrix)
chisq.test(SEMPMatrix)
chisq.test(SEXMatrix)
chisq.test(WKWcondMatrix)
chisq.test(DISMatrix)
chisq.test(MSPcondMatrix)
chisq.test(NATIVITYMatrix)
chisq.test(INTPMatrix)

library(reshape2)
library(plotly)



ggplot(TestPlusClust, aes(x = as.factor(JWAPcond))) + geom_bar(width = 0.25) + facet_grid(clusterNums ~ .)

ggplot(TestPlusClust, aes(x = as.factor(COWcond))) + geom_bar(width = 0.25) + facet_grid(clusterNums ~ .)

ggplot(TestPlusClust, aes(x = as.factor(DOUT))) + geom_bar(width = 0.25) + facet_grid(clusterNums ~ .)

ggplot(TestPlusClust, aes(x = as.factor(HINS1))) + geom_bar(width = 0.25) + facet_grid(clusterNums ~ .)

ggplot(TestPlusClust, aes(x = as.factor(LANX))) + geom_bar(width = 0.25) + facet_grid(clusterNums ~ .)

ggplot(TestPlusClust, aes(x = as.factor(MILcond))) + geom_bar(width = 0.25) + facet_grid(clusterNums ~ .)

ggplot(TestPlusClust, aes(x = as.factor(SEMP))) + geom_bar(width = 0.25) + facet_grid(clusterNums ~ .)

```






#Creating field of study matrix
```{r}
mnames = vector(length=length(newfilterPwi$FOD1P))
mnum = na.omit(RenameOCP$mnum)
mnum = as.integer(mnum)
RenameOCP$mtext = as.character(RenameOCP$mtext)
filterPwi$FOD1P = as.integer(newfilterPwi$FOD1P)
for(i in 0:length(mnum)){
  mnumindex = which(mnum[i]==newfilterPwi$FOD1P)
  mnames[mnumindex] = (RenameOCP$mtext[i])
  
}
newfilterPwi = data.frame(newfilterPwi,mnames)


fos = mnames
majorslist = sort(unique(fos))
NumMajorsMatrix = matrix(nr = dim(newfilterPwi)[1],nc = 0)
for(major in majorslist){
  nummajor = ifelse(fos == major,1,0)
NumMajorsMatrix =cbind(NumMajorsMatrix,nummajor)
}
colnames(NumMajorsMatrix) = majorslist




cluster = clusterNums
clusterlist = sort(unique(cluster))
ClusterMatrix = matrix(nr = dim(newfilterPwi)[1],nc = 0)
for(clusters in clusterlist){
  clusternumm = ifelse(cluster == clusters,1,0)
ClusterMatrix =cbind(ClusterMatrix,clusternumm)
}
colnames(ClusterMatrix) = clusterlist
NewMarketMatrix = cbind(NumMajorsMatrix,ClusterMatrix)


#NumMajorsMatrix=rbind(majorslist,NumMajorsMatrix)
# end of loop over majors
```
#Creating field of study matrix 2

```{r}
#fos2 = filterPwi$FOD2P
#Field of study2 = fos2
#majorslist2 = sort(unique(fos2))
#Values ending in .5 indicate second field of study

#NumMajorsMatrix2 = matrix(nr = dim(filterPwi)[1],nc = 0)
#for(major2 in majorslist2){
  #nummajor2 = nummajor2=rep(0,length(fos2))
  #nummajor2[which(fos2 == major2)] =1
#NumMajorsMatrix2=cbind(NumMajorsMatrix2,nummajor2)
#}
#majorslist2 = majorslist2 + .5
#NumMajorsMatrix2=rbind(majorslist2,NumMajorsMatrix2)
# end of loop over majors2
```



```{r}
jobtypes = vector(length=length(newfilterPwi$OCCP))
for(i in 0:length(RenameOCP$Num)){
  jnumindex = which(RenameOCP$Num[i]==newfilterPwi$OCCP)
  jobtypes[jnumindex] = paste(RenameOCP$Type[i])
  
}
newfilterPwi = data.frame(newfilterPwi,jobtypes) #not sure if this is working
Ocp = jobtypes
Occupation = Ocp
Ocplist = sort(unique(Ocp))
#Ocplist = Ocplist + .5
NumOcpMatrix = matrix(nr = dim(newfilterPwi)[1],nc = 0)

for(Ocps in Ocplist){
  NumOcp = ifelse(Ocp == Ocps,1,0)
NumOcpMatrix =cbind(NumOcpMatrix,NumOcp)
}
colnames(NumOcpMatrix) = Ocplist

#NumOcpMatrix=rbind(Ocplist,NumOcpMatrix)

# end of loop over Ocp
```

#Next Cbind the three matrices.

```{r}
colnames(NumMajorsMatrix) = substring(colnames(NumMajorsMatrix),1,10)

colnames(NumMajorsMatrix) = make.names(colnames(NumMajorsMatrix),unique = TRUE)

combCensus = cbind(NumOcpMatrix,NumMajorsMatrix)
#lblVec = combCensus[1,]
#combCensus = combCensus[-1,]
#colnames(combCensus) = lblVec

```

```{r}
#for one through each row of rename find the index of the the colname that equals the value in rename and set that index equal to the value in col 2 in  rename 
#for(i in 1:dim(renamefos)[1]){
#currCol = which(parse_number(colnames(combCensus))==renamefos[i,1])
#colnames(combCensus)[currCol] = as.character(renamefos[i,2])
#}

#for(i in 1:dim(RenameOCP)[1]){
#currCol = which(parse_number(colnames(combCensus))==RenameOCP[i,1])
#colnames(combCensus)[currCol] = as.character(RenameOCP[i,2])
#}

```

#Create Item Matrix

```{r}
library(arules)
itemCensus = as(combCensus,"itemMatrix")

itemcluster = as(NewMarketMatrix,"itemMatrix")
```

```{r}
itemCensusRules=apriori(itemCensus,parameter = list(support = .005, confidence = .1 ))
itemCensusRules = sort(itemCensusRules, by='lift', decreasing = TRUE)

inspect(head(itemCensusRules, n = 20, by = "lift"))


itemFrequencyPlot(itemCensus,support = .05,population = itemCensus, cex = .5, order(decreasing = TRUE))

install.packages("arulesViz")
library(arulesViz)

topRules <- itemCensusRules[1:10]
plot(topRules)
```












```{r}
itemCensusRules
````


#Helpful Hints
```{r}
which((any(is.na(combCensus))))
#Find Nas
which(is.na(combCensus[,1]))

data(twofilterPwi)
 unlist(twofilterPwi)
 rapply(twofilterPwi, c)


which(apply(combCensus,1,is.na))

sum(NumMajorsMatrix[2,] 

itemFrequencyPlot(itemCensus,support = .025,population = itemCensus, cex = .5, order(decreasing = FALSE))

plot(itemCensusRules,xlim = c(0,0.1))

newfilterPwi$AGEP = parse_number(newfilterPwiAGEP)
plot(newfilterPwi$AGEP ~ newfilterPwi$WAGP)
plot(newfilterPwi$AGEP ~ newfilterPwiWAGP, ylim = c(3))
highinc = which(newfilterPwi$WAGP == 385000)
