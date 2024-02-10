rm(list = ls(all = TRUE)) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)   # Data visualization[c]
library(tidyr)   # Tidy data management
library(dplyr)
library(cowplot)
library(pracma)
library("RColorBrewer")

K = 1

Dmean = 1000
Tmean = 15
wmean = .6
rmean = .05
SCCmean = 150
FixedC = 10000

wvec = seq(.3,.9,length.out = 20)
rvec = seq(0,.1,length.out = 20)

X = vector()
PVB = vector()
PVC = vector()


X[1] = .01

findB = function(D,r,T,w,SCC)
{
  for(t in 1:50)
  {
    B = SCC*T*X[t]/K
    PVB[t] = B/(1+r)^(t-1)
    X[t+1] = X[t] + w*X[t]*(1-X[t]/K)
    PVC[t] = D/(1+r)^(t-1)
  }
  PVbenefits = sum(PVB)
  PVcost = sum(PVC) + FixedC
  NPV = PVbenefits - PVcost
  DFout = data.frame(D=D,r=r,T=T,w=w,NPV=NPV)
}

#Try it
Z = findB(D=1000,r=.05,T=15,w=.6,SCC=100)

#Partial Sensitivity
# r
DFr = data.frame()
for(i in 1:length(rvec))
{
  r = rvec[i]
  tmp = findB(D=Dmean,r=r,T=Tmean,w=wmean,SCC=SCCmean)
  DFr = bind_rows(DFr,tmp)
}

Pr = ggplot(data=DFr) +
  geom_line(aes(x=r,y=NPV),color="blue",size=2) +
  xlab("Discount rate, r") +
  geom_segment(aes(x=0,xend=.1,y=0,yend=0),color="black",size=2) +
  theme_bw()
Pr

DFw = data.frame()
for(i in 1:length(wvec))
{
  w = wvec[i]
  tmp = findB(D=Dmean,r=rmean,T=Tmean,w=w,SCC=SCCmean)
  DFw = bind_rows(DFw,tmp)
}

Pw = ggplot(data=DFw) +
  geom_line(aes(x=w,y=NPV),color="green",size=2) +
  xlab("Growth Rate of Kelp, w") +
  geom_segment(aes(x=0,xend=.9,y=0,yend=0),color="black",size=2) +
  theme_bw()
Pw

Prw = plot_grid(Pr,Pw)
Prw

ggsave(filename="Partial_Sensitivity.png",plot=Prw,height=8,width=15,units="cm")

#Monte Carlo
DFall = data.frame()
for(i in 1:5000)
{
  w = rnorm(1,mean=wmean,sd=.2)
  r = rnorm(1,mean=rmean,sd=.02)
  T = rnorm(1,mean=Tmean,sd=3)
  SCC = rnorm(1,mean=SCCmean,sd=20)
  D = rnorm(1,mean=Dmean,sd=50)
  
  tmp = findB(D=D,r=r,T=T,w=w,SCC=SCC)
  DFall = bind_rows(DFall,tmp)
}

Pall = ggplot(data=DFall) + 
  geom_histogram(aes(x=NPV),bins=50,color="black",fill="lightblue") +
  geom_segment(aes(x=0,xend=0,y=0,yend=1000),size=2) +
  theme_bw()
Pall
ggsave(filename="Monte_Carlo.png",plot=Pall,height=8,width=15,units="cm")


DFall = DFall %>%
  mutate(positive = NPV>0)
Frac_pos = sum(DFall$positive==TRUE)/nrow(DFall)


