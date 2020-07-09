rm(list=ls())
cat("\014")

library(ggplot2)
#Mass flux
kab <- 1/10
kba <- 1/12
kau <- 1/10
kua <- 1/14
kud <- 1/14
kdu <- 1/595

#GCCM 1.1(i):Introducing  a CO2 emission of 5 Gt C in the year 1900 and0 Gt C otherwise.

#Intial Values

na <- 615
dy=1

#Constructing dataframe

df <- data.frame(year = c(1900:2000),"na"=NA,"nb"=NA,"nu"=NA,"nd"=NA,"gamma"=c(5,rep(0,100)))

#1.1_steady state model

df[1,"na"] <- na
df[1,"nb"] <- (kab*na)/(kba)
df[1,"nu"] <- (kau*na)/(kua)
df[1,"nd"] <- (kud*df[1,4])/(kdu)


#GCCM (ii): Calculating the change in the CO2 concentration in the atmosphere for 1900 - 2000 and 
#           the concentration difference between the disturbed and the reference (pre-industrial) concentration.
#Time Step :1 year

for (i in 1:100) {
  df[i+1,"na"] <- df[i,"na"] + (-kab*df[i,"na"]-kau*df[i,"na"]+kba*df[i,"nb"]+kua*df[i,"nu"]+df[i,"gamma"])*dy
  df[i+1,"nb"] <- df[i,"nb"] + (kab*df[i,"na"]-kba*df[i,"nb"])*dy
  df[i+1,"nu"] <- df[i,"nu"] + (kau*df[i,"na"]-kud*df[i,"nu"]-kua*df[i,"nu"]+kdu*df[i,"nd"])*dy
  df[i+1,"nd"] <- df[i,"nd"] + (kud*df[i,"nu"] - kdu*df[i,"nd"])*dy
  df[i+1,"kab*na/dy"] <- (kab*df[i,"na"])/dy
  df[i+1,"kba*nb/dy"] <- (kba*df[i,"nb"])/dy
  df[i+1,"kau*na/dy"] <- (kau*df[i,"na"])/dy
  df[i+1,"kua*nu/dy"] <- (kua*df[i,"nu"])/dy
  df[i+1,"kud*nu/dy"] <- (kud*df[i,"nu"])/dy
  df[i+1,"kdu*nd/dy"] <- (kdu*df[i,"nd"])/dy
  df[i+1,"nana0"] <- df[i+1,"na"]-df[1,"na"]
  df[i+1,"na_i-na_i-1"] <- df[i+1,"nana0"]-df[i,"nana0"]
  
}

#plotting data

df[1,"nana0"]=0
g1<-ggplot(df,aes(x=year))+
  geom_line(aes(y=nana0),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("CO2 concentarion difference between disturbed and pre-industrial concentration")+
  xlab("Year")+
  ylab("na - na0")+
  theme(text = element_text(size=13))
print(g1)

#Time Step :0.25 year
dy25 <- 0.25
df_25 <- data.frame(year = c(1900,rep(0,400)),"na"=NA,"nb"=NA,"nu"=NA,"nd"=NA,"gamma"=c(5,rep(0,400)))

#1.1_Intial values
df_25[1,"na"] <- 615
df_25[1,"nb"] <- 738
df_25[1,"nu"] <- 861
df_25[1,"nd"] <- 36592.5

for (i in 1:400){
  df_25[i+1,"year"] <- df_25[i,"year"]+dy25
  df_25[i+1,"na"] <- df_25[i,"na"] + (-kab*df_25[i,"na"]-kau*df_25[i,"na"]+kba*df_25[i,"nb"]+kua*df_25[i,"nu"]+df_25[i,"gamma"])*dy25
  df_25[i+1,"nb"] <- df_25[i,"nb"] + (kab*df_25[i,"na"]-kba*df_25[i,"nb"])*dy25
  df_25[i+1,"nu"] <- df_25[i,"nu"] + (kau*df_25[i,"na"]-kud*df_25[i,"nu"]-kua*df_25[i,"nu"]+kdu*df_25[i,"nd"])*dy25
  df_25[i+1,"nd"] <- df_25[i,"nd"] + (kud*df_25[i,"nu"] - kdu*df_25[i,"nd"])*dy25
  df_25[i+1,"nana0"] <- df_25[i+1,"na"]-df_25[1,"na"]
  df_25[i+1,"na_i-na_i-1"] <- df_25[i+1,"nana0"]-df_25[i,"nana0"]
}

#plotting
df_25[1,"nana0"]=0
gg25<-ggplot(df_25,aes(x=year))+
  geom_line(aes(y=nana0),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle(" 0.25 CO2 concentarion difference between disturbed and pre-industrial concentration")+
  xlab("Year")+
  ylab("na - na0")+
  theme(text = element_text(size=13))
print(gg25)

#time stpes : .50 year
dy50 <- 0.50
df_50 <- data.frame(year = c(1900,rep(0,200)),"na"=NA,"nb"=NA,"nu"=NA,"nd"=NA,"gamma"=c(5,rep(0,200)))

#1.1_steady state model
df_50[1,"na"] <- 615
df_50[1,"nb"] <- 738
df_50[1,"nu"] <- 861
df_50[1,"nd"] <- 36592.5

for (i in 1:200){
  df_50[i+1,"year"] <- df_50[i,"year"]+dy50
  df_50[i+1,"na"] <- df_50[i,"na"] + (-kab*df_50[i,"na"]-kau*df_50[i,"na"]+kba*df_50[i,"nb"]+kua*df_50[i,"nu"]+df_50[i,"gamma"])*dy50
  df_50[i+1,"nb"] <- df_50[i,"nb"] + (kab*df_50[i,"na"]-kba*df_50[i,"nb"])*dy50
  df_50[i+1,"nu"] <- df_50[i,"nu"] + (kau*df_50[i,"na"]-kud*df_50[i,"nu"]-kua*df_50[i,"nu"]+kdu*df_50[i,"nd"])*dy50
  df_50[i+1,"nd"] <- df_50[i,"nd"] + (kud*df_50[i,"nu"] - kdu*df_50[i,"nd"])*dy50
  df_50[i+1,"nana0"] <- df_50[i+1,"na"]-df_50[1,"na"]
  df_50[i+1,"na_i-na_i-1"] <- df_50[i+1,"nana0"]-df_50[i,"nana0"]
}
#plotting
df_50[1,"nana0"]=0
g50<-ggplot(df_50,aes(x=year))+
  geom_line(aes(y=nana0),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("0.50 CO2 concentarion difference between disturbed and pre-industrial concentration")+
  xlab("Year")+
  ylab("na - na0")+
  theme(text = element_text(size=13))
print(g50)
#1.2_time steps : 2 years

dy2=2
df2 <- data.frame(year = c(1900,rep(0,100)),"na"=NA,"nb"=NA,"nu"=NA,"nd"=NA,"gamma"=c(5,rep(0,100)))

#1.1_steady state model
df2[1,"na"] <- 615
df2[1,"nb"] <- 738
df2[1,"nu"] <- 861
df2[1,"nd"] <- 36592.5

for (i in 1:100){
  df2[i+1,"year"] <- df2[i,"year"]+dy2
  df2[i+1,"na"] <- df2[i,"na"] + (-kab*df2[i,"na"]-kau*df2[i,"na"]+kba*df2[i,"nb"]+kua*df2[i,"nu"]+df2[i,"gamma"])*dy2
  df2[i+1,"nb"] <- df2[i,"nb"] + (kab*df2[i,"na"]-kba*df2[i,"nb"])*dy2
  df2[i+1,"nu"] <- df2[i,"nu"] + (kau*df2[i,"na"]-kud*df2[i,"nu"]-kua*df2[i,"nu"]+kdu*df2[i,"nd"])*dy2
  df2[i+1,"nd"] <- df2[i,"nd"] + (kud*df2[i,"nu"] - kdu*df2[i,"nd"])*dy2
  df2[i+1,"nana0"] <- df2[i+1,"na"]-df2[1,"na"]
  df2[i+1,"na_i-na_i-1"] <- df2[i+1,"nana0"]-df2[i,"nana0"]
}

#plotting
win.graph()
df2[1,"nana0"]=0
g2<-ggplot(df2,aes(x=year))+
  geom_line(aes(y=nana0),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("CO2 concentarion difference between disturbed and pre-industrial concentration")+
  xlab("Year")+
  ylab("na - na0")+
  theme(text = element_text(size=13))+
  annotate("text",2050,8,label="Time step \ndy = 2",size=12,hjust=0,color= "navy")
  
print(g2)

#time steps : 4 years
dy4  <- 4
df_4 <- data.frame(year = c(1900,rep(0,100)),"na"=NA,"nb"=NA,"nu"=NA,"nd"=NA,"gamma"=c(5,rep(0,100)))

#1.1_intial values
df_4[1,"na"] <- 615
df_4[1,"nb"] <- 738
df_4[1,"nu"] <- 861
df_4[1,"nd"] <- 36592.5

for (i in 1:100){
  df_4[i+1,"year"] <- df_4[i,"year"]+dy4
  df_4[i+1,"na"] <- df_4[i,"na"] + (-kab*df_4[i,"na"]-kau*df_4[i,"na"]+kba*df_4[i,"nb"]+kua*df_4[i,"nu"]+df_4[i,"gamma"])*dy4
  df_4[i+1,"nb"] <- df_4[i,"nb"] + (kab*df_4[i,"na"]-kba*df_4[i,"nb"])*dy4
  df_4[i+1,"nu"] <- df_4[i,"nu"] + (kau*df_4[i,"na"]-kud*df_4[i,"nu"]-kua*df_4[i,"nu"]+kdu*df_4[i,"nd"])*dy4
  df_4[i+1,"nd"] <- df_4[i,"nd"] + (kud*df_4[i,"nu"] - kdu*df_4[i,"nd"])*dy4
  df_4[i+1,"nana0"] <- df_4[i+1,"na"]-df_4[1,"na"]
  df_4[i+1,"na_i-na_i-1"] <- df_4[i+1,"nana0"]-df_4[i,"nana0"]
}
#plotting
df_4[1,"nana0"]=0
win.graph()
g4<-ggplot(df_4,aes(x=year))+
  geom_line(aes(y=nana0),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("4 CO2 concentarion difference between disturbed and pre-industrial concentration")+
  xlab("Year")+
  ylab("na - na0")+
  theme(text = element_text(size=13))
print(g4)
#time steps :6 years
dy6 <-6
df_6 <- data.frame(year = c(1900,rep(0,100)),"na"=NA,"nb"=NA,"nu"=NA,"nd"=NA,"gamma"=c(5,rep(0,100)))

#1.1_steady state model
df_6[1,"na"] <- 615
df_6[1,"nb"] <- 738
df_6[1,"nu"] <- 861
df_6[1,"nd"] <- 36592.5

for (i in 1:100){
  df_6[i+1,"year"] <- df_6[i,"year"]+dy6
  df_6[i+1,"na"] <- df_6[i,"na"] + (-kab*df_6[i,"na"]-kau*df_6[i,"na"]+kba*df_6[i,"nb"]+kua*df_6[i,"nu"]+df_6[i,"gamma"])*dy6
  df_6[i+1,"nb"] <- df_6[i,"nb"] + (kab*df_6[i,"na"]-kba*df_6[i,"nb"])*dy6
  df_6[i+1,"nu"] <- df_6[i,"nu"] + (kau*df_6[i,"na"]-kud*df_6[i,"nu"]-kua*df_6[i,"nu"]+kdu*df_6[i,"nd"])*dy6
  df_6[i+1,"nd"] <- df_6[i,"nd"] + (kud*df_6[i,"nu"] - kdu*df_6[i,"nd"])*dy6
  df_6[i+1,"nana0"] <- df_6[i+1,"na"]-df_6[1,"na"]
  df_6[i+1,"na_i-na_i-1"] <- df_6[i+1,"nana0"]-df_6[i,"nana0"]
}
#plotting
df_6[1,"nana0"]=0
win.graph()
g2<-ggplot(df_6,aes(x=year))+
  geom_line(aes(y=nana0),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("6 CO2 concentarion difference between disturbed and pre-industrial concentration")+
  xlab("Year")+
  ylab("na - na0")+
  theme(text = element_text(size=13))
print(g6)
#time_steps : 8 years
dy8 <- 8
df_8 <- data.frame(year = c(1900,rep(0,100)),"na"=NA,"nb"=NA,"nu"=NA,"nd"=NA,"gamma"=c(5,rep(0,100)))

#1.1_steady state model
df_8[1,"na"] <- 615
df_8[1,"nb"] <- 738
df_8[1,"nu"] <- 861
df_8[1,"nd"] <- 36592.5

for (i in 1:100){
  df_8[i+1,"year"] <- df_8[i,"year"]+dy8
  df_8[i+1,"na"] <- df_8[i,"na"] + (-kab*df_8[i,"na"]-kau*df_8[i,"na"]+kba*df_8[i,"nb"]+kua*df_8[i,"nu"]+df_8[i,"gamma"])*dy8
  df_8[i+1,"nb"] <- df_8[i,"nb"] + (kab*df_8[i,"na"]-kba*df_8[i,"nb"])*dy8
  df_8[i+1,"nu"] <- df_8[i,"nu"] + (kau*df_8[i,"na"]-kud*df_8[i,"nu"]-kua*df_8[i,"nu"]+kdu*df_8[i,"nd"])*dy8
  df_8[i+1,"nd"] <- df_8[i,"nd"] + (kud*df_8[i,"nu"] - kdu*df_8[i,"nd"])*dy8
  df_8[i+1,"nana0"] <- df_8[i+1,"na"]-df_8[1,"na"]
  df_8[i+1,"na_i-na_i-1"] <- df_8[i+1,"nana0"]-df_8[i,"nana0"]
}
#plotting
df_8[1,"nana0"]=0
win.graph()
g8<-ggplot(df_8,aes(x=year))+
  geom_line(aes(y=nana0),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("8 CO2 concentarion difference between disturbed and pre-industrial concentration")+
  xlab("Year")+
  ylab("na - na0")+
  theme(text = element_text(size=13))
print(g8)



data=data.frame("na1"=df["na"],"na2"=df2["na"])
colnames(data)[2]<-"na2"
lmna=lm(formula = na2~na,data=data)
summary(lmna)

#normplot
qqnorm(df$na,col=3)
qqline(df$na,col="2")



