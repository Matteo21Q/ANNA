###########################################
####    ANNA: Analysis      ###############
###########################################

#Load libraries and set directory

library(readxl)
library(splines)
library(tableone)
setwd("C:/Users/Matteo/Downloads/Anna trial")
source("forestPlot.R")

#Load the data and set data to appropriate format:

load("AnnaData.RData")

################################################
#### Try simple model with treatment only:  ####

fit.unadj<-glm(Movements~Treatment, data = anna, family = poisson)
summary(fit.unadj)
exp(coef(summary(fit.unadj))[2,1])
exp(confint(fit.unadj)[2,])
c(exp(coef(fit.unadj)[1]+coef(fit.unadj)[2]),
  exp(coef(fit.unadj)[1]),
  exp(coef(fit.unadj)[1]+coef(fit.unadj)[2])-exp(coef(fit.unadj)[1])
)

################################################
#### Try simple model with binary moved:    ####

fit.unadj.01<-glm(moved~Treatment, data = anna, family = binomial)
summary(fit.unadj.01)
exp(coef(summary(fit.unadj.01))[2,1])
exp(confint(fit.unadj.01)[2,])

#################################################
#### Try adjusted model with categor covs:   ####

fit.adj<-glm(Movements~Treatment+Position+closemeal+Pregmonth+Timeofday, 
             data = anna, family = poisson)
summary(fit.adj)
exp(coef(summary(fit.adj))[2,1])
exp(confint(fit.adj)[2,])
c(exp(coef(fit.adj)[1]+coef(fit.adj)[2]),
  exp(coef(fit.adj)[1]),
  exp(coef(fit.adj)[1]+coef(fit.adj)[2])-exp(coef(fit.adj)[1])
)

####################################################
#### Try adjusted model with binary outcome&cov:####

fit.adj.01<-glm(moved~Treatment+Position+closemeal+Pregmonth+Timeofday, 
                data = anna, family = binomial)
summary(fit.adj.01)
exp(coef(summary(fit.adj.01))[2,1])
exp(confint(fit.adj.01)[2,])

################################################
#### Try adjusted model with splines:       ####

fit.adj.sp<-glm(Movements~Treatment+Position+ns(Lastmeal,3)+ns(Time,3)+ns(Date.scaled,3),
             data = anna, family = poisson)
summary(fit.adj.sp)
exp(coef(summary(fit.adj.sp))[2,1])
exp(confint(fit.adj.sp)[2,])
c(exp(coef(fit.adj.sp)[1]+coef(fit.adj.sp)[2]),
  exp(coef(fit.adj.sp)[1]),
  exp(coef(fit.adj.sp)[1]+coef(fit.adj.sp)[2])-exp(coef(fit.adj.sp)[1])
)

####################################################
#### Try adjusted model with binary outcome:    ####

fit.adj.sp01<-glm(moved~Treatment+Position+ns(Lastmeal,3)+ns(Time,3)+ns(Date.scaled,3), 
                data = anna, family = binomial)
summary(fit.adj.sp01)
exp(coef(summary(fit.adj.sp01))[2,1])
exp(confint(fit.adj.sp01)[2,])


####################################################
####  Now print table 1                         ####

Tab1<-CreateTableOne(data=anna, 
                     vars = c("Time", "Timeofday","Lastmeal","Position","Pregmonth"),
                     strata = "Treatment")
Tab1p<-print(Tab1, nonnormal = c("Time", "Lastmeal"))
write.csv(Tab1p, file = "Table1.csv")

####################################################
#### Now print table 2                          ####

Tab2<-data.frame(matrix(NA,4,7))
colnames(Tab2)<-c("Analysis", "Mean difference", "Mean in control", "Mean in active",
                  "Estimate", "SE", "p-value")
Tab2[1,]<-c("Poisson - Fully adjusted", 
            exp(coef(fit.adj.sp)[1]+coef(fit.adj.sp)[2])-exp(coef(fit.adj.sp)[1]),
            exp(coef(fit.adj.sp)[1]),
            exp(coef(fit.adj.sp)[1]+coef(fit.adj.sp)[2]),
            coef(summary(fit.adj.sp))[2,1],
            coef(summary(fit.adj.sp))[2,2],
            coef(summary(fit.adj.sp))[2,4])
Tab2[2,]<-c("Poisson - Unadjusted", 
            exp(coef(fit.unadj)[1]+coef(fit.unadj)[2])-exp(coef(fit.unadj)[1]),
            exp(coef(fit.unadj)[1]),
            exp(coef(fit.unadj)[1]+coef(fit.unadj)[2]),
            coef(summary(fit.unadj))[2,1],
            coef(summary(fit.unadj))[2,2],
            coef(summary(fit.unadj))[2,4])

preds<-predict.glm(fit.adj.sp01,
            newdata = data.frame(Treatment=c("Silence","Talking"), 
                                 Lastmeal=rep(mean(anna$Lastmeal),2),
                                 Time=rep(mean(anna$Time),2),
                                 Date.scaled=rep(mean(anna$Date.scaled),2),
                                 Position=rep("Sitting",2)), type = "resp")
Tab2[3,]<-c("Logistic - Fully adjusted", 
            exp(coef(summary(fit.adj.sp01))[2,1]),
            preds[1]*100,
            preds[2]*100,
            coef(summary(fit.adj.sp01))[2,1],
            coef(summary(fit.adj.sp01))[2,2],
            coef(summary(fit.adj.sp01))[2,4])

preds2<-predict.glm(fit.unadj.01,
                   newdata = data.frame(Treatment=c("Silence","Talking")), type = "resp")
Tab2[4,]<-c("Logistic - Unadjusted", 
            exp(coef(summary(fit.unadj.01))[2,1]),
            preds[1]*100,
            preds[2]*100,
            coef(summary(fit.unadj.01))[2,1],
            coef(summary(fit.unadj.01))[2,2],
            coef(summary(fit.unadj.01))[2,4])
for (i in 2:7) Tab2[,i]<-as.numeric(Tab2[,i])
Tab2[,2:7]<-round(Tab2[,2:7],2)
Tab2<-Tab2[,c(1,5,6,2,3,4,7)]
View(Tab2)                                         

####################################################
####  Now Time series plot                      ####

anna$wholetime<-anna$Day-min(anna$Day)+anna$Time*60*60
par(mar=c(6.1, 4.1, 2.1, 1.1))
plot(Movements~wholetime,data=anna, pch=20, type="h",
     xaxt="n", xlab="", ylab="Number of movements", main="N. of movements over time")
axis(1, at=seq(54000,6012000,length.out=9), 
     labels = format(seq(as.Date("2020/05/30"), as.Date("2020/08/07"), length.out=9), "%b %d"), 
     las=2)
# Add cumulative moving average:
anna$movav<-anna$Movements
for (i in 1:nrow(anna)) {
  anna$movav[i]<-mean(as.numeric(anna[1:i,"Movements"]))
}
lines(movav~wholetime, data=anna, col="red")

###################################################
####  Now forest plot                          ####

n.r<-16
IRR<-rep(NA,n.r)
l95<-rep(NA,n.r)
u95<-rep(NA,n.r)
p.inter<-rep(NA,n.r)
N<-rep(NA,n.r)
label<-rep(NA,n.r)

#### First, Overall:
IRR[1]<-exp(coef(fit.unadj)[2])
l95[1]<-exp(confint(fit.unadj)[2,])[1]
u95[1]<-exp(confint(fit.unadj)[2,])[2]
N[1]<-200

####Now, by pregnancy month:

label[1]<-"Pregnancy month"
label[2]<-"Sixth"
label[3]<-"Seventh"
label[4]<-"Eighth"

for (i in 6:8) {
  fit<-glm(Movements~Treatment, data = anna[anna$Pregmonth==i,], family = poisson)
  IRR[i-3]<-exp(coef(fit)[2])
  l95[i-3]<-exp(confint(fit)[2,])[1]
  u95[i-3]<-exp(confint(fit)[2,])[2]
  N[i-3]<-nrow(anna[anna$Pregmonth==i,])
}

# Test for interaction:
fit.noint<-glm(Movements~Treatment+factor(Pregmonth), data = anna, family = poisson)
fit.int<-glm(Movements~Treatment*factor(Pregmonth), data = anna, family = poisson)
p.inter[3]<-anova(fit.noint,fit.int, test = "Chisq")[2,5]


####Now, by time of day:
label[5]<-"Time of Day"
label[6]<-"Morning (0-12)"
label[7]<-"Afternoon (12-19)"
label[8]<-"Evening (19-24)"

Times.of.day<-c("Morning", "Afternoon", "Evening")

for (i in 1:3) {
  fit<-glm(Movements~Treatment, data = anna[anna$Timeofday==Times.of.day[i],], family = poisson)
  IRR[i+6]<-exp(coef(fit)[2])
  l95[i+6]<-exp(confint(fit)[2,])[1]
  u95[i+6]<-exp(confint(fit)[2,])[2]
  N[i+6]<-nrow(anna[anna$Timeofday==Times.of.day[i],])
}

# Test for interaction:
fit.noint<-glm(Movements~Treatment+factor(Timeofday), data = anna, family = poisson)
fit.int<-glm(Movements~Treatment*factor(Timeofday), data = anna, family = poisson)
p.inter[7]<-anova(fit.noint,fit.int, test = "Chisq")[2,5]


####Now, by distance from meal:
label[9]<-"Proximity last meal"
label[10]<-"Less than 2 hours"
label[11]<-"At least 2 hours"

TF<-c(T,F)

for (i in 1:2) {
  fit<-glm(Movements~Treatment, data = anna[anna$closemeal==TF[i],], family = poisson)
  IRR[i+10]<-exp(coef(fit)[2])
  l95[i+10]<-exp(confint(fit)[2,])[1]
  u95[i+10]<-exp(confint(fit)[2,])[2]
  N[i+10]<-nrow(anna[anna$closemeal==TF[i],])
}

# Test for interaction:
fit.noint<-glm(Movements~Treatment+factor(closemeal), data = anna, family = poisson)
fit.int<-glm(Movements~Treatment*factor(closemeal), data = anna, family = poisson)
p.inter[11]<-anova(fit.noint,fit.int, test = "Chisq")[2,5]


####Now, by Position:
label[12]<-"Position"
label[13]<-"Supine"
label[14]<-"Sitting"
label[15]<-"Standing"

Pos<-label[13:15]

for (i in 1:3) {
  fit<-glm(Movements~Treatment, data = anna[anna$Position==Pos[i],], family = poisson)
  IRR[i+13]<-exp(coef(fit)[2])
  l95[i+13]<-exp(confint(fit)[2,])[1]
  u95[i+13]<-exp(confint(fit)[2,])[2]
  N[i+13]<-nrow(anna[anna$Position==Pos[i],])
}

# Test for interaction:
fit.noint<-glm(Movements~Treatment+factor(Position), data = anna, family = poisson)
fit.int<-glm(Movements~Treatment*factor(Position), data = anna, family = poisson)
p.inter[14]<-anova(fit.noint,fit.int, test = "Chisq")[2,5]

Table.res<-data.frame(label,IRR,l95,u95,p.inter,N)
Table.res[,2:4]<-round(Table.res[,2:4],2)
Table.res[,5]<-round(Table.res[,5],3)
Table.res[3,5]<-"<0.001"
View(Table.res)


forestPlot(IRR,l95,u95,p.inter,label,Table.res, pos.p = 4.5, pch.graf = 18)