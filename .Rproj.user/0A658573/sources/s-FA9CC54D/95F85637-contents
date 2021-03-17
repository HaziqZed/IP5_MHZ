library(datasets)
library(tidyverse)
library(psych)

df <- read.csv("student-mat.csv")
head(df)

nrow(df)

str(df)

new <- df
new <- subset(new, select=-c(school, sex,address,famsize,Pstatus, Mjob, Fjob,
                             reason, guardian, schoolsup, famsup, paid,
                             activities, nursery, higher, internet, romantic))
t2 <- describe(new)
t2

school <- table(df$school)
sex <- table(df$sex)
addr <- table(df$address)
fsize <- table(df$famsize)
Pstatus <- table(df$Pstatus)
Mjob <- table(df$Mjob)
Fjob <- table(df$Fjob)
reason <- table(df$reason)
guard <- table(df$guardian)
ssup <- table(df$schoolsup)
fsup <- table(df$famsup)
paid <- table(df$paid)
actv <- table(df$activities)
nrs <- table(df$nursery)
higher <- table(df$higher)
internet <- table(df$internet)
rom <- table(df$romantic)

corr <- cor(new)
corr

heatmap(corr)

densF <- density(df$Fedu)
plot(densF, main='Distribution of Fathers Education', xlab='Fedu', ylab='Count')
polygon(densF, col="green", border="black")

densM <- density(df$Medu)
plot(densM, main='Distribution of Mothers Education', xlab='Medu', ylab='Count')
polygon(densM, col="Red", border="black")

densA <- density(df$age)
plot(densA, main='Distribution of Students Age', xlab='Age', ylab='Count')
polygon(densA, col="blue", border="black")

densT <- density(df$traveltime)
plot(densT, main='Distribution of Travel Time', xlab='traveltime', ylab='Count')
polygon(densT, col="purple", border="black")

attach(df)
plot(G1, G3, main="G1/G3", 
     xlab="G1  ", ylab="G3 ", pch=19)

plot(G2, G3, main="G2/G3", 
     xlab="G2  ", ylab="G3 ", pch=19)

plot(failures, G3, main="Failures/G3", 
     xlab="Failures  ", ylab="G3 ", pch=19)

plot(absences, G3, main="Absences/G3", 
     xlab="Absences  ", ylab="G3 ", pch=19)

plot(studytime, G3, main="Study Time/G3", 
     xlab="Study Time  ", ylab="G3 ", pch=19)

plot(Dalc, G3, main="Workday Alcohol Consumption/G3", 
     xlab="Workday Alcohol Consumption  ", ylab="G3 ", pch=19)

barplot(Fjob, main="Number of Students by Fathers Job", ylab="Fathers Job ", xlab="Count ", horiz=TRUE,
        names.arg=c("at_home", "heatlh", "other","services","teacher"),cex.names=0.8)

barplot(sex, main="Number of Students by Sex", ylab="Sex ", xlab="Count ", horiz=TRUE,
        names.arg=c("F","M"),cex.names=0.8)

barplot(Mjob, main="Number of Students by Mothers Job", ylab="Mothers Job ", xlab="Count ", horiz=TRUE,
        names.arg=c("at_home", "heatlh", "other","services","teacher"),cex.names=0.8)

barplot(internet, main="Number of Students by Internet", ylab="Internet ", xlab="Count ", horiz=TRUE,
        names.arg=c("no","yes"),cex.names=0.8)

barplot(addr, main="Number of Students by Address", ylab="Address ", xlab="Count ", horiz=TRUE,
        names.arg=c("R","U"),cex.names=0.8)

barplot(ssup, main="Number of Students by School Support", ylab="School Support ", xlab="Count ", horiz=TRUE,
        names.arg=c("no","yes"),cex.names=0.8)

barplot(fsup, main="Number of Students by Family Support", ylab="Family Support ", xlab="Count ", horiz=TRUE,
        names.arg=c("no","yes"),cex.names=0.8)

barplot(reason, main="Number of Students by Reason", ylab="Reason ", xlab="Count ", horiz=TRUE,
        names.arg=c("course","home","other","reputation"),cex.names=0.8)

barplot(fsize, main="Number of Students by Family Size", ylab="Family Size ",
        xlab="Count ", horiz=TRUE, names.arg=c("GT3","LE3"),cex.names=0.8)

noint <- df[which(df$internet=='no'),]
yint <- df[which(df$internet=='yes'),]
sint <- data.frame(mean(noint$G3),mean(yint$G3))
barplot(as.matrix(sint), main="Relationship between Internet and G3", 
        ylab="Mean G3", names.arg=c("No Internet", "Internet"))

urb <- df[which(df$address=='U'),]
rur <- df[which(df$address=='R'),]
sadd <- data.frame(mean(urb$G3),mean(rur$G3))
barplot(as.matrix(sadd), main="Relationship between Address and G3", 
        ylab="Mean G3", names.arg=c("Urban", "Rural"))

nofs <- df[which(df$famsup=='no'),]
yfs <- df[which(df$famsup=='yes'),]
sfs <- data.frame(mean(nofs$G3),mean(yfs$G3))
barplot(as.matrix(sfs), main="Relationship between Family Education Support and G3", 
        ylab="Mean G3", names.arg=c("No Family Support", "Family Support"))

boxplot(G3~age, data=rur, main='Boxplot grouped by age')

boxplot(G3~age, data=urb, main='Boxplot grouped by age')



