View(draft_data)

#predictor variable 
GP = draft_data$GP
#responce variables 
Age = draft_data$Age
Pos = draft_data$Pos#Qualtiative variable
MP = draft_data$MP
PTS = draft_data$PTS
TRB = draft_data$TRB
AST = draft_data$AST
STL = draft_data$STL
BLK = draft_data$BLK
FG = draft_data$FG.
ThreeP = draft_data$X3P.
FT = draft_data$FT.
WS48 = draft_data$WS.48

Model = lm(GP ~ Age + Pos + MP + PTS + TRB + AST +
             STL + BLK + FG + ThreeP + FT +
             WS48, data=draft_data)
summary(Model)


InteractionModel = lm(GP ~ Age + Pos + MP + PTS + TRB + AST +
                 STL + BLK + FG + ThreeP + FT +
                 WS48 + (Age*Pos) + (MP *Pos) + (PTS*Pos) + (TRB*Pos) + (AST*Pos) +
                 (STL*Pos) + (BLK*Pos) + (FG*Pos) + (ThreeP*Pos) + (FT*Pos) +
                 (WS48 *Pos) , data=draft_data)
summary(InteractionModel)

anova(Model,InteractionModel)


r = rstudent(Model)
plot(Model$fitted.values,r)
plot(r, type="b")
# Find outliers in the y space
which(r > 3)
which(r < -3)
points(which(r < -3), r[which(r < -3)], col = 'red')
points(which(r > 3), r[which(r > 3)], col = 'red')
NewData = draft_data[-c(258,315,661,733),]

#check for outliers
#remove outliers from original 



#View(NewData3)

#test the need for interaction


summary(Model)


r = rstudent(Model)
plot(Model$fitted.values,r)
plot(r, type="b")
# Find outliers in the y space
which(r > 3)
which(r < -3)
points(which(r < -3), r[which(r < -3)], col = 'red')
points(which(r > 3), r[which(r > 3)], col = 'red')
#315, 258, 661, 733

#Remove Outliers
NewData = draft_data[-c(258,315,661,733),]
#View(NewData)

Model = lm(GP ~ Age + Pos + MP + PTS + TRB + AST +
             STL + BLK + FG + ThreeP + FT +
             WS48, data=NewData)

summary(Model)
#slight r squre improvement 

r = rstudent(Model)
plot(Model$fitted.values,r)
plot(r, type="b")
which(r > 3)
which(r < -3)
points(which(r < -3), r[which(r < -3)], col = 'red')
points(which(r > 3), r[which(r > 3)], col = 'red')



GP = NewData$GP
Age = NewData$Age
Pos = NewData$Pos#Q
MP = NewData$MP
PTS = NewData$PTS
TRB = NewData$TRB
AST = NewData$AST
STL = NewData$STL
BLK = NewData$BLK
FG = NewData$FG.
ThreeP = NewData$X3P.
FT = NewData$FT.
WS48 = NewData$WS.48


Model = lm(GP ~ Age + Pos + MP + PTS + TRB + AST +
             STL + BLK + FG + ThreeP + FT +
             WS48, data=NewData)

summary(Model)
#slight r squre improvement 

r = rstudent(Model)
plot(Model$fitted.values,r)
plot(r, type="b")
which(r > 3)
which(r < -3)
points(which(r < -3), r[which(r < -3)], col = 'red')
points(which(r > 3), r[which(r > 3)], col = 'red')

NewData1 = NewData[-c(390,569),]
View(NewData1)

GP = NewData1$GP
Age = NewData1$Age
MP = NewData1$MP
PTS = NewData1$PTS
TRB = NewData1$TRB
AST = NewData1$AST
STL = NewData1$STL
BLK = NewData1$BLK
FG = NewData1$FG.
ThreeP = NewData1$X3P.
FT = NewData1$FT.
WS48 = NewData1$WS.48


Model = lm(GP ~ Age + Pos + MP + PTS + TRB + AST +
             STL + BLK + FG + ThreeP + FT +
             WS48, data=NewData1)

summary(Model)
#slight r squre improvement 

r = rstudent(Model)
plot(Model$fitted.values,r)
plot(r, type="b")
which(r > 3)
which(r < -3)
points(which(r < -3), r[which(r < -3)], col = 'red')
points(which(r > 3), r[which(r > 3)], col = 'red')

NewData2 = NewData1[-c(10),]
#View(NewData2)

GP = NewData2$GP
MP = NewData2$MP
Age = NewData2$Age
PTS = NewData2$PTS
TRB = NewData2$TRB
AST = NewData2$AST
STL = NewData2$STL
BLK = NewData2$BLK
FG = NewData2$FG.
ThreeP = NewData2$X3P.
FT = NewData2$FT.
WS48 = NewData2$WS.48


#regression with interaction


Model = lm(GP ~ Age + Pos + MP + PTS + TRB + AST +
             STL + BLK + FG + ThreeP + FT +
             WS48, data=NewData2)




summary(Model)
#slight r squre improvement 

r = rstudent(Model)
plot(Model$fitted.values,r)
plot(r, type="b")
which(r > 3)
which(r < -3)
points(which(r < -3), r[which(r < -3)], col = 'red')
points(which(r > 3), r[which(r > 3)], col = 'red')

View(NewData2)

NewData3 = NewData2[-c(649,654),]


GP = NewData3$GP
MP = NewData3$MP
Age = NewData3$Age
PTS = NewData3$PTS
TRB = NewData3$TRB
AST = NewData3$AST
STL = NewData3$STL
BLK = NewData3$BLK
FG = NewData3$FG.
ThreeP = NewData3$X3P.
FT = NewData3$FT.
WS48 = NewData3$WS.48



Model = lm(GP ~ Age + Pos + MP + PTS + TRB + AST +
             STL + BLK + FG + ThreeP + FT +
             WS48, data=NewData3)
summary(Model)
View(NewData3)
#slight r squre improvement 
r = rstudent(Model)
plot(Model$fitted.values,r)
plot(r, type="b")
which(r > 3)
which(r < -3)
points(which(r < -3), r[which(r < -3)], col = 'red')
points(which(r > 3), r[which(r > 3)], col = 'red')





#come back least squares regression line


#assumption analysis 



#linearity assumption

plot(GP,r)
#plot(Pk,r)

plot(Age,r)
plot(MP,r)
plot(PTS,r)
plot(TRB,r)
plot(AST,r)
plot(STL,r)
plot(BLK,r)
plot(FG,r)
plot(ThreeP,r)
plot(FT,r)

plot(WS48,r)

plot(Model)





#check for autocorreltion on error terms
library(lmtest)
dwtest(Model)
#high p value autocorrletion is not present






#check for predictor variables assumption 3
pairs(~GP+ Age + MP + PTS + TRB + AST +
        STL + BLK + FG + ThreeP + FT +
        WS48)


#fitted values
plot(Model$fitted.values,r)

#of numeric data corplot
View(NewData3)

cor(data.frame(GP, Age ,MP , PTS, TRB, AST,
               STL,BLK, FG,ThreeP, FT,WS48))


cor(data.frame(GP, Age ,MP , PTS, TRB, AST,STL,BLK, FG,ThreeP, FT,WS48))
#look at VIF    

View(NewData3)
cormatrix = cor(NewData3[1:743, -(1: 3)]) #take out year and Y variable by using (1:2) 
cormatrix 

library(car)
vif(Model) #MP is >10 

#mean(vif(Model))
#Eigan values

View(NewData3)

#get only numerical data for Eigen and Vif





#take out player and Y variable (GP) by using (1) 

View(NewData3)
NewData4 = NewData3[,- 1]
NewData5 = NewData4[,- 3]
View(NewData5)


cormatrix = cor(NewData5) 
cormatrix 

View(NewData5)

eigenVecs = eigen(cormatrix)
eigenVecs$values
sum(1 /eigenVecs$values)

#11*5 = 55 > 45.92372 

#45.92372 

#we  have confirmation that
#collinearity is not present  using the sum of eignvalues


#try an interaction variable

FullModel = lm(GP ~ Age + Pos + MP + PTS + TRB + AST +
                 STL + BLK + FG + ThreeP + FT +
                 WS48 + (Age*Pos) + (MP *Pos) + (PTS*Pos) + (TRB*Pos) + (AST*Pos) +
                 (STL*Pos) + (BLK*Pos) + (FG*Pos) + (ThreeP*Pos) + (FT*Pos) +
                 (WS48 *Pos) , data=NewData3)
#View(NewData3)

summary(FullModel)

anova(Model,FullModel)

#View(anova(Model,FullModel))

#deafualt is = frontcourt
# Compare the two models (note the slight loss of R2)
#Ho: B13 through B23 = 0 -> The reduced model is adequate
#Ha: at least one equality does not hold -> the full mode is adequate

# It appears that the reduced model is as adequate. So we do not need interaction


#want to find a model with a higher r sqaured and signicant p values


trying1 =lm(GP ~  MP+ WS48 + PTS + FG +Pos  , data=NewData3) 
#trying1 =lm(GP ~  MP+ WS48 + PTS + FG +Pos , data=NewData3)
View(NewData3)
#trying1 =lm(GP ~ WS48 +MP + FG + BLK +STL , data=NewData3) #if two quantitative is kosher
summary(trying1)
anova(trying1,Model)

trying1 =lm(GP ~  MP+ WS48 + Age, data=NewData3) #if two quantitative is kosher
summary(trying1)


#works at a really low alpha level 

trying1 = lm(GP ~ WS48 +Pos, data=NewData3)



summary(trying)

anova(FullModel,trying)

View(anova(Model,trying))
s