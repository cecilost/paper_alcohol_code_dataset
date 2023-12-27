
# R Codes for:
# López Steinmetz LC, Leyes CA, Fong SB & Godoy JC."Alcohol consumption, 
# alcohol expectancies, and drinking contexts in young Argentinean college students 
# before and during the COVID-19 pandemic: A one-year follow-up study"

####################################################################################
############################ ABBREVIATIONS #########################################
####################################################################################

# Abbreviations:
# EA = Alcohol Expectancy (Expectativas hacia el alcohol)
# CC = Consumption Contexts (Contextos de consumo de alcohol)

####################################################################################
########################## LOAD THE DATASETS #######################################
####################################################################################

#################################################
######## Load the dataset (LONGITUDINAL) ######## 

dataset<-read.table("clipboard",header=TRUE, encoding = "Latin 1", dec=",", sep="\t") # LONGITUDINAL


## To explore the data
library(dplyr)
glimpse(dataset)  # here we can also check the type of the variables
summary(dataset)

# Check the type of the variables (LONGITUDINAL) and transform them (when necessary)

str(dataset)

# $ sexo               : chr  "mujer" "mujer" "mujer" "mujer" ... CONVERT TO Factor
# $ aunidalcsem        : chr  "1.65" "1.65" "1.65" "1.8" ... CONVERT TO num (or dbl) # numeric is identical to double (and real)
# $ aALCSEM            : chr  "1.65" "11.55" "4.95" "1.8" ... CONVERT TO num (or dbl)
# $ bunidalcsem        : chr  "1.65" "1.65" "1.65" "3" ... CONVERT TO num (or dbl)
# $ bALCSEM            : chr  "1.65" "8.25" "4.95" "21" ... CONVERT TO num

dataset$sexo<-as.factor(dataset$sexo)
dataset$aunidalcsem<-as.numeric(dataset$aunidalcsem)
dataset$aALCSEM<-as.numeric(dataset$aALCSEM)
dataset$bunidalcsem<-as.numeric(dataset$bunidalcsem)
dataset$bALCSEM<-as.numeric(dataset$bALCSEM)

# To order categories

dataset$sexo <- factor(dataset$sexo, levels=c("mujer", "varon")) # to order categories

str(dataset)

#################################################
###### Load the dataset (CROSS-SECTIONAL) #######

transv<-read.table("clipboard",header=TRUE, encoding = "Latin 1", dec=",", sep="\t") # CROSS-SECTIONAL

transv$tsexo <- factor(transv$tsexo, levels=c("Wom", "Man")) # to order categories


# Check the type of the variables (CROSS-SECTIONAL) and transform them (when necessary)

str(transv)

# $ tunidadesalc       : chr  "3" "1.65" "1.65" "3" ... CONVERT TO num
# $ tALCSEM            : chr  "12" "1.65" "1.65" "9" ... CONVERT TO num

transv$tunidadesalc<-as.numeric(transv$tunidadesalc)
transv$tALCSEM<-as.numeric(transv$tALCSEM)

str(transv)

####################################################################################
########################### METHODS SECTION ########################################
####################################################################################

####################################################################################
################# SKEWNESS, KURTOSIS, & MULTICOLLINEARITY ##########################

# To test Skewness and Kurtosis: See codes for supplementary files (at the end of these scripts)
# To test for multicollinearity: See codes supplementary files (at the end of these scripts)


####################################################################################
###################### DESCRIPTION OF THE SAMPLE ###################################

#################################################
########  LONGITUDINAL ######## 

# Sex
table(dataset$sexo)
# mujer varon 
# 210    90 
prop.table(table(dataset$sexo))*100
# mujer varon 
# 70    30

# Age
mean(dataset$aedad) # at the first measurement
# 20.79
sd(dataset$aedad) # at the first measurement
# 1.8504

# Age of onset of alcohol consumption
mean(dataset$aedadinicioalc)
# 14.16
sd(dataset$aedadinicioalc)
# 1.364039

# Weekly alcohol consumption (Units of alcohol): First measurement:
mean(dataset$aALCSEM)
# 6.9124
sd(dataset$aALCSEM)
# 5.883082

# Weekly alcohol consumption (Units of alcohol): Second measurement:
mean(dataset$bALCSEM, na.rm = TRUE)
# 8.258067
sd(dataset$bALCSEM, na.rm = TRUE)
# 7.791377


#################################################
########  CROSS-SECTIONAL ########

# Sex
table(transv$tsexo)
# Wom Man 
# 130  35 
prop.table(table(transv$tsexo))*100
#      Wom      Man 
# 78.78788 21.21212

# Age
mean(transv$tedad) # at the first measurement
# 22.2
sd(transv$tedad) # at the first measurement
# 1.707944

# Age of onset of alcohol consumption
mean(transv$tedadinicioalc)
# 14.64242
sd(transv$tedadinicioalc)
# 2.152569

# Weekly alcohol consumption (Units of alcohol)
mean(transv$tALCSEM)
# 8.802182
sd(transv$tALCSEM)
# 11.52124


####################################################################################
######################### LOG TRANSFORMATIONS ######################################

###################################################################################
######################### LONGITUDINAL SAMPLE #####################################
###################################################################################

newdata <- dataset

####### Log transformations ####### 

########### Time 1 (T1)

# # # # WEEKLY CONSUMPTION OF ALCOHOLIC BEVERAGES
newdata$logaALCSEM <- log(newdata$aALCSEM + 1) 

# # # # ALCOHOL EXPECTANCIES
### POSITIVE EA. Time 1:
newdata$logaEAPOS <- log(newdata$aEAPOS + 1)

### NEGATIVE EA. Time 1:
newdata$logaEANEG <- log(newdata$aEANEG + 1)

# # # # CONSUMPTION CONTEXTS

### SOCIAL FACILITATION. Time 1:
newdata$logafacilitsoc <- log(newdata$afacilitsoc + 1)

### PEER GROUP ACCEPTANCE. Time 1:
newdata$logaaceptpares <- log(newdata$aaceptpares + 1)

### PARENTAL CONTROL. Time 1:
newdata$logactrolparent <- log(newdata$actrolparent + 1)

### STRESS CONTROL. Time 1:
newdata$logactrolestres <- log(newdata$actrolestres + 1)


########### Time 2 (T2)
# # # # WEEKLY CONSUMPTION OF ALCOHOLIC BEVERAGES
newdata$logbALCSEM <- log(newdata$bALCSEM + 1)

# # # # ALCOHOL EXPECTANCIES
### POSITIVE EA. Time 2:
newdata$logbEAPOS <- log(newdata$bEAPOS + 1)

### NEGATIVE EA. Time 2:
newdata$logbEANEG <- log(newdata$bEANEG + 1)

# # # # CONSUMPTION CONTEXTS
### SOCIAL FACILITATION. Time 2:
newdata$logbfacilitsoc <- log(newdata$bfacilitsoc + 1)

### PEER GROUP ACCEPTANCE. Time 2:
newdata$logbaceptpares <- log(newdata$baceptpares + 1)

### PARENTAL CONTROL. Time 2:
newdata$logbctrolparent <- log(newdata$bctrolparent + 1)

### STRESS CONTROL. Time 2:
newdata$logbctrolestres <- log(newdata$bctrolestres + 1)



###################################################################################
######################### LONGITUDINAL SAMPLE #####################################
###################################################################################
###################################################################################
############# BASED ON MEASURES OF EA AND cc AT THE 1ST MEASUREMENT ###############
###################################################################################

####### Preparing the data: based on the LOG TRANSFORMED VALUES

# To select the variables from the "newdata" dataset: FOR EA AND CC HERE WE USE THE SCORES AS MEASURED AT TIME 1
library(dplyr)
datasetlongt1<-select(newdata, code, sexo, aedad, aedadinicioalc, logaEAPOS, logaEANEG, logafacilitsoc, logaaceptpares, logactrolparent, logactrolestres, logaALCSEM, logbALCSEM)

# To convert the format of the dataframe into the long format:
library(reshape)
longt1<-melt(datasetlongt1, id = c("code","sexo", "aedad", "aedadinicioalc", "logaEAPOS", "logaEANEG", "logafacilitsoc", "logaaceptpares", "logactrolparent", "logactrolestres"), measured = c("logaALCSEM","logbALCSEM"))

# To rename variables:
names(longt1)<-c("participant","sexo", "aedad", "aedadinicioalc", "logaEAPOS", "logaEANEG", "logafacilitsoc", "logaaceptpares", "logactrolparent", "logactrolestres", "variable", "scores") # "variable" refers to the repeated-measures variable

# This creates a variable "period" in the dataframe longt1:
longt1$period<-gl(2, 300, labels = c("first", "second")) # We created 2 sets of 300 scores, the labels option then specifies the names to attach to these 2 sets, which correspond to the levels of "period" (first measurement and follow-up).

# To make it clearer that there are two observations for each participant we have sorted the data by participant:
longt1<-longt1[order(longt1$participant),]

longt1[1:6, ]

library(mice)

set.seed(123)

iniLONGT1 <- mice(longt1, maxit = 0)

predLONGT1 <- iniLONGT1$pred
predLONGT1

# Setting values of variables I'd like to leave out to 0 in the predictor matrix
predLONGT1[, c("variable", "period")] <- 0
predLONGT1

predLONGT1["scores", ] <- c(-2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # The class variable (only one is allowed) is coded by a `-2'.

impLONGT1 <- mice(longt1, meth = c("", "", "", "", "", "", "", "", "", "", "", "2l.norm", ""), m = 10, pred = predLONGT1, maxit = 5, seed = 123) # The dependent variable is "scores" (alcohol consumption)

print(impLONGT1) 
impLONGT1$imp$scores # to see the imputed values

# Distribution of observed/imputed values
xyplot(impLONGT1, scores ~ logaEAPOS | .imp, pch = 20, cex=1.4)
xyplot(impLONGT1, scores ~ logaEANEG | .imp, pch = 20, cex=1.4)
xyplot(impLONGT1, scores ~ logafacilitsoc | .imp, pch = 20, cex=1.4)
xyplot(impLONGT1, scores ~ logaaceptpares | .imp, pch = 20, cex=1.4)
xyplot(impLONGT1, scores ~ logactrolparent | .imp, pch = 20, cex=1.4)
xyplot(impLONGT1, scores ~ logactrolestres | .imp, pch = 20, cex=1.4)

# Complete the dataset:
myDatalongt1 <- complete(impLONGT1, 10) # WE IMPUTED THE 10TH DATASET



####### To select the order in which the predictors will be included into the model:
library(leaps)

bestLONGT1<-regsubsets(scores ~ period + sexo + aedad + aedadinicioalc + logaEAPOS + logaEANEG + logafacilitsoc + logaaceptpares + logactrolparent + logactrolestres, data = myDatalongt1, nvmax = 11, method = "exhaustive")

summary(bestLONGT1, all.best = TRUE)
# Subset selection object
# Call: regsubsets.formula(scores ~ period + sexo + aedad + aedadinicioalc + 
#     logaEAPOS + logaEANEG + logafacilitsoc + logaaceptpares + 
#     logactrolparent + logactrolestres, data = myDatalongt1, nvmax = 11, 
#     method = "exhaustive")
# 10 Variables  (and intercept)
#                 Forced in Forced out
# periodsecond        FALSE      FALSE
# sexovaron           FALSE      FALSE
# aedad               FALSE      FALSE
# aedadinicioalc      FALSE      FALSE
# logaEAPOS           FALSE      FALSE
# logaEANEG           FALSE      FALSE
# logafacilitsoc      FALSE      FALSE
# logaaceptpares      FALSE      FALSE
# logactrolparent     FALSE      FALSE
# logactrolestres     FALSE      FALSE
# 1 subsets of each size up to 10
# Selection Algorithm: exhaustive
#           periodsecond sexovaron aedad aedadinicioalc logaEAPOS logaEANEG logafacilitsoc logaaceptpares logactrolparent logactrolestres
# 1  ( 1 )  " "          " "       " "   " "            " "       " "       "*"            " "            " "             " "            
# 2  ( 1 )  " "          " "       " "   " "            " "       " "       "*"            " "            " "             "*"            
# 3  ( 1 )  " "          " "       " "   " "            " "       " "       "*"            " "            "*"             "*"            
# 4  ( 1 )  " "          " "       " "   "*"            " "       " "       "*"            "*"            " "             "*"            
# 5  ( 1 )  " "          " "       " "   "*"            " "       " "       "*"            "*"            "*"             "*"            
# 6  ( 1 )  " "          "*"       " "   "*"            " "       " "       "*"            "*"            "*"             "*"            
# 7  ( 1 )  " "          "*"       "*"   "*"            " "       " "       "*"            "*"            "*"             "*"            
# 8  ( 1 )  "*"          "*"       "*"   "*"            " "       " "       "*"            "*"            "*"             "*"            
# 9  ( 1 )  "*"          "*"       "*"   "*"            "*"       " "       "*"            "*"            "*"             "*"            
# 10  ( 1 ) "*"          "*"       "*"   "*"            "*"       "*"       "*"            "*"            "*"             "*"           

plot(bestLONGT1, scale = "r2", main = "Best R^2") # R2 = 0.16
plot(bestLONGT1, scale = "adjr2", main = "Best Adjusted R^2") # Adj R2 = 0.15 

####### MULTILEVEL ANALYSYS (MIXED EFFECTS)

# Building the model
library(nlme)

interceptOnlyT1 <-gls(scores ~ 1, data = myDatalongt1, method = "ML") # only the intercept

model1<-lme(scores ~ 1, random = ~1|participant, data = myDatalongt1, method = "ML", control = lmeControl(opt = "optim")) # the nested structure indicates repeated measures (within variable) # repeated measures nested within participants
anova(interceptOnlyT1, model1)  # to compare models

# To see the overall effect of each main effect we added them to the model one at a time:

t1facsoc<-update(model1, .~. + logafacilitsoc)
summary(t1facsoc)  
anova(interceptOnlyT1, model1, t1facsoc) 

t1ctrlstress<-update(t1facsoc, .~. + logactrolestres)
summary(t1ctrlstress)  
anova(interceptOnlyT1, model1, t1facsoc, t1ctrlstress) 

t1ctrlparental<-update(t1ctrlstress, .~. + logactrolparent)
summary(t1ctrlparental)  
anova(interceptOnlyT1, model1, t1facsoc, t1ctrlstress, t1ctrlparental)

t1edadinicio<-update(t1ctrlparental, .~. + aedadinicioalc)
summary(t1edadinicio) 
anova(interceptOnlyT1, model1, t1facsoc, t1ctrlstress, t1ctrlparental, t1edadinicio)

t1aceptpares<-update(t1edadinicio, .~. + logaaceptpares)
summary(t1aceptpares) 
anova(interceptOnlyT1, model1, t1facsoc, t1ctrlstress, t1ctrlparental, t1edadinicio, t1aceptpares)

t1sexo<-update(t1aceptpares, .~. + sexo)
summary(t1sexo) 
anova(interceptOnlyT1, model1, t1facsoc, t1ctrlstress, t1ctrlparental, t1edadinicio, t1aceptpares, t1sexo)

t1edad<-update(t1sexo, .~. + aedad)
summary(t1edad)  
anova(interceptOnlyT1, model1, t1facsoc, t1ctrlstress, t1ctrlparental, t1edadinicio, t1aceptpares, t1sexo, t1edad)

t1period<-update(t1edad, .~. + period)
summary(t1period)  
anova(interceptOnlyT1, model1, t1facsoc, t1ctrlstress, t1ctrlparental, t1edadinicio, t1aceptpares, t1sexo, t1edad, t1period)

t1eapos<-update(t1period, .~. + logaEAPOS)
summary(t1eapos)  
anova(interceptOnlyT1, model1, t1facsoc, t1ctrlstress, t1ctrlparental, t1edadinicio, t1aceptpares, t1sexo, t1edad, t1period, t1eapos)  

t1eaneg<-update(t1eapos, .~. + logaEANEG)
summary(t1eaneg) 
anova(interceptOnlyT1, model1, t1facsoc, t1ctrlstress, t1ctrlparental, t1edadinicio, t1aceptpares, t1sexo, t1edad, t1period, t1eapos, t1eaneg)
#                 Model df      AIC      BIC    logLik     Test   L.Ratio p-value
# interceptOnlyT1     1  2 1263.216 1272.010 -629.6082                           
# model1              2  3 1158.747 1171.938 -576.3737   1 vs 2 106.46901  <.0001
# t1facsoc            3  4 1120.185 1137.772 -556.0923   2 vs 3  40.56269  <.0001
# t1ctrlstress        4  5 1113.118 1135.102 -551.5589   3 vs 4   9.06691  0.0026
# t1ctrlparental      5  6 1109.236 1135.617 -548.6177   4 vs 5   5.88223  0.0153
# t1edadinicio        6  7 1106.765 1137.543 -546.3823   5 vs 6   4.47082  0.0345
# t1aceptpares        7  8 1103.877 1139.052 -543.9385   6 vs 7   4.88768  0.0270 ####
# t1sexo              8  9 1102.114 1141.687 -542.0571   7 vs 8   3.76269  0.0524
# t1edad              9 10 1101.746 1145.715 -540.8730   8 vs 9   2.36835  0.1238
# t1period           10 11 1102.690 1151.056 -540.3451  9 vs 10   1.05580  0.3042
# t1eapos            11 12 1104.589 1157.352 -540.2946 10 vs 11   0.10097  0.7507
# t1eaneg            12 13 1106.553 1163.714 -540.2767 11 vs 12   0.03571  0.8501


summary(t1aceptpares)
# Linear mixed-effects model fit by maximum likelihood
# Data: myDatalongt1 
# AIC      BIC    logLik
# 1103.877 1139.052 -543.9385

# Random effects:
#   Formula: ~1 | participant
#          (Intercept)  Residual
# StdDev:   0.4349729 0.4652997

# Fixed effects: scores ~ logafacilitsoc + logactrolestres + logactrolparent + aedadinicioalc + logaaceptpares 
#                      Value Std.Error  DF   t-value p-value
# (Intercept)     -0.4106120 0.6331039 300 -0.648570  0.5171
# logafacilitsoc   0.6866266 0.1819691 294  3.773315  0.0002
# logactrolestres  0.3272813 0.1125224 294  2.908588  0.0039
# logactrolparent  0.2252867 0.1112699 294  2.024686  0.0438
# aedadinicioalc  -0.0497394 0.0237692 294 -2.092601  0.0372
# logaaceptpares  -0.2910891 0.1317922 294 -2.208698  0.0280
# Correlation: 
#   (Intr) lgfclt lgctrls lgctrlp addncl
# logafacilitsoc  -0.470                              
# logactrolestres -0.166 -0.183                       
# logactrolparent -0.078 -0.528 -0.067                
# aedadinicioalc  -0.633  0.004  0.129   0.108        
# logaaceptpares  -0.188 -0.316 -0.144   0.082  -0.016

# Standardized Within-Group Residuals:
#   Min            Q1           Med            Q3           Max 
# -2.9396728254 -0.4916676942  0.0004019576  0.4830690997  3.0103228682 

# Number of Observations: 600
# Number of Groups: 300 


intervals(t1aceptpares)
# Approximate 95% confidence intervals

# Fixed effects:
#                        lower        est.        upper
# (Intercept)     -1.650253920 -0.41061203  0.829029853
# logafacilitsoc   0.330294635  0.68662663  1.042958631
# logactrolestres  0.106939854  0.32728127  0.547622684
# logactrolparent  0.007397831  0.22528670  0.443175573
# aedadinicioalc  -0.096284163 -0.04973937 -0.003194577
# logaaceptpares  -0.549164586 -0.29108910 -0.033013617
# attr(,"label")
# [1] "Fixed effects:"

# Random Effects:
#   Level: participant 
#                     lower      est.    upper
# sd((Intercept)) 0.3804773 0.4349729 0.497274

# Within-group standard error:
#     lower      est.     upper 
# 0.4295220 0.4652997 0.5040576 



library(pastecs)
by(myDatalongt1$scores, list(myDatalongt1$period), stat.desc, basic = FALSE)
by(myDatalongt1$scores, list(myDatalongt1$period, myDatalongt1$sexo), stat.desc, basic = FALSE)


# Calculating effect sizes
library(DSUR.noof)
# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# rcontrast(t-value, DF) # this data is in: summary(t1aceptpares)

# logafacilitsoc  
rcontrast(3.773315, 294)
# [1] "r = 0.214921587183853"

# logactrolestres   
rcontrast(2.908588, 294)
# [1] "r = 0.167243141699136"

# logactrolparent    
rcontrast(2.024686, 294)
# [1] "r = 0.117267363977441"

# aedadinicioalc     
rcontrast(-2.092601, 294)
# [1] "r = 0.121144113656125"

# logaaceptpares
rcontrast(-2.208698, 294)
# [1] "r = 0.127758296303109"


###################################################################################
######################### LONGITUDINAL SAMPLE #####################################
###################################################################################
###################################################################################
################ BASED ON MEASURES OF EA AND cc AT THE FOLLOW UP ##################
###################################################################################

# We have run the same analysis, but now considering the EA and CC scores as measured at time 2 (follow-up)

####### Preparing the data: based on LOG TRANSFORMED VALUES

# To select the variables from the "newdata" dataset: FOR EA AND CC HERE WE USE THE SCORES AS MEASURED AT TIME 2
# library(dplyr)
datasetlongt2<-select(newdata, code, sexo, aedad, aedadinicioalc, logbEAPOS, logbEANEG, logbfacilitsoc, logbaceptpares, logbctrolparent, logbctrolestres, logaALCSEM, logbALCSEM)

# To convert the format of the dataframe into the long format:
# library(reshape)
longt2<-melt(datasetlongt2, id = c("code","sexo", "aedad", "aedadinicioalc", "logbEAPOS", "logbEANEG", "logbfacilitsoc", "logbaceptpares", "logbctrolparent", "logbctrolestres"), measured = c("logaALCSEM","logbALCSEM"))

# To rename variables:
names(longt2)<-c("participant","sexo", "aedad", "aedadinicioalc", "logbEAPOS", "logbEANEG", "logbfacilitsoc", "logbaceptpares", "logbctrolparent", "logbctrolestres", "variable", "scores") # "variable" refers to the repeated-measures variable

# This creates a variable "period" in the dataframe longt2:
longt2$period<-gl(2, 300, labels = c("first", "second")) # We created 2 sets of 300 scores, the labels option then specifies the names to attach to these 2 sets, which correspond to the levels of "period" (first measurement and follow-up).

# To make it clearer that there are two observations for each participant we have sorted the data by participant:
longt2<-longt2[order(longt2$participant),]


longt2[1:6, ]

# library(mice)
library(VIM)

# Missing data
p <- function(x) {sum(is.na(x))/length(x)*100}
apply(longt2, 2, p) 


library(mice)

set.seed(123)

iniLONGT2 <- mice(longt2, maxit = 0)

predLONGT2 <- iniLONGT2$pred
predLONGT2

# Setting values of variables I'd like to leave out to 0 in the predictor matrix
predLONGT2[, c("variable", "period")] <- 0
predLONGT2

predLONGT2["scores", ] <- c(-2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # The class variable (only one is allowed) is coded by a `-2'.

impLONGT2 <- mice(longt2, meth = c("", "", "", "", "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "", "2l.norm", ""), m = 10, pred = predLONGT2, maxit = 5, seed = 123) # The dependent variable is "scores" (alcohol consumption)

print(impLONGT2) 
impLONGT2$imp$scores # to see the imputed values


# Distribution of oberserved/imputed values
xyplot(impLONGT2, scores ~ logbEAPOS | .imp, pch = 20, cex=1.4)
xyplot(impLONGT2, scores ~ logbEANEG | .imp, pch = 20, cex=1.4)
xyplot(impLONGT2, scores ~ logbfacilitsoc | .imp, pch = 20, cex=1.4)
xyplot(impLONGT2, scores ~ logbaceptpares | .imp, pch = 20, cex=1.4)
xyplot(impLONGT2, scores ~ logbctrolparent | .imp, pch = 20, cex=1.4)
xyplot(impLONGT2, scores ~ logbctrolestres | .imp, pch = 20, cex=1.4)



# Complete the dataset:
myDatalongt2 <- complete(impLONGT2, 10) # WE IMPUTED THE 10TH DATASET



####### To select the order in which the predictors will be included into the model:
library(leaps)

bestLONGT2<-regsubsets(scores ~ period + sexo + aedad + aedadinicioalc + logbEAPOS + logbEANEG + logbfacilitsoc + logbaceptpares + logbctrolparent + logbctrolestres, data = myDatalongt2, nvmax = 11, method = "exhaustive")

summary(bestLONGT2, all.best = TRUE)
# Subset selection object
# Call: regsubsets.formula(scores ~ period + sexo + aedad + aedadinicioalc + 
#     logbEAPOS + logbEANEG + logbfacilitsoc + logbaceptpares + 
#     logbctrolparent + logbctrolestres, data = myDatalongt2, nvmax = 11, 
#     method = "exhaustive")
# 10 Variables  (and intercept)
#                 Forced in Forced out
# periodsecond        FALSE      FALSE
# sexovaron           FALSE      FALSE
# aedad               FALSE      FALSE
# aedadinicioalc      FALSE      FALSE
# logbEAPOS           FALSE      FALSE
# logbEANEG           FALSE      FALSE
# logbfacilitsoc      FALSE      FALSE
# logbaceptpares      FALSE      FALSE
# logbctrolparent     FALSE      FALSE
# logbctrolestres     FALSE      FALSE
# 1 subsets of each size up to 10
# Selection Algorithm: exhaustive
#           periodsecond sexovaron aedad aedadinicioalc logbEAPOS logbEANEG logbfacilitsoc logbaceptpares logbctrolparent logbctrolestres
# 1  ( 1 )  " "          " "       " "   " "            " "       " "       "*"            " "            " "             " "            
# 2  ( 1 )  " "          " "       " "   " "            " "       " "       "*"            " "            "*"             " "            
# 3  ( 1 )  " "          " "       " "   " "            " "       " "       "*"            " "            "*"             "*"            
# 4  ( 1 )  " "          "*"       " "   " "            " "       " "       "*"            " "            "*"             "*"            
# 5  ( 1 )  " "          "*"       " "   " "            " "       " "       "*"            "*"            "*"             "*"            
# 6  ( 1 )  " "          "*"       " "   " "            " "       "*"       "*"            "*"            "*"             "*"            
# 7  ( 1 )  "*"          "*"       " "   " "            " "       "*"       "*"            "*"            "*"             "*"            
# 8  ( 1 )  "*"          "*"       " "   "*"            " "       "*"       "*"            "*"            "*"             "*"            
# 9  ( 1 )  "*"          "*"       "*"   "*"            " "       "*"       "*"            "*"            "*"             "*"            
# 10  ( 1 ) "*"          "*"       "*"   "*"            "*"       "*"       "*"            "*"            "*"             "*"  


plot(bestLONGT2, scale = "r2", main = "Best R^2") # R2 = 0.14 
plot(bestLONGT2, scale = "adjr2", main = "Best Adjusted R^2") # Adj R2 = 0.13 



####### MULTILEVEL ANALYSYS (MIXED EFFECTS)

# Building the model
# library(nlme)

interceptOnlyT2 <-gls(scores ~ 1, data = myDatalongt2, method = "ML") # only the intercept

modelfu2<-lme(scores ~ 1, random = ~1|participant, data = myDatalongt2, method = "ML", control = lmeControl(opt = "optim")) # the nested structure indicates repeated measures (within variable) # repeated measures nested within participants
anova(interceptOnlyT2, modelfu2)  # to compare models

# To see the overall effect of each main effect we added them to the model one at a time:

t2facsoc<-update(modelfu2, .~. + logbfacilitsoc)
summary(t2facsoc)  
anova(interceptOnlyT2, modelfu2, t2facsoc) 

t2ctrlparental<-update(t2facsoc, .~. + logbctrolparent)
summary(t2ctrlparental)  
anova(interceptOnlyT2, modelfu2, t2facsoc, t2ctrlparental)  

t2ctrlstress<-update(t2ctrlparental, .~. + logbctrolestres)
summary(t2ctrlstress)  
anova(interceptOnlyT2, modelfu2, t2facsoc, t2ctrlparental, t2ctrlstress)  

t2sexo<-update(t2ctrlstress, .~. + sexo)
summary(t2sexo) 
anova(interceptOnlyT2, modelfu2, t2facsoc, t2ctrlparental, t2ctrlstress, t2sexo)

t2aceptpares<-update(t2sexo, .~. + logbaceptpares)
summary(t2aceptpares) 
anova(interceptOnlyT2, modelfu2, t2facsoc, t2ctrlparental, t2ctrlstress, t2sexo, t2aceptpares)

t2eaneg<-update(t2aceptpares, .~. + logbEANEG)
summary(t2eaneg) 
anova(interceptOnlyT2, modelfu2, t2facsoc, t2ctrlparental, t2ctrlstress, t2sexo, t2aceptpares, t2eaneg)

t2period<-update(t2eaneg, .~. + period)
summary(t2period)  
anova(interceptOnlyT2, modelfu2, t2facsoc, t2ctrlparental, t2ctrlstress, t2sexo, t2aceptpares, t2eaneg, t2period)  

t2edadinicio<-update(t2period, .~. + aedadinicioalc)
summary(t2edadinicio) 
anova(interceptOnlyT2, modelfu2, t2facsoc, t2ctrlparental, t2ctrlstress, t2sexo, t2aceptpares, t2eaneg, t2period, t2edadinicio)

t2edad<-update(t2edadinicio, .~. + aedad)
summary(t2edad) 
anova(interceptOnlyT2, modelfu2, t2facsoc, t2ctrlparental, t2ctrlstress, t2sexo, t2aceptpares, t2eaneg, t2period, t2edadinicio, t2edad)

t2eapos<-update(t2edad, .~. + logbEAPOS)
summary(t2eapos)  
anova(interceptOnlyT2, modelfu2, t2facsoc, t2ctrlparental, t2ctrlstress, t2sexo, t2aceptpares, t2eaneg, t2period, t2edadinicio, t2edad, t2eapos)  
#                 Model df      AIC      BIC    logLik     Test  L.Ratio p-value
# interceptOnlyT2     1  2 1260.919 1269.713 -628.4594                          
# modelfu2            2  3 1193.759 1206.950 -593.8794   1 vs 2 69.15995  <.0001
# t2facsoc            3  4 1167.300 1184.888 -579.6499   2 vs 3 28.45907  <.0001
# t2ctrlparental      4  5 1155.507 1177.492 -572.7535   3 vs 4 13.79275  0.0002 ####
# t2ctrlstress        5  6 1154.798 1181.179 -571.3990   4 vs 5  2.70908  0.0998
# t2sexo              6  7 1153.778 1184.556 -569.8889   5 vs 6  3.02016  0.0822
# t2aceptpares        7  8 1153.362 1188.538 -568.6811   6 vs 7  2.41552  0.1201
# t2eaneg             8  9 1152.504 1192.076 -567.2517   7 vs 8  2.85879  0.0909
# t2period            9 10 1153.443 1197.412 -566.7215   8 vs 9  1.06039  0.3031
# t2edadinicio       10 11 1154.504 1202.870 -566.2521  9 vs 10  0.93880  0.3326
# t2edad             11 12 1156.063 1208.826 -566.0316 10 vs 11  0.44115  0.5066
# t2eapos            12 13 1157.836 1214.996 -565.9178 11 vs 12  0.22756  0.6333


summary(t2ctrlparental)
# Linear mixed-effects model fit by maximum likelihood
#  Data: myDatalongt2 
#        AIC      BIC    logLik
#   1155.507 1177.492 -572.7535

# Random effects:
#  Formula: ~1 | participant
#         (Intercept)  Residual
# StdDev:    0.378808 0.5261415

# Fixed effects: scores ~ logbfacilitsoc + logbctrolparent 
#                      Value Std.Error  DF   t-value p-value
# (Intercept)     -1.2080742 0.4949937 299 -2.440585  0.0152
# logbfacilitsoc   0.6036421 0.1543574 298  3.910679  0.0001
# logbctrolparent  0.3283707 0.0875893 298  3.748983  0.0002
#  Correlation: 
#                 (Intr) lgbfcl
# logbfacilitsoc  -0.874       
# logbctrolparent -0.087 -0.405

# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -3.26988693 -0.58186932  0.02215835  0.50820469  3.48705494 

# Number of Observations: 600
# Number of Groups: 300 



intervals(t2ctrlparental)
# Approximate 95% confidence intervals

# Fixed effects:
#                      lower       est.      upper
# (Intercept)     -2.1797487 -1.2080742 -0.2363998
# logbfacilitsoc   0.3006339  0.6036421  0.9066503
# logbctrolparent  0.1564303  0.3283707  0.5003111
# attr(,"label")
# [1] "Fixed effects:"

# Random Effects:
#  Level: participant 
#                     lower     est.     upper
# sd((Intercept)) 0.3154907 0.378808 0.4548328

# Within-group standard error:
#     lower      est.     upper 
# 0.4847109 0.5261415 0.5711134 



# library(pastecs)
by(myDatalongt2$scores, list(myDatalongt2$period), stat.desc, basic = FALSE)
by(myDatalongt2$scores, list(myDatalongt2$period, myDatalongt2$sexo), stat.desc, basic = FALSE)


# Calculating effect sizes
# library(DSUR.noof)
# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# logbfacilitsoc   
rcontrast(3.910679, 298)
# [1] "r = 0.220941113887446"

# logbctrolparent
rcontrast(3.748983, 298)
# [1] "r = 0.212225705655735"



####################################################################################
########################### CROSS-VALIDATION #######################################
######################## CROSS-SECTIONAL SAMPLE ####################################
####################################################################################

#################################################
###### Load the dataset (CROSS-SECTIONAL) #######

# transv<-read.table("clipboard",header=TRUE, encoding = "Latin 1", dec=",", sep="\t") # CROSS-SECTIONAL

# transv$tsexo <- factor(transv$tsexo, levels=c("Wom", "Man")) # to order categories


####################################################################################
########################### METHODS SECTION ########################################
####################################################################################

transvlog <- transv

#### CROSS-SECTIONAL SAMPLE #### 

####### Log transformations

# # # # WEEKLY CONSUMPTION OF ALCOHOLIC BEVERAGES
transvlog$logtALCSEM <- log(transvlog$tALCSEM + 1)

# # # # ALCOHOL EXPECTANCIES

### POSITIVE EA:
transvlog$logtEAPOS <- log(transvlog$tEAPOS + 1)

### NEGATIVE EA:
transvlog$logtEANEG <- log(transvlog$tEANEG + 1)

# # # # CONSUMPTION CONTEXTS

### SOCIAL FACILITATION:
transvlog$logtfacilitsoc <- log(transvlog$tfacilitsoc + 1)

### PEER GROUP ACCEPTANCE:
transvlog$logtaceptpares <- log(transvlog$taceptpares + 1)

### PARENTAL CONTROL:
transvlog$logtctrolparent <- log(transvlog$tctrolparent + 1)

### STRESS CONTROL:
transvlog$logtctrolestres <- log(transvlog$tctrolestres + 1)


####### Preparing the data: based on LOG TRANSFORMED VALUES
#### CROSS-SECTIONAL SAMPLE #### 

# To see the NAs
library(mice)
datatransv<-subset(transvlog,select=c(tsexo, tedad, tedadinicioalc, logtALCSEM, logtEAPOS, logtEANEG, logtfacilitsoc, logtaceptpares, logtctrolparent, logtctrolestres))
md.pattern(datatransv, rotate.names = TRUE)
md.pairs(datatransv) 

# We used the pmm (predictive mean matching) method for imputation

# Libraries
# library(mice)
# library(VIM)

# Missing data
p <- function(x) {sum(is.na(x))/length(x)*100}
apply(datatransv, 2, p) 

# Impute
set.seed(123)

# We run the mice code with 0 iterations 
imptr <- mice(datatransv, maxit=0, seed = 123)

# Extract predictorMatrix and methods of imputation 
predM <- imptr$predictorMatrix
meth <- imptr$method

# To view the first few rows of the predictor matrix
head(predM)

# See the methods of imputations
meth


imputeT <- mice(datatransv, m = 10, maxit = 5, predictorMatrix = predM, method = meth, seed = 123)


print(imputeT) 
imputeT$imp$logtEAPOS
imputeT$imp$logtEANEG 
imputeT$imp$logtfacilitsoc 
imputeT$imp$logtaceptpares  
imputeT$imp$logtctrolparent  
imputeT$imp$logtctrolestres 

# Distribution of oberserved/imputed values
xyplot(imputeT, logtEAPOS ~ logtALCSEM | .imp, pch = 20, cex=1.4)
xyplot(imputeT, logtEANEG ~ logtALCSEM | .imp, pch = 20, cex=1.4)
xyplot(imputeT, logtfacilitsoc ~ logtALCSEM | .imp, pch = 20, cex=1.4)
xyplot(imputeT, logtaceptpares ~ logtALCSEM | .imp, pch = 20, cex=1.4)
xyplot(imputeT, logtctrolparent ~ logtALCSEM | .imp, pch = 20, cex=1.4)
xyplot(imputeT, logtctrolestres ~ logtALCSEM | .imp, pch = 20, cex=1.4)


# Complete the dataset:
newtransv <- complete(imputeT, 10) # We choosed the first imputation to complete the dataset

detach("package:mice", unload = TRUE)
detach("package:VIM", unload = TRUE)



####################################################################################
######################## CROSS-SECTIONAL SAMPLE ####################################

####################################################################################
#### OUTCOME, EA AND CC MEASURED DURING QUARANTINE DUE TO THE COVID-19 PANDEMIC)####
############## (CROSS-SECTIONAL SAMPLE)


# To select the order in which the predictors will be included into the model:
library(leaps)

bestcrosssect<-regsubsets(logtALCSEM ~ tsexo + tedad + tedadinicioalc + logtEAPOS + logtEANEG + logtfacilitsoc + logtaceptpares + logtctrolparent + logtctrolestres, data = newtransv, nvmax = 11, method = "exhaustive")

summary(bestcrosssect, all.best = TRUE)
# Subset selection object
# Call: regsubsets.formula(logtALCSEM ~ tsexo + tedad + tedadinicioalc + 
#     logtEAPOS + logtEANEG + logtfacilitsoc + logtaceptpares + 
#     logtctrolparent + logtctrolestres, data = newtransv, nvmax = 11, 
#     method = "exhaustive")
# 9 Variables  (and intercept)
#                 Forced in Forced out
# tsexoMan            FALSE      FALSE
# tedad               FALSE      FALSE
# tedadinicioalc      FALSE      FALSE
# logtEAPOS           FALSE      FALSE
# logtEANEG           FALSE      FALSE
# logtfacilitsoc      FALSE      FALSE
# logtaceptpares      FALSE      FALSE
# logtctrolparent     FALSE      FALSE
# logtctrolestres     FALSE      FALSE
# 1 subsets of each size up to 9
# Selection Algorithm: exhaustive
#          tsexoMan tedad tedadinicioalc logtEAPOS logtEANEG logtfacilitsoc logtaceptpares logtctrolparent logtctrolestres
# 1  ( 1 ) " "      " "   " "            " "       " "       " "            " "            " "             "*"            
# 2  ( 1 ) " "      " "   " "            " "       " "       " "            " "            "*"             "*"            
# 3  ( 1 ) " "      " "   " "            "*"       " "       " "            " "            "*"             "*"            
# 4  ( 1 ) " "      "*"   " "            "*"       " "       " "            " "            "*"             "*"            
# 5  ( 1 ) "*"      "*"   " "            "*"       " "       " "            " "            "*"             "*"            
# 6  ( 1 ) "*"      "*"   " "            "*"       " "       "*"            " "            "*"             "*"            
# 7  ( 1 ) "*"      "*"   " "            "*"       "*"       "*"            " "            "*"             "*"            
# 8  ( 1 ) "*"      "*"   "*"            "*"       "*"       "*"            " "            "*"             "*"            
# 9  ( 1 ) "*"      "*"   "*"            "*"       "*"       "*"            "*"            "*"             "*"  


plot(bestcrosssect, scale = "r2", main = "Best R^2") # R2 = 0.46
plot(bestcrosssect, scale = "adjr2", main = "Best Adjusted R^2") # adj R2 = 0.44

# Model:
fullT<-lm(logtALCSEM ~ logtctrolestres + logtctrolparent + logtEAPOS + tedad + tsexo + logtfacilitsoc + logtEANEG + tedadinicioalc + logtaceptpares, data = newtransv)

summary(fullT)
# Call:
# lm(formula = logtALCSEM ~ logtctrolestres + logtctrolparent + 
#     logtEAPOS + tedad + tsexo + logtfacilitsoc + logtEANEG + 
#     tedadinicioalc + logtaceptpares, data = newtransv)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.65694 -0.39593 -0.07731  0.44021  1.98105 

# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     -3.576827   1.358118  -2.634  0.00930 ** 
# logtctrolestres  0.988220   0.167387   5.904 2.17e-08 ***
# logtctrolparent  0.671798   0.181162   3.708  0.00029 ***
# logtEAPOS        0.478877   0.263728   1.816  0.07133 .  
# tedad           -0.026668   0.032133  -0.830  0.40785    
# tsexoMan        -0.063993   0.135040  -0.474  0.63625    
# logtfacilitsoc   0.092211   0.323915   0.285  0.77627    
# logtEANEG       -0.108678   0.223914  -0.485  0.62811    
# tedadinicioalc  -0.009752   0.026814  -0.364  0.71659    
# logtaceptpares   0.039226   0.200270   0.196  0.84497    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 0.6705 on 155 degrees of freedom
# Multiple R-squared:  0.4564,	Adjusted R-squared:  0.4248 
# F-statistic: 14.46 on 9 and 155 DF,  p-value: < 2.2e-16


# 95% Confidence interval of best-fitted model:
confint(fullT, level = 0.95)
#                       2.5 %      97.5 %
# (Intercept)     -6.25963496 -0.89401903
# logtctrolestres  0.65756545  1.31887493
# logtctrolparent  0.31393324  1.02966316
# logtEAPOS       -0.04208707  0.99984106
# tedad           -0.09014377  0.03680717
# tsexoMan        -0.33074830  0.20276241
# logtfacilitsoc  -0.54764715  0.73206934
# logtEANEG       -0.55099388  0.33363819
# tedadinicioalc  -0.06272096  0.04321678
# logtaceptpares  -0.35638398  0.43483675



# ERROR RATE of best-fitted model:
sigma(fullT)/mean(newtransv$logtALCSEM)
# 0.3556886
# In our multiple regression, the Residual Standard Error (RSE) or sigma is 0.6705 corresponding to 35.57% error rate.

par(mfrow=c(2,2))
# Figure 
plot(fullT)
par(mfrow=c(1,1))


effectsize::cohens_f(fullT, ci = 0.95)
# Parameter       | Cohen's f (partial) |       95% CI
# ----------------------------------------------------
# logtctrolestres |                0.78 | [0.60, 0.96]
# logtctrolparent |                0.42 | [0.26, 0.58]
# logtEAPOS       |                0.22 | [0.06, 0.38]
# tedad           |                0.08 | [0.00, 0.24]
# tsexo           |                0.03 | [0.00, 0.18]
# logtfacilitsoc  |                0.03 | [0.00, 0.18]
# logtEANEG       |                0.03 | [0.00, 0.19]
# tedadinicioalc  |                0.03 | [0.00, 0.18]
# logtaceptpares  |                0.02 | [0.00, 0.15]




####################################################################################
######################## SUPPLEMENTARY MATERIALS ###################################
####################################################################################

################################################
######## To test Skewness and Kurtosis #########
############ & log transformation ##############

########## Testing Skewness and Kurtosis
# Criteria: range of acceptable values -1 to +1 for Skewness and -3 to +3 for Kurtosis (Brown, 2006) or near to.
# Reference: Brown, T. A. (2006). Confirmatory factor analysis for applied research. New York: Guilford Press.

library(moments)

# We have tested skewness and kurtosis in the original data (from which included NAs)
# Then, we have tested skewness and kurtosis transforming the data by using the log transformation (natural logarithm)

#### LONGITUDINAL SAMPLE #### 

########### Time 1 (T1)

# # # # WEEKLY CONSUMPTION OF ALCOHOLIC BEVERAGES
skewness(newdata$aALCSEM)
# 2.036603
kurtosis(newdata$aALCSEM)
# 7.973155
hist(newdata$aALCSEM)

skewness(newdata$logaALCSEM)
# 0.3090412
kurtosis(newdata$logaALCSEM)
# 2.733757
hist(newdata$logaALCSEM)

# # # # ALCOHOL EXPECTANCIES

### POSITIVE EA. Time 1:
skewness(newdata$aEAPOS)
# 0.06496527
kurtosis(newdata$aEAPOS)
# 2.572754
hist(newdata$aEAPOS)

skewness(newdata$logaEAPOS)
# -0.5855385
kurtosis(newdata$logaEAPOS)
# 2.998297
hist(newdata$logaEAPOS)

### NEGATIVE EA. Time 1:
skewness(newdata$aEANEG)
# 0.6671249
kurtosis(newdata$aEANEG)
# 3.011873
hist(newdata$aEANEG)

skewness(newdata$logaEANEG)
# 0.02637938
kurtosis(newdata$logaEANEG)
# 2.410175
hist(newdata$logaEANEG)

# # # # CONSUMPTION CONTEXTS

### SOCIAL FACILITATION. Time 1:
skewness(newdata$afacilitsoc)
# -0.09455288
kurtosis(newdata$afacilitsoc)
# 2.64605
hist(newdata$afacilitsoc)

skewness(newdata$logafacilitsoc)
# -0.716349
kurtosis(newdata$logafacilitsoc)
# 3.556122
hist(newdata$logafacilitsoc)

### PEER GROUP ACCEPTANCE. Time 1:
skewness(newdata$aaceptpares)
# 1.621552
kurtosis(newdata$aaceptpares)
# 6.237001
hist(newdata$aaceptpares)

skewness(newdata$logaaceptpares)
# 0.8996885
kurtosis(newdata$logaaceptpares)
# 3.296438
hist(newdata$logaaceptpares)

### PARENTAL CONTROL. Time 1:
skewness(newdata$actrolparent)
# 0.4892952
kurtosis(newdata$actrolparent)
# 2.450725
hist(newdata$actrolparent)

skewness(newdata$logactrolparent)
# -0.1385326
kurtosis(newdata$logactrolparent)
# 2.272852
hist(newdata$logactrolparent)

### STRESS CONTROL. Time 1:
skewness(newdata$actrolestres)
# 2.335563
kurtosis(newdata$actrolestres)
# 9.792254
hist(newdata$actrolestres)

skewness(newdata$logactrolestres)
# 1.429438
kurtosis(newdata$logactrolestres)
# 4.48949
hist(newdata$logactrolestres)


########### Time 2 (T2) 

# # # # WEEKLY CONSUMPTION OF ALCOHOLIC BEVERAGES
skewness(newdata$bALCSEM, na.rm = TRUE)
# 1.830084
kurtosis(newdata$bALCSEM, na.rm = TRUE)
# 6.556951
hist(newdata$bALCSEM)

skewness(newdata$logbALCSEM, na.rm = TRUE)
# -0.1222795
kurtosis(newdata$logbALCSEM, na.rm = TRUE)
# 2.801384
hist(newdata$logbALCSEM)

# # # # ALCOHOL EXPECTANCIES
### POSITIVE EA. Time 2:
skewness(newdata$bEAPOS, na.rm = TRUE)
# -0.167808
kurtosis(newdata$bEAPOS, na.rm = TRUE)
# 2.490901
hist(newdata$bEAPOS)

skewness(newdata$logbEAPOS, na.rm = TRUE)
# -0.8629646
kurtosis(newdata$logbEAPOS, na.rm = TRUE)
# 3.523152
hist(newdata$logbEAPOS)

### NEGATIVE EA. Time 2:
skewness(newdata$bEANEG, na.rm = TRUE)
# 0.7051564
kurtosis(newdata$bEANEG, na.rm = TRUE)
# 3.125947
hist(newdata$bEANEG)

skewness(newdata$logbEANEG, na.rm = TRUE)
# 0.08503744
kurtosis(newdata$logbEANEG, na.rm = TRUE)
# 2.587574
hist(newdata$logbEANEG)

# # # # CONSUMPTION CONTEXTS
### SOCIAL FACILITATION. Time 2:
skewness(newdata$bfacilitsoc, na.rm = TRUE)
# -0.3262725
kurtosis(newdata$bfacilitsoc, na.rm = TRUE)
# 2.97932
hist(newdata$bfacilitsoc)

skewness(newdata$logbfacilitsoc, na.rm = TRUE)
# -0.9951637
kurtosis(newdata$logbfacilitsoc, na.rm = TRUE)
# 3.998576
hist(newdata$logbfacilitsoc)

### PEER GROUP ACCEPTANCE. Time 2:
skewness(newdata$baceptpares, na.rm = TRUE)
# 1.988188
kurtosis(newdata$baceptpares, na.rm = TRUE)
# 7.95572
hist(newdata$baceptpares)

skewness(newdata$logbaceptpares, na.rm = TRUE)
# 1.023497
kurtosis(newdata$logbaceptpares, na.rm = TRUE)
# 3.870381
hist(newdata$logbaceptpares)

### PARENTAL CONTROL. Time 2:
skewness(newdata$bctrolparent, na.rm = TRUE)
# -0.1021466
kurtosis(newdata$bctrolparent, na.rm = TRUE)
# 2.40119
hist(newdata$bctrolparent)

skewness(newdata$logbctrolparent, na.rm = TRUE)
# -0.841098
kurtosis(newdata$logbctrolparent, na.rm = TRUE)
# 3.301226
hist(newdata$logbctrolparent)

### STRESS CONTROL. Time 2:
skewness(newdata$bctrolestres, na.rm = TRUE)
# 1.898529
kurtosis(newdata$bctrolestres, na.rm = TRUE)
# 6.490371
hist(newdata$bctrolestres)

skewness(newdata$logbctrolestres, na.rm = TRUE)
# 1.125564
kurtosis(newdata$logbctrolestres, na.rm = TRUE)
# 3.460741
hist(newdata$logbctrolestres)



#### CROSS-SECTIONAL SAMPLE #### 

# # # # WEEKLY CONSUMPTION OF ALCOHOLIC BEVERAGES
skewness(transv$tALCSEM)
# 4.167678
kurtosis(transv$tALCSEM)
# 25.26356
hist(transv$tALCSEM)

skewness(transvlog$logtALCSEM)
# 0.002373328
kurtosis(transvlog$logtALCSEM)
# 3.244347
hist(transvlog$logtALCSEM)

# # # # ALCOHOL EXPECTANCIES

### POSITIVE EA:
skewness(transv$tEAPOS, na.rm = TRUE)
# 0.08209859
kurtosis(transv$tEAPOS, na.rm = TRUE)
# 2.71206
hist(transv$tEAPOS)

skewness(transvlog$logtEAPOS, na.rm = TRUE)
# -0.6991041
kurtosis(transvlog$logtEAPOS, na.rm = TRUE)
# 3.273841
hist(transvlog$logtEAPOS)

### NEGATIVE EA:
skewness(transv$tEANEG, na.rm = TRUE)
# 0.8976816
kurtosis(transv$tEANEG, na.rm = TRUE)
# 4.076898
hist(transv$tEANEG)

skewness(transvlog$logtEANEG, na.rm = TRUE)
# 0.161761
kurtosis(transvlog$logtEANEG, na.rm = TRUE)
# 2.48951
hist(transvlog$logtEANEG)

# # # # CONSUMPTION CONTEXTS

### SOCIAL FACILITATION:
skewness(transv$tfacilitsoc, na.rm = TRUE)
# -0.4153691
kurtosis(transv$tfacilitsoc, na.rm = TRUE)
# 2.957427
hist(transv$tfacilitsoc)

skewness(transvlog$logtfacilitsoc, na.rm = TRUE)
# -1.220954
kurtosis(transvlog$logtfacilitsoc, na.rm = TRUE)
# 4.525307
hist(transvlog$logtfacilitsoc)

### PEER GROUP ACCEPTANCE:
skewness(transv$taceptpares, na.rm = TRUE)
# 2.109396
kurtosis(transv$taceptpares, na.rm = TRUE)
# 9.078028
hist(transv$taceptpares)

skewness(transvlog$logtaceptpares, na.rm = TRUE)
# 0.954218
kurtosis(transvlog$logtaceptpares, na.rm = TRUE)
# 3.815478
hist(transvlog$logtaceptpares)

### PARENTAL CONTROL:
skewness(transv$tctrolparent, na.rm = TRUE)
# 0.09376151
kurtosis(transv$tctrolparent, na.rm = TRUE)
# 2.455895
hist(transv$tctrolparent)

skewness(transvlog$logtctrolparent, na.rm = TRUE)
# -0.6195134
kurtosis(transvlog$logtctrolparent, na.rm = TRUE)
# 2.711219
hist(transvlog$logtctrolparent)

### STRESS CONTROL:
skewness(transv$tctrolestres, na.rm = TRUE)
# 1.604419
kurtosis(transv$tctrolestres, na.rm = TRUE)
# 5.391631
hist(transv$tctrolestres)

skewness(transvlog$logtctrolestres, na.rm = TRUE)
# 0.9098268
kurtosis(transvlog$logtctrolestres, na.rm = TRUE)
# 2.836232
hist(transvlog$logtctrolestres)


################################################
######## To assess  multicollinearity #######

# To check for multicollinearity, we used the VIF (Variance Inflation Factor) values. We adopted the following criteria (Field et al., 2012):
# If VIF values are less than 10 then that indicates there probably isn't cause for concern.
# If the average of VIF values is not substantially greater than 1, then that also indicates that there's no cause for concern.
# Tolerance below 0.1 and below 0.2 indicates a serious and a potential problem
# Reference: Field, A., Miles, J., & Field, Z. (2012). Discovering statistics using R. London: SAGE.
# For assessing the assumption of no multicollinearity we used the vif() function, for the initial model:

library(car)

######### WEEKLY CONSUMPTION OF ALCOHOLIC BEVERAGES (OUTCOME)
# MODEL INCLUDING SCORES (log transformed) OF EA AND CC DURING THE FIRST MEASUREMENT:
fitlogconsA<-lm(logaALCSEM ~ sexo + aedad + aedadinicioalc + logaEAPOS + logaEANEG + logafacilitsoc + logaaceptpares + logactrolparent + logactrolestres, data = newdata)

# The VIF
vif(fitlogconsA)
#        sexo           aedad  aedadinicioalc       logaEAPOS       logaEANEG  logafacilitsoc  logaaceptpares logactrolparent logactrolestres 
#    1.045905        1.055853        1.059770        1.997362        1.613679        2.067836        1.252198        1.530247        1.307667 

# The mean VIF
mean(vif(fitlogconsA))
#  1.436724

# The tolerance
1/vif(fitlogconsA)
#           sexo           aedad  aedadinicioalc       logaEAPOS       logaEANEG  logafacilitsoc  logaaceptpares logactrolparent logactrolestres 
#      0.9561098       0.9471014       0.9436012       0.5006604       0.6197021       0.4835975       0.7985955       0.6534891       0.7647206 


######### WEEKLY CONSUMPTION OF ALCOHOLIC BEVERAGES (OUTCOME)
# MODEL INCLUDING SCORES (log transformed) OF EA AND CC DURING THE SECOND MEASUREMENT:
fitlogconsB<-lm(logbALCSEM ~ sexo + aedad + aedadinicioalc + logbEAPOS + logbEANEG + logbfacilitsoc + logbaceptpares + logbctrolparent + logbctrolestres, data = newdata)

# The VIF
vif(fitlogconsB)
#            sexo           aedad  aedadinicioalc       logbEAPOS       logbEANEG  logbfacilitsoc  logbaceptpares logbctrolparent logbctrolestres 
#        1.107867        1.185509        1.139820        2.491497        1.652286        3.172370        1.395701        1.747044        1.474054 

# The mean VIF
mean(vif(fitlogconsB))
# 1.70735

# The tolerance
1/vif(fitlogconsB)
#             sexo           aedad  aedadinicioalc       logbEAPOS       logbEANEG  logbfacilitsoc  logbaceptpares logbctrolparent logbctrolestres 
#        0.9026355       0.8435193       0.8773316       0.4013652       0.6052222       0.3152217       0.7164858       0.5723953       0.6784010 


######### WEEKLY CONSUMPTION OF ALCOHOLIC BEVERAGES (OUTCOME)
# MODEL INCLUDING SCORES (log transformed) OF EA AND CC IN THE CROSS-SECTIONAL SAMPLE:
fitlogconsT<-lm(logtALCSEM ~ tsexo + tedad + tedadinicioalc + logtEAPOS + logtEANEG + logtfacilitsoc + logtaceptpares + logtctrolparent + logtctrolestres, data = transvlog)

# The VIF
vif(fitlogconsT)
#           tsexo           tedad  tedadinicioalc       logtEAPOS       logtEANEG  logtfacilitsoc  logtaceptpares logtctrolparent logtctrolestres 
#        1.098402        1.085764        1.188774        2.702804        1.555721        3.005110        1.467375        1.587368        1.441402

# The mean VIF
mean(vif(fitlogconsT))
# 1.681413

# The tolerance
1/vif(fitlogconsT)
#          tsexo           tedad  tedadinicioalc       logtEAPOS       logtEANEG  logtfacilitsoc  logtaceptpares logtctrolparent logtctrolestres 
#      0.9104133       0.9210101       0.8412025       0.3699861       0.6427889       0.3327665       0.6814893       0.6299738       0.6937691 


detach("package:car", unload = TRUE)



#################################################
######## Load the dataset (LONGITUDINAL) ######## 

dataset<-read.table("clipboard",header=TRUE, encoding = "Latin 1", dec=",", sep="\t") # LONGITUDINAL

table(dataset$sex) # 210 women, 90 men

## HEAVY DRINKERS- LONGITUDINAL ##

# WOMEN:

dataset[dataset$aconsumosem>7 & dataset$sexo=="mujer",] # to select heavy drinkers that are only women. CDC criterium for heavy drinkers in women: consuming 8 drinks or more per week

longwomheavy<-dataset[dataset$aconsumosem>7 & dataset$sexo=="mujer",] 

summary(longwomheavy) 


# MEN:

dataset[dataset$aconsumosem>14 & dataset$sexo=="varon",] # to select heavy drinkers that are only men. CDC criterium for heavy drinkers in men: consuming 15 drinks or more per week

longmenheavy<-dataset[dataset$aconsumosem>14 & dataset$sexo=="varon",] 

summary(longmenheavy) 


## NO HEAVY DRINKERS- LONGITUDINAL ##

# WOMEN:

dataset[dataset$aconsumosem<8 & dataset$sexo=="mujer",] # to select NO heavy drinkers that are only women. CDC criterium for NO heavy drinkers in women: consuming LESS THAN 8 drinks per week

longwomNOheavy<-dataset[dataset$aconsumosem<8 & dataset$sexo=="mujer",] 

summary(longwomNOheavy)


# MEN:

dataset[dataset$aconsumosem<15 & dataset$sexo=="varon",] # to select NO heavy drinkers that are only men. CDC criterium for NO heavy drinkers in men: consuming LESS THAN 15 drinks per week

longmenNOheavy<-dataset[dataset$aconsumosem<15 & dataset$sexo=="varon",] 

summary(longmenNOheavy) 



#################################################
###### Load the dataset (CROSS-SECTIONAL) #######

transv<-read.table("clipboard",header=TRUE, encoding = "Latin 1", dec=",", sep="\t") # CROSS-SECTIONAL

## HEAVY DRINKERS- CROSS-SECTIONAL ##

# WOMEN:

table(transv$tsexo)

transv[transv$tconsumosem>7 & transv$tsexo=="Wom",] # to select heavy drinkers that are only women. CDC criterium for heavy drinkers in women: consuming 8 drinks or more per week

transvwomheavy<-transv[transv$tconsumosem>7 & transv$tsexo=="Wom",] 

summary(transvwomheavy) 


# MEN:

transv[transv$tconsumosem>14 & transv$tsexo=="Man",] # to select heavy drinkers that are only men. CDC criterium for heavy drinkers in men: consuming 15 drinks or more per week

transvmenheavy<-transv[transv$tconsumosem>14 & transv$tsexo=="Man",] 

summary(transvmenheavy) 



## NO HEAVY DRINKERS- CROSS-SECTIONAL ##

# WOMEN:

transv[transv$tconsumosem<8 & transv$tsexo=="Wom",] # to select NO heavy drinkers that are only women. CDC criterium for NO heavy drinkers in women: consuming LESS THAN 8 drinks per week

transvwomNOheavy<-transv[transv$tconsumosem<8 & transv$tsexo=="Wom",] 

summary(transvwomNOheavy) 


# MEN:

transv[transv$tconsumosem<15 & transv$tsexo=="Man",] # to select NO heavy drinkers that are only men. CDC criterium for NO heavy drinkers in men: consuming LESS THAN 15 drinks per week

transvmenNOheavy<-transv[transv$tconsumosem<15 & transv$tsexo=="Man",] 

summary(transvmenNOheavy)




## Participants with missing data (NAs) in the follow-up:

zNA<-dataset[!complete.cases(dataset$bconsumosem), ] # both sexes with NAs in the variable weekly alcohol consumption: n = 181
zwomNA<-dataset[!complete.cases(dataset$bconsumosem) & dataset$sexo == "mujer", ] # women with NAs in the variable weekly alcohol consumption: n = 116
zmenNA<-dataset[!complete.cases(dataset$bconsumosem) & dataset$sexo == "varon", ] # men with NAs in the variable weekly alcohol consumption: n = 65

## Participants without missing data (NAs) in the follow-up:

zCOMPL<-dataset[complete.cases(dataset$bconsumosem), ] # both sexes without NAs in the variable weekly alcohol consumption: n = 119
zwomCOMPL<-dataset[complete.cases(dataset$bconsumosem) & dataset$sexo == "mujer", ] # women without NAs in the variable weekly alcohol consumption: n = 94
zmenCOMPL<-dataset[complete.cases(dataset$bconsumosem) & dataset$sexo == "varon", ] # men without NAs in the variable weekly alcohol consumption: n = 25

## Mean age of participants without missing data (NAs) in the follow-up:

mean(zCOMPL$aedad) # both sexes
# 20.72269
sd(zCOMPL$aedad) # both sexes
# 1.711993

mean(zwomCOMPL$aedad) # women
# 20.51064
sd(zwomCOMPL$aedad) # women
# 1.611415

mean(zmenCOMPL$aedad) # men
# 21.52
sd(zmenCOMPL$aedad) # men
# 1.873499


## Mean age of participants with missing data (NAs) in the follow-up:

mean(zNA$aedad) # both sexes
# 20.83425
sd(zNA$aedad) # both sexes
# 1.939398 

mean(zwomNA$aedad) # women
# 20.69828
sd(zwomNA$aedad) # women
# 1.828375

mean(zmenNA$aedad) # men
# 21.07692
sd(zmenNA$aedad) # men
# 2.116215



## Mean weekly alcohol consumption (during the first measurement) among participants without missing data (NAs) in the follow-up:

mean(zCOMPL$aconsumosem) # both sexes
# 3.218487
sd(zCOMPL$aconsumosem) # both sexes
# 2.558335

mean(zwomCOMPL$aconsumosem) # women
# 3.074468
sd(zwomCOMPL$aconsumosem) # women
# 2.472382

mean(zmenCOMPL$aconsumosem) # men
# 3.76
sd(zmenCOMPL$aconsumosem) # men
# 2.847221


## Mean weekly alcohol consumption (during the first measurement) among participants with missing data (NAs) in the follow-up:

mean(zNA$aconsumosem) # both sexes
# 3.823204
sd(zNA$aconsumosem) # both sexes
# 3.016936

mean(zwomNA$aconsumosem) # women
# 3.5
sd(zwomNA$aconsumosem) # women
# 2.419127

mean(zmenNA$aconsumosem) # men
# 4.4
sd(zmenNA$aconsumosem) # men
# 3.815265



## Differences between means:

# Age: without vs with missing values in both sexes:
t.test(x = zCOMPL$aedad, y = zNA$aedad, paired = FALSE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Welch Two Sample t-test
# data:  zCOMPL$aedad and zNA$aedad
# t = -0.52354, df = 273.49, p-value = 0.601
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.5310827  0.3079526
# sample estimates:
# mean of x mean of y 
#  20.72269  20.83425 

# Age: without vs with missing values in women:
t.test(x = zwomCOMPL$aedad, y = zwomNA$aedad, paired = FALSE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Welch Two Sample t-test
# data:  zwomCOMPL$aedad and zwomNA$aedad
# t = -0.7898, df = 206.51, p-value = 0.4306
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.6560240  0.2807489
# sample estimates:
# mean of x mean of y 
#  20.51064  20.69828

# Age: without vs with missing values in men:
t.test(x = zmenCOMPL$aedad, y = zmenNA$aedad, paired = FALSE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Welch Two Sample t-test
# data:  zmenCOMPL$aedad and zmenNA$aedad
# t = 0.96849, df = 48.917, p-value = 0.3376
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.476325  1.362479
# sample estimates:
# mean of x mean of y 
# 21.52000  21.07692


# Weekly alcohol consumption: without vs with missing values in both sexes:
t.test(x = zCOMPL$aconsumosem, y = zNA$aconsumosem, paired = FALSE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Welch Two Sample t-test
# data:  zCOMPL$aconsumosem and zNA$aconsumosem
# t = -1.8636, df = 279.34, p-value = 0.06342
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -1.24345402  0.03401997
# sample estimates:
# mean of x mean of y 
# 3.218487  3.823204

# Weekly alcohol consumption: without vs with missing values in women:
t.test(x = zwomCOMPL$aconsumosem, y = zwomNA$aconsumosem, paired = FALSE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Welch Two Sample t-test
# data:  zwomCOMPL$aconsumosem and zwomNA$aconsumosem
# t = -1.2522, df = 197.26, p-value = 0.212
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -1.0956801  0.2446162
# sample estimates:
# mean of x mean of y 
# 3.074468  3.500000

# Weekly alcohol consumption: without vs with missing values in men:
t.test(x = zmenCOMPL$aconsumosem, y = zmenNA$aconsumosem, paired = FALSE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Welch Two Sample t-test
# data:  zmenCOMPL$aconsumosem and zmenNA$aconsumosem
# t = -0.86438, df = 58.189, p-value = 0.3909
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -2.121992  0.841992
# sample estimates:
# mean of x mean of y 
# 3.76      4.40



####################################################################################
####################################################################################
############################### THE END ############################################
####################################################################################
####################################################################################



