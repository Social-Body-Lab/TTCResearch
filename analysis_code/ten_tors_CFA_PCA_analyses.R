"
Author: Arran J. Davis
Email: arran.davis@anthro.ox.ac.uk | davis.arran@gmail.com
Affiliation: Social Body Lab, Institute of Cognitive and Evolutionary Anthropology, University of Oxford
Date: 05/01/2021
"

#clean environment
rm(list = ls())

#set current working directory to the one this script is in (when in RStudio)
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)

#read metadata
metadata = readLines('../data/TTC_data_anonymised.csv', 2)
print(metadata)

#load the data
dat = read.csv('../data/TTC_data_anonymised.csv', skip = 2)

################################################################################################################################################

### VARIABLE CREATION ###

#find columns with the "Not included" (and variants) string
not_included = dat[colSums(sapply(dat, grepl, pattern = 'No')) > 0]
names(not_included)

#see "Not included" variants
table(not_included$Q3.6.1Pain, useNA = "always")
table(not_included$Q3.6.2Fatigue, useNA = "always")
table(not_included$Q3.9.2Help, useNA = "always")
table(not_included$Q3.9.1Need, useNA = "always")
table(not_included$Q3.6.3Effort, useNA = "always")

#change all "Not included" variants to 'NA'
is.na(not_included$Q3.6.1Pain) = grepl("[A-Za-z]", not_included$Q3.6.1Pain)
is.na(not_included$Q3.6.2Fatigue) = grepl("[A-Za-z]", not_included$Q3.6.2Fatigue)
is.na(not_included$Q3.9.2Help) = grepl("[A-Za-z]", not_included$Q3.9.2Help)
is.na(not_included$Q3.9.1Need) = grepl("[A-Za-z]", not_included$Q3.9.1Need)
is.na(not_included$Q3.6.3Effort) = grepl("[A-Za-z]", not_included$Q3.6.3Effort)

#drop the unused levels and make the variable numeric
not_included$Q3.6.1Pain = as.numeric(as.character(not_included$Q3.6.1Pain))
not_included$Q3.6.2Fatigue = as.numeric(as.character(not_included$Q3.6.2Fatigue))
not_included$Q3.9.2Help = as.numeric(as.character(not_included$Q3.9.2Help))
not_included$Q3.9.1Need = as.numeric(as.character(not_included$Q3.9.1Need))
not_included$Q3.6.3Effort = as.numeric(as.character(not_included$Q3.6.3Effort))

#check the changes
table(not_included$Q3.6.1Pain, useNA = "always")
table(not_included$Q3.6.2Fatigue, useNA = "always")
table(not_included$Q3.9.2Help, useNA = "always")
table(not_included$Q3.9.1Need, useNA = "always")
table(not_included$Q3.6.3Effort, useNA = "always")

#replace these variables in the main dataframe (making them numeric)
dat$Q3.6.1Pain = not_included$Q3.6.1Pain
dat$Q3.6.2Fatigue = not_included$Q3.6.2Fatigue
dat$Q3.9.2Help = not_included$Q3.9.2Help
dat$Q3.9.1Need = not_included$Q3.9.1Need
dat$Q3.6.3Effort = not_included$Q3.6.3Effort

#make the TeamIDVarVar a factor
dat$TeamIDVar = as.factor(dat$TeamIDVar)
unique(dat$TeamIDVar)

################################################################################################################################################

### CONFIRMATORY FACTOR ANALYSIS ON THE STRESS APPRAISAL MEASURE (T2) ###

#create a data frame of the variables of interest
threat_dat_t2 = dat[, c("Q282threat", "Q285Anx", "Q28a.1nega", "Q28a.6nega", #these are the threat sub-questions
                        "Q281.impact", "Q28a.3Eager", "Q28a.5Stron", "Q284Excite", #these are the challege sub-questions
                        "Q286Conse", "Q283Affect", "Q28a.2Impli", "Q28a.4Long")] #these are the centrality questions

threat_dat_t2_no_NA = threat_dat_t2[complete.cases(threat_dat_t2), ]

#correlation matrix of the question answers
round(cor(threat_dat_t2_no_NA[,c("Q282threat", "Q285Anx", "Q28a.1nega", "Q28a.6nega", #these are the threat sub-questions
                                 "Q281.impact", "Q28a.3Eager", "Q28a.5Stron", "Q284Excite", #these are the challege sub-questions
                                 "Q286Conse", "Q283Affect", "Q28a.2Impli", "Q28a.4Long")]), 2)

#visusal correlation matrix
library(corrplot)
corrplot(cor(threat_dat_t2_no_NA[,c("Q282threat", "Q285Anx", "Q28a.1nega", "Q28a.6nega", #these are the threat sub-questions
                                    "Q281.impact", "Q28a.3Eager", "Q28a.5Stron", "Q284Excite", #these are the challege sub-questions
                                    "Q286Conse", "Q283Affect", "Q28a.2Impli", "Q28a.4Long")]), order = "original", tl.col='black', tl.cex=.75) 

#code the model into a lavaan model object
library(lavaan)

### ZERO FACTOR MODEL ###

#model with zero common factors 
stress_0factor = ' #start of model

# latent variable definitions (common factors)
# latent variable variances
# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q282threat ~~ Q282threat
  Q285Anx ~~ Q285Anx
  Q28a.1nega ~~ Q28a.1nega
  Q28a.6nega ~~ Q28a.6nega
  Q281.impact ~~ Q281.impact
  Q28a.3Eager ~~ Q28a.3Eager
  Q28a.5Stron ~~ Q28a.5Stron
  Q284Excite ~~ Q284Excite
  Q286Conse ~~ Q286Conse
  Q283Affect ~~ Q283Affect
  Q28a.2Impli ~~ Q28a.2Impli
  Q28a.4Long ~~ Q28a.4Long
            
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q282threat ~ 1
  Q285Anx ~ 1
  Q28a.1nega ~ 1
  Q28a.6nega ~ 1
  Q281.impact ~ 1
  Q28a.3Eager ~ 1
  Q28a.5Stron ~ 1
  Q284Excite ~ 1
  Q286Conse ~ 1
  Q283Affect ~ 1
  Q28a.2Impli ~ 1
  Q28a.4Long ~ 1
' #end of model

#fit the model and evaluate it
fit0 = lavaan(stress_0factor, data = threat_dat_t2_no_NA, mimic = "mplus")
summary(fit0, standardized=TRUE, fit.measures=TRUE)

library(semPlot)
semPaths(fit0, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

### THREE FACTOR MODEL ###

#model with three common factors
stress_3factor = ' #start of model

# latent variable definitions (common factors)
  threat =~ NA*Q282threat + 
            NA*Q285Anx +
            NA*Q28a.1nega +
            NA*Q28a.6nega
  challenge =~ NA*Q281.impact +
            NA*Q28a.3Eager +
            NA*Q28a.5Stron +
            NA*Q284Excite
  centrality =~ NA*Q286Conse +
            NA*Q283Affect +
            NA*Q28a.2Impli +
            NA*Q28a.4Long

# latent variable variances
  threat ~~ 1*threat
  challenge ~~ 1*challenge
  centrality ~~ 1*centrality

# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q282threat ~~ Q282threat
  Q285Anx ~~ Q285Anx
  Q28a.1nega ~~ Q28a.1nega
  Q28a.6nega ~~ Q28a.6nega
  Q281.impact ~~ Q281.impact
  Q28a.3Eager ~~ Q28a.3Eager
  Q28a.5Stron ~~ Q28a.5Stron
  Q284Excite ~~ Q284Excite
  Q286Conse ~~ Q286Conse
  Q283Affect ~~ Q283Affect
  Q28a.2Impli ~~ Q28a.2Impli
  Q28a.4Long ~~ Q28a.4Long
            
# manifest variable covariances (uniquenesses)

# manifest variable means 
  Q282threat ~ 1
  Q285Anx ~ 1
  Q28a.1nega ~ 1
  Q28a.6nega ~ 1
  Q281.impact ~ 1
  Q28a.3Eager ~ 1
  Q28a.5Stron ~ 1
  Q284Excite ~ 1
  Q286Conse ~ 1
  Q283Affect ~ 1
  Q28a.2Impli ~ 1
  Q28a.4Long ~ 1
' #end of model

#fit the model and evaluate it
fit3 = lavaan(stress_3factor, data= threat_dat_t2_no_NA, mimic = "mplus")
summary(fit3, standardized=TRUE, fit.measures=TRUE)

#get the factor loadings
inspect(fit3,what="std")$lambda

#plot the factor loadings
semPaths(fit3, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#compare the fit of this model to the fit of the zero factor model
round(cbind(m1=inspect(fit0, 'fit.measures'), m2=inspect(fit3, 'fit.measures')),3)
anova(fit0, fit3)

### FOUR FACTOR MODEL ###

#look at the correlation matrix to identify a four factor model
corrplot(cor(threat_dat_t2_no_NA[,c("Q282threat", "Q285Anx", "Q28a.1nega", "Q28a.6nega", #these are the threat sub-questions
                                    "Q281.impact", "Q28a.3Eager", "Q28a.5Stron", "Q284Excite", #these are the challege sub-questions
                                    "Q286Conse", "Q283Affect", "Q28a.2Impli", "Q28a.4Long")]), order = "original", tl.col='black', tl.cex=.75) 

#based on the correlation matrix, amongst the threat subset, Q282threat and Q285Anx correlate highly, along with Q28a.1nega and Q28a.6nega
#indeed, the threat factor had low factor loadings in the three factor model reported above
#create a model with four common factors, splitting up the threat subset
#model with 4 common factors
stress_4factor = ' #start of model

# latent variable definitions (common factors)
  threat_e =~ NA*Q282threat + 
            NA*Q285Anx
  threat_o =~ NA*Q28a.1nega +
            NA*Q28a.6nega
  challenge =~ NA*Q281.impact +
            NA*Q28a.3Eager +
            NA*Q28a.5Stron +
            NA*Q284Excite
  centrality =~ NA*Q286Conse +
            NA*Q283Affect +
            NA*Q28a.2Impli +
            NA*Q28a.4Long

# latent variable variances
  threat_e ~~ 1*threat_e
  threat_o ~~ 1*threat_o
  challenge ~~ 1*challenge
  centrality ~~ 1*centrality

# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q282threat ~~ Q282threat
  Q285Anx ~~ Q285Anx
  Q28a.1nega ~~ Q28a.1nega
  Q28a.6nega ~~ Q28a.6nega
  Q281.impact ~~ Q281.impact
  Q28a.3Eager ~~ Q28a.3Eager
  Q28a.5Stron ~~ Q28a.5Stron
  Q284Excite ~~ Q284Excite
  Q286Conse ~~ Q286Conse
  Q283Affect ~~ Q283Affect
  Q28a.2Impli ~~ Q28a.2Impli
  Q28a.4Long ~~ Q28a.4Long
            
# manifest variable covariances (uniquenesses)

# manifest variable means 
  Q282threat ~ 1
  Q285Anx ~ 1
  Q28a.1nega ~ 1
  Q28a.6nega ~ 1
  Q281.impact ~ 1
  Q28a.3Eager ~ 1
  Q28a.5Stron ~ 1
  Q284Excite ~ 1
  Q286Conse ~ 1
  Q283Affect ~ 1
  Q28a.2Impli ~ 1
  Q28a.4Long ~ 1
' #end of model

#fit the model and evaluate it
fit4 = lavaan(stress_4factor, data= threat_dat_t2_no_NA, mimic = "mplus")
summary(fit4, standardized=TRUE, fit.measures=TRUE)
cor(threat_dat_t2_no_NA$Q28a.1nega, threat_dat_t2_no_NA$Q28a.6nega) #the model may not be identified due to collinearity in the 'threat_outcome' variables

#get the factor loadings
inspect(fit4,what="std")$lambda

semPaths(fit4, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#compare the model to the other models
round(cbind(m1=inspect(fit0, 'fit.measures'), m2=inspect(fit3, 'fit.measures'),m3=inspect(fit4, 'fit.measures')),3)

#correct the model df and compare it to the other two
fit4@test$standard$df = 50
anova(fit0, fit4)
anova(fit3, fit4)

### PRINCIPAL COMPONENT ANALYSIS ON THREAT AT T2 (threat_t2) ###

library("corpcor")
library("GPArotation")
library("psych")

#create a data frame of the variables of interest
threat_dat_t2 = dat[, c("Q282threat", "Q285Anx", "Q28a.1nega", "Q28a.6nega")]
threat_dat_t2_id = dat[, c("Q282threat", "Q285Anx", "Q28a.1nega", "Q28a.6nega", "GeneralIndex")]

threat_dat_t2 = na.omit(threat_dat_t2)
threat_dat_t2_id = na.omit(threat_dat_t2_id)

#create a matrix
threat.matrix = cor(threat_dat_t2)

#Bartlett's test to see if PCA is appropriate (see Field et al., 2014; p. 775)
cortest.bartlett(threat_dat_t2) #test is significant, indicatig that a PCA is appropriate

#do a KMO (see Field et al., 2014; p. 776)
KMO(threat_dat_t2) #values between .5 and .7 are "mediocre"

#determinant of correlation matrix (see Field et al., 2014; p. 777)
det(threat.matrix) #this is not problematic (since it is greater than .00001)

#the PCA: analyses suggest extracting either one or two factors
PCA.threat_t2 = principal(threat_dat_t2, nfactors = 4, scores = TRUE)
plot(PCA.threat_t2$values, type = "b") #scree plot has in inflection at 2 and 3, so keep one factor or two factors

PCA.threat.two.factors_t2 =principal(threat_dat_t2, nfactors = 2, rotate = "oblimin", scores = TRUE)
print.psych(PCA.threat.two.factors_t2, cut = 0.3, sort = TRUE)

PCA.threat.one.factor_t2 = principal(threat_dat_t2, nfactors = 1, rotate = "oblimin", scores = TRUE)
print.psych(PCA.threat.one.factor_t2, cut = 0.3, sort = TRUE) 
summary(prcomp(threat_dat_t2, scale = TRUE)) #this gives a more accurate proportion of variance explained by the first component (PC1)

#Cronbach's alpha for all factors together
psych::alpha(threat_dat_t2) #this yeilds an unacceptable (.64) Cronbach's alpha

### ### ###

#Cronbach's alpha for all questions on threat outcome (warning a result of running analysis on only two variables)
psych::alpha(threat_dat_t2[, c("Q28a.1nega", "Q28a.6nega")]) #this yeilds an acceptable (.78) Cronbach's alpha

#get the descriptives of this component - threat outcome
PCA.threat_outcome.one.factor_t2 = principal(threat_dat_t2[c("Q28a.1nega", "Q28a.6nega")], nfactors = 1, rotate = "oblimin", scores = TRUE)
print.psych(PCA.threat_outcome.one.factor_t2, cut = 0.3, sort = TRUE)
summary(prcomp(threat_dat_t2[, c("Q28a.1nega", "Q28a.6nega")], scale = TRUE))

### ### ###

#Cronbach's alpha for all questions on emotional reactions to the challenge (warning a result of running analysis on only two variables)
psych::alpha(threat_dat_t2[c("Q282threat", "Q285Anx")]) #this yeilds an unacceptable (.67) Cronbach's alpha (warning can be ignored)

#get the descriptives of this component - threat emotion
PCA.threat_emotion.one.factor_t2 =principal(threat_dat_t2[c("Q282threat", "Q285Anx")], nfactors = 1, rotate = "oblimin", scores = TRUE)
print.psych(PCA.threat_emotion.one.factor_t2, cut = 0.3, sort = TRUE)
summary(prcomp(threat_dat_t2[c("Q282threat", "Q285Anx")], scale = TRUE))

### ### ###

#add all three components to the dataset
threat_dat_t2_id$threat_t2 = PCA.threat.one.factor_t2$scores[,1]
threat_dat_t2_id$threat_t2_outcome = PCA.threat_outcome.one.factor_t2$scores[,1]
threat_dat_t2_id$threat_t2_emotion = PCA.threat_emotion.one.factor_t2$scores[,1]
dat = transform(merge(dat, threat_dat_t2_id[, c("threat_t2", "threat_t2_outcome", "threat_t2_emotion", "GeneralIndex")], by.x = "GeneralIndex", by.y = "GeneralIndex", all = T))

#test the data structure (it looks good so far)
t = dat[,c("GeneralIndex", "Q4.7Circles", "Q4.9.1Connect", "Q4.9.2Bond", "Q4.9.3Commit",
           "Q282threat", "Q285Anx", "Q28a.1nega", "Q28a.6nega","threat_t2", "threat_t2_outcome", "threat_t2_emotion", "GeneralIndex")]

#which variable has the most variance
hist(dat$threat_t2)
hist(dat$threat_t2_emotion)
hist(dat$threat_t2_outcome) #this variable has almost no variance, and probably should not be used.

### CREATION OF CHALLENGE AND CENTRALITY VARIABLES ###

#the confirmatory factor analysis showed that the challenge and centrality questions of the Stress Appraisal Measure fell into two seperate components, as predicted
#both variables will thus be the average of responses to the four questions on challenge and centrality
dat$challenge_t2 = rowMeans(dat[,c("Q281.impact", "Q28a.3Eager", "Q28a.5Stron", "Q284Excite")], na.rm = TRUE)
dat$centrality_t2 = rowMeans(dat[,c("Q286Conse", "Q283Affect", "Q28a.2Impli", "Q28a.4Long")], na.rm = TRUE)

#convert NaNs to NAs
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

dat[is.nan(dat)] = NA

#test the data structure
t = dat[,c("GeneralIndex", "Q4.7Circles","Q281.impact", "Q28a.3Eager", "Q28a.5Stron", "Q284Excite", "challenge_t2", "centrality_t2")]

################################################################################################################################################

### CONFIRMATORY FACTOR ANALYSIS ON THE MOOD SCALE (T2) ###

#create a data frame of the variables of interest
mood_dat_t2 = dat[, c("Q231Panicky",
                      "Q232Lively",
                      "Q233Confused",
                      "Q34Worn",
                      "Q235Depres",
                      "Q236Down",
                      "Q23a.1Annoy",
                      "Q23a.2Exhaust",
                      "Q23a.3Mix",
                      "Q23a.4Sleep",
                      "Q23a.5Bitter",
                      "Q23a.6Unhap",
                      "Q23b.1Worr",
                      "Q23b.2Energ",
                      "Q23b.3Mis",
                      "Q23b.4Muddl",
                      "Q23b.5Nerv",
                      "Q23b.6Angry",
                      "Q23c.1Acti",
                      "Q23c.2Anx",
                      "Q23c.3BadT",
                      "Q23c.4Tire",
                      "Q23c.5Alert",
                      "Q23c.6Uncet")]

mood_dat_t2_no_NA = mood_dat_t2[complete.cases(mood_dat_t2), ]

#correlation matrix of the question answers
round(cor(mood_dat_t2_no_NA[,c("Q231Panicky",
                               "Q232Lively",
                               "Q233Confused",
                               "Q34Worn",
                               "Q235Depres",
                               "Q236Down",
                               "Q23a.1Annoy",
                               "Q23a.2Exhaust",
                               "Q23a.3Mix",
                               "Q23a.4Sleep",
                               "Q23a.5Bitter",
                               "Q23a.6Unhap",
                               "Q23b.1Worr",
                               "Q23b.2Energ",
                               "Q23b.3Mis",
                               "Q23b.4Muddl",
                               "Q23b.5Nerv",
                               "Q23b.6Angry",
                               "Q23c.1Acti",
                               "Q23c.2Anx",
                               "Q23c.3BadT",
                               "Q23c.4Tire",
                               "Q23c.5Alert",
                               "Q23c.6Uncet")]), 2)

#visusal correlation matrix
library(corrplot)
corrplot(cor(mood_dat_t2_no_NA[,c("Q231Panicky",
                                  "Q232Lively",
                                  "Q233Confused",
                                  "Q34Worn",
                                  "Q235Depres",
                                  "Q236Down",
                                  "Q23a.1Annoy",
                                  "Q23a.2Exhaust",
                                  "Q23a.3Mix",
                                  "Q23a.4Sleep",
                                  "Q23a.5Bitter",
                                  "Q23a.6Unhap",
                                  "Q23b.1Worr",
                                  "Q23b.2Energ",
                                  "Q23b.3Mis",
                                  "Q23b.4Muddl",
                                  "Q23b.5Nerv",
                                  "Q23b.6Angry",
                                  "Q23c.1Acti",
                                  "Q23c.2Anx",
                                  "Q23c.3BadT",
                                  "Q23c.4Tire",
                                  "Q23c.5Alert",
                                  "Q23c.6Uncet")]), order = "original", tl.col='black', tl.cex=.75) 

### ZERO FACTOR MODEL ###

#model with zero common factors 
mood_0factor = ' #start of model

# latent variable definitions (common factors)
# latent variable variances
# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q231Panicky ~~ Q231Panicky
  Q232Lively ~~ Q232Lively
  Q233Confused ~~ Q233Confused
  Q34Worn ~~ Q34Worn
  Q235Depres ~~ Q235Depres
  Q236Down ~~ Q236Down
  Q23a.1Annoy ~~ Q23a.1Annoy
  Q23a.2Exhaust ~~ Q23a.2Exhaust
  Q23a.3Mix ~~ Q23a.3Mix
  Q23a.4Sleep ~~ Q23a.4Sleep
  Q23a.5Bitter ~~ Q23a.5Bitter
  Q23a.6Unhap ~~ Q23a.6Unhap
  Q23b.1Worr ~~ Q23b.1Worr
  Q23b.2Energ ~~ Q23b.2Energ
  Q23b.3Mis ~~ Q23b.3Mis
  Q23b.4Muddl ~~ Q23b.4Muddl
  Q23b.5Nerv ~~ Q23b.5Nerv
  Q23b.6Angry ~~ Q23b.6Angry
  Q23c.1Acti ~~ Q23c.1Acti
  Q23c.2Anx ~~ Q23c.2Anx
  Q23c.3BadT ~~ Q23c.3BadT
  Q23c.4Tire ~~ Q23c.4Tire
  Q23c.5Alert ~~ Q23c.5Alert
  Q23c.6Uncet ~~ Q23c.6Uncet
            
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q231Panicky ~ 1
  Q232Lively ~ 1
  Q233Confused ~ 1
  Q34Worn ~ 1
  Q235Depres ~ 1
  Q236Down ~ 1
  Q23a.1Annoy ~ 1
  Q23a.2Exhaust ~ 1
  Q23a.3Mix ~ 1
  Q23a.4Sleep ~ 1
  Q23a.5Bitter ~ 1
  Q23a.6Unhap ~ 1
  Q23b.1Worr ~ 1
  Q23b.2Energ ~ 1
  Q23b.3Mis ~ 1
  Q23b.4Muddl ~ 1
  Q23b.5Nerv ~ 1
  Q23b.6Angry ~ 1
  Q23c.1Acti ~ 1
  Q23c.2Anx ~ 1
  Q23c.3BadT ~ 1
  Q23c.4Tire ~ 1
  Q23c.5Alert ~ 1
  Q23c.6Uncet ~ 1
' #end of model

#fit the model and evaluate it
fit0 = lavaan(mood_0factor, data = mood_dat_t2_no_NA, mimic = "mplus")
summary(fit0, standardized=TRUE, fit.measures=TRUE)

semPaths(fit0, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

### SIX FACTOR MODEL ###

#model with six common factors 
mood_6factor = ' #start of model

# latent variable definitions (common factors)
  tension =~ NA*Q231Panicky + NA*Q23c.2Anx + NA*Q23b.1Worr + NA*Q23b.5Nerv
  vigour =~ NA*Q232Lively + NA*Q23b.2Energ + NA*Q23c.1Acti + NA*Q23c.5Alert
  confusion =~ NA*Q233Confused + NA*Q23a.3Mix + NA*Q23b.4Muddl + NA*Q23c.6Uncet
  fatigue =~ NA*Q34Worn + NA*Q23a.2Exhaust + NA*Q23a.4Sleep + NA*Q23c.4Tire
  depression =~ NA*Q235Depres + NA*Q236Down + NA*Q23a.6Unhap + NA*Q23b.3Mis
  anger =~ NA*Q23a.1Annoy + NA*Q23a.5Bitter + NA*Q23b.6Angry + NA*Q23c.3BadT
  
# latent variable variances
  tension ~~ tension
  vigour ~~ vigour
  confusion ~~ confusion
  fatigue ~~ fatigue
  depression ~~ depression
  anger ~~ anger
  
# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q231Panicky ~~ Q231Panicky
  Q232Lively ~~ Q232Lively
  Q233Confused ~~ Q233Confused
  Q34Worn ~~ Q34Worn
  Q235Depres ~~ Q235Depres
  Q236Down ~~ Q236Down
  Q23a.1Annoy ~~ Q23a.1Annoy
  Q23a.2Exhaust ~~ Q23a.2Exhaust
  Q23a.3Mix ~~ Q23a.3Mix
  Q23a.4Sleep ~~ Q23a.4Sleep
  Q23a.5Bitter ~~ Q23a.5Bitter
  Q23a.6Unhap ~~ Q23a.6Unhap
  Q23b.1Worr ~~ Q23b.1Worr
  Q23b.2Energ ~~ Q23b.2Energ
  Q23b.3Mis ~~ Q23b.3Mis
  Q23b.4Muddl ~~ Q23b.4Muddl
  Q23b.5Nerv ~~ Q23b.5Nerv
  Q23b.6Angry ~~ Q23b.6Angry
  Q23c.1Acti ~~ Q23c.1Acti
  Q23c.2Anx ~~ Q23c.2Anx
  Q23c.3BadT ~~ Q23c.3BadT
  Q23c.4Tire ~~ Q23c.4Tire
  Q23c.5Alert ~~ Q23c.5Alert
  Q23c.6Uncet ~~ Q23c.6Uncet
  
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q231Panicky ~ 1
  Q232Lively ~ 1
  Q233Confused ~ 1
  Q34Worn ~ 1
  Q235Depres ~ 1
  Q236Down ~ 1
  Q23a.1Annoy ~ 1
  Q23a.2Exhaust ~ 1
  Q23a.3Mix ~ 1
  Q23a.4Sleep ~ 1
  Q23a.5Bitter ~ 1
  Q23a.6Unhap ~ 1
  Q23b.1Worr ~ 1
  Q23b.2Energ ~ 1
  Q23b.3Mis ~ 1
  Q23b.4Muddl ~ 1
  Q23b.5Nerv ~ 1
  Q23b.6Angry ~ 1
  Q23c.1Acti ~ 1
  Q23c.2Anx ~ 1
  Q23c.3BadT ~ 1
  Q23c.4Tire ~ 1
  Q23c.5Alert ~ 1
  Q23c.6Uncet ~ 1
' #end of model

#fit the model and evaluate it
fit6 = lavaan(mood_6factor, data = mood_dat_t2_no_NA, mimic = "mplus")
summary(fit6, standardized=TRUE, fit.measures=TRUE)

semPaths(fit6, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#get the factor loadings
inspect(fit6,what="std")$lambda

#get the variance of the anger question
sd(dat$Q23b.6Angry, na.rm = TRUE)
mean(c(sd(dat$Q23a.1Annoy, na.rm = TRUE), sd(dat$Q23a.5Bitter, na.rm = TRUE), sd(dat$Q23c.3BadT, na.rm = TRUE)))

#compare the fit of this model to the fit of the zero factor model
round(cbind(m1=inspect(fit0, 'fit.measures'), m2=inspect(fit6, 'fit.measures')),3)
anova(fit0, fit6)

#Cronbach's alpha for the emotional and esteem factors (exclude "anger" question because factor loading is less than .4)
tension_data_t2 = na.exclude(dat[,c("Q231Panicky",  "Q23c.2Anx",  "Q23b.1Worr", "Q23b.5Nerv")])
psych::alpha(tension_data_t2) #this yeilds an acceptable (.89) Cronbach's alpha

vigour_data_t2 = na.exclude(dat[,c("Q232Lively", "Q23b.2Energ", "Q23c.1Acti", "Q23c.5Alert")])
psych::alpha(vigour_data_t2) #this yeilds an acceptable (.75) Cronbach's alpha

confusion_data_t2 = na.exclude(dat[,c("Q233Confused", "Q23a.3Mix", "Q23b.4Muddl", "Q23c.6Uncet")])
psych::alpha(confusion_data_t2) #this yeilds an acceptable (.61) Cronbach's alpha

fatigue_data_t2 = na.exclude(dat[,c("Q34Worn", "Q23a.2Exhaust", "Q23a.4Sleep", "Q23c.4Tire")])
psych::alpha(fatigue_data_t2) #this yeilds an acceptable (.80) Cronbach's alpha

depression_data_t2 = na.exclude(dat[,c("Q235Depres", "Q236Down", "Q23a.6Unhap", "Q23b.3Mis")])
psych::alpha(depression_data_t2) #this yeilds an acceptable (.74) Cronbach's alpha

anger_data_t2 = na.exclude(dat[,c("Q23a.1Annoy", "Q23a.5Bitter", "Q23c.3BadT")])
psych::alpha(anger_data_t2) #this yeilds an acceptable (.74) Cronbach's alpha

#percetage of variance explained
print(principal(tension_data_t2, nfactors = 1, scores = TRUE))
print(principal(vigour_data_t2, nfactors = 1, scores = TRUE))
print(principal(confusion_data_t2, nfactors = 1, scores = TRUE))
print(principal(fatigue_data_t2, nfactors = 1, scores = TRUE))
print(principal(depression_data_t2, nfactors = 1, scores = TRUE))
print(principal(anger_data_t2, nfactors = 1, scores = TRUE))

### VARIABLE CREATION ###

#the analyses confirmed a six factor model, made up of the following six factors and their sub-questions (exclude "anger" question because factor loading is less than .4)
dat$tension_t2 = rowMeans(dat[,c("Q231Panicky",  "Q23c.2Anx",  "Q23b.1Worr", "Q23b.5Nerv")], na.rm = TRUE)
dat$vigour_t2 = rowMeans(dat[,c("Q232Lively", "Q23b.2Energ", "Q23c.1Acti", "Q23c.5Alert")], na.rm = TRUE)
dat$confusion_t2 = rowMeans(dat[,c("Q233Confused", "Q23a.3Mix", "Q23b.4Muddl", "Q23c.6Uncet")], na.rm = TRUE)
dat$fatigue_t2 = rowMeans(dat[,c("Q34Worn", "Q23a.2Exhaust", "Q23a.4Sleep", "Q23c.4Tire")], na.rm = TRUE)
dat$depression_t2 = rowMeans(dat[,c("Q235Depres", "Q236Down", "Q23a.6Unhap", "Q23b.3Mis")], na.rm = TRUE)
dat$anger_t2 = rowMeans(dat[,c("Q23a.1Annoy", "Q23a.5Bitter", "Q23c.3BadT")], na.rm = TRUE)

#convert NaNs to NAs
dat[is.nan(dat)] = NA

#test the data structure
t = dat[,c("GeneralIndex", "tension_t2", "vigour_t2", "confusion_t2", "fatigue_t2", "depression_t2", "anger_t2")]

################################################################################################################################################

### CONFIRMATORY FACTOR ANALYSIS ON THE MOOD SCALE (T3) ###

#create a data frame of the variables of interest
mood_dat_t3 = dat[, c("Q3.5.1Panicky",
                      "Q3.5.2Lively",
                      "Q3.5.3Confus",
                      "Q3.5.4Worn",
                      "Q3.5.5Depres",
                      "Q3.5.6Down",
                      "Q3.5.a.1Annoy",
                      "Q3.5.a.2Exhaus",
                      "Q3.5.a.3Mix",
                      "Q3.5.a.4Sleep",
                      "Q3.5.a.5Bitt",
                      "Q3.5.a.6Unhap",
                      "Q3.5.b.1Worri",
                      "Q3.5.b.2Energ",
                      "Q3.5.b.3Mis",
                      "Q3.5.b.4Muddl",
                      "Q3.5.b.5Nerv",
                      "Q3.5.b.6Angr",
                      "Q3.5.c.1Activ",
                      "Q3.5.c.2Anx",
                      "Q3.5.c.3BadT",
                      "Q3.5.c.4Tire",
                      "Q3.5.c.5Alert",
                      "Q3.5.c.6Uncer")]

mood_dat_t3$Q3.5.c.6Uncer = as.integer(mood_dat_t3$Q3.5.c.6Uncer)
mood_dat_t3_no_NA = mood_dat_t3[complete.cases(mood_dat_t3), ]

#correlation matrix of the question answers
round(cor(mood_dat_t3_no_NA[,c("Q3.5.1Panicky",
                               "Q3.5.2Lively",
                               "Q3.5.3Confus",
                               "Q3.5.4Worn",
                               "Q3.5.5Depres",
                               "Q3.5.6Down",
                               "Q3.5.a.1Annoy",
                               "Q3.5.a.2Exhaus",
                               "Q3.5.a.3Mix",
                               "Q3.5.a.4Sleep",
                               "Q3.5.a.5Bitt",
                               "Q3.5.a.6Unhap",
                               "Q3.5.b.1Worri",
                               "Q3.5.b.2Energ",
                               "Q3.5.b.3Mis",
                               "Q3.5.b.4Muddl",
                               "Q3.5.b.5Nerv",
                               "Q3.5.b.6Angr",
                               "Q3.5.c.1Activ",
                               "Q3.5.c.2Anx",
                               "Q3.5.c.3BadT",
                               "Q3.5.c.4Tire",
                               "Q3.5.c.5Alert",
                               "Q3.5.c.6Uncer")]), 2)

#visusal correlation matrix
library(corrplot)
corrplot(cor(mood_dat_t3_no_NA[,c("Q3.5.1Panicky",
                                  "Q3.5.2Lively",
                                  "Q3.5.3Confus",
                                  "Q3.5.4Worn",
                                  "Q3.5.5Depres",
                                  "Q3.5.6Down",
                                  "Q3.5.a.1Annoy",
                                  "Q3.5.a.2Exhaus",
                                  "Q3.5.a.3Mix",
                                  "Q3.5.a.4Sleep",
                                  "Q3.5.a.5Bitt",
                                  "Q3.5.a.6Unhap",
                                  "Q3.5.b.1Worri",
                                  "Q3.5.b.2Energ",
                                  "Q3.5.b.3Mis",
                                  "Q3.5.b.4Muddl",
                                  "Q3.5.b.5Nerv",
                                  "Q3.5.b.6Angr",
                                  "Q3.5.c.1Activ",
                                  "Q3.5.c.2Anx",
                                  "Q3.5.c.3BadT",
                                  "Q3.5.c.4Tire",
                                  "Q3.5.c.5Alert",
                                  "Q3.5.c.6Uncer")]), order = "original", tl.col='black', tl.cex=.75) 

### ZERO FACTOR MODEL ###

#model with zero common factors 
mood_0factor = ' #start of model

# latent variable definitions (common factors)
# latent variable variances
# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q3.5.1Panicky ~~ Q3.5.1Panicky
  Q3.5.2Lively ~~ Q3.5.2Lively
  Q3.5.3Confus ~~ Q3.5.3Confus
  Q3.5.4Worn ~~ Q3.5.4Worn
  Q3.5.5Depres ~~ Q3.5.5Depres
  Q3.5.6Down ~~ Q3.5.6Down
  Q3.5.a.1Annoy ~~ Q3.5.a.1Annoy
  Q3.5.a.2Exhaus ~~ Q3.5.a.2Exhaus
  Q3.5.a.3Mix ~~ Q3.5.a.3Mix
  Q3.5.a.4Sleep ~~ Q3.5.a.4Sleep
  Q3.5.a.5Bitt ~~ Q3.5.a.5Bitt
  Q3.5.a.6Unhap ~~ Q3.5.a.6Unhap
  Q3.5.b.1Worri ~~ Q3.5.b.1Worri
  Q3.5.b.2Energ ~~ Q3.5.b.2Energ
  Q3.5.b.3Mis ~~ Q3.5.b.3Mis
  Q3.5.b.4Muddl ~~ Q3.5.b.4Muddl
  Q3.5.b.5Nerv ~~ Q3.5.b.5Nerv
  Q3.5.b.6Angr ~~ Q3.5.b.6Angr
  Q3.5.c.1Activ ~~ Q3.5.c.1Activ
  Q3.5.c.2Anx ~~ Q3.5.c.2Anx
  Q3.5.c.3BadT ~~ Q3.5.c.3BadT
  Q3.5.c.4Tire ~~ Q3.5.c.4Tire
  Q3.5.c.5Alert ~~ Q3.5.c.5Alert
  Q3.5.c.6Uncer ~~ Q3.5.c.6Uncer
            
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q3.5.1Panicky ~ 1
  Q3.5.2Lively ~ 1 
  Q3.5.3Confus ~ 1
  Q3.5.4Worn ~ 1
  Q3.5.5Depres ~ 1
  Q3.5.6Down ~ 1
  Q3.5.a.1Annoy ~ 1
  Q3.5.a.2Exhaus ~ 1
  Q3.5.a.3Mix ~ 1
  Q3.5.a.4Sleep ~ 1
  Q3.5.a.5Bitt ~ 1
  Q3.5.a.6Unhap ~ 1
  Q3.5.b.1Worri ~ 1
  Q3.5.b.2Energ ~ 1
  Q3.5.b.3Mis ~ 1
  Q3.5.b.4Muddl ~ 1
  Q3.5.b.5Nerv ~ 1
  Q3.5.b.6Angr ~ 1
  Q3.5.c.1Activ ~ 1
  Q3.5.c.2Anx ~ 1
  Q3.5.c.3BadT ~ 1
  Q3.5.c.4Tire ~ 1
  Q3.5.c.5Alert ~ 1
  Q3.5.c.6Uncer ~ 1
' #end of model

#fit the model and evaluate it
fit0 = lavaan(mood_0factor, data = mood_dat_t3_no_NA, mimic = "mplus")
summary(fit6, standardized=TRUE, fit.measures=TRUE)

semPaths(fit6, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

### SIX FACTOR MODEL ###

#model with six common factors 
mood_6factor = ' #start of model

# latent variable definitions (common factors)
  tension =~ NA*Q3.5.1Panicky + NA*Q3.5.c.2Anx + NA*Q3.5.b.1Worri + NA*Q3.5.b.5Nerv
  vigour =~ NA*Q3.5.2Lively + NA*Q3.5.b.2Energ + NA*Q3.5.c.1Activ + NA*Q3.5.c.5Alert
  confusion =~ NA*Q3.5.3Confus + NA*Q3.5.a.3Mix + NA*Q3.5.b.4Muddl + NA*Q3.5.c.6Uncer
  fatigue =~ NA*Q3.5.4Worn + NA*Q3.5.a.2Exhaus + NA*Q3.5.a.4Sleep + NA*Q3.5.c.4Tire
  depression =~ NA*Q3.5.5Depres + NA*Q3.5.6Down + NA*Q3.5.a.6Unhap + NA*Q3.5.b.3Mis
  anger =~ NA*Q3.5.a.1Annoy + NA*Q3.5.a.5Bitt + NA*Q3.5.b.6Angr + NA*Q3.5.c.3BadT
  
# latent variable variances
  tension ~~ tension
  vigour ~~ vigour
  confusion ~~ confusion
  fatigue ~~ fatigue
  depression ~~ depression
  anger ~~ anger
  
# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q3.5.1Panicky ~~ Q3.5.1Panicky
  Q3.5.2Lively ~~ Q3.5.2Lively
  Q3.5.3Confus ~~ Q3.5.3Confus
  Q3.5.4Worn ~~ Q3.5.4Worn
  Q3.5.5Depres ~~ Q3.5.5Depres
  Q3.5.6Down ~~ Q3.5.6Down
  Q3.5.a.1Annoy ~~ Q3.5.a.1Annoy
  Q3.5.a.2Exhaus ~~ Q3.5.a.2Exhaus
  Q3.5.a.3Mix ~~ Q3.5.a.3Mix
  Q3.5.a.4Sleep ~~ Q3.5.a.4Sleep
  Q3.5.a.5Bitt ~~ Q3.5.a.5Bitt
  Q3.5.a.6Unhap ~~ Q3.5.a.6Unhap
  Q3.5.b.1Worri ~~ Q3.5.b.1Worri
  Q3.5.b.2Energ ~~ Q3.5.b.2Energ
  Q3.5.b.3Mis ~~ Q3.5.b.3Mis
  Q3.5.b.4Muddl ~~ Q3.5.b.4Muddl
  Q3.5.b.5Nerv ~~ Q3.5.b.5Nerv
  Q3.5.b.6Angr ~~ Q3.5.b.6Angr
  Q3.5.c.1Activ ~~ Q3.5.c.1Activ
  Q3.5.c.2Anx ~~ Q3.5.c.2Anx
  Q3.5.c.3BadT ~~ Q3.5.c.3BadT
  Q3.5.c.4Tire ~~ Q3.5.c.4Tire
  Q3.5.c.5Alert ~~ Q3.5.c.5Alert
  Q3.5.c.6Uncer ~~ Q3.5.c.6Uncer
            
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q3.5.1Panicky ~ 1
  Q3.5.2Lively ~ 1 
  Q3.5.3Confus ~ 1
  Q3.5.4Worn ~ 1
  Q3.5.5Depres ~ 1
  Q3.5.6Down ~ 1
  Q3.5.a.1Annoy ~ 1
  Q3.5.a.2Exhaus ~ 1
  Q3.5.a.3Mix ~ 1
  Q3.5.a.4Sleep ~ 1
  Q3.5.a.5Bitt ~ 1
  Q3.5.a.6Unhap ~ 1
  Q3.5.b.1Worri ~ 1
  Q3.5.b.2Energ ~ 1
  Q3.5.b.3Mis ~ 1
  Q3.5.b.4Muddl ~ 1
  Q3.5.b.5Nerv ~ 1
  Q3.5.b.6Angr ~ 1
  Q3.5.c.1Activ ~ 1
  Q3.5.c.2Anx ~ 1
  Q3.5.c.3BadT ~ 1
  Q3.5.c.4Tire ~ 1
  Q3.5.c.5Alert ~ 1
  Q3.5.c.6Uncer ~ 1
' #end of model

#fit the model and evaluate it
fit6 = lavaan(mood_6factor, data = mood_dat_t3_no_NA, mimic = "mplus")
summary(fit6, standardized=TRUE, fit.measures=TRUE)

semPaths(fit6, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#get the factor loadings
inspect(fit6,what="std")$lambda

#compare the fit of this model to the fit of the 0 factor model
round(cbind(m1=inspect(fit0, 'fit.measures'), m2=inspect(fit6, 'fit.measures')),3)
anova(fit0, fit6)

#Cronbach's alpha for the emotional and esteem factors (exclude "anger" question because factor loading is less than .4)
tension_data_t3 = na.exclude(dat[,c("Q3.5.1Panicky", "Q3.5.c.2Anx", "Q3.5.b.1Worri", "Q3.5.b.5Nerv")])
psych::alpha(tension_data_t3) #this yeilds an acceptable (.81) Cronbach's alpha

vigour_data_t3 = na.exclude(dat[,c("Q3.5.2Lively", "Q3.5.b.2Energ", "Q3.5.c.1Activ", "Q3.5.c.5Alert")])
psych::alpha(vigour_data_t3) #this yeilds an acceptable (.76) Cronbach's alpha

confusion_data_t3 = na.exclude(dat[,c("Q3.5.3Confus", "Q3.5.a.3Mix", "Q3.5.b.4Muddl", "Q3.5.c.6Uncer")])
psych::alpha(confusion_data_t3) #this yeilds an acceptable (.75) Cronbach's alpha

fatigue_data_t3 = na.exclude(dat[,c("Q3.5.4Worn", "Q3.5.a.2Exhaus", "Q3.5.a.4Sleep", "Q3.5.c.4Tire")])
psych::alpha(fatigue_data_t3) #this yeilds an acceptable (.85) Cronbach's alpha

depression_data_t3 = na.exclude(dat[,c("Q3.5.5Depres", "Q3.5.6Down", "Q3.5.a.6Unhap", "Q3.5.b.3Mis")])
psych::alpha(depression_data_t3) #this yeilds an acceptable (.90) Cronbach's alpha

anger_data_t3 = na.exclude(dat[,c("Q3.5.a.1Annoy", "Q3.5.a.5Bitt", "Q3.5.b.6Angr", "Q3.5.c.3BadT")])
psych::alpha(anger_data_t3) #this yeilds an acceptable (.87) Cronbach's alpha

#percetage of variance explained
print(principal(tension_data_t3, nfactors = 1, scores = TRUE))
print(principal(vigour_data_t3, nfactors = 1, scores = TRUE))
print(principal(confusion_data_t3, nfactors = 1, scores = TRUE))
print(principal(fatigue_data_t3, nfactors = 1, scores = TRUE))
print(principal(depression_data_t3, nfactors = 1, scores = TRUE))
print(principal(anger_data_t3, nfactors = 1, scores = TRUE))

### VARIABLE CREATION ###

#the analyses confirmed a six factor model, made up of the following six factors and their sub-questions
dat$Q3.5.c.6Uncer = as.integer(dat$Q3.5.c.6Uncer)
dat$tension_t3 = rowMeans(dat[,c("Q3.5.1Panicky", "Q3.5.c.2Anx", "Q3.5.b.1Worri", "Q3.5.b.5Nerv")], na.rm = TRUE)
dat$vigour_t3 = rowMeans(dat[,c("Q3.5.2Lively", "Q3.5.b.2Energ", "Q3.5.c.1Activ", "Q3.5.c.5Alert")], na.rm = TRUE)
dat$confusion_t3 = rowMeans(dat[,c("Q3.5.3Confus", "Q3.5.a.3Mix", "Q3.5.b.4Muddl", "Q3.5.c.6Uncer")], na.rm = TRUE)
dat$fatigue_t3 = rowMeans(dat[,c("Q3.5.4Worn", "Q3.5.a.2Exhaus", "Q3.5.a.4Sleep", "Q3.5.c.4Tire")], na.rm = TRUE)
dat$depression_t3 = rowMeans(dat[,c("Q3.5.5Depres", "Q3.5.6Down", "Q3.5.a.6Unhap", "Q3.5.b.3Mis")], na.rm = TRUE)
dat$anger_t3 = rowMeans(dat[,c("Q3.5.a.1Annoy", "Q3.5.a.5Bitt", "Q3.5.b.6Angr", "Q3.5.c.3BadT")], na.rm = TRUE)

#convert NaNs to NAs
dat[is.nan(dat)] = NA

#test the data structure
t = dat[,c("GeneralIndex", "tension_t2", "tension_t3", "vigour_t3", "confusion_t3", "fatigue_t3", "depression_t3", "Q3.5.a.1Annoy", "anger_t3")]

################################################################################################################################################

### CONFIRMATORY FACTOR ANALYSIS ON THE PERCEPTIONS OF RELATIONSHIP SCALE (T2) ###

#create a data frame of the variables of interest
relationship_dat_t2 = dat[, c("Q261Close",
                              "Q262Simi",
                              "Q263Talk",
                              "Q264Enjoy",
                              "Q265Thing",
                              "Q266Valu.R.",
                              "Q266Valu",
                              "Q267Oft",
                              "Q268Like",
                              "Q26a.1Personal",
                              "Q26a.2Relat",
                              "Q26a.3Attit",
                              "Q26a.4Opin",
                              "Q26a.5Import",
                              "Q26a.6EDay",
                              "Q26a.7Simi")]

relationship_dat_t2_no_NA = relationship_dat_t2[complete.cases(relationship_dat_t2), ]

#correlation matrix of the question answers
round(cor(relationship_dat_t2_no_NA[,c("Q261Close",
                                       "Q262Simi",
                                       "Q263Talk",
                                       "Q264Enjoy",
                                       "Q265Thing",
                                       "Q266Valu.R.",
                                       "Q267Oft",
                                       "Q268Like",
                                       "Q26a.1Personal",
                                       "Q26a.2Relat",
                                       "Q26a.3Attit",
                                       "Q26a.4Opin",
                                       "Q26a.5Import",
                                       "Q26a.6EDay",
                                       "Q26a.7Simi")]), 2)

#visusal correlation matrix
library(corrplot)
corrplot(cor(relationship_dat_t2_no_NA[,c("Q261Close",
                                          "Q262Simi",
                                          "Q263Talk",
                                          "Q264Enjoy",
                                          "Q265Thing",
                                          "Q266Valu.R.",
                                          "Q266Valu",
                                          "Q267Oft",
                                          "Q268Like",
                                          "Q26a.1Personal",
                                          "Q26a.2Relat",
                                          "Q26a.3Attit",
                                          "Q26a.4Opin",
                                          "Q26a.5Import",
                                          "Q26a.6EDay",
                                          "Q26a.7Simi")]), order = "original", tl.col='black', tl.cex=.75) 

### ZERO FACTOR MODEL ###

#model with zero common factors 
relationship_0factor = ' #start of model

# latent variable definitions (common factors)
# latent variable variances
# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q261Close ~~ Q261Close
  Q262Simi ~~ Q262Simi
  Q263Talk ~~ Q263Talk
  Q264Enjoy ~~ Q264Enjoy
  Q265Thing ~~ Q265Thing
  Q266Valu.R. ~~ Q266Valu.R.
  Q267Oft ~~ Q267Oft
  Q268Like ~~ Q268Like
  Q26a.1Personal ~~ Q26a.1Personal
  Q26a.2Relat ~~ Q26a.2Relat
  Q26a.3Attit ~~ Q26a.3Attit
  Q26a.4Opin ~~ Q26a.4Opin
  Q26a.5Import ~~ Q26a.5Import
  Q26a.6EDay ~~ Q26a.6EDay
  Q26a.7Simi ~~ Q26a.7Simi
            
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q261Close ~ 1
  Q262Simi ~ 1
  Q263Talk ~ 1
  Q264Enjoy ~ 1
  Q265Thing ~ 1
  Q266Valu.R. ~ 1
  Q267Oft ~ 1
  Q268Like ~ 1
  Q26a.1Personal ~ 1
  Q26a.2Relat ~ 1
  Q26a.3Attit ~ 1
  Q26a.4Opin ~ 1
  Q26a.5Import ~ 1
  Q26a.6EDay ~ 1
  Q26a.7Simi ~ 1
' #end of model

#fit the model and evaluate it
fit0 = lavaan(relationship_0factor, data = relationship_dat_t2_no_NA, mimic = "mplus")
summary(fit0, standardized=TRUE, fit.measures=TRUE)

semPaths(fit0, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

### THREE FACTOR MODEL ###

#model with zero common factors 
relationship_3factor = ' #start of model

# latent variable definitions (common factors)
  close =~ NA*Q261Close + NA*Q268Like + NA*Q26a.1Personal + NA*Q26a.5Import + NA*Q26a.4Opin + NA*Q264Enjoy + NA*Q26a.2Relat
  similarity =~ NA*Q265Thing + NA*Q26a.3Attit + NA*Q266Valu.R. + NA*Q26a.7Simi + NA*Q262Simi
  eday_centrality =~ NA*Q267Oft + NA*Q26a.6EDay + NA*Q263Talk

# latent variable variances
  close ~~ close
  similarity ~~ similarity
  eday_centrality ~~ eday_centrality

# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q261Close ~~ Q261Close
  Q262Simi ~~ Q262Simi
  Q263Talk ~~ Q263Talk
  Q264Enjoy ~~ Q264Enjoy
  Q265Thing ~~ Q265Thing
  Q266Valu.R. ~~ Q266Valu.R.
  Q267Oft ~~ Q267Oft
  Q268Like ~~ Q268Like
  Q26a.1Personal ~~ Q26a.1Personal
  Q26a.2Relat ~~ Q26a.2Relat
  Q26a.3Attit ~~ Q26a.3Attit
  Q26a.4Opin ~~ Q26a.4Opin
  Q26a.5Import ~~ Q26a.5Import
  Q26a.6EDay ~~ Q26a.6EDay
  Q26a.7Simi ~~ Q26a.7Simi
            
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q261Close ~ 1
  Q262Simi ~ 1
  Q263Talk ~ 1
  Q264Enjoy ~ 1
  Q265Thing ~ 1
  Q266Valu.R. ~ 1
  Q267Oft ~ 1
  Q268Like ~ 1
  Q26a.1Personal ~ 1
  Q26a.2Relat ~ 1
  Q26a.3Attit ~ 1
  Q26a.4Opin ~ 1
  Q26a.5Import ~ 1
  Q26a.6EDay ~ 1
  Q26a.7Simi ~ 1
' #end of model

#fit the model and evaluate it
fit3 = lavaan(relationship_3factor, data = relationship_dat_t2_no_NA, mimic = "mplus")
summary(fit3, standardized=TRUE, fit.measures=TRUE)

semPaths(fit3, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#get the factor loadings
inspect(fit3,what="std")$lambda

#compare the fit of this model to the fit of the 0 factor model
round(cbind(m1=inspect(fit0, 'fit.measures'), m2=inspect(fit3, 'fit.measures')),3)
anova(fit0, fit3)

#Cronbach's alpha for the perceptions of relationships factor
closeness_data_t2 = na.exclude(dat[,c("Q261Close", "Q268Like", "Q26a.1Personal", "Q26a.5Import", "Q264Enjoy", "Q26a.2Relat")])
psych::alpha(closeness_data_t2) #this yeilds an acceptable (.80) Cronbach's alpha

similarity_data_t2 = na.exclude(dat[,c("Q265Thing", "Q26a.3Attit","Q26a.7Simi", "Q262Simi")])
psych::alpha(similarity_data_t2) #this yeilds an acceptable (.87) Cronbach's alpha

centrality_data_t2 = na.exclude(dat[,c("Q267Oft", "Q26a.6EDay", "Q263Talk")])
psych::alpha(centrality_data_t2) #this yeilds an acceptable (.76) Cronbach's alpha

#percetage of variance explained
PCA.closeness_t2 = principal(closeness_data_t2, nfactors = 1, scores = TRUE)
print(PCA.closeness_t2)

PCA.similarity_t2 = principal(similarity_data_t2, nfactors = 1, scores = TRUE)
print(PCA.similarity_t2)

PCA.centrality_t2 = principal(centrality_data_t2, nfactors = 1, scores = TRUE)
print(PCA.centrality_t2)

### VARIABLE CREATION ###

#the analyses confirmed a three factor model, made up of the following three factors and their sub-questions
dat$closeness_t2 = rowMeans(dat[,c("Q261Close", "Q268Like", "Q26a.1Personal", "Q26a.5Import", "Q264Enjoy", "Q26a.2Relat")], na.rm = TRUE)
dat$similarity_t2 = rowMeans(dat[,c("Q265Thing", "Q26a.3Attit", "Q26a.7Simi", "Q262Simi")], na.rm = TRUE)
dat$eday_centrality_t2 = rowMeans(dat[,c("Q267Oft", "Q26a.6EDay", "Q263Talk")], na.rm = TRUE)

#convert NaNs to NAs
dat[is.nan(dat)] = NA

#test the data structure
t = dat[,c("GeneralIndex", "Q26a.1Personal", "closeness_t2", "similarity_t2", "eday_centrality_t2", "fatigue_t3", "anger_t3")]

################################################################################################################################################

### CONFIRMATORY FACTOR ANALYSIS ON THE PERCEPTIONS OF RELATIONSHIP SCALE (T4) ###

#create a data frame of the variables of interest
relationship_dat_t4 = dat[, c("Q4.8.1Close",
                              "Q4.8.2Simi",
                              "Q4.8.3Talk",
                              "Q4.8.4Time",
                              "Q4.8.5Things",
                              "Q4.8.6ValuesR",
                              "Q4.8.7See",
                              "Q4.8.8Like",
                              "Q4.8.a.1Personal",
                              "Q4.8.a.2Relat",
                              "Q4.8.a.3Attit",
                              "Q4.8.a.4Opin",
                              "Q4.8.a.5Import",
                              "Q4.8.a.6EDay",
                              "Q4.8.a.7Vsimi")]

relationship_dat_t4_no_NA = relationship_dat_t4[complete.cases(relationship_dat_t4), ]

#correlation matrix of the question answers
round(cor(relationship_dat_t4_no_NA[,c("Q4.8.1Close",
                                       "Q4.8.2Simi",
                                       "Q4.8.3Talk",
                                       "Q4.8.4Time",
                                       "Q4.8.5Things",
                                       "Q4.8.6ValuesR",
                                       "Q4.8.7See",
                                       "Q4.8.8Like",
                                       "Q4.8.a.1Personal",
                                       "Q4.8.a.2Relat",
                                       "Q4.8.a.3Attit",
                                       "Q4.8.a.4Opin",
                                       "Q4.8.a.5Import",
                                       "Q4.8.a.6EDay",
                                       "Q4.8.a.7Vsimi")]), 2)

#visusal correlation matrix
library(corrplot)
corrplot(cor(relationship_dat_t4_no_NA[,c("Q4.8.1Close",
                                          "Q4.8.2Simi",
                                          "Q4.8.3Talk",
                                          "Q4.8.4Time",
                                          "Q4.8.5Things",
                                          "Q4.8.6ValuesR",
                                          "Q4.8.7See",
                                          "Q4.8.8Like",
                                          "Q4.8.a.1Personal",
                                          "Q4.8.a.2Relat",
                                          "Q4.8.a.3Attit",
                                          "Q4.8.a.4Opin",
                                          "Q4.8.a.5Import",
                                          "Q4.8.a.6EDay",
                                          "Q4.8.a.7Vsimi")]), order = "original", tl.col='black', tl.cex=.75) 

### ZERO FACTOR MODEL ###

#model with zero common factors 
relationship_0factor = ' #start of model

# latent variable definitions (common factors)
# latent variable variances
# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q4.8.1Close ~~ Q4.8.1Close
  Q4.8.2Simi ~~ Q4.8.2Simi
  Q4.8.3Talk ~~ Q4.8.3Talk
  Q4.8.4Time ~~ Q4.8.4Time
  Q4.8.5Things ~~ Q4.8.5Things
  Q4.8.6ValuesR ~~ Q4.8.6ValuesR
  Q4.8.7See ~~ Q4.8.7See
  Q4.8.8Like ~~ Q4.8.8Like
  Q4.8.a.1Personal ~~ Q4.8.a.1Personal
  Q4.8.a.2Relat ~~ Q4.8.a.2Relat
  Q4.8.a.3Attit ~~ Q4.8.a.3Attit
  Q4.8.a.4Opin ~~ Q4.8.a.4Opin
  Q4.8.a.5Import ~~ Q4.8.a.5Import
  Q4.8.a.6EDay ~~ Q4.8.a.6EDay
  Q4.8.a.7Vsimi ~~ Q4.8.a.7Vsimi
            
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q4.8.1Close ~ 1
  Q4.8.2Simi ~ 1
  Q4.8.3Talk ~ 1
  Q4.8.4Time ~ 1
  Q4.8.5Things ~ 1
  Q4.8.6ValuesR ~ 1
  Q4.8.7See ~ 1
  Q4.8.8Like ~ 1
  Q4.8.a.1Personal ~ 1
  Q4.8.a.2Relat ~ 1
  Q4.8.a.3Attit ~ 1
  Q4.8.a.4Opin ~ 1
  Q4.8.a.5Import ~ 1
  Q4.8.a.6EDay ~ 1
  Q4.8.a.7Vsimi ~ 1
' #end of model

#fit the model and evaluate it
fit0 = lavaan(relationship_0factor, data = relationship_dat_t4_no_NA, mimic = "mplus")
summary(fit0, standardized=TRUE, fit.measures=TRUE)

semPaths(fit0, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

### THREE FACTOR MODEL ###

#model with three common factors 
relationship_3factor = ' #start of model

# latent variable definitions (common factors)
  close =~ NA*Q4.8.1Close + NA*Q4.8.8Like + NA*Q4.8.a.1Personal + NA*Q4.8.a.5Import + NA*Q4.8.a.4Opin + NA*Q4.8.4Time + NA*Q4.8.a.2Relat
  similarity =~ NA*Q4.8.5Things + NA*Q4.8.a.3Attit + NA*Q4.8.6ValuesR + Q4.8.2Simi + NA*Q4.8.a.7Vsimi
  eday_centrality =~ NA*Q4.8.7See + NA*Q4.8.a.6EDay + NA*Q4.8.3Talk
  
# latent variable variances
  close ~~ close
  similarity ~~ similarity
  eday_centrality ~~ eday_centrality

# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q4.8.1Close ~~ Q4.8.1Close
  Q4.8.2Simi ~~ Q4.8.2Simi
  Q4.8.3Talk ~~ Q4.8.3Talk
  Q4.8.4Time ~~ Q4.8.4Time
  Q4.8.5Things ~~ Q4.8.5Things
  Q4.8.6ValuesR ~~ Q4.8.6ValuesR
  Q4.8.7See ~~ Q4.8.7See
  Q4.8.8Like ~~ Q4.8.8Like
  Q4.8.a.1Personal ~~ Q4.8.a.1Personal
  Q4.8.a.2Relat ~~ Q4.8.a.2Relat
  Q4.8.a.3Attit ~~ Q4.8.a.3Attit
  Q4.8.a.4Opin ~~ Q4.8.a.4Opin
  Q4.8.a.5Import ~~ Q4.8.a.5Import
  Q4.8.a.6EDay ~~ Q4.8.a.6EDay
  Q4.8.a.7Vsimi ~~ Q4.8.a.7Vsimi
            
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q4.8.1Close ~ 1
  Q4.8.2Simi ~ 1
  Q4.8.3Talk ~ 1
  Q4.8.4Time ~ 1
  Q4.8.5Things ~ 1
  Q4.8.6ValuesR ~ 1
  Q4.8.7See ~ 1
  Q4.8.8Like ~ 1
  Q4.8.a.1Personal ~ 1
  Q4.8.a.2Relat ~ 1
  Q4.8.a.3Attit ~ 1
  Q4.8.a.4Opin ~ 1
  Q4.8.a.5Import ~ 1
  Q4.8.a.6EDay ~ 1
  Q4.8.a.7Vsimi ~ 1
' #end of model

#fit the model and evaluate it
fit3 = lavaan(relationship_3factor, data = relationship_dat_t4_no_NA, mimic = "mplus")
summary(fit3, standardized=TRUE, fit.measures=TRUE)

semPaths(fit3, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#get the factor loadings
inspect(fit3,what="std")$lambda

#compare the fit of this model to the fit of the 0 factor model
round(cbind(m1=inspect(fit0, 'fit.measures'), m2=inspect(fit3, 'fit.measures')),3)
anova(fit0, fit3)

#Cronbach's alpha for the perceptions of relationships factor
closeness_data_t4 = na.exclude(dat[,c("Q4.8.1Close", "Q4.8.8Like", "Q4.8.a.1Personal", "Q4.8.a.5Import", "Q4.8.a.4Opin", "Q4.8.4Time", "Q4.8.a.2Relat")])
psych::alpha(closeness_data_t4) #this yeilds an acceptable (.89) Cronbach's alpha

similarity_data_t4 = na.exclude(dat[,c("Q4.8.5Things", "Q4.8.a.3Attit", "Q4.8.2Simi", "Q4.8.a.7Vsimi")])
psych::alpha(similarity_data_t4) #this yeilds an acceptable (.92) Cronbach's alpha

centrality_data_t4 = na.exclude(dat[,c("Q4.8.7See", "Q4.8.a.6EDay", "Q4.8.3Talk")])
psych::alpha(centrality_data_t4) #this yeilds an acceptable (.88) Cronbach's alpha

#percetage of variance explained
PCA.closeness_t4 = principal(closeness_data_t4, nfactors = 1, scores = TRUE)
print(PCA.closeness_t4)

PCA.similarity_t4 = principal(similarity_data_t4, nfactors = 1, scores = TRUE)
print(PCA.similarity_t4)

PCA.centrality_t4 = principal(centrality_data_t2, nfactors = 1, scores = TRUE)
print(PCA.centrality_t4)

### VARIABLE CREATION ###

#the analyses confirmed a three factor model, made up of the following three factors and their sub-questions
dat$closeness_t4 = rowMeans(dat[,c("Q4.8.1Close", "Q4.8.8Like", "Q4.8.a.1Personal", "Q4.8.a.5Import", "Q4.8.a.4Opin", "Q4.8.4Time", "Q4.8.a.2Relat")], na.rm = TRUE)
dat$similarity_t4 = rowMeans(dat[,c("Q4.8.5Things", "Q4.8.a.3Attit", "Q4.8.2Simi", "Q4.8.a.7Vsimi")], na.rm = TRUE)
dat$eday_centrality_t4 = rowMeans(dat[,c("Q4.8.7See", "Q4.8.a.6EDay", "Q4.8.3Talk")], na.rm = TRUE)

#convert NaNs to NAs
dat[is.nan(dat)] = NA

#test the data structure
t = dat[,c("GeneralIndex", "Q4.8.1Close", "closeness_t4", "similarity_t4", "eday_centrality_t4", "fatigue_t3", "anger_t3")]

################################################################################################################################################

### CONFIRMATORY FACTOR ANALYSIS ON THE WARWICK-EDINBURGH MENTAL WELLBEING SCALE (T1) ###

#create a data frame of the variables of interest
wellbeing_dat_t1 = dat[, c("Q1.3.1Optim",
                           "Q1.3.2Useful",
                           "Q1.3.3Relax",
                           "Q1.3.4Interest",
                           "Q1.3.5Energy",
                           "Q1.3.6Probl",
                           "Q1.3.7Clear",
                           "Q1.4.1Good",
                           "Q1.4.2Close",
                           "Q1.4.3Confi",
                           "Q1.4.4Mind",
                           "Q1.4.5Love",
                           "Q1.4.6NewInt",
                           "Q1.4.7Cheer")]

wellbeing_dat_t1_no_NA = wellbeing_dat_t1[complete.cases(wellbeing_dat_t1), ]

#correlation matrix of the question answers
round(cor(wellbeing_dat_t1_no_NA[,c("Q1.3.1Optim",
                                    "Q1.3.2Useful",
                                    "Q1.3.3Relax",
                                    "Q1.3.4Interest",
                                    "Q1.3.5Energy",
                                    "Q1.3.6Probl",
                                    "Q1.3.7Clear",
                                    "Q1.4.1Good",
                                    "Q1.4.2Close",
                                    "Q1.4.3Confi",
                                    "Q1.4.4Mind",
                                    "Q1.4.5Love",
                                    "Q1.4.6NewInt",
                                    "Q1.4.7Cheer")]), 2)

#visusal correlation matrix
library(corrplot)
corrplot(cor(wellbeing_dat_t1_no_NA[,c("Q1.3.1Optim",
                                       "Q1.3.2Useful",
                                       "Q1.3.3Relax",
                                       "Q1.3.4Interest",
                                       "Q1.3.5Energy",
                                       "Q1.3.6Probl",
                                       "Q1.3.7Clear",
                                       "Q1.4.1Good",
                                       "Q1.4.2Close",
                                       "Q1.4.3Confi",
                                       "Q1.4.4Mind",
                                       "Q1.4.5Love",
                                       "Q1.4.6NewInt",
                                       "Q1.4.7Cheer")]), order = "original", tl.col='black', tl.cex=.75) 

### ZERO FACTOR MODEL ###

#model with zero common factors 
wellbeing_0factor = ' #start of model

# latent variable definitions (common factors)
# latent variable variances
# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q1.3.1Optim ~~ Q1.3.1Optim
  Q1.3.2Useful ~~ Q1.3.2Useful
  Q1.3.3Relax ~~ Q1.3.3Relax
  Q1.3.4Interest ~~ Q1.3.4Interest
  Q1.3.5Energy ~~ Q1.3.5Energy
  Q1.3.6Probl ~~ Q1.3.6Probl
  Q1.3.7Clear ~~ Q1.3.7Clear
  Q1.4.1Good ~~ Q1.4.1Good
  Q1.4.2Close ~~ Q1.4.2Close
  Q1.4.3Confi ~~ Q1.4.3Confi
  Q1.4.4Mind ~~ Q1.4.4Mind
  Q1.4.5Love ~~ Q1.4.5Love
  Q1.4.6NewInt ~~ Q1.4.6NewInt
  Q1.4.7Cheer ~~ Q1.4.7Cheer
            
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q1.3.1Optim ~ 1
  Q1.3.2Useful ~ 1
  Q1.3.3Relax ~ 1
  Q1.3.4Interest ~ 1
  Q1.3.5Energy ~ 1
  Q1.3.6Probl ~ 1
  Q1.3.7Clear ~ 1
  Q1.4.1Good ~ 1
  Q1.4.2Close ~ 1
  Q1.4.3Confi ~ 1
  Q1.4.4Mind ~ 1
  Q1.4.5Love ~ 1
  Q1.4.6NewInt ~ 1
  Q1.4.7Cheer ~ 1
' #end of model

#fit the model and evaluate it
fit0 = lavaan(wellbeing_0factor, data = wellbeing_dat_t1_no_NA, mimic = "mplus")
summary(fit0, standardized=TRUE, fit.measures=TRUE)

semPaths(fit0, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

### ONE FACTOR MODEL ###

#model with one common factor
wellbeing_1factor = ' #start of model

# latent variable definitions (common factors)
  wellbeing =~ NA*Q1.3.1Optim + NA*Q1.3.2Useful + NA*Q1.3.3Relax + NA*Q1.3.4Interest + NA*Q1.3.5Energy +
  NA*Q1.3.6Probl + NA*Q1.3.7Clear + NA*Q1.4.1Good + NA*Q1.4.2Close + NA*Q1.4.3Confi + NA*Q1.4.4Mind +
  NA*Q1.4.5Love + NA*Q1.4.6NewInt + NA*Q1.4.7Cheer
  
# latent variable variances
  wellbeing ~~ wellbeing

# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q1.3.1Optim ~~ Q1.3.1Optim
  Q1.3.2Useful ~~ Q1.3.2Useful
  Q1.3.3Relax ~~ Q1.3.3Relax
  Q1.3.4Interest ~~ Q1.3.4Interest
  Q1.3.5Energy ~~ Q1.3.5Energy
  Q1.3.6Probl ~~ Q1.3.6Probl
  Q1.3.7Clear ~~ Q1.3.7Clear
  Q1.4.1Good ~~ Q1.4.1Good
  Q1.4.2Close ~~ Q1.4.2Close
  Q1.4.3Confi ~~ Q1.4.3Confi
  Q1.4.4Mind ~~ Q1.4.4Mind
  Q1.4.5Love ~~ Q1.4.5Love
  Q1.4.6NewInt ~~ Q1.4.6NewInt
  Q1.4.7Cheer ~~ Q1.4.7Cheer
            
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q1.3.1Optim ~ 1
  Q1.3.2Useful ~ 1
  Q1.3.3Relax ~ 1
  Q1.3.4Interest ~ 1
  Q1.3.5Energy ~ 1
  Q1.3.6Probl ~ 1
  Q1.3.7Clear ~ 1
  Q1.4.1Good ~ 1
  Q1.4.2Close ~ 1
  Q1.4.3Confi ~ 1
  Q1.4.4Mind ~ 1
  Q1.4.5Love ~ 1
  Q1.4.6NewInt ~ 1
  Q1.4.7Cheer ~ 1
' #end of model

#fit the model and evaluate it
fit1 = lavaan(wellbeing_1factor, data = wellbeing_dat_t1_no_NA, mimic = "mplus")
summary(fit1, standardized=TRUE, fit.measures=TRUE)

semPaths(fit1, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#get the factor loadings
inspect(fit1,what="std")$lambda

#compare the fit of this model to the fit of the zero factor model
round(cbind(m1=inspect(fit0, 'fit.measures'), m2=inspect(fit1, 'fit.measures')),3)
anova(fit0, fit1)

#Cronbach's alpha for the wellness factor
wellbeing_data_t1 = na.exclude(dat[,c("Q1.3.1Optim", "Q1.3.2Useful", "Q1.3.3Relax", "Q1.3.5Energy","Q1.3.6Probl",
                                      "Q1.3.7Clear", "Q1.4.1Good", "Q1.4.2Close", "Q1.4.3Confi", "Q1.4.4Mind", "Q1.4.7Cheer")])
psych::alpha(wellbeing_data_t1) #this yeilds an acceptable (.84) Cronbach's alpha

#percetage of variance explained
PCA.wellbeing_t1 = principal(wellbeing_data_t1, nfactors = 1, scores = TRUE)
print(PCA.wellbeing_t1)

### VARIABLE CREATION ###

#the analyses confirmed a one factor model, made up of the following sub-questions
dat$wellbeing_t1 = rowMeans(dat[,c("Q1.3.1Optim", "Q1.3.2Useful", "Q1.3.3Relax", "Q1.3.5Energy","Q1.3.6Probl",
                                   "Q1.3.7Clear", "Q1.4.1Good", "Q1.4.2Close", "Q1.4.3Confi", "Q1.4.4Mind", "Q1.4.7Cheer")], na.rm = TRUE)

#convert NaNs to NAs
dat[is.nan(dat)] = NA

#test the data structure
t = dat[,c("GeneralIndex", "Q1.3.2Useful", "wellbeing_t1", "anger_t3")]

################################################################################################################################################

### CONFIRMATORY FACTOR ANALYSIS ON THE WARWICK-EDINBURGH MENTAL WELLBEING SCALE (T4) ####

#create a data frame of the variables of interest
wellbeing_dat_t4 = dat[, c("Q4.5.1Optim",
                           "Q4.5.2Use",
                           "Q4.5.3Relax",
                           "Q4.5.4Intere", 
                           "Q4.5.5Energ",
                           "Q4.5.6Probl",
                           "Q4.5.7Clear",
                           "Q4.6.1Good",
                           "Q4.6.2Close",
                           "Q4.6.3Confi",
                           "Q4.6.4Mind",
                           "Q4.6.5Love",
                           "Q4.6.6New",
                           "Q4.6.7Cheer")]

wellbeing_dat_t4_no_NA = wellbeing_dat_t4[complete.cases(wellbeing_dat_t4), ]

#correlation matrix of the question answers
round(cor(wellbeing_dat_t4_no_NA[,c("Q4.5.1Optim",
                                    "Q4.5.2Use",
                                    "Q4.5.3Relax",
                                    "Q4.5.4Intere", 
                                    "Q4.5.5Energ",
                                    "Q4.5.6Probl",
                                    "Q4.5.7Clear",
                                    "Q4.6.1Good",
                                    "Q4.6.2Close",
                                    "Q4.6.3Confi",
                                    "Q4.6.4Mind",
                                    "Q4.6.5Love",
                                    "Q4.6.6New",
                                    "Q4.6.7Cheer")]), 2)

#visusal correlation matrix
library(corrplot)
corrplot(cor(wellbeing_dat_t4_no_NA[,c("Q4.5.1Optim",
                                       "Q4.5.2Use",
                                       "Q4.5.3Relax",
                                       "Q4.5.4Intere", 
                                       "Q4.5.5Energ",
                                       "Q4.5.6Probl",
                                       "Q4.5.7Clear",
                                       "Q4.6.1Good",
                                       "Q4.6.2Close",
                                       "Q4.6.3Confi",
                                       "Q4.6.4Mind",
                                       "Q4.6.5Love",
                                       "Q4.6.6New",
                                       "Q4.6.7Cheer")]), order = "original", tl.col='black', tl.cex=.75) 

### ZERO FACTOR MODEL ###

#model with zero common factors 
wellbeing_0factor = ' #start of model

# latent variable definitions (common factors)
# latent variable variances
# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q4.5.1Optim ~~ Q4.5.1Optim
  Q4.5.2Use ~~ Q4.5.2Use
  Q4.5.3Relax ~~ Q4.5.3Relax
  Q4.5.4Intere ~~ Q4.5.4Intere
  Q4.5.5Energ ~~ Q4.5.5Energ
  Q4.5.6Probl ~~ Q4.5.6Probl
  Q4.5.7Clear ~~ Q4.5.7Clear
  Q4.6.1Good ~~ Q4.6.1Good
  Q4.6.2Close ~~ Q4.6.2Close
  Q4.6.3Confi ~~ Q4.6.3Confi
  Q4.6.4Mind ~~ Q4.6.4Mind
  Q4.6.5Love ~~ Q4.6.5Love
  Q4.6.6New ~~ Q4.6.6New
  Q4.6.7Cheer ~~ Q4.6.7Cheer
            
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q4.5.1Optim ~ 1
  Q4.5.2Use ~ 1
  Q4.5.3Relax ~ 1
  Q4.5.4Intere ~ 1
  Q4.5.5Energ ~ 1
  Q4.5.6Probl ~ 1
  Q4.5.7Clear ~ 1
  Q4.6.1Good ~ 1
  Q4.6.2Close ~ 1
  Q4.6.3Confi ~ 1
  Q4.6.4Mind ~ 1
  Q4.6.5Love ~ 1
  Q4.6.6New ~ 1
  Q4.6.7Cheer ~ 1
' #end of model

#fit the model and evaluate it
fit0 = lavaan(wellbeing_0factor, data = wellbeing_dat_t4_no_NA, mimic = "mplus")
summary(fit0, standardized=TRUE, fit.measures=TRUE)

semPaths(fit0, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

### ONE FACTOR MODEL ###

#model with one common factor
wellbeing_1factor = ' #start of model

# latent variable definitions (common factors)
  wellbeing =~ NA*Q4.5.1Optim + NA*Q4.5.2Use + NA*Q4.5.3Relax + NA*Q4.5.4Intere + NA*Q4.5.5Energ +
  NA*Q4.5.6Probl + NA*Q4.5.7Clear + NA*Q4.6.1Good + NA*Q4.6.2Close + NA*Q4.6.3Confi + NA*Q4.6.4Mind +
  NA*Q4.6.5Love + NA*Q4.6.6New + NA*Q4.6.7Cheer
  
# latent variable variances
  wellbeing ~~ wellbeing
  
# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q4.5.1Optim ~~ Q4.5.1Optim
  Q4.5.2Use ~~ Q4.5.2Use
  Q4.5.3Relax ~~ Q4.5.3Relax
  Q4.5.4Intere ~~ Q4.5.4Intere
  Q4.5.5Energ ~~ Q4.5.5Energ
  Q4.5.6Probl ~~ Q4.5.6Probl
  Q4.5.7Clear ~~ Q4.5.7Clear
  Q4.6.1Good ~~ Q4.6.1Good
  Q4.6.2Close ~~ Q4.6.2Close
  Q4.6.3Confi ~~ Q4.6.3Confi
  Q4.6.4Mind ~~ Q4.6.4Mind
  Q4.6.5Love ~~ Q4.6.5Love
  Q4.6.6New ~~ Q4.6.6New
  Q4.6.7Cheer ~~ Q4.6.7Cheer
            
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q4.5.1Optim ~ 1
  Q4.5.2Use ~ 1
  Q4.5.3Relax ~ 1
  Q4.5.4Intere ~ 1
  Q4.5.5Energ ~ 1
  Q4.5.6Probl ~ 1
  Q4.5.7Clear ~ 1
  Q4.6.1Good ~ 1
  Q4.6.2Close ~ 1
  Q4.6.3Confi ~ 1
  Q4.6.4Mind ~ 1
  Q4.6.5Love ~ 1
  Q4.6.6New ~ 1
  Q4.6.7Cheer ~ 1
' #end of model

#fit the model and evaluate it
fit1 = lavaan(wellbeing_1factor, data = wellbeing_dat_t4_no_NA, mimic = "mplus")
summary(fit1, standardized=TRUE, fit.measures=TRUE)

semPaths(fit1, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#get the factor loadings
inspect(fit1,what="std")$lambda

#compare the fit of this model to the fit of the 0 factor model
round(cbind(m1=inspect(fit0, 'fit.measures'), m2=inspect(fit1, 'fit.measures')),3)
anova(fit0, fit1)

#Cronbach's alpha for wellness factor
wellbeing_data_t4 = na.exclude(dat[,c("Q4.5.1Optim", "Q4.5.2Use", "Q4.5.3Relax", "Q4.5.4Intere", "Q4.5.5Energ", "Q4.5.6Probl", "Q4.5.7Clear",
                                      "Q4.6.1Good", "Q4.6.2Close", "Q4.6.3Confi", "Q4.6.4Mind", "Q4.6.5Love", "Q4.6.6New", "Q4.6.7Cheer")])
psych::alpha(wellbeing_data_t4) #this yeilds an acceptable (.88) Cronbach's alpha

#percetage of variance explained
PCA.wellbeing_t4 = principal(wellbeing_data_t4, nfactors = 1, scores = TRUE)
print(PCA.wellbeing_t4)

### VARIABLE CREATION ###

#the analyses confirmed a one factor model, made up of the following sub-questions
dat$wellbeing_t4 = rowMeans(dat[,c("Q4.5.1Optim", "Q4.5.2Use", "Q4.5.3Relax", "Q4.5.4Intere", "Q4.5.5Energ", "Q4.5.6Probl", "Q4.5.7Clear",
                                   "Q4.6.1Good", "Q4.6.2Close", "Q4.6.3Confi", "Q4.6.4Mind", "Q4.6.5Love", "Q4.6.6New", "Q4.6.7Cheer")], na.rm = TRUE)

#convert NaNs to NAs
dat[is.nan(dat)] = NA

#test the data structure
t = dat[,c("GeneralIndex", "Q4.6.3Confi", "wellbeing_t4", "anger_t3")]

################################################################################################################################################

### CONFIRMATORY FACTOR ANALYSIS ON THE GROUP INTERDEPENDENCE SCALE (T2) ###

#reverse score variables
dat$Q251RelyR = 8 - dat$Q251Rely
dat$Q254CoopR = 8 - dat$Q254Coop

#create a data frame of the variables of interest
groupid_dat_t2 = dat[, c("Q251RelyR", "Q252Cont", "Q253TWork", "Q254CoopR")]
groupid_dat_t2_no_NA = groupid_dat_t2[complete.cases(groupid_dat_t2), ]

#correlation matrix of the question answers
round(cor(groupid_dat_t2_no_NA[,c("Q251RelyR", "Q252Cont", "Q253TWork", "Q254CoopR")]), 2)

#visusal correlation matrix
corrplot(cor(groupid_dat_t2_no_NA[,c("Q251RelyR", "Q252Cont", "Q253TWork", "Q254CoopR")]), order = "original", tl.col='black', tl.cex=.75) 

### ZERO FACTOR MODEL ###

#model with zero common factors 
interdependence_0factor = ' #start of model

# latent variable definitions (common factors)
# latent variable variances
# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q251RelyR ~~ Q251RelyR
  Q252Cont ~~ Q252Cont
  Q253TWork ~~ Q253TWork
  Q254CoopR ~~ Q254CoopR
            
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q251RelyR ~ 1
  Q252Cont ~ 1
  Q253TWork ~ 1
  Q254CoopR ~ 1
' #end of model

#fit the model and evaluate it
fit0 = lavaan(interdependence_0factor, data = groupid_dat_t2_no_NA, mimic = "mplus")
summary(fit0, standardized=TRUE, fit.measures=TRUE)

semPaths(fit0, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

### ONE FACTOR MODEL ###

#model with zero common factors 
interdependence_1factor = ' #start of model

# latent variable definitions (common factors)
  interdependence =~ NA*Q251RelyR + NA*Q252Cont + NA*Q253TWork + NA*Q254CoopR

# latent variable variances
  interdependence ~~ interdependence

# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q251RelyR ~~ Q251RelyR
  Q252Cont ~~ Q252Cont
  Q253TWork ~~ Q253TWork
  Q254CoopR ~~ Q254CoopR
            
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q251RelyR ~ 1
  Q252Cont ~ 1
  Q253TWork ~ 1
  Q254CoopR ~ 1
' #end of model

#fit the model and evaluate it
fit1 = lavaan(interdependence_1factor, data = groupid_dat_t2_no_NA, mimic = "mplus")
summary(fit1, standardized=TRUE, fit.measures=TRUE)

semPaths(fit1, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#get the factor loadings
inspect(fit1,what="std")$lambda

#compare the fit of this model to the fit of the zero factor model
round(cbind(m1=inspect(fit0, 'fit.measures'), m2=inspect(fit1, 'fit.measures')),3)
anova(fit0, fit1)

#Cronbach's alpha for the group interdependence factor
interdependence_data_t2 = na.exclude(dat[,c("Q252Cont", "Q253TWork", "Q254CoopR")])
psych::alpha(interdependence_data_t2) #this yeilds a rather unacceptable (.55) Cronbach's alpha

#percetage of variance explained
PCA.interdependence_t2 = principal(interdependence_data_t2, nfactors = 1, scores = TRUE)
print(PCA.interdependence_t2)

### VARIABLE CREATION ###

#the analyses confirmed a three factor model, made up of the following three factors and their sub-questions
dat$interdependence_t2 = rowMeans(dat[,c("Q252Cont", "Q253TWork", "Q254CoopR")], na.rm = TRUE)

#convert NaNs to NAs
dat[is.nan(dat)] = NA

#test the data structure
t = dat[, c("GeneralIndex", "Q252Cont", "Q253TWork", "interdependence_t2", "wellbeing_t4", "anger_t3")]

################################################################################################################################################

### CONFIRMATORY FACTOR ANALYSIS ON COLLECTIVE EFFICACY AT T2 ###

#create a data frame of the variables of interest
collective_efficacy_dat_t2 = dat[, c("Q291Able", "Q292Prep", "Q293Distr", "Q294Perf")]
collective_efficacy_dat_t2_no_NA = collective_efficacy_dat_t2[complete.cases(collective_efficacy_dat_t2), ]

#correlation matrix of the question answers
round(cor(collective_efficacy_dat_t2_no_NA[,c("Q291Able", "Q292Prep", "Q293Distr", "Q294Perf")]), 2)

#visusal correlation matrix
corrplot(cor(collective_efficacy_dat_t2_no_NA[,c("Q291Able", "Q292Prep", "Q293Distr", "Q294Perf")]), order = "original", tl.col='black', tl.cex=.75) 

### ZERO FACTOR MODEL ###

#model with zero common factors 
collective_efficacy_0factor = ' #start of model

# latent variable definitions (common factors)
# latent variable variances
# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q291Able ~~ Q291Able
  Q292Prep ~~ Q292Prep
  Q293Distr ~~ Q293Distr
  Q294Perf ~~ Q294Perf
            
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q291Able ~ 1
  Q292Prep ~ 1
  Q293Distr ~ 1
  Q294Perf ~ 1
' #end of model

#fit the model and evaluate it
fit0 = lavaan(collective_efficacy_0factor, data = collective_efficacy_dat_t2_no_NA, mimic = "mplus")
summary(fit0, standardized=TRUE, fit.measures=TRUE)

semPaths(fit0, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

### ONE FACTOR MODEL ###

#model with one common factor 
collective_efficacy_1factor = ' #start of model

# latent variable definitions (common factors)
  collective_efficacy =~ NA*Q291Able + NA*Q292Prep + NA*Q293Distr + NA*Q294Perf

# latent variable variances
  collective_efficacy ~~ collective_efficacy

# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q291Able ~~ Q291Able
  Q292Prep ~~ Q292Prep
  Q293Distr ~~ Q293Distr
  Q294Perf ~~ Q294Perf
            
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q291Able ~ 1
  Q292Prep ~ 1
  Q293Distr ~ 1
  Q294Perf ~ 1
' #end of model

#fit the model and evaluate it
fit1 = lavaan(collective_efficacy_1factor, data = collective_efficacy_dat_t2_no_NA, mimic = "mplus")
summary(fit1, standardized=TRUE, fit.measures=TRUE)

semPaths(fit1, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#get the factor loadings
inspect(fit1,what="std")$lambda

#compare the fit of this model to the fit of the zero factor model
round(cbind(m1=inspect(fit0, 'fit.measures'), m2=inspect(fit1, 'fit.measures')),3)
anova(fit0, fit1)

#the analyses confirmed a one factor model, made up of the following four questions
dat$collective_efficacy_t2 = rowMeans(dat[, c("Q291Able", "Q292Prep", "Q293Distr", "Q294Perf")], na.rm = TRUE)

#Cronbach's alpha for the collective efficacy factor
collective_efficacy_t2 = na.exclude(dat[,c("Q291Able", "Q292Prep", "Q293Distr", "Q294Perf")])
psych::alpha(collective_efficacy_t2) #this yeilds an acceptable (.79) Cronbach's alpha

#percentage of variance explained
PCA.collective_efficacy_t2 = principal(collective_efficacy_t2, nfactors = 1, scores = TRUE)
print(PCA.collective_efficacy_t2)

#convert NaNs to NAs
dat[is.na(dat)] = NA

#test the data structure
t = dat[, c("GeneralIndex","Q291Able", "Q292Prep", "Q293Distr", "Q294Perf", "collective_efficacy_t2", "anger_t3")]

################################################################################################################################################

### CONFIRMATORY FACTOR ANALYSIS ON THE PERCEIVED AVAILABLE SUPPORT IN SPORTS SCALE AT T2 ###

#create a data frame of the variables of interest
perceived_support_t2 = dat[c("Q2.10.1Comf", "Q2.10.2Posi", "Q2.10.3Confi", "Q2.10.4Be", 
                             "Q2.10.a.1Care", "Q2.10.a.2Boost", "Q2.10.a.3Concern", "Q2.10.a.4SEst")]
perceived_support_t2_no_NA = perceived_support_t2[complete.cases(perceived_support_t2), ]

#correlation matrix of the question answers
round(cor(perceived_support_t2_no_NA[c("Q2.10.1Comf", "Q2.10.2Posi", "Q2.10.3Confi", "Q2.10.4Be",
                                       "Q2.10.a.1Care", "Q2.10.a.2Boost", "Q2.10.a.3Concern", "Q2.10.a.4SEst")]), 2)

#visusal correlation matrix
corrplot(cor(perceived_support_t2_no_NA[c("Q2.10.1Comf", "Q2.10.2Posi", "Q2.10.3Confi", "Q2.10.4Be",
                                          "Q2.10.a.1Care", "Q2.10.a.2Boost", "Q2.10.a.3Concern", "Q2.10.a.4SEst")]), order = "original", tl.col='black', tl.cex=.75) 

### ZERO FACTOR MODEL ###

#model with zero common factors 
perceived_support_0factor = ' #start of model

# latent variable definitions (common factors)
# latent variable variances
# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q2.10.1Comf ~~ Q2.10.1Comf
  Q2.10.2Posi ~~ Q2.10.2Posi
  Q2.10.3Confi ~~ Q2.10.3Confi
  Q2.10.4Be ~~ Q2.10.4Be
  Q2.10.a.1Care ~~ Q2.10.a.1Care
  Q2.10.a.2Boost ~~ Q2.10.a.2Boost
  Q2.10.a.3Concern ~~ Q2.10.a.3Concern
  Q2.10.a.4SEst ~~ Q2.10.a.4SEst
            
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q2.10.1Comf ~ 1
  Q2.10.2Posi ~ 1
  Q2.10.3Confi ~ 1
  Q2.10.4Be ~ 1
  Q2.10.a.1Care ~ 1
  Q2.10.a.2Boost ~ 1
  Q2.10.a.3Concern ~ 1
  Q2.10.a.4SEst ~ 1
' #end of model

#fit the model and evaluate it
fit0 = lavaan(perceived_support_0factor, data = perceived_support_t2_no_NA, mimic = "mplus")
summary(fit0, standardized=TRUE, fit.measures=TRUE)

semPaths(fit0, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

### ONE FACTOR MODEL ###

#model with one common factor
perceived_support_1factor = ' #start of model

# latent variable definitions (common factors)
  support =~ NA*Q2.10.1Comf + NA*Q2.10.4Be + NA*Q2.10.a.1Care + NA*Q2.10.a.3Concern + NA*Q2.10.2Posi + NA*Q2.10.a.4SEst + NA*Q2.10.3Confi + NA*Q2.10.a.2Boost

# latent variable variances
  support ~~ support

# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q2.10.1Comf ~~ Q2.10.1Comf
  Q2.10.2Posi ~~ Q2.10.2Posi
  Q2.10.3Confi ~~ Q2.10.3Confi
  Q2.10.4Be ~~ Q2.10.4Be
  Q2.10.a.1Care ~~ Q2.10.a.1Care
  Q2.10.a.2Boost ~~ Q2.10.a.2Boost
  Q2.10.a.3Concern ~~ Q2.10.a.3Concern
  Q2.10.a.4SEst ~~ Q2.10.a.4SEst
  
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q2.10.1Comf ~ 1
  Q2.10.2Posi ~ 1
  Q2.10.3Confi ~ 1
  Q2.10.4Be ~ 1
  Q2.10.a.1Care ~ 1
  Q2.10.a.2Boost ~ 1
  Q2.10.a.3Concern ~ 1
  Q2.10.a.4SEst ~ 1
' #end of model

#fit the model and evaluate it
fit1 = lavaan(perceived_support_1factor, data = perceived_support_t2_no_NA, mimic = "mplus")
summary(fit1, standardized=TRUE, fit.measures=TRUE)

semPaths(fit1, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#get the factor loadings
inspect(fit1,what="std")$lambda

#compare the fit of this model to the fit of the zero factor model
round(cbind(m0=inspect(fit0, 'fit.measures'), m1=inspect(fit1, 'fit.measures')),3)
anova(fit0, fit1)

### TWO FACTOR MODEL ###

#model with two common factors 
perceived_support_2factor = ' #start of model

# latent variable definitions (common factors)
  emotional_support =~ NA*Q2.10.1Comf + NA*Q2.10.4Be + NA*Q2.10.a.1Care + NA*Q2.10.a.3Concern
  esteem_support =~ NA*Q2.10.2Posi + NA*Q2.10.a.4SEst + NA*Q2.10.3Confi + NA*Q2.10.a.2Boost

# latent variable variances
  emotional_support ~~ emotional_support
  esteem_support ~~ esteem_support

# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q2.10.1Comf ~~ Q2.10.1Comf
  Q2.10.2Posi ~~ Q2.10.2Posi
  Q2.10.3Confi ~~ Q2.10.3Confi
  Q2.10.4Be ~~ Q2.10.4Be
  Q2.10.a.1Care ~~ Q2.10.a.1Care
  Q2.10.a.2Boost ~~ Q2.10.a.2Boost
  Q2.10.a.3Concern ~~ Q2.10.a.3Concern
  Q2.10.a.4SEst ~~ Q2.10.a.4SEst
  
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q2.10.1Comf ~ 1
  Q2.10.2Posi ~ 1
  Q2.10.3Confi ~ 1
  Q2.10.4Be ~ 1
  Q2.10.a.1Care ~ 1
  Q2.10.a.2Boost ~ 1
  Q2.10.a.3Concern ~ 1
  Q2.10.a.4SEst ~ 1
' #end of model

#fit the model and evaluate it
fit2 = lavaan(perceived_support_2factor, data = perceived_support_t2_no_NA, mimic = "mplus")
summary(fit2, standardized=TRUE, fit.measures=TRUE)

semPaths(fit2, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#get the factor loadings
inspect(fit2,what="std")$lambda

#compare the fit of this model to the fit of the zero and one factor model
round(cbind(m0=inspect(fit0, 'fit.measures'), m1=inspect(fit1, 'fit.measures'), m2=inspect(fit2, 'fit.measures')),3)
anova(fit0, fit2)
anova(fit2, fit1)
anova(fit0, fit1)

#the analyses confirmed a two-factor model, made up of the following to factors and their sub-questions
dat$perceived_support_t2_emotional = rowMeans(dat[,c("Q2.10.1Comf", "Q2.10.a.3Concern", "Q2.10.a.1Care", "Q2.10.4Be")], na.rm = TRUE)
dat$perceived_support_t2_esteem =  rowMeans(dat[,c("Q2.10.2Posi", "Q2.10.3Confi", "Q2.10.a.2Boost", "Q2.10.a.4SEst")], na.rm = TRUE)

#Cronbach's alpha for the emotional and esteem support factors
emotional_data_t2 = na.exclude(dat[,c("Q2.10.1Comf", "Q2.10.a.3Concern", "Q2.10.a.1Care", "Q2.10.4Be")])
psych::alpha(emotional_data_t2) #this yeilds an acceptable (.87) Cronbach's alpha

esteem_data_t2 = na.exclude(dat[,c("Q2.10.2Posi", "Q2.10.3Confi", "Q2.10.a.2Boost", "Q2.10.a.4SEst")])
psych::alpha(esteem_data_t2) #this yeilds an acceptable (.88) Cronbach's alpha

#percetage of variance explained
PCA.emotional_t2 = principal(emotional_data_t2, nfactors = 1, scores = TRUE)
print(PCA.emotional_t2)

PCA.esteem_t2 = principal(esteem_data_t2, nfactors = 1, scores = TRUE)
print(PCA.esteem_t2)

#convert NaNs to NAs
dat[is.na(dat)] = NA

#test the data structure
t = dat[, c("GeneralIndex", "collective_efficacy_t2", "Q2.10.a.1Care", "perceived_support_t2_emotional", "Q2.10.a.2Boost", "perceived_support_t2_esteem", "anger_t3")]

################################################################################################################################################

### CONFIRMATORY FACTOR ANALYSIS ON THE PERCEIVED AVAILABLE SUPPORT IN SPORTS SCALE AT T3 ###

#create a data frame of the variables of interest
received_support_t3 = dat[c("Q3.10.1Cheer", "Q3.10.2Able", "Q3.10.3.ves", "Q3.10.4Listen", "Q3.10.5Comfort",
                            "Q3.10.a.1Concern", "Q3.10.a.2Can", "Q3.10.a.3Encourage", "Q3.10.a.4Confi", "Q3.10.a.5Be")]

received_support_t3_no_NA = received_support_t3[complete.cases(received_support_t3), ]

#correlation matrix of the question answers
round(cor(received_support_t3_no_NA[c("Q3.10.1Cheer", "Q3.10.2Able", "Q3.10.3.ves", "Q3.10.4Listen", "Q3.10.5Comfort",
                                      "Q3.10.a.1Concern", "Q3.10.a.2Can", "Q3.10.a.3Encourage", "Q3.10.a.4Confi", "Q3.10.a.5Be")]), 2)

#visusal correlation matrix
corrplot(cor(received_support_t3_no_NA[c("Q3.10.1Cheer", "Q3.10.2Able", "Q3.10.3.ves", "Q3.10.4Listen", "Q3.10.5Comfort",
                                         "Q3.10.a.1Concern", "Q3.10.a.2Can", "Q3.10.a.3Encourage", "Q3.10.a.4Confi", "Q3.10.a.5Be")]), order = "original", tl.col='black', tl.cex=.75) 

### ZERO FACTOR MODEL ###

#model with zero common factors 
received_support_0factor = ' #start of model

# latent variable definitions (common factors)
# latent variable variances
# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q3.10.1Cheer ~~ Q3.10.1Cheer
  Q3.10.2Able ~~ Q3.10.2Able
  Q3.10.3.ves ~~ Q3.10.3.ves
  Q3.10.4Listen ~~ Q3.10.4Listen
  Q3.10.5Comfort ~~ Q3.10.5Comfort
  Q3.10.a.1Concern ~~ Q3.10.a.1Concern
  Q3.10.a.2Can ~~ Q3.10.a.2Can
  Q3.10.a.3Encourage ~~ Q3.10.a.3Encourage
  Q3.10.a.4Confi ~~ Q3.10.a.4Confi
  Q3.10.a.5Be ~~ Q3.10.a.5Be

# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q3.10.1Cheer ~ 1
  Q3.10.2Able ~ 1
  Q3.10.3.ves ~ 1
  Q3.10.4Listen ~ 1
  Q3.10.5Comfort ~ 1
  Q3.10.a.1Concern ~ 1
  Q3.10.a.2Can ~ 1
  Q3.10.a.3Encourage ~ 1
  Q3.10.a.4Confi ~ 1
  Q3.10.a.5Be ~ 1
' #end of model

#fit the model and evaluate it
fit0 = lavaan(received_support_0factor, data = received_support_t3_no_NA, mimic = "mplus")
summary(fit0, standardized=TRUE, fit.measures=TRUE)

semPaths(fit0, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

### ONE FACTOR MODEL ###

#model with one common factors
received_support_1factor = ' #start of model

# latent variable definitions (common factors)
  support =~ NA*Q3.10.1Cheer + NA*Q3.10.2Able + NA*Q3.10.3.ves + NA*Q3.10.4Listen + NA*Q3.10.5Comfort + NA*Q3.10.a.1Concern + NA*Q3.10.a.2Can + NA*Q3.10.a.3Encourage + NA*Q3.10.a.4Confi + NA*Q3.10.a.5Be

# latent variable variances
  support ~~ support

# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q3.10.1Cheer ~~ Q3.10.1Cheer
  Q3.10.2Able ~~ Q3.10.2Able
  Q3.10.3.ves ~~ Q3.10.3.ves
  Q3.10.4Listen ~~ Q3.10.4Listen
  Q3.10.5Comfort ~~ Q3.10.5Comfort
  Q3.10.a.1Concern ~~ Q3.10.a.1Concern
  Q3.10.a.2Can ~~ Q3.10.a.2Can
  Q3.10.a.3Encourage ~~ Q3.10.a.3Encourage
  Q3.10.a.4Confi ~~ Q3.10.a.4Confi
  Q3.10.a.5Be ~~ Q3.10.a.5Be
  
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q3.10.1Cheer ~ 1
  Q3.10.2Able ~ 1
  Q3.10.3.ves ~ 1
  Q3.10.4Listen ~ 1
  Q3.10.5Comfort ~ 1
  Q3.10.a.1Concern ~ 1
  Q3.10.a.2Can ~ 1
  Q3.10.a.3Encourage ~ 1
  Q3.10.a.4Confi ~ 1
  Q3.10.a.5Be ~ 1
' #end of model

#fit the model and evaluate it
fit1 = lavaan(received_support_1factor, data = received_support_t3_no_NA, mimic = "mplus")
summary(fit1, standardized=TRUE, fit.measures=TRUE)

semPaths(fit1, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#get the factor loadings
inspect(fit1,what="std")$lambda

#compare the fit of this model to the fit of the zero factor model
round(cbind(m0=inspect(fit0, 'fit.measures'), m1=inspect(fit1, 'fit.measures')),3)
anova(fit0, fit1)

### TWO FACTOR MODEL ###

#model with two common factors 
received_support_2factor = ' #start of model

# latent variable definitions (common factors)
  emotional_support =~ NA*Q3.10.1Cheer + NA*Q3.10.4Listen + NA*Q3.10.a.1Concern + NA*Q3.10.a.5Be + NA*Q3.10.5Comfort
  esteem_support =~ NA*Q3.10.2Able + NA*Q3.10.3.ves + NA*Q3.10.a.2Can + NA*Q3.10.a.3Encourage + NA*Q3.10.a.4Confi

# latent variable variances
  emotional_support ~~ emotional_support
  esteem_support ~~ esteem_support

# latent variable covariances
# latent variable means

# manifest variable variances (uniquenesses)
  Q3.10.1Cheer ~~ Q3.10.1Cheer
  Q3.10.2Able ~~ Q3.10.2Able
  Q3.10.3.ves ~~ Q3.10.3.ves
  Q3.10.4Listen ~~ Q3.10.4Listen
  Q3.10.5Comfort ~~ Q3.10.5Comfort
  Q3.10.a.1Concern ~~ Q3.10.a.1Concern
  Q3.10.a.2Can ~~ Q3.10.a.2Can
  Q3.10.a.3Encourage ~~ Q3.10.a.3Encourage
  Q3.10.a.4Confi ~~ Q3.10.a.4Confi
  Q3.10.a.5Be ~~ Q3.10.a.5Be
  
# manifest variable covariances (uniquenesses)

#manifest variable means 
  Q3.10.1Cheer ~ 1
  Q3.10.2Able ~ 1
  Q3.10.3.ves ~ 1
  Q3.10.4Listen ~ 1
  Q3.10.5Comfort ~ 1
  Q3.10.a.1Concern ~ 1
  Q3.10.a.2Can ~ 1
  Q3.10.a.3Encourage ~ 1
  Q3.10.a.4Confi ~ 1
  Q3.10.a.5Be ~ 1
' #end of model

#fit the model and evaluate it
fit2 = lavaan(received_support_2factor, data = received_support_t3_no_NA, mimic = "mplus")
summary(fit2, standardized=TRUE, fit.measures=TRUE)

semPaths(fit2, what="std", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#get the factor loadings
inspect(fit2,what="std")$lambda

#compare the fit of this model to the fit of the zero and one factor model
round(cbind(m0=inspect(fit0, 'fit.measures'), m1=inspect(fit1, 'fit.measures'), m2=inspect(fit2, 'fit.measures')),3)
anova(fit0, fit2)
anova(fit1, fit2)
anova(fit0, fit1)

#the analyses confirmed a two factor model, made up of the following factors and their sub-questions
dat$received_support_t3_emotional = rowMeans(dat[,c("Q3.10.1Cheer", "Q3.10.4Listen", "Q3.10.a.1Concern", "Q3.10.a.5Be",  "Q3.10.5Comfort")], na.rm = TRUE)
dat$received_support_t3_esteem = rowMeans(dat[,c("Q3.10.2Able", "Q3.10.3.ves", "Q3.10.a.2Can", "Q3.10.a.3Encourage", "Q3.10.a.4Confi")], na.rm = TRUE)

#Cronbach's alpha for the emotional and esteem support factors
emotional_data_t3 = na.exclude(dat[,c("Q3.10.1Cheer", "Q3.10.4Listen", "Q3.10.a.1Concern", "Q3.10.a.5Be",  "Q3.10.5Comfort")])
psych::alpha(emotional_data_t3) #this yeilds an acceptable (.84) Cronbach's alpha

esteem_data_t3 = na.exclude(dat[,c("Q3.10.2Able", "Q3.10.3.ves","Q3.10.a.2Can", "Q3.10.a.3Encourage", "Q3.10.a.4Confi")])
psych::alpha(esteem_data_t3) #this yeilds an acceptable (.88) Cronbach's alpha

#percetage of variance explained
PCA.emotional_t3 = principal(emotional_data_t3, nfactors = 1, scores = TRUE)
print(PCA.emotional_t3 )

PCA.esteem_t3 = principal(esteem_data_t3, nfactors = 1, scores = TRUE)
print(PCA.esteem_t3)

#convert NaNs to NAs
dat[is.na(dat)] = NA

#test the data structure
t = dat[, c("GeneralIndex", "collective_efficacy_t2", "Q3.10.a.5Be", "received_support_t3_emotional", "Q3.10.a.2Can", "received_support_t3_esteem", "anger_t3")]

################################################################################################################################################

### PRINCIPAL COMPONENT ANALYSIS ON BONDING AT T2 (bonding_t2) ###

library("corpcor")
library("GPArotation")
library("psych")

#create a data frame of the variables of interest
bonding_dat_t2 = dat[, c("Q2.4Circles", "Q271Conn", "Q272Bond", "Q273Commi", "GeneralIndex")]
bonding_dat_t2 = na.omit(bonding_dat_t2)

#create a matrix
bonding.matrix = cor(bonding_dat_t2[, -which(names(bonding_dat_t2) %in% c("GeneralIndex"))])

#Bartlett's test to see if PCA is appropriate (see Field et al., 2014; p. 775)
cortest.bartlett(bonding_dat_t2[, -which(names(bonding_dat_t2) %in% c("GeneralIndex"))]) #test is significant, indicatig that a PCA is appropriate

#do a KMO (see Field et al., 2014; p. 776)
KMO(bonding_dat_t2[, -which(names(bonding_dat_t2) %in% c("GeneralIndex"))]) #values between .7 and .9 are "good" to "great"

#determinant of correlation matrix (see Field et al., 2014; p. 777)
det(bonding.matrix) #this is not problematic (since it is greater than .00001)

#the PCA: analyses suggest extracting either one or two factors (everything and then fusion on its own)
PCA.bonding_t2 = principal(bonding_dat_t2[, -which(names(bonding_dat_t2) %in% c("GeneralIndex"))], nfactors = 4, scores = TRUE)
plot(PCA.bonding_t2$values, type = "b") #scree plot has in inflection at 2, so keep one factor

PCA.bonding.one.factor_t2 =principal(bonding_dat_t2[, -which(names(bonding_dat_t2) %in% c("GeneralIndex"))], nfactors = 1, rotate = "oblimin", scores = TRUE)
print.psych(PCA.bonding.one.factor_t2, cut = 0.3, sort = TRUE) 
summary(prcomp(bonding_dat_t2[, -which(names(bonding_dat_t2) %in% c("GeneralIndex"))], scale = TRUE)) #this gives a more accurate proportion of variance explained by the first component (PC1)

#Cronbach's alpha for all the factors together
psych::alpha(bonding_dat_t2[, -which(names(bonding_dat_t2) %in% c("GeneralIndex"))]) #this yeilds an acceptable (.78) Cronbach's alpha

#Cronbach's alpha for the three non-fusion variables
psych::alpha(bonding_dat_t2[c("Q271Conn", "Q272Bond", "Q273Commi")]) #the Cronbach's alpha (.83) is relatively unchanged

#since there is little difference in the Cronbach's alphas for the factors with three and four variables, all four are included
bonding_dat_t2$bonding_t2 = PCA.bonding.one.factor_t2$scores[,1]

#add scores to the main dataset
library(plyr)
dat = transform(merge(dat, bonding_dat_t2[,c("bonding_t2", "GeneralIndex")], by = "GeneralIndex", all = T))
dat = dat[ order(as.numeric(dat$GeneralIndex)),]

#test the data structure
t = dat[, c("GeneralIndex", "Q2.4Circles", "Q271Conn", "Q272Bond", "Q273Commi", "Q3.7Circles", "bonding_t2", "anger_t3")]

################################################################################################################################################

### PRINCIPAL COMPONENT ANALYSIS ON BONDING AT T3 (bonding_t3) ###

#create a data frame of the variables of interest
bonding_dat_t3 = dat[c("Q3.7Circles", "Q3.8.1Connect", "Q3.8.2Bond", "Q3.8.3.Commit", "GeneralIndex")]
bonding_dat_t3 = na.omit(bonding_dat_t3)

#create a matrix
bonding.matrix = cor(bonding_dat_t3[, -which(names(bonding_dat_t3) %in% c("GeneralIndex"))])

#Bartlett's test to see if PCA is appropriate (see Field et al., 2014; p. 775)
cortest.bartlett(bonding_dat_t3[, -which(names(bonding_dat_t3) %in% c("GeneralIndex"))]) #test is significant, indicatig that a PCA is appropriate

#do a KMO (see Field et al., 2014; p. 776)
KMO(bonding_dat_t3[, -which(names(bonding_dat_t3) %in% c("GeneralIndex"))]) #values above .7 are "good"

#determinant of correlation matrix (see Field et al., 2014; p. 777)
det(bonding.matrix) #this is not problematic (since it is greater than .00001)

#the PCA: analyses suggest extracting either one or two factors (everything and then fusion on its own)
PCA.bonding_t3 = principal(bonding_dat_t3[, -which(names(bonding_dat_t3) %in% c("GeneralIndex"))], nfactors = 4, scores = TRUE)
plot(PCA.bonding_t3$values, type = "b") #scree plot has in inflection at 2 and 3, so keep one factor or two factors

PCA.bonding.two.factors_t3 =principal(bonding_dat_t3[, -which(names(bonding_dat_t3) %in% c("GeneralIndex"))], nfactors = 2, rotate = "oblimin", scores = TRUE)
print.psych(PCA.bonding.two.factors_t3, cut = 0.3, sort = TRUE)

PCA.bonding.one.factor_t3 =principal(bonding_dat_t3[, -which(names(bonding_dat_t3) %in% c("GeneralIndex"))], nfactors = 1, rotate = "oblimin", scores = TRUE)
print.psych(PCA.bonding.one.factor_t3, cut = 0.3, sort = TRUE) 
summary(prcomp(bonding_dat_t3[, -which(names(bonding_dat_t3) %in% c("GeneralIndex"))], scale = TRUE)) #this gives a more accurate proportion of variance explained by the first component (PC1)

#Cronbach's alpha for the all factors together
psych::alpha(bonding_dat_t3[, -which(names(bonding_dat_t3) %in% c("GeneralIndex"))]) #this yeilds an acceptable (.84) Cronbach's alpha

#Cronbach's alpha for the three non-fusion variables
psych::alpha(bonding_dat_t3[, -which(names(bonding_dat_t3) %in% c("GeneralIndex"))][c("Q3.8.1Connect", "Q3.8.2Bond", "Q3.8.3.Commit")]) #the Cronbach's alpha (.89) is relatively unchanged

#since there is little difference in the Cronbachs alphas for the components with three and four variables, all four are included
bonding_dat_t3$bonding_t3 = PCA.bonding.one.factor_t3$scores[,1]

#add scores to the main dataset
library(plyr) 
dat = transform(merge(dat, bonding_dat_t3[,c("bonding_t3", "GeneralIndex")], by = "GeneralIndex", all = T))

#test the data structure
t = dat[,c("GeneralIndex", "Q2.4Circles", "Q3.7Circles", "Q3.8.1Connect", "Q3.8.2Bond", "Q3.8.3.Commit", "bonding_t3", "bonding_t2", "GeneralIndex")]

################################################################################################################################################

### PRINCIPAL COMPONENT ANALYSIS ON BONDING AT T4 (bonding_t4) ###

#create a data frame of the variables of interest
bonding_dat_t4 = dat[c("Q4.7Circles", "Q4.9.1Connect", "Q4.9.2Bond", "Q4.9.3Commit", "GeneralIndex")]
bonding_dat_t4 = na.omit(bonding_dat_t4)

#create a matrix
bonding.matrix = cor(bonding_dat_t4[, -which(names(bonding_dat_t4) %in% c("GeneralIndex"))])

#Bartlett's test to see if PCA is appropriate (see Field et al., 2014; p. 775)
cortest.bartlett(bonding_dat_t4[, -which(names(bonding_dat_t4) %in% c("GeneralIndex"))]) #test is significant, indicatig that a PCA is appropriate

#do a KMO (see Field et al., 2014; p. 776)
KMO(bonding_dat_t4[, -which(names(bonding_dat_t4) %in% c("GeneralIndex"))]) #values above .7 are "good"

#determinant of correlation matrix (see Field et al., 2014; p. 777)
det(bonding.matrix) #this is not problematic (since it is greater than .00001)

#the PCA: analyses suggest extracting either one or two factors (everything and then fusion on its own)
PCA.bonding_t4 = principal(bonding_dat_t4[, -which(names(bonding_dat_t4) %in% c("GeneralIndex"))], nfactors = 4, scores = TRUE)
plot(PCA.bonding_t4$values, type = "b") #scree plot has in inflection at 2 and 3, so keep one factor or two factors

PCA.bonding.two.factors_t4 =principal(bonding_dat_t4[, -which(names(bonding_dat_t4) %in% c("GeneralIndex"))], nfactors = 2, rotate = "oblimin", scores = TRUE)
print.psych(PCA.bonding.two.factors_t3, cut = 0.3, sort = TRUE)

PCA.bonding.one.factor_t4 =principal(bonding_dat_t4[, -which(names(bonding_dat_t4) %in% c("GeneralIndex"))], nfactors = 1, rotate = "oblimin", scores = TRUE)
print.psych(PCA.bonding.one.factor_t4, cut = 0.3, sort = TRUE) 
summary(prcomp(bonding_dat_t4[, -which(names(bonding_dat_t4) %in% c("GeneralIndex"))], scale = TRUE)) #this gives a more accurate proportion of variance explained by the first component (PC1)

#Cronbach's alpha for the all factors together
psych::alpha(bonding_dat_t4[, -which(names(bonding_dat_t4) %in% c("GeneralIndex"))]) #this yeilds an acceptable (.87) Cronbach's alpha

#Cronbach's alpha for the three non-fusion variables
psych::alpha(bonding_dat_t4[c("Q4.9.1Connect", "Q4.9.2Bond", "Q4.9.3Commit")]) #the Cronbach's alpha (.92) is relatively unchanged

#since there is little difference in the Cronbachs alphas for the components with three and four variables, all four are included
bonding_dat_t4$bonding_t4 = PCA.bonding.one.factor_t4$scores[,1]

#add scores to the main dataset
library(plyr) 
dat = transform(merge(dat, bonding_dat_t4[,c("bonding_t4", "GeneralIndex")], by = "GeneralIndex", all = T))

#test the data structure
t = dat[,c("GeneralIndex", "Q2.4Circles", "Q271Conn", "Q272Bond", "Q273Commi",
           "Q3.7Circles", "Q3.8.1Connect", "Q3.8.2Bond", "Q3.8.3.Commit",
           "Q4.7Circles", "Q4.9.1Connect", "Q4.9.2Bond", "Q4.9.3Commit",
           "bonding_t2", "bonding_t3", "bonding_t4")]

################################################################################################################################################

### PRINCIPAL COMPONENT ANALYSIS ON PHYSICAL DISCOMFORT AT T3 (physical_discomfort_t3) ###

#create a data frame of the variables of interest
phys_diff_dat_t3 = dat[c("Q3.6.1Pain", "Q3.6.2Fatigue", "GeneralIndex")]
phys_diff_dat_t3 = na.omit(phys_diff_dat_t3)

#create a matrix
phys_diff.matrix = cor(phys_diff_dat_t3[, -which(names(phys_diff_dat_t3) %in% c("GeneralIndex"))])

#Bartlett's test to see if PCA is appropriate (see Field et al., 2014; p. 775)
cortest.bartlett(phys_diff_dat_t3[, -which(names(phys_diff_dat_t3) %in% c("GeneralIndex"))]) #test is significant, indicatig that a PCA is appropriate

#do a KMO (see Field et al., 2014; p. 776)
KMO(phys_diff_dat_t3[, -which(names(phys_diff_dat_t3) %in% c("GeneralIndex"))]) 

#determinant of correlation matrix (see Field et al., 2014; p. 777)
det(phys_diff.matrix) #this is not problematic (since it is greater than .00001)

#the PCA: analyses suggest extracting either one or two factors (everything and then fusion on its own)
PCA.phys_diff.one.factor_t3 =principal(phys_diff_dat_t3[, -which(names(phys_diff_dat_t3) %in% c("GeneralIndex"))], nfactors = 1, rotate = "oblimin", scores = TRUE)
print.psych(PCA.phys_diff.one.factor_t3, cut = 0.3, sort = TRUE) 
summary(prcomp(phys_diff_dat_t3[, -which(names(phys_diff_dat_t3) %in% c("GeneralIndex"))], scale = TRUE))

#get the correlation between the two variables
cor(phys_diff_dat_t3$Q3.6.1Pain, phys_diff_dat_t3$Q3.6.2Fatigue) #this is rather weak

#add all three components to the dataset
phys_diff_dat_t3$phys_diff_t3 = PCA.phys_diff.one.factor_t3$scores[,1]
dat = transform(merge(dat, phys_diff_dat_t3[,c("phys_diff_t3", "GeneralIndex")], by = "GeneralIndex", all = T))

#test the data structure 
t = dat[,c("GeneralIndex", "Q4.7Circles", "Q4.9.1Connect", "Q4.9.2Bond", "Q4.9.3Commit",
           "Q3.6.1Pain", "Q3.6.2Fatigue", "phys_diff_t3")]

################################################################################################################################################

### PRINCIPAL COMPONENT ANALYSIS ON PERFORMANCE SATISFACTION AT T3 (peform_satisfaction_t3) ###

#create a data frame of the variables of interest
peform_satis_dat_t3 = dat[, c("Q3.11.1Indiv", "Q3.11.2Team", "GeneralIndex")]
peform_satis_dat_t3 = na.omit(peform_satis_dat_t3)

#create a matrix
peform_satis.matrix = cor(peform_satis_dat_t3[, -which(names(peform_satis_dat_t3) %in% c("GeneralIndex"))])

#Bartlett's test to see if PCA is appropriate (see Field et al., 2014; p. 775)
cortest.bartlett(peform_satis_dat_t3[, -which(names(peform_satis_dat_t3) %in% c("GeneralIndex"))]) #test is significant, indicatig that a PCA is appropriate

#do a KMO (see Field et al., 2014; p. 776)
KMO(peform_satis_dat_t3[, -which(names(peform_satis_dat_t3) %in% c("GeneralIndex"))]) 

#determinant of correlation matrix (see Field et al., 2014; p. 777)
det(peform_satis.matrix) #this is not problematic (since it is greater than .00001)

#the PCA: analyses suggest extracting either one or two factors (everything and then fusion on its own)
PCA.peform_satis.one.factor_t3 =principal(peform_satis_dat_t3[, -which(names(peform_satis_dat_t3) %in% c("GeneralIndex"))], nfactors = 1, rotate = "oblimin", scores = TRUE)
print.psych(PCA.peform_satis.one.factor_t3, cut = 0.3, sort = TRUE) 
summary(prcomp(peform_satis_dat_t3[, -which(names(peform_satis_dat_t3) %in% c("GeneralIndex"))], scale = TRUE))

#get the correlation between the two variables
cor(peform_satis_dat_t3$Q3.11.1Indiv, peform_satis_dat_t3$Q3.11.2Team)

#add all three components to the dataset
peform_satis_dat_t3$peform_satisfaction_t3 = PCA.peform_satis.one.factor_t3$scores[,1]
dat = transform(merge(dat, peform_satis_dat_t3[,c("peform_satisfaction_t3", "GeneralIndex")], by = "GeneralIndex", all = T))

#test the data structure
t = dat[,c("GeneralIndex", "Q4.7Circles", "Q4.9.1Connect", "Q4.9.2Bond", "Q4.9.3Commit",
           "Q3.11.1Indiv", "Q3.11.2Team", "peform_satisfaction_t3")]

################################################################################################################################################

### PRINCIPAL COMPONENT ANALYSIS ON PERFORMANCE SATISFACTION RELATIVE TO EXPECTATIONS AT T3 (peform_satisfaction_expect_t3) ###

#create a data frame of the variables of interest
peform_satis_expect_dat_t3 = dat[, c("Q3.12.1IExpect", "Q3.12.2TExpect", "GeneralIndex")]
peform_satis_expect_dat_t3 = na.omit(peform_satis_expect_dat_t3)

#create a matrix
peform_satis_expect.matrix = cor(peform_satis_expect_dat_t3[, -which(names(peform_satis_expect_dat_t3) %in% c("GeneralIndex"))])

#Bartlett's test to see if PCA is appropriate (see Field et al., 2014; p. 775)
cortest.bartlett(peform_satis_expect_dat_t3[, -which(names(peform_satis_expect_dat_t3) %in% c("GeneralIndex"))]) #test is significant, indicatig that a PCA is appropriate

#do a KMO (see Field et al., 2014; p. 776)
KMO(peform_satis_expect_dat_t3[, -which(names(peform_satis_expect_dat_t3) %in% c("GeneralIndex"))]) 

#determinant of correlation matrix (see Field et al., 2014; p. 777)
det(peform_satis_expect.matrix) #this is not problematic (since it is greater than .00001)

#the PCA: analyses suggest extracting either one or two factors (everything and then fusion on its own)
PCA.peform_satis_expect.one.factor_t3 = principal(peform_satis_expect_dat_t3[, -which(names(peform_satis_expect_dat_t3) %in% c("GeneralIndex"))], nfactors = 1, rotate = "oblimin", scores = TRUE)
print.psych(PCA.peform_satis_expect.one.factor_t3, cut = 0.3, sort = TRUE) 
summary(prcomp(peform_satis_expect_dat_t3[, -which(names(peform_satis_expect_dat_t3) %in% c("GeneralIndex"))], scale = TRUE))

#get the correlation between the two variables
cor(peform_satis_expect_dat_t3$Q3.12.1IExpect, peform_satis_expect_dat_t3$Q3.12.2TExpect)

#add all three components to the dataset
peform_satis_expect_dat_t3$peform_satisfaction_expect_t3 = PCA.peform_satis_expect.one.factor_t3$scores[,1]
dat = transform(merge(dat, peform_satis_expect_dat_t3[,c("peform_satisfaction_expect_t3", "GeneralIndex")], by = "GeneralIndex", all = T))

#test the data structure (it looks good so far)
t = dat[,c("GeneralIndex", "Q4.7Circles", "Q4.9.1Connect", "Q4.9.2Bond", "Q4.9.3Commit", "Q3.12.1IExpect", "Q3.12.2TExpect", "peform_satisfaction_expect_t3")]

################################################################################################################################################

### PRINCIPAL COMPONENT ANALYSIS ON EXPERIENCED INTERDEPENDENCE AT T3 (experienced_interdependence_t3) ###

#create a data frame of the variables of interest
experienced_interdependence_dat_t3 = dat[, c("Q3.9.1Need", "Q3.9.2Help", "GeneralIndex")]
experienced_interdependence_dat_t3 = na.omit(experienced_interdependence_dat_t3)

#create a matrix
experienced_interdependence.matrix = cor(experienced_interdependence_dat_t3[, -which(names(experienced_interdependence_dat_t3) %in% c("GeneralIndex"))])

#Bartlett's test to see if PCA is appropriate (see Field et al., 2014; p. 775)
cortest.bartlett(experienced_interdependence_dat_t3[, -which(names(experienced_interdependence_dat_t3) %in% c("GeneralIndex"))]) #test is significant, indicatig that a PCA is appropriate

#do a KMO (see Field et al., 2014; p. 776)
KMO(experienced_interdependence_dat_t3[, -which(names(experienced_interdependence_dat_t3) %in% c("GeneralIndex"))]) #values between .5 and .7 are "mediocre"

#determinant of correlation matrix (see Field et al., 2014; p. 777)
det(experienced_interdependence.matrix) #this is not problematic (since it is greater than .00001)

#the PCA: analyses suggest extracting either one or two factors (everything and then fusion on its own)
PCA.experienced_interdependence_dat_t3 = principal(experienced_interdependence_dat_t3[, -which(names(experienced_interdependence_dat_t3) %in% c("GeneralIndex"))], nfactors = 1, scores = TRUE)
print.psych(PCA.experienced_interdependence_dat_t3, cut = 0.3, sort = TRUE) 
summary(prcomp(experienced_interdependence_dat_t3[, -which(names(experienced_interdependence_dat_t3) %in% c("GeneralIndex"))], scale = TRUE))

#get the correlation between the two variables
cor(experienced_interdependence_dat_t3$Q3.9.1Need, experienced_interdependence_dat_t3$Q3.9.2Help)

#add all three components to the dataset
experienced_interdependence_dat_t3$experienced_interdependence_t3 = PCA.experienced_interdependence_dat_t3$scores[,1]
dat = transform(merge(dat, experienced_interdependence_dat_t3[,c("experienced_interdependence_t3", "GeneralIndex")], by = "GeneralIndex", all = T))

#test the data structure (it looks good so far)
t = dat[, c("GeneralIndex", "Q4.7Circles", "Q3.9.1Need", "Q3.9.2Help", "experienced_interdependence_t3")]

################################################################################################################################################

### SAVE DATA ###

#write metadata to the file
writeLines("../data/TTC_data_CFA_PCA_variables.csv", text = c(metadata[1], metadata[2]))

#append the dataframe to the annotated file
write.table(dat, file = "../data/TTC_data_CFA_PCA_variables.csv", append = TRUE, row.names = TRUE, col.names = TRUE, sep = ',')
