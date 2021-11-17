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
metadata = readLines('../data/TTC_data_CFA_PCA_variables.csv', 2)
print(metadata)

#load the data
dat = read.csv('../data/TTC_data_CFA_PCA_variables.csv', skip = 2)

################################################################################################################################################

### MODEL ASSUMPTION CHECKS ###

#load local functions adapted from the R script provided by Snijders & Bosker (2012); https://www.stats.ox.ac.uk/~snijders/ch10.r
source("assumption_check_functions.R")

### FUNCTION FOR MAKING MODEL RESULTS ###

library(officer)
library(flextable)
library(magrittr)
library(dplyr)

#make a table in Microsoft Word summarising the results
make_MS_Word_model_summary_table = function(model) { 

  #get the name of the model as a string
  model_name = deparse(substitute(model))
  filename = paste0(model_name, ".docx")

  #extract (and round) all the coefficients for the table (and rename them)
  predictor_coefficients = round(as.data.frame(summary(model)['coefficients']), 3)
  predictor_coefficients[, 3] = round(predictor_coefficients[, 3], 1)
  predictor_coefficients[, 4] = round(predictor_coefficients[, 4], 2)
  predictor_coefficients = cbind(data.frame(variables = rownames(predictor_coefficients)), predictor_coefficients)
  
  #combine the df and t-value columns, drop the old ones, and reorder the dataframe
  predictor_coefficients["t (df)"] = sprintf("%2.2f (%2.2f)", predictor_coefficients$coefficients.t.value, predictor_coefficients$coefficients.df)
  drops = c("coefficients.t.value", "coefficients.df")
  predictor_coefficients = predictor_coefficients[, !(names(predictor_coefficients) %in% drops)]
  predictor_coefficients = predictor_coefficients[, c(1, 2, 3, 5, 4)]
  
  #add the random effects variable name line
  re_names = data.frame("Random part", "Variance", "SD", " ", " ")
  names(re_names) = names(predictor_coefficients)
  predictor_coefficients = rbind(predictor_coefficients, re_names)
  
  #get the random effects (remove the last line and unneeded variables)
  random_effects = as.data.frame(summary(model)['varcor']['varcor'])
  random_effects = random_effects[c(1:(nrow(random_effects) - 1)), ]
  random_effects = random_effects[, c(1, 4, 5)]
  random_effects = random_effects %>% mutate_if(is.numeric, round, digits=3)
  
  #add blank variables and rename the column
  random_effects["blank1"] = " "
  random_effects["blank2"] = " "
  names(random_effects) = names(predictor_coefficients)
  
  #add the random effects variables to the dataset
  predictor_coefficients = rbind(predictor_coefficients , random_effects)
  
  #add the R-squared variables 
  predictor_coefficients$variables = as.character(predictor_coefficients$variables)
  rs = paste(as.character(round(r.squaredGLMM(model)[1], 3)), as.character(round(r.squaredGLMM(model)[2], 3)))
  predictor_coefficients[nrow(predictor_coefficients) +1, ] = list(rs, " ", " ", " ", " ")
  
  #change the names of the columns
  names(predictor_coefficients) = c("Variable", "b", "SE", "t (df)", "p")
  
  #create flextable object and a temporary file
  ft = flextable(data = predictor_coefficients) %>% theme_booktabs %>% autofit
  tmp = tempfile(fileext = filename)  
  
  #create a temporary 'docx' file
  read_docx() %>% 
    body_add_flextable(ft) %>% 
    print(target = tmp)

  #open Word document
  browseURL(tmp)
  
}  
  
################################################################################################################################################

### DESCRIPTIVE STATISTICS - SURVEY COMPLETION ###

#total number of participants and teams 
print(paste("TOTAL PARTICIPANTS:", length(unique(dat$GeneralIndex))))
print(paste("TOTAL TEAMS:", length(unique(dat$TeamIDVar))))

#mean, SD, and range for observations per team
obvs_per_team = dat %>% group_by(TeamIDVar) %>% dplyr::summarise(count = mean(dplyr::n()))
mean(obvs_per_team$count)
sd(obvs_per_team$count)
range(obvs_per_team$count)
hist(obvs_per_team$count)

### ### ###

#number of individuals and teams who completed the first round of questions (look at survey questions that cannot be skipped)
t1_q_cols = c(1, 2, 5:18)
t1_qs = na.omit(dat[, t1_q_cols])

print(paste("TIME 1 UNIQUE SURVEY RESPONSES:", as.character(length(unique(t1_qs$GeneralIndex)))))
print(paste0("TIME 1 UNIQUE SURVEY RESPONSES:", as.character(length(unique(t1_qs$GeneralIndex))), "(", 5 ,")"))

print(paste("TIME 1 UNIQUE TEAM RESPONSES:", as.character(length(unique(t1_qs$TeamIDVar)))))

#get data on observations per team
t1_qs_obvs_per_team = t1_qs %>% group_by(TeamIDVar) %>% summarise(count = mean(n()))
print(paste("TIME 1 MEAN RESPONSE(S) PER TEAM:", round(mean(t1_qs_obvs_per_team$count), 3)))
print(paste("TIME 1 SD RESPONSE(S) PER TEAM:", round(sd(t1_qs_obvs_per_team$count), 3)))
print(paste("TIME 1 RANGE RESPONSE(S) PER TEAM:", range(t1_qs_obvs_per_team$count)[1], "-", range(t1_qs_obvs_per_team$count)[2]))

### ### ###

#number of individuals and teams who completed the second round of questions (look at survey questions that cannot be skipped)
t2_q_cols = c(1, 2, 20:92)
t2_qs = na.omit(dat[, t2_q_cols])

print(paste("TIME 2 UNIQUE SURVEY RESPONSES:", as.character(length(unique(t2_qs$GeneralIndex)))))
print(paste("TIME 2 UNIQUE TEAM RESPONSES:", as.character(length(unique(t2_qs$TeamIDVar)))))

#get data on observations per team
t2_qs_obvs_per_team = t2_qs %>% group_by(TeamIDVar) %>% summarise(count = mean(n()))
print(paste("TIME 2 MEAN RESPONSE(S) PER TEAM:", round(mean(t2_qs_obvs_per_team$count), 3)))
print(paste("TIME 2 SD RESPONSE(S) PER TEAM:", round(sd(t2_qs_obvs_per_team$count), 3)))
print(paste("TIME 2 RANGE RESPONSE(S) PER TEAM:", range(t2_qs_obvs_per_team$count)[1], "-", range(t2_qs_obvs_per_team$count)[2]))

### ### ###

#number of individuals and teams who completed the third round of questions (look at survey questions that cannot be skipped)
t3_q_cols = c(1, 2, 94:139)
t3_qs = na.omit(dat[, t3_q_cols])

print(paste("TIME 3 UNIQUE SURVEY RESPONSES:", as.character(length(unique(t3_qs$GeneralIndex)))))
print(paste("TIME 3 UNIQUE TEAM RESPONSES:", as.character(length(unique(t3_qs$TeamIDVar)))))

#get data on observations per team
t3_qs_obvs_per_team = t3_qs %>% group_by(TeamIDVar) %>% summarise(count = mean(n()))
print(paste("TIME 3 MEAN RESPONSE(S) PER TEAM:", round(mean(t3_qs_obvs_per_team$count), 3)))
print(paste("TIME 3 SD RESPONSE(S) PER TEAM:", round(sd(t3_qs_obvs_per_team$count), 3)))
print(paste("TIME 3 RANGE RESPONSE(S) PER TEAM:", range(t3_qs_obvs_per_team$count)[1], "-", range(t3_qs_obvs_per_team$count)[2]))

### ### ###

#number of individuals and teams who completed the fourth round of questions (look at survey questions that cannot be skipped)
t4_q_cols = c(1, 2, 141:173)
t4_qs = na.omit(dat[, t4_q_cols])

print(paste("TIME 4 UNIQUE SURVEY RESPONSES:", as.character(length(unique(t4_qs$GeneralIndex)))))
print(paste("TIME 4 UNIQUE TEAM RESPONSES:", as.character(length(unique(t4_qs$TeamIDVar)))))

#get data on observations per team
t4_qs_obvs_per_team = t4_qs %>% group_by(TeamIDVar) %>% summarise(count = mean(n()))
print(paste("TIME 4 MEAN RESPONSE(S) PER TEAM:", round(mean(t4_qs_obvs_per_team$count), 3)))
print(paste("TIME 4 SD RESPONSE(S) PER TEAM:", round(sd(t4_qs_obvs_per_team$count), 3)))
print(paste("TIME 4 RANGE RESPONSE(S) PER TEAM:", range(t4_qs_obvs_per_team$count)[1], "-", range(t4_qs_obvs_per_team$count)[2]))

### ### ###

#counts and percentages of individuals who finished one, two, three, and four surveys
completions_counts = c()
counter = 1
for (p in unique(dat$GeneralIndex)){
  completion_count = 0
  
  #check if the participant is in each of the questionnaire datasets
  if (p %in% unique(t1_qs$GeneralIndex)){
    completion_count = completion_count + 1} 
  if (p %in% unique(t2_qs$GeneralIndex)){
    completion_count = completion_count + 1} 
  if (p %in% unique(t3_qs$GeneralIndex)){
    completion_count = completion_count + 1} 
  if (p %in% unique(t4_qs$GeneralIndex)){
    completion_count = completion_count + 1} 
  
  #add completion count to list of completion counts (and add to counter)
  completions_counts[[counter]] = completion_count
  counter = counter + 1
}

#get completions counts and percentages for survey completion
print(paste0("COUNT AND PERCENTAGE OF PARTICIPANTS WHO FINISHED ONE SURVEY: ", table(completions_counts)[1], " (", round((table(completions_counts)[1]/sum(table(completions_counts)))*100, 2), "%)"))
print(paste0("COUNT AND PERCENTAGE OF PARTICIPANTS WHO FINISHED TWO SURVEYS: ", table(completions_counts)[2], " (", round((table(completions_counts)[2]/sum(table(completions_counts)))*100, 2), "%)"))
print(paste0("COUNT AND PERCENTAGE OF PARTICIPANTS WHO FINISHED THREE SURVEYS: ", table(completions_counts)[3], " (", round((table(completions_counts)[3]/sum(table(completions_counts)))*100, 2), "%)"))
print(paste0("COUNT AND PERCENTAGE OF PARTICIPANTS WHO FINISHED FOUR SURVEYS: ", table(completions_counts)[4], " (", round((table(completions_counts)[4]/sum(table(completions_counts)))*100, 2), "%)"))

### ### ###

#list of potential completion combinations
one_two_three_four = c()
one_two_three = c()
two_three_four = c()
one_three_four = c()
one_two_four = c()
one_two = c()
one_three = c()
one_four = c()
two_three = c()
two_four = c()
three_four = c()
one = c()
two = c()
three = c()
four = c()

#get the completion combination for each participant
for (p in unique(dat$GeneralIndex)){
  
  if (p %in% unique(t1_qs$GeneralIndex) & p %in% unique(t2_qs$GeneralIndex) & p %in% unique(t3_qs$GeneralIndex) & p %in% unique(t4_qs$GeneralIndex)){
    one_two_three_four[[length(one_two_three_four) + 1]] = p
    next} 
  
  if (p %in% unique(t1_qs$GeneralIndex) & p %in% unique(t2_qs$GeneralIndex) & p %in% unique(t3_qs$GeneralIndex)){
    one_two_three[[length(one_two_three) + 1]] = p
    next} 
  
  if (p %in% unique(t2_qs$GeneralIndex) & p %in% unique(t3_qs$GeneralIndex) & p %in% unique(t4_qs$GeneralIndex)){
    two_three_four[[length(two_three_four) + 1]] = p
    next} 
  
  if (p %in% unique(t1_qs$GeneralIndex) & p %in% unique(t3_qs$GeneralIndex) & p %in% unique(t4_qs$GeneralIndex)){
    one_three_four[[length(one_three_four) + 1]] = p
    next}  

  if (p %in% unique(t1_qs$GeneralIndex) & p %in% unique(t2_qs$GeneralIndex) & p %in% unique(t4_qs$GeneralIndex)){
    one_two_four[[length(one_two_four) + 1]] = p
    next}  
  
  if (p %in% unique(t1_qs$GeneralIndex) & p %in% unique(t2_qs$GeneralIndex)){
    one_two[[length(one_two) + 1]] = p
    next}  
  
  if (p %in% unique(t1_qs$GeneralIndex) & p %in% unique(t3_qs$GeneralIndex)){
    one_three[[length(one_three) + 1]] = p
    next}    

  if (p %in% unique(t1_qs$GeneralIndex) & p %in% unique(t4_qs$GeneralIndex)){
    one_four[[length(one_four) + 1]] = p
    next}    

  if (p %in% unique(t2_qs$GeneralIndex) & p %in% unique(t3_qs$GeneralIndex)){
    two_three[[length(two_three) + 1]] = p
    next}    
  
  if (p %in% unique(t2_qs$GeneralIndex) & p %in% unique(t4_qs$GeneralIndex)){
    two_four[[length(two_four) + 1]] = p
    next}    
  
  if (p %in% unique(t3_qs$GeneralIndex) & p %in% unique(t4_qs$GeneralIndex)){
    three_four[[length(three_four) + 1]] = p
    next}    

  if (p %in% unique(t1_qs$GeneralIndex)){
    one[[length(one) + 1]] = p}  
  
  if (p %in% unique(t2_qs$GeneralIndex)){
    two[[length(two) + 1]] = p}  
  
  if (p %in% unique(t3_qs$GeneralIndex)){
    three[[length(three) + 1]] = p}  
  
  if (p %in% unique(t4_qs$GeneralIndex)){
    four[[length(four) + 1]] = p}  
}

#check that all survey completion combinations add up to the total number of participants (226)
length(one_two_three_four) + length(one_two_three) + length(two_three_four) + length(one_three_four) + length(one_two_four) + length(one_two) +
length(one_three) + length(one_four) + length(two_three) + length(two_four) + length(three_four) + length(one) + length(two) + length(three) + length(four)

#check that all survey completion combinations (excluding combinations with zero participants) add up to the total number of participants (226)
length(one_two_three_four) +
  length(one) +
  length(one_three_four) +
  length(one_two_three) +
  length(two_three) +
  length(three) +
  length(one_four) +
  length(two_three_four) +
  length(three_four) +
  length(one_three) +
  length(one_two_four) +
  length(one_two) +
  length(four)
  
#get the percentage of participants for each survey completion combination    
round(length(one_two_three_four)/226 * 100, 2)
round(length(one)/226 * 100, 2)
round(length(one_three_four)/226 * 100, 2)
round(length(one_two_three)/226 * 100, 2)
round(length(two_three)/226 * 100, 2)
round(length(three)/226 * 100, 2)
round(length(one_four)/226 * 100, 2)
round(length(two_three_four)/226 * 100, 2)
round(length(one_two_four)/226 * 100, 2)
round(length(one_two)/226 * 100, 2)
round(length(four)/226 * 100, 2)

################################################################################################################################################

### DESCRIPTIVE STATISTICS - SURVEY RESPONSES ###

#participant sex counts
print(paste0("COUNT AND PERCENTAGE OF FEMALE PARTICIPANTS: ", table(dat$Sex)[1], " (",  round(table(dat$Sex)[1] / sum(table(dat$Sex)) * 100, 2), "%)"))

### ### ### 

#counts and percentages of individuals who would recommend the Ten Tors Challenge
print(paste0("COUNT AND PERCENTAGE OF PARTICIPANTS WHO WOULD 'RECOMMEND' THE TEN TORS CHALLENGE: ", table(dat$Q4.11Recomm)[2], " (", round((table(dat$Q4.11Recomm)[2]/sum(table(dat$Q4.11Recomm)))*100, 2), "%)"))

#get the percentage of participants who would do the Ten Tors Challenge again
print(paste0("PERCENTAGE OF PARTICIPANTS WHO WOULD DO THE TEN TORS CHALLENGE AGAIN: ", round(table(dat$Q4.10TTAgain.)[2]/sum(table(dat$Q4.10TTAgain.)) * 100, 2)))

### ### ### 

#mean, SD for well-being scores (t1 and t4)
mean(dat$wellbeing_t1, na.rm = TRUE)
sd(dat$wellbeing_t1, na.rm = TRUE)

mean(dat$wellbeing_t4, na.rm = TRUE)
sd(dat$wellbeing_t4, na.rm = TRUE)

#mean, SD for behavioural-interdependence scores (t2)
mean(dat$interdependence_t2, na.rm = TRUE)
sd(dat$interdependence_t2, na.rm = TRUE)

#mean, SD for mood variables (t2 and t2)
mean(dat$anger_t2, na.rm = TRUE)
sd(dat$anger_t2, na.rm = TRUE)
mean(dat$anger_t3, na.rm = TRUE)
sd(dat$anger_t3, na.rm = TRUE)

mean(dat$confusion_t2, na.rm = TRUE)
sd(dat$confusion_t2, na.rm = TRUE)
mean(dat$confusion_t3, na.rm = TRUE)
sd(dat$confusion_t3, na.rm = TRUE)

mean(dat$depression_t2, na.rm = TRUE)
sd(dat$depression_t2, na.rm = TRUE)
mean(dat$depression_t3, na.rm = TRUE)
sd(dat$depression_t3, na.rm = TRUE)

mean(dat$fatigue_t2, na.rm = TRUE)
sd(dat$fatigue_t2, na.rm = TRUE)
mean(dat$fatigue_t3, na.rm = TRUE)
sd(dat$fatigue_t3, na.rm = TRUE)

mean(dat$tension_t2, na.rm = TRUE)
sd(dat$tension_t2, na.rm = TRUE)
mean(dat$tension_t3, na.rm = TRUE)
sd(dat$tension_t3, na.rm = TRUE)

mean(dat$vigour_t2, na.rm = TRUE)
sd(dat$vigour_t2, na.rm = TRUE)
mean(dat$vigour_t3, na.rm = TRUE)
sd(dat$vigour_t3, na.rm = TRUE)

#mean, SD for perceptions of relationships variables (t2 and t4)
mean(dat$closeness_t2, na.rm = TRUE)
sd(dat$closeness_t2, na.rm = TRUE)
mean(dat$closeness_t4, na.rm = TRUE)
sd(dat$closeness_t4, na.rm = TRUE)

mean(dat$similarity_t2, na.rm = TRUE)
sd(dat$similarity_t2, na.rm = TRUE)
mean(dat$similarity_t4, na.rm = TRUE)
sd(dat$similarity_t4, na.rm = TRUE)

mean(dat$eday_centrality_t2, na.rm = TRUE)
sd(dat$eday_centrality_t2, na.rm = TRUE)
mean(dat$eday_centrality_t4, na.rm = TRUE)
sd(dat$eday_centrality_t4, na.rm = TRUE)

#mean, SD for bonding variables (t2, t3, and t4)
mean(dat$Q271Conn, na.rm = TRUE)
sd(dat$Q271Conn, na.rm = TRUE)
mean(dat$Q3.8.1Connect, na.rm = TRUE)
sd(dat$Q3.8.1Connect, na.rm = TRUE)
mean(dat$Q4.9.1Connect, na.rm = TRUE)
sd(dat$Q4.9.1Connect, na.rm = TRUE)

mean(dat$Q272Bond, na.rm = TRUE)
sd(dat$Q272Bond, na.rm = TRUE)
mean(dat$Q3.8.2Bond, na.rm = TRUE)
sd(dat$Q3.8.2Bond, na.rm = TRUE)
mean(dat$Q4.9.2Bond, na.rm = TRUE)
sd(dat$Q4.9.2Bond, na.rm = TRUE)

mean(dat$Q273Commi, na.rm = TRUE)
sd(dat$Q273Commi, na.rm = TRUE)
mean(dat$Q3.8.3.Commit, na.rm = TRUE)
sd(dat$Q3.8.3.Commit, na.rm = TRUE)
mean(dat$Q4.9.3Commit, na.rm = TRUE)
sd(dat$Q4.9.3Commit, na.rm = TRUE)

mean(dat$Q2.4Circles, na.rm = TRUE)
sd(dat$Q2.4Circles, na.rm = TRUE)
mean(dat$Q3.7Circles, na.rm = TRUE)
sd(dat$Q3.7Circles, na.rm = TRUE)
mean(dat$Q4.7Circles, na.rm = TRUE)
sd(dat$Q4.7Circles, na.rm = TRUE)

#create a variable that is a mean of all the variables in the bonding component (for descriptive purposes)
dat$bonding_components_mean_t2 = (dat$Q271Conn + dat$Q272Bond + dat$Q273Commi + dat$Q2.4Circles) / 4
mean(dat$bonding_components_mean_t2, na.rm = TRUE)
sd(dat$bonding_components_mean_t2, na.rm = TRUE)

dat$bonding_components_mean_t3 = (dat$Q3.8.1Connect + dat$Q3.8.2Bond + dat$Q3.8.3.Commit + dat$Q3.7Circles) / 4
mean(dat$bonding_components_mean_t3, na.rm = TRUE)
sd(dat$bonding_components_mean_t3, na.rm = TRUE)

dat$bonding_components_mean_t4 = (dat$Q4.9.1Connect + dat$Q4.9.2Bond + dat$Q4.9.3Commit + dat$Q4.7Circles) / 4
mean(dat$bonding_components_mean_t4, na.rm = TRUE)
sd(dat$bonding_components_mean_t4, na.rm = TRUE)

#mean, SD for social support variables (perceived support at t2 and received support at t3)
mean(dat$perceived_support_t2_emotional, na.rm = TRUE)
sd(dat$perceived_support_t2_emotional, na.rm = TRUE)

mean(dat$perceived_support_t2_esteem, na.rm = TRUE)
sd(dat$perceived_support_t2_esteem, na.rm = TRUE)

mean(dat$received_support_t3_emotional, na.rm = TRUE)
sd(dat$received_support_t3_emotional, na.rm = TRUE)

mean(dat$received_support_t3_esteem, na.rm = TRUE)
sd(dat$received_support_t3_esteem, na.rm = TRUE)

#mean, SD for stress appraisal variables (t2); means for constituent components of variables that are PCA component scores
dat$threat_emotion_components_t2 = (dat$Q282threat + dat$Q285Anx) / 2
mean(dat$threat_emotion_components_t2, na.rm = TRUE)
sd(dat$threat_emotion_components_t2, na.rm = TRUE)

dat$threat_outcome_components_t2 = (dat$Q28a.1nega + dat$Q28a.6nega) / 2
mean(dat$threat_outcome_components_t2, na.rm = TRUE)
sd(dat$threat_outcome_components_t2, na.rm = TRUE)

mean(dat$challenge_t2, na.rm = TRUE)
sd(dat$challenge_t2, na.rm = TRUE)

mean(dat$centrality_t2, na.rm = TRUE)
sd(dat$centrality_t2, na.rm = TRUE)

#mean, SD for collective efficacy (t2)
mean(dat$collective_efficacy_t2, na.rm = TRUE)
sd(dat$collective_efficacy_t2, na.rm = TRUE)

#mean, SD for actual interdependence (t3); means for constituent components as variable is a PCA component score
dat$experienced_interdependence_t3_components = (dat$Q3.9.1Need + dat$Q3.9.2Help) / 2

mean(dat$experienced_interdependence_t3_components, na.rm = TRUE)
sd(dat$collective_efficacy_t2, na.rm = TRUE)

mean(dat$Q3.9.1Need, na.rm = TRUE)
sd(dat$Q3.9.1Need, na.rm = TRUE)

mean(dat$Q3.9.2Help, na.rm = TRUE)
sd(dat$Q3.9.2Help, na.rm = TRUE)

#mean, SD for performance satisfaction (t3); means for constituent components as variable is a PCA component score
dat$performance_satisfaction_t3_components = (dat$Q3.11.1Indiv + dat$Q3.11.2Team) / 2
mean(dat$performance_satisfaction_t3_components, na.rm = TRUE)
sd(dat$performance_satisfaction_t3_components, na.rm = TRUE)

mean(dat$Q3.11.1Indiv, na.rm = TRUE)
sd(dat$Q3.11.1Indiv, na.rm = TRUE)

mean(dat$Q3.11.2Team, na.rm = TRUE)
sd(dat$Q3.11.2Team, na.rm = TRUE)

#mean, SD for performance satisfaction relative to expectations (t3); means for constituent components as variable is a PCA component score
dat$performance_satisfaction_expect_t3_components = (dat$Q3.12.1IExpect + dat$Q3.12.2TExpect) / 2
mean(dat$performance_satisfaction_expect_t3_components, na.rm = TRUE)
sd(dat$performance_satisfaction_expect_t3_components, na.rm = TRUE)

mean(dat$Q3.12.1IExpect, na.rm = TRUE)
sd(dat$Q3.12.1IExpect, na.rm = TRUE)

mean(dat$Q3.12.2TExpect, na.rm = TRUE)
sd(dat$Q3.12.2TExpect, na.rm = TRUE)

#mean, SD for physical discomfort (t3); means for constituent components as variable is a PCA component score
dat$physical_discomfort_t3_components = (dat$Q3.6.1Pain + dat$Q3.6.2Fatigue) / 2

mean(dat$physical_discomfort_t3_components, na.rm = TRUE)
sd(dat$physical_discomfort_t3_components, na.rm = TRUE)

mean(as.numeric(dat$Q3.6.2Fatigue), na.rm = TRUE)
sd(as.numeric(dat$Q3.6.2Fatigue), na.rm = TRUE)

mean(as.numeric(dat$Q3.6.1Pain),  na.rm = TRUE)
sd(as.numeric(dat$Q3.6.1Pain), na.rm = TRUE)

#mean, SD for physical effort (t3)
mean(dat$Q3.6.3Effort, na.rm = TRUE)
sd(dat$Q3.6.3Effort, na.rm = TRUE)

################################################################################################################################################

### POWER ANALYSIS ###

library(simr)

#set the random seed for reproducibility (and Jackie Robinson)
set.seed(42)

#create a representative dataset
pst3_dat = dat[complete.cases(dat[ , c("peform_satisfaction_t3")]),]

#determine 80% of the full sample of participants, and make the dataset that length
samp_80 = round(nrow(dat)*.8, 0)
pst3_dat = pst3_dat[1:samp_80, ]

#set the distribution of the predictor variable
x = pst3_dat$peform_satisfaction_t3

#set the assumed fixed effect intercept and slope estimate
b = c(0, 0.05)

#set the assumed random intercept variance
rand_int_var = 0.01

#set the assumed residual variance
s = 0.175 

#create the model to be used in the simulations
peform_satisfaction_t3_power_analysis = makeLmer(y ~ x + (1|TeamIDVar), fixef = b, VarCorr = rand_int_var, sigma = s, data = pst3_dat)

#run the simulations and return the power analysis
powerSim(peform_satisfaction_t3_power_analysis, nsim = 100)

################################################################################################################################################

### CHANGES IN MOOD OVER TIME (T2 to T3) ###

library(tidyr)
library(lme4)
library(lmerTest)
library(MuMIn)

#create long form datasets
mood_dat = dat[, c("GeneralIndex", "anger_t2", "anger_t3", "confusion_t2", "confusion_t3", "depression_t2", "depression_t3",
                   "fatigue_t2", "fatigue_t3", "tension_t2", "tension_t3", "vigour_t2", "vigour_t3", "Sex", "TeamIDVar")]

anger_long = gather(mood_dat[, c("GeneralIndex", "anger_t2", "anger_t3", "Sex", "TeamIDVar")], time, anger_score, anger_t2:anger_t3, factor_key = TRUE, na.rm = TRUE)
depression_long = gather(mood_dat[, c("GeneralIndex", "depression_t2", "depression_t3", "Sex", "TeamIDVar")], time, depression_score, depression_t2:depression_t3, factor_key = TRUE, na.rm = TRUE)
fatigue_long = gather(mood_dat[, c("GeneralIndex", "fatigue_t2", "fatigue_t3", "Sex", "TeamIDVar")], time, fatigue_score, fatigue_t2:fatigue_t3, factor_key = TRUE, na.rm = TRUE)
tension_long = gather(mood_dat[, c("GeneralIndex", "tension_t2", "tension_t3", "Sex", "TeamIDVar")], time, tension_score, tension_t2:tension_t3, factor_key = TRUE, na.rm = TRUE)
vigour_long = gather(mood_dat[, c("GeneralIndex", "vigour_t2", "vigour_t3", "Sex", "TeamIDVar")], time, vigour_score, vigour_t2:vigour_t3, factor_key = TRUE, na.rm = TRUE)
confusion_long = gather(mood_dat[, c("GeneralIndex", "confusion_t2", "confusion_t3", "Sex", "TeamIDVar")], time, confusion_score, confusion_t2:confusion_t3, factor_key = TRUE, na.rm = TRUE)

#simple mood change models (more complex models failed to converge)
anger_change_model = lmer(anger_score ~ time + Sex + (1 | TeamIDVar/GeneralIndex), data = anger_long)
summary(anger_change_model) 
#make_MS_Word_model_summary_table(anger_change_model)

depression_change_model = lmer(depression_score ~ time + Sex + (1 | TeamIDVar/GeneralIndex), data = depression_long)
summary(depression_change_model)
#make_MS_Word_model_summary_table(depression_change_model)

fatigue_change_model = lmer(fatigue_score ~ time + Sex + (1 | TeamIDVar/GeneralIndex), data = fatigue_long)
summary(fatigue_change_model)
#make_MS_Word_model_summary_table(fatigue_change_model)

tension_change_model = lmer(tension_score ~ time + Sex + (1 | TeamIDVar/GeneralIndex), data = tension_long)
summary(tension_change_model)
#make_MS_Word_model_summary_table(tension_change_model)

vigour_change_model = lmer(vigour_score ~ time + Sex + (1 | TeamIDVar/GeneralIndex), data = vigour_long)
summary(vigour_change_model)
#make_MS_Word_model_summary_table(vigour_change_model)

confusion_change_model = lmer(confusion_score ~ time + Sex + (1 | TeamIDVar/GeneralIndex), data = confusion_long)
summary(confusion_change_model)
#make_MS_Word_model_summary_table(confusion_change_model)

### CHECK MODEL ASSUMPTIONS ###

#remove individuals with only one observation (at T2 or T3) and add an integer version of the time predictor
anger_long_sub = anger_long[anger_long$GeneralIndex %in% names(which(table(anger_long$GeneralIndex)>1)), ]
anger_long_sub$time_int = as.integer(anger_long_sub$time)

depression_long_sub = depression_long[depression_long$GeneralIndex %in% names(which(table(depression_long$GeneralIndex)>1)), ]
depression_long_sub$time_int = as.integer(depression_long_sub$time)

fatigue_long_sub = fatigue_long[fatigue_long$GeneralIndex %in% names(which(table(fatigue_long$GeneralIndex)>1)), ]
fatigue_long_sub$time_int = as.integer(fatigue_long_sub$time)

tension_long_sub = tension_long[tension_long$GeneralIndex %in% names(which(table(tension_long$GeneralIndex)>1)), ]
tension_long_sub$time_int = as.integer(tension_long_sub$time)

vigour_long_sub = vigour_long[vigour_long$GeneralIndex %in% names(which(table(vigour_long$GeneralIndex)>1)), ]
vigour_long_sub$time_int = as.integer(tension_long_sub$time)

#check assumptions for models with statistical significance
mood_mlms = c(anger_change_model, depression_change_model, fatigue_change_model, tension_change_model, vigour_change_model)
mood_mlm_dir_names = c("anger_change_model", "depression_change_model", "fatigue_change_model", "tension_change_model", "vigour_change_model")
predictors = rep("time", 5)
predictor_integers = rep("time_int", 5)
predictor_names = rep("Time (T2 to T3)", 5)
outcomes = c("anger_score", "depression_score", "fatigue_score", "tension_score", "vigour_score")
level_two_variable = "TeamIDVar"
level_two_residual_variable = "GeneralIndex:TeamIDVar"
level_one_variable = "Sex"
all_data = list(anger_long_sub, depression_long_sub, fatigue_long_sub, tension_long_sub, vigour_long_sub)

#do assumption checks for each model
check_assumptions_change_over_time_models(code_dir, mood_mlms, mood_mlm_dir_names, predictors, predictor_integers, predictor_names, outcomes, level_two_variable, level_two_residual_variable, level_one_variable, all_data)

################################################################################################################################################

### CHANGES IN TEAM RELATIONSHIPS OVER TIME (T2 to T3) ###

#create long form datasets
team_relationship_dat = dat[, c("GeneralIndex", "closeness_t2", "closeness_t4", "similarity_t2", "similarity_t4", "eday_centrality_t2", "eday_centrality_t4", "Sex", "TeamIDVar")]

closeness_long = gather(team_relationship_dat[, c("GeneralIndex", "closeness_t2", "closeness_t4", "Sex", "TeamIDVar")], time, closeness_score, closeness_t2:closeness_t4, factor_key = TRUE, na.rm = TRUE)
similarity_long = gather(team_relationship_dat[, c("GeneralIndex", "similarity_t2", "similarity_t4", "Sex", "TeamIDVar")], time, similarity_score, similarity_t2:similarity_t4, factor_key = TRUE, na.rm = TRUE)
eday_centrality_long = gather(team_relationship_dat[, c("GeneralIndex", "eday_centrality_t2", "eday_centrality_t4", "Sex", "TeamIDVar")], time, eday_centrality_score, eday_centrality_t2:eday_centrality_t4, factor_key = TRUE, na.rm = TRUE)

#simple model (more complex models failed to converge)
closeness_change_model = lmer(closeness_score ~ time + Sex + (1 | TeamIDVar/GeneralIndex), data = closeness_long)
summary(closeness_change_model)
#make_MS_Word_model_summary_table(closeness_change_model)

eday_centrality_change_model = lmer(eday_centrality_score ~ time + Sex + (1 | TeamIDVar/GeneralIndex), data = eday_centrality_long)
summary(eday_centrality_change_model)
#make_MS_Word_model_summary_table(eday_centrality_change_model)

similarity_change_model = lmer(similarity_score ~ time + Sex + (1 | TeamIDVar/GeneralIndex), data = similarity_long)
summary(similarity_change_model)
#make_MS_Word_model_summary_table(similarity_change_model)

### CHECK MODEL ASSUMPTIONS ###

#remove individuals with only one observation (at T2 or T3) and add an integer version of the time predictor
closeness_long_sub = closeness_long[closeness_long$GeneralIndex %in% names(which(table(closeness_long$GeneralIndex)>1)), ]
closeness_long_sub$time_int = as.integer(closeness_long_sub$time)

eday_centrality_long_sub = eday_centrality_long[eday_centrality_long$GeneralIndex %in% names(which(table(eday_centrality_long$GeneralIndex)>1)), ]
eday_centrality_long_sub$time_int = as.integer(eday_centrality_long_sub$time)

#check assumptions for models with statistical significance
relationships_mlms = c(closeness_change_model, eday_centrality_change_model)
relationships_mlm_dir_names = c("closeness_change_model", "eday_centrality_change_model")
predictor = rep("time", 2)
predictors_integer = rep("time_int", 2)
predictor_name = rep("Time (T2 to T3)", 2)
outcomes = c("closeness_score", "eday_centrality_score")
level_two_variable = "TeamIDVar"
level_two_residual_variable = "GeneralIndex:TeamIDVar"
level_one_variable = "Sex"
all_data = list(closeness_long_sub, eday_centrality_long_sub)

#do assumption checks for each model
check_assumptions_change_over_time_models(code_dir, relationships_mlms, relationships_mlm_dir_names, predictor, predictors_integer, predictor_name, outcomes, level_two_variable, level_two_residual_variable, level_one_variable, all_data)

################################################################################################################################################

### CHANGES IN WELLBEING OVER TIME (T1 to T4) ###

#create a long form dataset
wellbeing_dat = dat[, c("GeneralIndex", "wellbeing_t1", "wellbeing_t4", "TeamIDVar", "Sex", "AgeMonths")]
wellbeing_long = gather(wellbeing_dat, time, wellbeing_score, wellbeing_t1:wellbeing_t4, factor_key = TRUE, na.rm = TRUE)

#rename the time variable
wellbeing_long$time = ifelse(wellbeing_long$time == "wellbeing_t1", "Time 1", ifelse(wellbeing_long$time == "wellbeing_t4", "Time 4", NA))

#simple model (more complex models failed to converge)
wellbeing_change_model = lmer(wellbeing_score ~ time + Sex + (1 | TeamIDVar/GeneralIndex), data = wellbeing_long)
wellbeing_change_model_age = lmer(wellbeing_score ~ time + Sex + AgeMonths + (1 | TeamIDVar/GeneralIndex), data = wellbeing_long)
summary(wellbeing_change_model)
summary(wellbeing_change_model_age)
#make_MS_Word_model_summary_table(wellbeing_change_model)

### CHECK MODEL ASSUMPTIONS ###

#remove individuals with only one observation (at T1 or T4) and add an integer version of the time predictor
wellbeing_long_sub = wellbeing_long[wellbeing_long$GeneralIndex %in% names(which(table(wellbeing_long$GeneralIndex)>1)), ]
wellbeing_long_sub$time_int = as.integer(as.factor(wellbeing_long_sub$time))

#check assumptions for models with statistical significance
wellbeing_mlm = c(wellbeing_change_model)
wellbeing_mlm_dir_names = c("wellbeing_change_model")
predictor = c("time")
predictors_integer = c("time_int")
predictor_name = "Time (T1 to T4)"
outcomes = c("wellbeing_score")
level_two_variable = "TeamIDVar"
level_two_residual_variable = "GeneralIndex:TeamIDVar"
level_one_variable = "Sex"
all_data = list(wellbeing_long_sub)

#do assumption checks for each model
check_assumptions_change_over_time_models(code_dir, wellbeing_mlm, wellbeing_mlm_dir_names, predictor, predictors_integer, predictor_name, outcomes, level_two_variable, level_two_residual_variable, level_one_variable, all_data)

################################################################################################################################################

### PERFORMANCE SATISFACTION AT T3 PREDICTS CHANGES IN WELLBEING (T1 to T4) ###

#create the wellbeing change variable
dat$wellbeing_change = dat$wellbeing_t4 - dat$wellbeing_t1

#plot the relationships by individual
plot(dat$peform_satisfaction_t3, dat$wellbeing_change)
abline(lm(dat$wellbeing_change ~ dat$peform_satisfaction_t3, data = dat))

#simple model (more complex models failed to converge)
peform_satisfaction_t3_wellbeing_change_model = lmer(wellbeing_change ~ peform_satisfaction_t3 + Sex + (1 | TeamIDVar), data = dat)
summary(peform_satisfaction_t3_wellbeing_change_model)

#adds a weak prior to get an approximate Bayesian maximum a posteriori estimate that avoids singularity
peform_satisfaction_t3_wellbeing_change_blme_model = blmer(wellbeing_change ~ peform_satisfaction_t3 + Sex + (1 | TeamIDVar), data = dat, cov.prior = invwishart(df = 5))
summary(peform_satisfaction_t3_wellbeing_change_blme_model)
r.squaredGLMM(peform_satisfaction_t3_wellbeing_change_blme_model)

#make table of the basic, 'lmer' models, but manually fill them in with the 'blmer' models (p-values can be calculated with the 'pt' function, see example below, and degrees of freedom from the 'lmer' models)
#make_MS_Word_model_summary_table(peform_satisfaction_t3_wellbeing_change_model)
main_predictor_p_val_example = 2*pt(q=2.793, df=136,lower.tail = FALSE)

### CHECK MODEL ASSUMPTIONS ###

#check assumptions for models with statistical significance
peform_satisfaction_t3_wellbeing_change_mlm = c(peform_satisfaction_t3_wellbeing_change_blme_model)
peform_satisfaction_t3_wellbeing_change_mlm_dir_names = c("peform_satisfaction_t3_wellbeing_change_blme_model")
predictor = c("peform_satisfaction_t3")
predictors_integer = c("peform_satisfaction_t3")
predictor_name = c("Performance Satisfaction (T3)")
outcomes = c("wellbeing_change")
level_two_variable = "TeamIDVar"
level_one_variable = "Sex"

#run assumption checks on data with complete cases for relavant variables
complete_case_data = dat[complete.cases(dat[ , c("peform_satisfaction_t3", "Sex", "wellbeing_change")]), ]
all_data = list(complete_case_data)

#do assumption checks for each model
check_assumptions_main_models(code_dir, peform_satisfaction_t3_wellbeing_change_mlm, peform_satisfaction_t3_wellbeing_change_mlm_dir_names,
                                          predictor, predictors_integer, predictor_name, outcomes, level_two_variable, level_one_variable, all_data)

################################################################################################################################################

### BONDING AT T2 AND T3 PREDICTS CHANGES IN WELLBEING (T1 to T4) ###

library(blme)
library(MuMIn)

#plot the relationships by individual
plot(dat$bonding_t2, dat$wellbeing_change)
abline(lm(dat$wellbeing_change ~ dat$bonding_t2, data = dat))

plot(dat$bonding_t3, dat$wellbeing_change)
abline(lm(dat$wellbeing_change ~ dat$bonding_t3, data = dat))

#simple models (more complex models failed to converge)
bonding_t2_wellbeing_change_model = lmer(wellbeing_change ~ bonding_t2 + Sex + (1 | TeamIDVar), data = dat)
summary(bonding_t2_wellbeing_change_model)

bonding_t3_wellbeing_change_model = lmer(wellbeing_change ~ bonding_t3 + Sex + (1 | TeamIDVar), data = dat)
summary(bonding_t3_wellbeing_change_model)

#adds a weak prior to get an approximate Bayesian maximum a posteriori estimate that avoids singularity
bonding_t2_wellbeing_change_blme_model = blmer(wellbeing_change ~ bonding_t2 + Sex + (1 | TeamIDVar), data = dat, cov.prior = invwishart(df = 5))
summary(bonding_t2_wellbeing_change_blme_model)
r.squaredGLMM(bonding_t2_wellbeing_change_blme_model)

bonding_t3_wellbeing_change_blme_model = blmer(wellbeing_change ~ bonding_t3 + Sex + (1 | TeamIDVar), data = dat, cov.prior = invwishart(df = 5))
summary(bonding_t3_wellbeing_change_blme_model)
r.squaredGLMM(bonding_t3_wellbeing_change_blme_model)

#make table of the basic, 'lmer' models, but manually fill them in with the 'blmer' models (p-values can be calculated with the 'pt' function, see example below, and degrees of freedom from the 'lmer' models)
#make_MS_Word_model_summary_table(bonding_t2_wellbeing_change_model)
#make_MS_Word_model_summary_table(bonding_t3_wellbeing_change_model)
main_predictor_p_val_example = 2*pt(q=2.808, df=136,lower.tail = FALSE)

### CHECK MODEL ASSUMPTIONS ###

#check assumptions for models with statistical significance
bonding_wellbeing_change_mlm = c(bonding_t3_wellbeing_change_blme_model)
bonding_wellbeing_change_mlm_dir_names = c("bonding_t3_wellbeing_change_blme_model")
predictors = c("bonding_t3")
predictors_integers = c("bonding_t3")
predictor_names = c("Bonding (T3)")
outcomes = c("wellbeing_change")
level_two_variable = "TeamIDVar"
level_one_variable = "Sex"

#run assumption checks on data with complete cases for relavant variables
complete_case_bonding_t3 = dat[complete.cases(dat[ , c("bonding_t3", "Sex", "wellbeing_change")]), ]
all_data = list(complete_case_bonding_t3)

#do assumption checks for each model
check_assumptions_main_models(code_dir, bonding_wellbeing_change_mlm, bonding_wellbeing_change_mlm_dir_names,
                              predictors, predictors_integers, predictor_names, outcomes, level_two_variable, level_one_variable, all_data)

################################################################################################################################################

### CHANGES IN BONDING OVER TIME ###

#create a long form dataset (the mean of the component variables must be used, since the component scores are standardized)
bonding_dat = dat[, c("GeneralIndex", "bonding_components_mean_t2", "bonding_components_mean_t3", "bonding_components_mean_t4", "TeamIDVar", "Sex")]
bonding_long = gather(bonding_dat, time, bonding_score, bonding_components_mean_t2:bonding_components_mean_t4, factor_key = TRUE, na.rm = TRUE)

#rename the time variable
bonding_long$time = ifelse(bonding_long$time == "bonding_components_mean_t2", "Time 2", 
                           ifelse(bonding_long$time == "bonding_components_mean_t3", "Time 3",
                                  ifelse(bonding_long$time == "bonding_components_mean_t4", "Time 4", NA)))

#simple model for bonding change T2 to T3 (more complex models failed to converge)
bonding_long_t2_t3 = droplevels(subset(bonding_long, bonding_long$time != "Time 4"))
bonding_change_model_t2_t3 = lmer(bonding_score ~ time + Sex + (1 | TeamIDVar/GeneralIndex), data = bonding_long_t2_t3)
summary(bonding_change_model_t2_t3)
#make_MS_Word_model_summary_table(bonding_change_model_t2_t3)

#simple model for bonding change T2 to T4 (more complex models failed to converge)
bonding_long_t2_t4 = droplevels(subset(bonding_long, bonding_long$time != "Time 3"))
bonding_change_model_t2_t4 = lmer(bonding_score ~ time + Sex + (1 | TeamIDVar/GeneralIndex), data = bonding_long_t2_t4)
summary(bonding_change_model_t2_t4)
#make_MS_Word_model_summary_table(bonding_change_model_t2_t4)

### CHECK MODEL ASSUMPTIONS ###

#remove individuals with only one observation (at T2 or T3) and add an integer version of the time predictor
bonding_long_t2_t3_sub = bonding_long_t2_t3[bonding_long_t2_t3$GeneralIndex %in% names(which(table(bonding_long_t2_t3$GeneralIndex)>1)), ]
bonding_long_t2_t3_sub$time_int = as.integer(as.factor(bonding_long_t2_t3_sub$time))

bonding_long_t2_t4_sub = bonding_long_t2_t4[bonding_long_t2_t4$GeneralIndex %in% names(which(table(bonding_long_t2_t4$GeneralIndex)>1)), ]
bonding_long_t2_t4_sub$time_int = as.integer(as.factor(bonding_long_t2_t4_sub$time))

#check assumptions for models with statistical significance
bonding_change_mlms = c(bonding_change_model_t2_t3, bonding_change_model_t2_t4)
bonding_change_mlm_dir_names = c("bonding_change_model_t2_t3", "bonding_change_model_t2_t4")
predictor = rep("time", 2)
predictors_integer = rep("time_int", 2)
predictor_name = c("Time (T2 to T3)", "Time (T2 to T4)")
outcomes = rep("bonding_score", 2)
level_two_variable = "TeamIDVar"
level_two_residual_variable = "GeneralIndex:TeamIDVar"
level_one_variable = "Sex"
all_data = list(bonding_long_t2_t3_sub, bonding_long_t2_t4_sub)

#do assumption checks for each model
check_assumptions_change_over_time_models(code_dir, bonding_change_mlms, bonding_change_mlm_dir_names, predictor, predictors_integer, predictor_name, outcomes, level_two_variable, level_two_residual_variable, level_one_variable, all_data)

################################################################################################################################################

### CHANGES IN BONDING (T2 TO T3) PREDICT CHANGES IN WELLBEING (T1 to T4) ###

#create the change in bonding and wellbeing variables (and get their means and SDs)
dat$bonding_change_t2_t3 = dat$bonding_t3 - dat$bonding_t2
dat$wellbeing_change = dat$wellbeing_t4 - dat$wellbeing_t1

mean(dat$bonding_change_t2_t3, na.rm = TRUE)
sd(dat$bonding_change_t2_t3, na.rm = TRUE)

mean(dat$wellbeing_change, na.rm = TRUE)
sd(dat$wellbeing_change, na.rm = TRUE)

#plot the relationships by individual
plot(dat$bonding_change_t2_t3, dat$wellbeing_change)
abline(lm(dat$wellbeing_change ~ dat$bonding_change_t2_t3, data = dat))

#simple model (more complex models failed to converge)
bonding_change_wellbeing_change_model = lmer(wellbeing_change ~ bonding_change_t2_t3 + Sex + (1 | TeamIDVar), data = dat)
summary(bonding_change_wellbeing_change_model)

#adds a weak prior to get an approximate Bayesian maximum a posteriori estimate that avoids singularity
bonding_change_wellbeing_change_blme_model = blmer(wellbeing_change ~ bonding_change_t2_t3 + Sex + (1 | TeamIDVar), data = dat, cov.prior = invwishart(df = 5))
summary(bonding_change_wellbeing_change_blme_model)
r.squaredGLMM(bonding_change_wellbeing_change_blme_model)

#make table of the basic, 'lmer' models, but manually fill them in with the 'blmer' models (p-values can be calculated with the 'pt' function, see example below, and degrees of freedom from the 'lmer' models)
#make_MS_Word_model_summary_table(bonding_change_wellbeing_change_model)
main_predictor_p_val_example = 2*pt(q=1.829, df=119,lower.tail = FALSE)

################################################################################################################################################

### PHYSICAL DISCOMFORT PREDICTS BONDING (T3) ###

#plot the relationships by individual
plot(dat$phys_diff_t3, dat$bonding_t3)
abline(lm(dat$bonding_t3 ~ dat$phys_diff_t3, data = dat))

#simple model (more complex models failed to converge)
discomfort_bonding_t3_model = lmer(bonding_t3 ~ phys_diff_t3 + Sex + (1 | TeamIDVar), data = dat)
summary(discomfort_bonding_t3_model)
#make_MS_Word_model_summary_table(discomfort_bonding_t3_model)

### CHECK MODEL ASSUMPTIONS ###

#check assumptions for models with statistical significance
discomfort_bonding_t3_mlm = c(discomfort_bonding_t3_model)
discomfort_bonding_t3_mlm_dir_names = c("discomfort_bonding_t3_model")
predictors = c("phys_diff_t3")
predictors_integers = c("phys_diff_t3")
predictor_names = c("Physical Discomfort")
outcomes = c("bonding_t3")
level_two_variable = "TeamIDVar"
level_one_variable = "Sex"

#run assumption checks on data with complete cases for relavant variables
complete_case_phys_diff_bonding_t3 = dat[complete.cases(dat[ , c("bonding_t3", "Sex", "phys_diff_t3")]), ]
all_data = list(complete_case_phys_diff_bonding_t3)

#do assumption checks for each model
check_assumptions_main_models(code_dir, discomfort_bonding_t3_mlm, discomfort_bonding_t3_mlm_dir_names,
                              predictors, predictors_integers, predictor_names, outcomes, level_two_variable, level_one_variable, all_data)

################################################################################################################################################

### EXPERIENCED INTERDEPENDENCE PREDICTS BONDING (T3) ###

#plot the relationships by individual
plot(dat$experienced_interdependence_t3, dat$bonding_t3)
abline(lm(dat$bonding_t3 ~ dat$experienced_interdependence_t3, data = dat))

#this model is singular
experienced_interdependence_bonding_t3_model = lmer(bonding_t3 ~ experienced_interdependence_t3 + Sex + (1 | TeamIDVar), data = dat)
summary(experienced_interdependence_bonding_t3_model)

### ### ###

library(blme)
library(MuMIn)

#adds a weak prior to get an approximate Bayesian maximum a posteriori estimate that avoids singularity
experienced_interdependence_bonding_t3_blme_model = blmer(bonding_t3 ~ experienced_interdependence_t3 + Sex + (1 | TeamIDVar), data = dat, cov.prior = invwishart(df = 5))
summary(experienced_interdependence_bonding_t3_blme_model)

#get approximate p-values from a blme object
parameters::p_value(experienced_interdependence_bonding_t3_blme_model)

#make table of the basic, 'lmer' model, but manually fill it in with the 'blmer' model
#make_MS_Word_model_summary_table(experienced_interdependence_bonding_t3_model)
r.squaredGLMM(experienced_interdependence_bonding_t3_blme_model)

### CHECK MODEL ASSUMPTIONS ###

#check assumptions for models with statistical significance
experienced_interdependence_bonding_t3_mlm = c(experienced_interdependence_bonding_t3_blme_model)
experienced_interdependence_bonding_t3_mlm_dir_names = c("experienced_interdependence_bonding_t3_blme_model")
predictors = c("experienced_interdependence_t3")
predictors_integers = c("experienced_interdependence_t3")
predictor_names = c("Experienced Interdependence")
outcomes = c("bonding_t3")
level_two_variable = "TeamIDVar"
level_one_variable = "Sex"

#run assumption checks on data with complete cases for relavant variables
complete_case_exp_interdependence_bonding_t3 = dat[complete.cases(dat[ , c("bonding_t3", "Sex", "experienced_interdependence_t3")]), ]
all_data = list(complete_case_exp_interdependence_bonding_t3)

#do assumption checks for each model
check_assumptions_main_models(code_dir, experienced_interdependence_bonding_t3_mlm, experienced_interdependence_bonding_t3_mlm_dir_names,
                              predictors, predictors_integers, predictor_names, outcomes, level_two_variable, level_one_variable, all_data)

################################################################################################################################################

### TEAM PERFORMANCE RELATIVE TO EXPECTATIONS PREDICTS BONDING (T3) ###

#plot the relationships by individual
plot(dat$Q3.12.2TExpect, dat$bonding_t3)
abline(lm(dat$bonding_t3 ~ dat$Q3.12.2TExpect, data = dat))

#simple model controlling for individual performance relative to expectations (more complex models failed to converge)
subjective_team_performance_bonding_t3_model = lmer(bonding_t3 ~ Q3.12.2TExpect + Sex + Q3.12.1IExpect + (1 | TeamIDVar), data = dat)
summary(subjective_team_performance_bonding_t3_model)
#make_MS_Word_model_summary_table(subjective_team_performance_bonding_t3_model)

### CHECK MODEL ASSUMPTIONS ###

#check assumptions for models with statistical significance
subjective_team_performance_bonding_t3_mlm = c(subjective_team_performance_bonding_t3_model)
subjective_team_performance_bonding_t3_mlm_dir_names = c("subjective_team_performance_bonding_t3_model")
predictors = c("Q3.12.2TExpect")
predictors_integers = c("Q3.12.2TExpect")
predictor_names = c("Team Performance Relative to Expectations")
outcomes = c("bonding_t3")
level_two_variable = "TeamIDVar"
level_one_variable = "Q3.12.1IExpect"

#run assumption checks on data with complete cases for relavant variables
complete_case_subjective_team_performance_bonding_t3 = dat[complete.cases(dat[ , c("bonding_t3", "Sex", "Q3.12.2TExpect")]), ]
all_data = list(complete_case_subjective_team_performance_bonding_t3)

#do assumption checks for each model
check_assumptions_main_models(code_dir, subjective_team_performance_bonding_t3_mlm, subjective_team_performance_bonding_t3_mlm_dir_names,
                              predictors, predictors_integers, predictor_names, outcomes, level_two_variable, level_one_variable, all_data)

################################################################################################################################################

### RECEIVED SUPPORT PREDICTS BONDING (T3) ###

#plot the relationships by individual
plot(dat$received_support_t3_emotional, dat$bonding_t3)
abline(lm(dat$bonding_t3 ~ dat$received_support_t3_emotional, data = dat))

plot(dat$received_support_t3_esteem, dat$bonding_t3)
abline(lm(dat$bonding_t3 ~ dat$received_support_t3_esteem, data = dat))

#simple models (more complex models failed to converge)
received_support_emotional_bonding_t3_model = lmer(bonding_t3 ~ received_support_t3_emotional + Sex + (1 | TeamIDVar), data = dat)
summary(received_support_emotional_bonding_t3_model)
#make_MS_Word_model_summary_table(received_support_emotional_bonding_t3_model)

#this model is singular
received_support_esteem_bonding_t3_model = lmer(bonding_t3 ~ received_support_t3_esteem + Sex + (1 | TeamIDVar), data = dat)
summary(received_support_esteem_bonding_t3_model)

#adds a weak prior to get an approximate Bayesian maximum a posteriori estimate that avoids singularity
received_support_esteem_bonding_t3_blme_model = blmer(bonding_t3 ~ received_support_t3_esteem + Sex + (1 | TeamIDVar), data = dat, cov.prior = invwishart(df = 5))
summary(received_support_esteem_bonding_t3_blme_model)

#get approximate p-values from a blme object
parameters::p_value(received_support_esteem_bonding_t3_blme_model)

#make table of the basic, 'lmer' model, but manually fill it in with the 'blmer' model
#make_MS_Word_model_summary_table(received_support_esteem_bonding_t3_model)
r.squaredGLMM(received_support_esteem_bonding_t3_blme_model)

### CHECK MODEL ASSUMPTIONS ###

#check assumptions for models with statistical significance
received_support_bonding_t3_mlm = c(received_support_emotional_bonding_t3_model, received_support_esteem_bonding_t3_blme_model)
received_support_bonding_t3_mlm_dir_names = c("received_support_emotional_bonding_t3_model", "received_support_esteem_bonding_t3_blme_model")
predictors = c("received_support_t3_emotional", "received_support_t3_esteem")
predictors_integers = c("received_support_t3_emotional", "received_support_t3_esteem")
predictor_names = c("Received Emotional Support", "Received Esteem Support")
outcomes = rep("bonding_t3", 2)
level_two_variable = "TeamIDVar"
level_one_variable = "Sex"

#run assumption checks on data with complete cases for relavant variables
complete_case_received_support_bonding_t3 = dat[complete.cases(dat[ , c("bonding_t3", "Sex", "received_support_t3_emotional")]), ]
all_data = list(complete_case_received_support_bonding_t3, complete_case_received_support_bonding_t3)

#do assumption checks for each model
check_assumptions_main_models(code_dir, received_support_bonding_t3_mlm, received_support_bonding_t3_mlm_dir_names,
                              predictors, predictors_integers, predictor_names, outcomes, level_two_variable, level_one_variable, all_data)

################################################################################################################################################

### INTERDEPENDENCE PREDICTS BONDING (T2) ###

#plot the relationships by individual
plot(dat$interdependence_t2, dat$bonding_t2)
abline(lm(dat$bonding_t2 ~ dat$interdependence_t2, data = dat))

#the model (more complex models failed to converge)
interdependence_bonding_t2_model = lmer(bonding_t2 ~ interdependence_t2 + Sex + (1 | TeamIDVar), data = dat)
summary(interdependence_bonding_t2_model)
#make_MS_Word_model_summary_table(interdependence_bonding_t2_model)

### CHECK MODEL ASSUMPTIONS ###

#check assumptions for models with statistical significance
interdependence_t2_bonding_t2_mlm = c(interdependence_bonding_t2_model)
interdependence_t2_bonding_t2_mlm_dir_names = c("interdependence_bonding_t2_model")
predictors = c("interdependence_t2")
predictors_integers = c("interdependence_t2")
predictor_names = c("Behavioural Interdependence")
outcomes = c("bonding_t2")
level_two_variable = "TeamIDVar"
level_one_variable = "Sex"

#run assumption checks on data with complete cases for relavant variables
complete_case_interdependence_t2_bonding_t2 = dat[complete.cases(dat[ , c("bonding_t2", "Sex", "interdependence_t2")]), ]
all_data = list(complete_case_interdependence_t2_bonding_t2)

#do assumption checks for each model
check_assumptions_main_models(code_dir, interdependence_t2_bonding_t2_mlm, interdependence_t2_bonding_t2_mlm_dir_names,
                              predictors, predictors_integers, predictor_names, outcomes, level_two_variable, level_one_variable, all_data)

################################################################################################################################################

### PERCEIVED SUPPORT PREDICTS BONDING (T2) ###

#plot the relationships by individual
plot(dat$perceived_support_t2_emotional, dat$bonding_t2)
abline(lm(dat$bonding_t2 ~ dat$perceived_support_t2_emotional, data = dat))

plot(dat$perceived_support_t2_esteem, dat$bonding_t2)
abline(lm(dat$bonding_t2 ~ dat$perceived_support_t2_esteem, data = dat))

#the model (more complex models failed to converge)
perceived_support_emotional_bonding_t2_model = lmer(bonding_t2 ~ perceived_support_t2_emotional + Sex + (1 | TeamIDVar), data = dat)
summary(perceived_support_emotional_bonding_t2_model)
#make_MS_Word_model_summary_table(perceived_support_emotional_bonding_t2_model)

perceived_support_esteem_bonding_t2_model = lmer(bonding_t2 ~ perceived_support_t2_esteem + Sex + (1 | TeamIDVar), data = dat)
summary(perceived_support_esteem_bonding_t2_model)
#make_MS_Word_model_summary_table(perceived_support_esteem_bonding_t2_model)

### CHECK MODEL ASSUMPTIONS ###

#check assumptions for models with statistical significance
support_t2_bonding_t2_mlm = c(perceived_support_emotional_bonding_t2_model, perceived_support_esteem_bonding_t2_model)
support_t2_bonding_t2_mlm_dir_names = c("perceived_support_emotional_bonding_t2_model", "perceived_support_esteem_bonding_t2_model")
predictors = c("perceived_support_t2_emotional", "perceived_support_t2_esteem")
predictors_integers = c("perceived_support_t2_emotional", "perceived_support_t2_esteem")
predictor_names = c("Perceived Emotional Support", "Perceived Esteem Support")
outcomes = rep("bonding_t2", 2)
level_two_variable = "TeamIDVar"
level_one_variable = "Sex"

#run assumption checks on data with complete cases for relavant variables
complete_case_support_t2_bonding_t2 = dat[complete.cases(dat[ , c("bonding_t2", "Sex", "perceived_support_t2_emotional")]), ]
all_data = list(complete_case_support_t2_bonding_t2, complete_case_support_t2_bonding_t2)

#do assumption checks for each model
check_assumptions_main_models(code_dir, support_t2_bonding_t2_mlm, support_t2_bonding_t2_mlm_dir_names,
                              predictors, predictors_integers, predictor_names, outcomes, level_two_variable, level_one_variable, all_data)

################################################################################################################################################

### BONDING AND PERCEIVED SOCIAL SUPPORT (T2) PREDICT PHYSICAL DISCOMFORT ###

#plot the relationships by individual
plot(dat$bonding_t2, dat$phys_diff_t3)
abline(lm(dat$phys_diff_t3 ~ dat$bonding_t2, data = dat))

#the model (more complex models failed to converge)
bonding_t2_phys_diff_t3_model = lmer(phys_diff_t3 ~ bonding_t2 + Sex + Q3.6.3Effort + (1 | TeamIDVar), data = dat)
summary(bonding_t2_phys_diff_t3_model)
#make_MS_Word_model_summary_table(bonding_t2_phys_diff_t3_model)

### ### ###

#plot the relationships by individual
plot(dat$perceived_support_t2_emotional, dat$phys_diff_t3)
abline(lm(dat$phys_diff_t3 ~ dat$perceived_support_t2_emotional, data = dat))

#the model (more complex models failed to converge)
perceived_support_t2_emotional_phys_diff_t3_model = lmer(phys_diff_t3 ~ perceived_support_t2_emotional + Sex + Q3.6.3Effort + (1 | TeamIDVar), data = dat)
summary(perceived_support_t2_emotional_phys_diff_t3_model)
#make_MS_Word_model_summary_table(perceived_support_t2_emotional_phys_diff_t3_model)

### ### ###

#plot the relationships by individual
plot(dat$perceived_support_t2_esteem, dat$phys_diff_t3)
abline(lm(dat$phys_diff_t3 ~ dat$perceived_support_t2_esteem, data = dat))

#the model (more complex models failed to converge)
perceived_support_t2_esteem_phys_diff_t3_model = lmer(phys_diff_t3 ~ perceived_support_t2_esteem + Sex + Q3.6.3Effort + (1 | TeamIDVar), data = dat)
summary(perceived_support_t2_esteem_phys_diff_t3_model)
#make_MS_Word_model_summary_table(perceived_support_t2_esteem_phys_diff_t3_model)

################################################################################################################################################

### EXPERIENCED INTERDEPENDENCE PREDICTS BONDING CHANGE (T2 to T3) ###

#plot the relationships by individual
plot(dat$experienced_interdependence_t3, dat$bonding_change_t2_t3)
abline(lm(dat$bonding_change_t2_t3 ~ dat$experienced_interdependence_t3, data = dat))

#the model (more complex models failed to converge)
experienced_interdependence_bonding_change_model = lmer(bonding_change_t2_t3 ~ experienced_interdependence_t3 + Sex + (1 | TeamIDVar), data = dat)
summary(experienced_interdependence_bonding_change_model)
#make_MS_Word_model_summary_table(experienced_interdependence_bonding_change_model)

### CHECK MODEL ASSUMPTIONS ###

#check model assumptions
experienced_interdependence_bonding_change_t2_t3_mlm = c(experienced_interdependence_bonding_change_model)
experienced_interdependence_bonding_change_t2_t3_mlm_dir_names = c("experienced_interdependence_bonding_change_model")
predictors = c("experienced_interdependence_t3")
predictors_integers = c("experienced_interdependence_t3")
predictor_names = c("Experienced Interdependence")
outcomes = c("bonding_change_t2_t3")
level_two_variable = "TeamIDVar"
level_one_variable = "Sex"

#run assumption checks on data with complete cases for relavant variables
complete_case_experienced_interdependence_bonding_change_t2_t3 = dat[complete.cases(dat[ , c("bonding_change_t2_t3", "Sex", "experienced_interdependence_t3")]), ]
all_data = list(complete_case_experienced_interdependence_bonding_change_t2_t3)

#do assumption checks for each model
check_assumptions_main_models(code_dir, experienced_interdependence_bonding_change_t2_t3_mlm, experienced_interdependence_bonding_change_t2_t3_mlm_dir_names,
                              predictors, predictors_integers, predictor_names, outcomes, level_two_variable, level_one_variable, all_data)

################################################################################################################################################

### PHYSICAL DISCOMFORT PREDICTS BONDING CHANGE (T2 to T3) ###

#plot the relationships by individual
plot(dat$phys_diff_t3, dat$bonding_change_t2_t3)
abline(lm(dat$bonding_change_t2_t3 ~ dat$phys_diff_t3, data = dat))

#the model (more complex models failed to converge)
discomfort_bonding_change_model = lmer(bonding_change_t2_t3 ~ phys_diff_t3 + Sex + (1 | TeamIDVar), data = dat)
summary(discomfort_bonding_change_model)
#make_MS_Word_model_summary_table(discomfort_bonding_change_model)

################################################################################################################################################

### BONDING (T3) PREDICTS PERFORMANCE SATISFACTION ###

#plot the relationships by individual
plot(dat$bonding_t3, dat$peform_satisfaction_t3)
abline(lm(dat$peform_satisfaction_t3 ~ dat$bonding_t3, data = dat))

#this model is singular
bonding_t3_peform_satisfaction_model = lmer(peform_satisfaction_t3 ~ bonding_t3 + Sex + (1 | TeamIDVar), data = dat)
summary(bonding_t3_peform_satisfaction_model)

#adds a weak prior to get an approximate Bayesian maximum a posteriori estimate that avoids singularity
bonding_t3_peform_satisfaction_blme_model = blmer(peform_satisfaction_t3 ~ bonding_t3 + Sex + (1 | TeamIDVar), data = dat, cov.prior = invwishart(df = 5))
summary(bonding_t3_peform_satisfaction_blme_model)

#get approximate p-values from a blme object
parameters::p_value(bonding_t3_peform_satisfaction_blme_model)

#make table of the basic, 'lmer' model, but manually fill it in with the 'blmer' model
#make_MS_Word_model_summary_table(bonding_t3_peform_satisfaction_blme_model)
r.squaredGLMM(bonding_t3_peform_satisfaction_blme_model)

### CHECK MODEL ASSUMPTIONS ###

#check model assumptions
peform_satisfaction_bonding_t3_mlm = c(bonding_t3_peform_satisfaction_blme_model)
peform_satisfaction_bonding_t3_mlm_dir_names = c("bonding_t3_peform_satisfaction_blme_model")
predictors = c("bonding_t3")
predictors_integers = c("bonding_t3")
predictor_names = c("Bonding (T3)")
outcomes = c("peform_satisfaction_t3")
level_two_variable = "TeamIDVar"
level_one_variable = "Sex"

#run assumption checks on data with complete cases for relavant variables
complete_case_peform_satisfaction_bonding_t3 = dat[complete.cases(dat[ , c("bonding_t3", "Sex", "peform_satisfaction_t3")]), ]
all_data = list(complete_case_peform_satisfaction_bonding_t3)

#do assumption checks for each model
check_assumptions_main_models(code_dir, peform_satisfaction_bonding_t3_mlm, peform_satisfaction_bonding_t3_mlm_dir_names,
                              predictors, predictors_integers, predictor_names, outcomes, level_two_variable, level_one_variable, all_data)

################################################################################################################################################

### INTERACTION BETWEEN PERCEPTIONS OF PHYSICAL DISCOMFORT AND EXPERIENCED INTERDEPENDENCE ON BONDING (T3) ###

library(blme)

#simple model (this model is singular)
interdependence_and_discomfort_on_bonding = lmer(bonding_t3 ~ experienced_interdependence_t3*phys_diff_t3 + Sex + (1 | TeamIDVar), data = dat)
summary(interdependence_and_discomfort_on_bonding)

#adds a weak prior to get an approximate Bayesian maximum a posteriori estimate that avoids singularity
interdependence_and_discomfort_on_bonding_blme_model = blmer(bonding_t3 ~ experienced_interdependence_t3*phys_diff_t3 + Sex + (1 | TeamIDVar), data = dat, cov.prior = invwishart(df = 5))
summary(interdependence_and_discomfort_on_bonding_blme_model)

#get approximate p-values from a blme object
parameters::p_value(interdependence_and_discomfort_on_bonding_blme_model)

#make table of the basic, 'lmer' model, but manually fill it in with the 'blmer' model
#make_MS_Word_model_summary_table(interdependence_and_discomfort_on_bonding)
r.squaredGLMM(interdependence_and_discomfort_on_bonding_blme_model)

################################################################################################################################################

### INTERACTIONS BETWEEN PERCEPTIONS OF PHYSICAL DISCOMFORT AND RECEIVED SUPPORT (BOTH EMOTIONAL AND ESTEEM - SEPARATE MODELS) ON BONDING ###

#simple model
received_support_emotional_and_discomfort_on_bonding = lmer(bonding_t3 ~ received_support_t3_emotional*phys_diff_t3 + Sex + (1 | TeamIDVar), data = dat)
summary(received_support_emotional_and_discomfort_on_bonding)
#make_MS_Word_model_summary_table(received_support_emotional_and_discomfort_on_bonding)

#simple model (this model is singular)
received_support_esteem_and_discomfort_on_bonding = lmer(bonding_t3 ~ received_support_t3_esteem*phys_diff_t3 + Sex + (1 | TeamIDVar), data = dat)
summary(received_support_esteem_and_discomfort_on_bonding)

#adds a weak prior to get an approximate Bayesian maximum a posteriori estimate that avoids singularity
received_support_esteem_and_discomfort_on_bonding_blme_model = blmer(bonding_t3 ~ received_support_t3_esteem*phys_diff_t3 + Sex + (1 | TeamIDVar), data = dat, cov.prior = invwishart(df = 5))
summary(received_support_esteem_and_discomfort_on_bonding_blme_model)

#get approximate p-values from a blme object
parameters::p_value(received_support_esteem_and_discomfort_on_bonding_blme_model)

#make table of the basic, 'lmer' model, but manually fill it in with the 'blmer' model
#make_MS_Word_model_summary_table(received_support_esteem_and_discomfort_on_bonding)
r.squaredGLMM(received_support_emotional_and_discomfort_on_bonding)
r.squaredGLMM(received_support_esteem_and_discomfort_on_bonding_blme_model)

################################################################################################################################################

### COLLECTIVE EFFICACY MEDIATES THE RELATIONSHIP BETWEEN PERCEIVED SUPPORT (BOTH EMOTIONAL AND ESTEEM - SEPARATE MODELS) AND THREAT APPRAISAL ###

library(mediation)
set.seed(2014)
detach("package:lmerTest", unload=TRUE)
library(MuMIn)

#mediation relationship: perceived_support_t2_emotional -> collective_efficacy_t2 -> threat_t2_emotion

med_fit_emos_ce_emot = lmer(collective_efficacy_t2 ~ perceived_support_t2_emotional + Sex + (1 | TeamIDVar), data = dat)
summary(med_fit_emos_ce_emot)
#make_MS_Word_model_summary_table(med_fit_emos_ce_emot)

out_fit_emos_ce_emot = lmer(threat_t2_emotion ~ collective_efficacy_t2 + perceived_support_t2_emotional + Sex + + (1 | TeamIDVar), data = dat)
summary(out_fit_emos_ce_emot)
#make_MS_Word_model_summary_table(out_fit_emos_ce_emot)

med_out_emos_ce_emot = mediate(med_fit_emos_ce_emot, out_fit_emos_ce_emot, treat = "perceived_support_t2_emotional", mediator = "collective_efficacy_t2", covariates = "Sex", sims = 1000, boot.ci.type = "bca")
summary(med_out_emos_ce_emot)

### ### ###

#mediation relationship: perceived_support_t2_esteem -> collective_efficacy_t2 -> threat_t2_emotion

med_fit_ests_ce_emot = lmer(collective_efficacy_t2 ~ perceived_support_t2_esteem + Sex + (1 | TeamIDVar), data = dat)
summary(med_fit_ests_ce_emot)
#make_MS_Word_model_summary_table(med_fit_ests_ce_emot)

out_fit_ests_ce_emot = lmer(threat_t2_emotion ~ perceived_support_t2_esteem + collective_efficacy_t2 + Sex + (1 | TeamIDVar), data = dat)
summary(out_fit_ests_ce_emot)
#make_MS_Word_model_summary_table(out_fit_ests_ce_emot)

med_out_ests_ce_emot = mediate(med_fit_ests_ce_emot, out_fit_ests_ce_emot, treat = "perceived_support_t2_esteem", mediator = "collective_efficacy_t2", covariates = "Sex", sims = 1000, boot.ci.type = "bca")
summary(med_out_ests_ce_emot)

### CHECK MODEL ASSUMPTIONS ###

#check model assumptions for the model with significant direct effect
out_fit_ests_collective_efficacy_esteem_support_threat_mlm = c(out_fit_ests_ce_emot)
out_fit_ests_collective_efficacy_esteem_support_threat_mlm_dir_names = c("out_fit_ests_collective_efficacy_esteem_support_threat_model")
predictors = c("perceived_support_t2_esteem")
predictors_integers = c("perceived_support_t2_esteem")
predictor_names = c("Perceived Esteem Support")
outcomes = c("threat_t2_emotion")
level_two_variable = "TeamIDVar"
level_one_variable = "collective_efficacy_t2"

#run assumption checks on data with complete cases for relavant variables
complete_case_collective_efficacy_esteem_support_threat = dat[complete.cases(dat[ , c("threat_t2_emotion", "collective_efficacy_t2", "perceived_support_t2_esteem")]), ]
all_data = list(complete_case_collective_efficacy_esteem_support_threat)

#do assumption checks for each model
check_assumptions_main_models(code_dir, out_fit_ests_collective_efficacy_esteem_support_threat_mlm, out_fit_ests_collective_efficacy_esteem_support_threat_mlm_dir_names,
                              predictors, predictors_integers, predictor_names, outcomes, level_two_variable, level_one_variable, all_data)

################################################################################################################################################

### GRAPH SETTINGS ###

library(ggplot2)

#graph theme
point_size = 1
header_size = 12
axis_size = 12

#create a theme for no legend
base_theme = theme(text=element_text(size=header_size, family="Arial"),
                   axis.text.x = element_text(color='black',size=axis_size*.9),
                   axis.text.y = element_text(color='black',size=axis_size*.9),
                   panel.background = element_rect(colour = '#FFFFFF',fill='#FFFFFF'),
                   axis.line = element_line(size = .25, colour = "black"),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_line(colour = '#e7e7e7',size=.25),
                   plot.margin = margin(1, 1, 1, 1, "cm"),
                   legend.key = element_blank(),
                   legend.background = element_blank(),
                   legend.title = element_text(face="bold"),
                   axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold"),
                   axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
                   plot.title = element_text(lineheight=1, face="bold"))

#create a theme for no legend
legend_theme = theme(text=element_text(size=header_size, family="Arial"),
                     axis.text.x = element_text(color='black',size=axis_size*.9),
                     axis.text.y = element_text(color='black',size=axis_size*.9),
                     panel.background = element_rect(colour = '#FFFFFF',fill='#FFFFFF'),
                     axis.line = element_line(size = .25, colour = "black"),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(colour = '#e7e7e7',size=.25),
                     plot.margin = margin(1, 1, 1, 1, "cm"),
#                     legend.key = element_blank(),
#                     legend.background = element_blank(),
                     legend.title = element_text(face="bold"),
                     axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold"),
                     axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
                     plot.title = element_text(lineheight=1, face="bold"))

#create a theme with space at the top for additional labels
top_space_theme = base_theme + theme(plot.margin = margin(2, 1, 1, 1, "cm"))

#create a theme with space at the top and right for additional labels
top_and_right_space_theme = base_theme + theme(plot.margin = margin(2, 2, 1, 1, "cm"))

#set current working directory to the one this script is in (when in RStudio)
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)

################################################################################################################################################

### PERFORMANCE SATISFACTION PREDICTS WELLBEING CHANGE (T1 TO T4) ###

library(sjPlot)
library(grid)

#get the data to be plotted
plotted_model_performance_sat_wellbeing_change = plot_model(peform_satisfaction_t3_wellbeing_change_blme_model, type = 'pred',
                                            terms = c('peform_satisfaction_t3'))[["data"]]

plot_dat_peform_satisfaction_t3_wellbeing_change = dat[, c("peform_satisfaction_t3", "wellbeing_change")]
plot_dat_peform_satisfaction_t3_wellbeing_change = na.omit(plot_dat_peform_satisfaction_t3_wellbeing_change)

#low, medium satisfaction, and high Performance Satisfaction 
low_sat = min(plot_dat_peform_satisfaction_t3_wellbeing_change$peform_satisfaction_t3, na.rm = TRUE)
mid_sat = mean(plot_dat_peform_satisfaction_t3_wellbeing_change$peform_satisfaction_t3, na.rm = TRUE)
high_sat = max(plot_dat_peform_satisfaction_t3_wellbeing_change$peform_satisfaction_t3, na.rm = TRUE)
all_lines = c(low_sat, mid_sat, high_sat)

#plot the data 
plot_ps_wc = ggplot() + 
             geom_line(data = plotted_model_performance_sat_wellbeing_change, aes(x = x, y = predicted)) +
             geom_ribbon(data = plotted_model_performance_sat_wellbeing_change, aes(x = x, ymax = conf.high, ymin = conf.low), alpha = 0.15, colour = NA) + 
             geom_point(data = plot_dat_peform_satisfaction_t3_wellbeing_change, aes(x = peform_satisfaction_t3, y = wellbeing_change), alpha = .5, na.rm = TRUE) +
             scale_x_continuous(limits = c(low_sat, round(high_sat)), breaks = c(-3, -2, -1, 0, 1), 
                                labels = c("-3", "-2", "-1", "0", "1")) +
             scale_y_continuous(limits = c(min(plot_dat_peform_satisfaction_t3_wellbeing_change$wellbeing_change), 2)) +
             ylab("Change in Well-being (T1 to T4)") + 
             xlab("Performance Satisfaction (T3)") + 
             base_theme

ggsave("../outputs/main_analysis_plots/wellbeing_change_by_performance_satisfaction.jpg", plot_ps_wc, width = 7.5, height = 7.5)

################################################################################################################################################

### BONDING (T3) PREDICTS WELLBEING CHANGE (T1 TO T4) ###

#get the data to be plotted
plotted_model_bonding_t3_wellbeing_change = plot_model(bonding_t3_wellbeing_change_blme_model, type = 'pred', terms = c('bonding_t3'))[["data"]]

plot_dat_bonding_t3_wellbeing_change = dat[, c("bonding_t3", "wellbeing_change")]
plot_dat_bonding_t3_wellbeing_change = na.omit(plot_dat_bonding_t3_wellbeing_change)

#low, medium, and high Bonding
low_bond = min(plot_dat_bonding_t3_wellbeing_change$bonding_t3, na.rm = TRUE)
mid_bond = mean(plot_dat_bonding_t3_wellbeing_change$bonding_t3, na.rm = TRUE)
high_bond = max(plot_dat_bonding_t3_wellbeing_change$bonding_t3, na.rm = TRUE)
all_lines = c(low_bond, mid_bond, high_bond)

#plot the data 
plot_bt3_wc = ggplot() + 
              geom_line(data = plotted_model_bonding_t3_wellbeing_change, aes(x = x, y = predicted)) +
              geom_ribbon(data = plotted_model_bonding_t3_wellbeing_change, aes(x = x, ymax = conf.high, ymin = conf.low), alpha = 0.15, colour = NA) + 
              geom_point(data = plot_dat_bonding_t3_wellbeing_change, aes(x = bonding_t3, y = wellbeing_change), alpha = .5, na.rm = TRUE) +
              scale_x_continuous(limits = c(round(low_bond), high_bond), breaks = c(-3, -2, -1, 0, 1), 
                                 labels = c("-3", "-2", "-1", "0", "1")) +
              scale_y_continuous(limits = c(min(plot_dat_peform_satisfaction_t3_wellbeing_change$wellbeing_change), 2)) +
              ylab("Change in Well-being (T1 to T4)") + 
              xlab("Bonding (T3)") + 
              base_theme

ggsave("../outputs/main_analysis_plots/wellbeing_change_by_bonding_t3.jpg", plot_bt3_wc, width = 7.5, height = 7.5)

################################################################################################################################################

### COMBINE PREDICTORS OF WELLBEING CHANGE PLOTS ###

library(ggpubr)

#combine the plots
combined_wc_plots = ggarrange(plot_ps_wc, plot_bt3_wc, ncol = 2, labels = c("(a)","(b)"), font.label = list(color="black",size = axis_size))

ggsave("../outputs/main_analysis_plots/combined_wellbeing_change_plots.jpg", combined_wc_plots, width = 12.5, height = 7.5)

################################################################################################################################################

### PHYSICAL DISCOMFORT PREDICTS BONDING (T3) ###

#get the data to be plotted
plotted_model_discomfort_bonding_t3 = plot_model(discomfort_bonding_t3_model, type = 'pred', terms = c('phys_diff_t3'))[["data"]]

plot_dat_discomfort_bonding_t3 = dat[, c("bonding_t3", "phys_diff_t3")]
plot_dat_discomfort_bonding_t3 = na.omit(plot_dat_discomfort_bonding_t3)

#low, medium, and high Physical Discomfort
low_pd = min(plot_dat_discomfort_bonding_t3$phys_diff_t3, na.rm = TRUE)
mid_pd = mean(plot_dat_discomfort_bonding_t3$phys_diff_t3, na.rm = TRUE)
high_pd = max(plot_dat_discomfort_bonding_t3$phys_diff_t3, na.rm = TRUE)
all_lines = c(low_pd, mid_pd, high_pd)

#plot the data 
plot_pd_bt3 = ggplot() + 
              geom_line(data = plotted_model_discomfort_bonding_t3, aes(x = x, y = predicted)) +
              geom_ribbon(data = plotted_model_discomfort_bonding_t3, aes(x = x, ymax = conf.high, ymin = conf.low), alpha = 0.15, colour = NA) + 
              geom_point(data = plot_dat_discomfort_bonding_t3, aes(x = phys_diff_t3, y = bonding_t3), alpha = .5, na.rm = TRUE) +
              scale_x_continuous(limits = c(round(low_pd), round(high_pd)), breaks = c(-3, low_pd, -2, -1, 0, mid_pd, 1, high_pd, 2), 
                                 labels = c("-3", "", "-2", "-1", "0", "", "1", "", "2")) +
              ylab("Bonding (T3)\n(component score)") + 
              xlab("Physical Discomfort\n(component score)") + 
              base_theme

ggsave("../outputs/main_analysis_plots/bonding_t3_by_physical_discomfort.jpg", plot_pd_bt3, width = 7.5, height = 7.5)

################################################################################################################################################

### EXPERIENCED INTERDEPENDENCE PREDICTS BONDING (T3) ###

#get the data to be plotted
plotted_model_exp_ind_bonding_t3 = plot_model(experienced_interdependence_bonding_t3_blme_model, type = 'pred', terms = c('experienced_interdependence_t3'))[["data"]]

plot_dat_exp_ind_bonding_t3 = dat[, c("bonding_t3", "experienced_interdependence_t3")]
plot_dat_exp_ind_bonding_t3 = na.omit(plot_dat_exp_ind_bonding_t3)

#low, medium, and high Experienced Interdependence
low_ei = min(plot_dat_exp_ind_bonding_t3$experienced_interdependence_t3, na.rm = TRUE)
mid_ei = mean(plot_dat_exp_ind_bonding_t3$experienced_interdependence_t3, na.rm = TRUE)
high_ei = max(plot_dat_exp_ind_bonding_t3$experienced_interdependence_t3, na.rm = TRUE)
all_lines = c(low_ei, mid_ei, high_ei)

#plot the data 
plot_ei_bt3 = ggplot() + 
              geom_line(data = plotted_model_exp_ind_bonding_t3, aes(x = x, y = predicted)) +
              geom_ribbon(data = plotted_model_exp_ind_bonding_t3, aes(x = x, ymax = conf.high, ymin = conf.low), alpha = 0.15, colour = NA) + 
              geom_point(data = plot_dat_exp_ind_bonding_t3, aes(x = experienced_interdependence_t3, y = bonding_t3), alpha = .5, na.rm = TRUE) +
              scale_x_continuous(limits = c(round(low_ei), round(high_ei)), breaks = c(-5, low_ei, -4, -3, -2, -1, 0, mid_ei, high_ei, 1), 
                                 labels = c("-5", "", "-4", "-3", "-2", "-1", "0", "", "", "1")) +
              ylab("Bonding (T3)\n(component score)") + 
              xlab("Experienced Interdependence\n(component score)") + 
              base_theme

ggsave("../outputs/main_analysis_plots/bonding_t3_by_experienced_interdependence.jpg", plot_ei_bt3, width = 7.5, height = 7.5)

################################################################################################################################################

### SUBJECTIVE RATINGS OF TEAM PERFORMANCE RELATIVE TO EXPECTATIONS PREDICT BONDING (T3) ###

#get the data to be plotted
plotted_model_team_perf_bonding_t3 = plot_model(subjective_team_performance_bonding_t3_model, type = 'pred', terms = c('Q3.12.2TExpect'))[["data"]]

plot_dat_team_perf_bonding_t3 = dat[, c("bonding_t3", "Q3.12.2TExpect")]
plot_dat_team_perf_bonding_t3 = na.omit(plot_dat_team_perf_bonding_t3)

#low, medium, and high subjective ratings of Team Performance Relative to Expectations
low_tp = min(plot_dat_team_perf_bonding_t3$Q3.12.2TExpect, na.rm = TRUE)
mid_tp = mean(plot_dat_team_perf_bonding_t3$Q3.12.2TExpect, na.rm = TRUE)
high_tp = max(plot_dat_team_perf_bonding_t3$Q3.12.2TExpect, na.rm = TRUE)
all_lines = c(low_tp, mid_tp, high_tp)

#plot the data 
plot_tp_bt3 = ggplot() + 
              geom_line(data = plotted_model_team_perf_bonding_t3, aes(x = x, y = predicted)) +
              geom_ribbon(data = plotted_model_team_perf_bonding_t3, aes(x = x, ymax = conf.high, ymin = conf.low), alpha = 0.15, colour = NA) + 
              geom_point(data = plot_dat_team_perf_bonding_t3, aes(x = Q3.12.2TExpect, y = bonding_t3), alpha = .5, na.rm = TRUE) +
              scale_x_continuous(limits = c(round(low_tp), round(high_tp)), breaks = c(2, 3, 4, 5, 6, 7, 8, 9), 
                                 labels = c("2", "3", "4", "5", "6", "7", "8", "9")) +
              ylab("Bonding (T3)\n(component score)") + 
              xlab("Team Performance Relative to Expectations\n(9-point Likert scale)") + 
              base_theme

ggsave("../outputs/main_analysis_plots/bonding_t3_by_subjective_team_performance.jpg", plot_tp_bt3, width = 7.5, height = 7.5)

################################################################################################################################################

### RECEIVED EMOTIONAL SUPPORT PREDICTS BONDING (T3) ###

#get the data to be plotted
plotted_model_received_support_emotional_bonding_t3 = plot_model(received_support_emotional_bonding_t3_model, type = 'pred',
                                                       terms = c('received_support_t3_emotional'))[["data"]]

plot_dat_received_support_emotional_bonding_t3 = dat[, c("bonding_t3", "received_support_t3_emotional")]
plot_dat_received_support_emotional_bonding_t3 = na.omit(plot_dat_received_support_emotional_bonding_t3)

#low, medium, and high Received Emotional Support
low_emo_s = min(plot_dat_received_support_emotional_bonding_t3$received_support_t3_emotional, na.rm = TRUE)
mid_emo_s = mean(plot_dat_received_support_emotional_bonding_t3$received_support_t3_emotional, na.rm = TRUE)
high_emo_s = max(plot_dat_received_support_emotional_bonding_t3$received_support_t3_emotional, na.rm = TRUE)
all_lines = c(low_emo_s, mid_emo_s, high_emo_s)

#plot the data 
plot_emo_s_bt3 = ggplot() + 
                 geom_line(data = plotted_model_received_support_emotional_bonding_t3, aes(x = x, y = predicted)) +
                 geom_ribbon(data = plotted_model_received_support_emotional_bonding_t3, aes(x = x, ymax = conf.high, ymin = conf.low), alpha = 0.15, colour = NA) + 
                 geom_point(data = plot_dat_received_support_emotional_bonding_t3, aes(x = received_support_t3_emotional, y = bonding_t3), alpha = .5, na.rm = TRUE) +
                 scale_x_continuous(limits = c(round(low_emo_s), round(high_emo_s)), breaks = c(1, 2, 3, 4, 5), 
                                    labels = c("1", "2", "3", "4", "5")) +
                 ylab("Bonding (T3)\n(component score)") + 
                 xlab("Received Emotional Support\n(5-point Likert Scale question mean)") + 
                 base_theme

ggsave("../outputs/main_analysis_plots/bonding_t3_by_received_emotional_support.jpg", plot_emo_s_bt3, width = 7.5, height = 7.5)

################################################################################################################################################

### RECEIVED ESTEEM SUPPORT PREDICTS BONDING (T3) ###

#get the data to be plotted
plotted_model_received_support_esteem_bonding_t3 = plot_model(received_support_esteem_bonding_t3_blme_model, type = 'pred',
                                                                 terms = c('received_support_t3_esteem'))[["data"]]

plot_dat_received_support_esteem_bonding_t3 = dat[, c("bonding_t3", "received_support_t3_esteem")]
plot_dat_received_support_esteem_bonding_t3 = na.omit(plot_dat_received_support_esteem_bonding_t3)

#low, medium, and high Received Esteem Support
low_est_s = min(plot_dat_received_support_esteem_bonding_t3$received_support_t3_esteem, na.rm = TRUE)
mid_est_s = mean(plot_dat_received_support_esteem_bonding_t3$received_support_t3_esteem, na.rm = TRUE)
high_est_s = max(plot_dat_received_support_esteem_bonding_t3$received_support_t3_esteem, na.rm = TRUE)
all_lines = c(low_est_s, mid_est_s, high_est_s)

#plot the data 
plot_est_s_bt3 = ggplot() + 
                 geom_line(data = plotted_model_received_support_esteem_bonding_t3, aes(x = x, y = predicted)) +
                 geom_ribbon(data = plotted_model_received_support_esteem_bonding_t3, aes(x = x, ymax = conf.high, ymin = conf.low), alpha = 0.15, colour = NA) + 
                 geom_point(data = plot_dat_received_support_esteem_bonding_t3, aes(x = received_support_t3_esteem, y = bonding_t3), alpha = .5, na.rm = TRUE) +
                 scale_x_continuous(limits = c(round(low_est_s), round(high_est_s)), breaks = c(1, 2, 3, 4, mid_est_s, 5), 
                                    labels = c("1", "2", "3", "4", "", "5")) +
                 scale_y_continuous(limits = c(-4, high_bond), breaks = c(-4, -3, -2, -1, 0, 1), 
                                    labels = c("-4", "-3", "-2", "-1", "0", "1")) +
                 ylab("Bonding (T3)\n(component score)") + 
                 xlab("Received Esteem Support\n(5-point Likert Scale question mean)") + 
                 base_theme

ggsave("../outputs/main_analysis_plots/bonding_t3_by_received_esteem_support.jpg", plot_est_s_bt3, width = 7.5, height = 7.5)

################################################################################################################################################

### BONDING (T3) PREDICTS PERFORMANCE SATISFACTION ###

#get the data to be plotted
plotted_model_bonding_t3_performance_sat = plot_model(bonding_t3_peform_satisfaction_blme_model, type = 'pred', terms = c('bonding_t3'))[["data"]]

plot_dat_bonding_t3_performance_sat = dat[, c("bonding_t3", "peform_satisfaction_t3")]
plot_dat_bonding_t3_performance_sat = na.omit(plot_dat_bonding_t3_performance_sat)

#low, medium, and high Bonding
low_bond = min(plot_dat_bonding_t3_performance_sat$bonding_t3, na.rm = TRUE)
mid_bond = mean(plot_dat_bonding_t3_performance_sat$bonding_t3, na.rm = TRUE)
high_bond = max(plot_dat_bonding_t3_performance_sat$bonding_t3, na.rm = TRUE)
all_lines_bonding = c(low_bond, mid_bond, high_bond)

#plot the data 
plot_bt3_ps = ggplot() + 
              geom_line(data = plotted_model_bonding_t3_performance_sat, aes(x = x, y = predicted)) +
              geom_ribbon(data = plotted_model_bonding_t3_performance_sat, aes(x = x, ymax = conf.high, ymin = conf.low), alpha = 0.15, colour = NA) + 
              geom_point(data = plot_dat_bonding_t3_performance_sat, aes(x = bonding_t3, y = peform_satisfaction_t3), alpha = .5, na.rm = TRUE) +
              scale_x_continuous(limits = c(min(plotted_model_bonding_t3_performance_sat$x), high_bond), breaks = c(-3, -2, -1, 0, 1), 
                                 labels = c("-3", "-2", "-1", "0", "1")) +
              ylab("Performance Satisfaction\n(component score)") + 
              xlab("Bonding (T3)\n(component score)") + 
              base_theme

ggsave("../outputs/main_analysis_plots/performance_satisfaction_by_bonding_t3.jpg", plot_bt3_ps, width = 7.5, height = 7.5)

################################################################################################################################################

### INTERACTIONS BETWEEN PERCEPTIONS OF PHYSICAL DISCOMFORT AND RECEIVED SUPPORT (BOTH EMOTIONAL AND ESTEEM - SEPARATE PLOTS) ON BONDING ###

#get the data to be plotted
plotted_model_received_support_esteem_and_discomfort_on_bonding = plot_model(received_support_esteem_and_discomfort_on_bonding_blme_model,
                                                                             type = 'pred', terms = c('received_support_t3_esteem', 'phys_diff_t3 [quart]'))[["data"]]

#set a color gradient
pd_cols = c("royalblue4", "springgreen4", "grey37", "darkorange", "red2")

#plot the data 
plot_rsest_pd_int_bonding = ggplot() +
                            geom_line(data = plotted_model_received_support_esteem_and_discomfort_on_bonding, aes(x = x, y = predicted, color = group)) + 
                            geom_ribbon(data = plotted_model_received_support_esteem_and_discomfort_on_bonding, 
                                        aes(x = x, ymax = conf.high, ymin = conf.low, fill = group), alpha = 0.15, linetype = 0) + 
                            ylab("Bonding (T3)\n(component score)") +
                            xlab("Received Esteem Support\n(5-point Likert Scale question mean)") +
                            scale_x_continuous(breaks = c(1, 2, 3, 4, 5), limits = c(1, 5)) +
                            scale_y_continuous(breaks = c(-4, -3, -2, -1, 0, 1), limits = c(-4, 1.5)) +
                            scale_fill_manual(name = "Physical Discomfort\n(quintile)", 
                                              labels = c("First (low)", "Second", "Third", "Fourth", "Fifth (high)"),
                                              values = pd_cols) +
                            scale_color_manual(name = "Physical Discomfort\n(quintile)", 
                                               labels = c("First (low)", "Second", "Third", "Fourth", "Fifth (high)"),
                                               values = pd_cols) +
                            legend_theme

ggsave("../outputs/main_analysis_plots/esteem_support_by_physical_discomfort_interaction_on_bonding.jpg", plot_rsest_pd_int_bonding, width = 10, height = 7.5)

### ### ###

#get the data to be plotted
plotted_model_received_support_emotional_and_discomfort_on_bonding = plot_model(received_support_emotional_and_discomfort_on_bonding,
                                                                                type = 'pred', terms = c('received_support_t3_emotional', 'phys_diff_t3 [quart]'))[["data"]]

#plot the data 
plot_rsemo_pd_int_bonding = ggplot() +
                            geom_line(data = plotted_model_received_support_emotional_and_discomfort_on_bonding, aes(x = x, y = predicted, color = group)) + 
                            geom_ribbon(data = plotted_model_received_support_emotional_and_discomfort_on_bonding, 
                            aes(x = x, ymax = conf.high, ymin = conf.low, fill = group), alpha = 0.15, linetype = 0) + 
                            scale_x_continuous(breaks = c(1, 2, 3, 4, 5), limits = c(1, 5)) +
                            scale_y_continuous(breaks = c(-4, -3, -2, -1, 0, 1), limits = c(-4, 1.5)) +
                            ylab("Bonding (T3)\n(component score)") +
                            xlab("Received Emotional Support\n(5-point Likert Scale question mean)") +
                            scale_fill_manual(name = "Physical Discomfort\n(quintile)", 
                                              labels = c("First (low)", "Second", "Third", "Fourth", "Fifth (high)"),
                                              values = pd_cols) +
                            scale_color_manual(name = "Physical Discomfort\n(quintile)", 
                            labels = c("First (low)", "Second", "Third", "Fourth", "Fifth (high)"),
                            values = pd_cols) +
                            legend_theme

ggsave("../outputs/main_analysis_plots/emotionaL_support_by_physical_discomfort_interaction_on_bonding.jpg", plot_rsemo_pd_int_bonding, width = 10, height = 7.5)
