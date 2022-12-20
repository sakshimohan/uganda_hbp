#############################################################
## Linear Programming Function to optimize Uganda's Health Benefits Package

## Created by: Sakshi Mohan; 13/12/20

## This file creates the linear constrained optimisation function to be used in
# scenario generation
#############################################################

# % Notes surrounded by "%" represent temporary code which should be substituted if a better way can be found to deal with the issue %
# Verification/browse code is indented and commented out surrounded by "^^"

##############################
# 0 - Load librairies
##############################
# Note the following packages need to be installed  readxl, lpSolveAPI, checkData, fmsb
library(readxl)
library(lpSolve)
library(tidyverse)
library(fmsb) # for radar chart
library(plyr)
library(dplyr)
library(ggplot2)
library(forcats) # to reorder plots
library(xtable) # for LaTeX tables
library(tidyr)
library(scales) # to formal axis labels
library(viridis) # load viridis colour palette

##############################
# 1 - Set Working Directory
##############################
setwd("C:/Users/sm2511/Dropbox/York/Research Projects/Uganda EHP/Analysis/repo/uganda_hbp/")

###################################
# 2 - Load and set up data for LPP
###################################
# Load epi/cost/CE dataset 
#****************************************************
df <- read_excel("2_data/hbp_data_clean_v2.xlsx", sheet = "data",col_names = TRUE,col_types=NULL,na="",skip=0)
# Load HR availability dataset
#****************************************************
df_hr <- read_excel("2_data/hbp_data_clean_v2.xlsx", sheet = "hr_constraint",col_names = TRUE,col_types=NULL,na="",skip=0)

# Set up dataframes
#****************************************************
colnames(df) = df[2,] # remove first two rows
df <- df[-c(1:2),]  # remove first two columns
df <- df[,c(1:32)]	# remove columns after 32
df <- na.omit(df) # drop rows containing missing values #df[!is.na(df$`DALYs averted per patient (Uganda)`)]

colnames(df_hr) = df_hr[1,] # remove first row

# Extract .csv versions of input data
write.csv(df, file = "3_processing/uganda_intervention_data.csv")
write.csv(df_hr, file = "3_processing/uganda_hr_data.csv")

# Set up HR constraint dataframes
#****************************************************
hr_minutes <- as.numeric(df_hr$'Total patient-facing time per year (minutes)'[2:9]) # number of minutes available per health worker cadre
hr_size <- as.numeric(df_hr$'Total staff'[2:9]) # size of health workforce

# Generate relevant lists from dataset
#****************************************************
# Rename columns
names(df)[names(df) == 'DALYs averted per patient (Uganda)'] <- 'dalys'
names(df)[names(df) == 'Average drugs and commodities cost (2019 USD)'] <- 'drugcost'
names(df)[names(df) == 'Coverage_2024'] <- 'maxcoverage' # Maximum feasible coverage in 2024 as per OneHealth Tool
names(df)[names(df) == 'Cost per case (Uganda) - 2019 USD'] <- 'fullcost'
names(df)[names(df) == 'Intervention'] <- 'intervention'
names(df)[names(df) == 'Cases_full_2020'] <- 'cases'
names(df)[names(df) == 'Code'] <- 'intcode'
names(df)[names(df) == 'Category'] <- 'category'

N <- length(df$dalys) # total number of interventions included in the analysis

# Convert columns to numeric
df <- df %>% mutate_at(c('drugcost', 'dalys', 'maxcoverage', 'fullcost', 'cases'), as.numeric)
str(df) # ^^ check format of all columns ^^	

###################################
# 3. Define customizable LPP/optimization function
###################################
find_optimal_package <- function(data.frame, # data on interventions 
                                 objective_input = "nethealth", # what is being maximised
                                 cet_input = 161, # chosen cost effectiveness threshold (only relevant if objective_input = "nethealth")
                                 drug_budget_input, # size of consumables budget
                                 drug_budget.scale = 1,  # use this to scale consumables budget up or down (1 -> no scaling applied)
                                 hr.scale,  # use this to scale health workforce size up or down individually fo each cadre (1 -> no scaling applied)
                                 use_feasiblecov_constraint = 1, # whether maximum feasible coverage constraints should be applied
                                 feascov_scale = 1, # use this to scale maximum feasible coverage constraints up or down (1 -> no scaling applied)
                                 compulsory_interventions = NULL, # list of "compulsory interventions" - to be included in package regardless of optimisation rule
                                 compcov_scale = 1, # use this to scale maximum feasible coverage constraints for compulsory interventions up or down (1 -> no scaling applied) - this is applied to maximum feasible coverage if use_feasiblecov_constraint = 1
                                 substitutes = NULL, # list of substitutable interventions 
                                 complements_nested = NULL, # List of complementary interventions
                                 task_shifting_pharm = 0) # whether task shifting is allowed (from pharmacists and nutrition officers to nurses)
{ 
  intervention <<- data.frame$intervention
  intcode <<- data.frame$intcode # list of intervention codes
  category <<- data.frame$category # program/category of intervention
  dalys <<- data.frame$dalys # Per person DALYs averted based on CE evidence
  drugcost <<- data.frame$drugcost #  Per person cost of drugs and commodities
  maxcoverage <<- data.frame$maxcoverage # Maximum possible coverage based on OneHealth Tool
  cases <<- data.frame$cases # Total number of cases based on OneHealth Tool
  fullcost <<- data.frame$fullcost # Full cost per patient based on CE evidence 
  hrneed <<- as.data.frame(apply(data.frame[,c(12:32)],2,as.numeric)) # Number of minutes of health worker time requires per intervention per person
  
  n <- length(dalys) # number of interventions included in the analysis
  
  ###################################
  # 3.1 Set up LPP
  ###################################
  
  # Objective - maximize DALYs or Net Health per person X Total number of cases X Coverage
  #****************************************************
  # Define net health
  cet <- cet_input
  nethealth <<- dalys - fullcost/cet
  
  # Define objective
  if (objective_input == 'nethealth'){
    objective <<- nethealth * cases
  }
  else if (objective_input == 'dalys'){
    objective <<- dalys * cases
  }
  else{
    print('ERROR: objective_input can take values dalys or nethealth')	
  }
  
  # Constraints - 1. Drug Budget, 2. HR Requirements
  #****************************************************
  # 1. Drug Budget
  #----------------
  cons_drug <<- drugcost * cases # Cost of drugs for the number of cases covered
  cons_drug.limit <<- drug_budget_input * drug_budget.scale
  cons_drug.limit_base <<- drug_budget_input # unscaled drug budget
  
  # 2. HR Constraints
  #---------------------
  hr_minutes_need <- hrneed * cases[row(hrneed)] # HR minutes required to deliver intervention to all cases in need
  
  # Update HR constraints so that nurses, pharmacists, medical officers, etc. represent joint constraints because the HR data
  # found for Uganda was not detailed enough 
  colnames(hr_minutes_need)
  medstaff <- hr_minutes_need[,1] + hr_minutes_need[,2]   # Medical officer + Clinical officer
  nursingstaff <- hr_minutes_need[,3] + hr_minutes_need[,4] + hr_minutes_need[,5] # Medical assistant + Nurse officer + Nurse midwife
  pharmstaff <- hr_minutes_need[,6] + hr_minutes_need[,7] + hr_minutes_need[,8] # Pharmacist + Pharmacist Technician + Pharmacist Assistant
  labstaff <- hr_minutes_need[,9] + hr_minutes_need[,10] + hr_minutes_need[,11] # Lab officer + Lab technician + Lab assistant
  # remove CHW
  dentalstaff <- hr_minutes_need[,13] + hr_minutes_need[,14] + hr_minutes_need[,15] # Dental officer + Dental therapist + Dental assistant
  mentalstaff <- hr_minutes_need[,16] # Mental health staff
  nutristaff <- hr_minutes_need[,17] # Nutrition staff
  diagstaff <- hr_minutes_need[,18] + hr_minutes_need[,19] + hr_minutes_need[,20] + hr_minutes_need[,21] # Radiographer + Radiography technician + Sonographer + Radiotherapist
  
  # Clean total minutes available per cadre  
  cons_hr.limit <- hr_minutes
  medstaffmins.limit <<- cons_hr.limit[1] 
  nursingstaffmins.limit <<- cons_hr.limit[2]
  pharmstaffmins.limit <<- cons_hr.limit[3] 
  labstaffmins.limit <<- cons_hr.limit[4] 
  dentalstaffmins.limit <<- cons_hr.limit[5]
  mentalstaffmins.limit <<- cons_hr.limit[6]
  nutristaffmins.limit <<- cons_hr.limit[7]
  diagstaffmins.limit <<- cons_hr.limit[8]
  
  reps <<- 4 # set the number of times that the matrix of interventions is duplicated
  # Define a function which duplicates a matrix horizontally
  duplicate_matrix_horizontally <- function(reps, matrix){
    matrix <- do.call(rbind, replicate(reps, matrix, simplify=FALSE))
  }
  if (task_shifting_pharm == 0){
    print("")
  } else if (task_shifting_pharm == 1){
    nursingstaff <- rbind(as.matrix(nursingstaff), as.matrix(nursingstaff + pharmstaff), as.matrix(nursingstaff + nutristaff), as.matrix(nursingstaff + nutristaff + pharmstaff))
    medstaff <- duplicate_matrix_horizontally(reps,as.matrix(medstaff))
    pharmstaff <- rbind(as.matrix(pharmstaff), as.matrix(rep(0,N)), as.matrix(pharmstaff), as.matrix(rep(0,N)))
    labstaff <- duplicate_matrix_horizontally(reps,as.matrix(labstaff))
    dentalstaff <- duplicate_matrix_horizontally(reps,as.matrix(dentalstaff))
    mentalstaff <- duplicate_matrix_horizontally(reps,as.matrix(mentalstaff))
    nutristaff <- rbind(as.matrix(nutristaff), as.matrix(nutristaff), as.matrix(rep(0,N)), as.matrix(rep(0,N)))
    diagstaff <- duplicate_matrix_horizontally(reps,as.matrix(diagstaff))  
  } else{
    print('ERROR: tash_shifting_pharm can take values 0 or 1')
  }
  
  
  # Clean total workforce size per cadre   
  hr_size.limit <- hr_size
  medstaff.limit <- hr_size.limit[1]
  nursingstaff.limit <- hr_size.limit[2]
  pharmstaff.limit <- hr_size.limit[3] 
  labstaff.limit <- hr_size.limit[4]
  dentalstaff.limit <- hr_size.limit[5]
  mentalstaff.limit <- hr_size.limit[6]
  nutristaff.limit <- hr_size.limit[7]
  diagstaff.limit <- hr_size.limit[8]
  
  medstaff.scale <- hr.scale[1]
  nursestaff.scale <- hr.scale[2]
  pharmstaff.scale <- hr.scale[3]
  labstaff.scale <- hr.scale[4]
  dentalstaff.scale <- hr.scale[5]
  mentalstaff.scale <- hr.scale[6]
  nutristaff.scale <- hr.scale[7]
  diagstaff.scale <- hr.scale[8]  
  # Each list here represents the number of staff (of each cadre) needed to deliver each intervention to all cases in need. 
  # Eg. for each cesarean section, 45 minutes of medical staff's time is needed (or 104,200 minutes for 2316 cases). On average 39,900 minutes are available per medical staff each year (257.3 million minutes in total divided by 6,400 medical staff). This means that for 2136 cases, 2.16 medical staff are needed (2316*45/(257.3m/6400))
  
  cons_hr <<- cbind(medstaff/(medstaffmins.limit/medstaff.limit), nursingstaff/(nursingstaffmins.limit/nursingstaff.limit), pharmstaff/(pharmstaffmins.limit/pharmstaff.limit), labstaff/(labstaffmins.limit/labstaff.limit), dentalstaff/(dentalstaffmins.limit/dentalstaff.limit), mentalstaff/(mentalstaffmins.limit/mentalstaff.limit), nutristaff/(nutristaffmins.limit/nutristaff.limit), diagstaff/(diagstaffmins.limit/diagstaff.limit))
  cons_hr.saved <<- cons_hr
  
  cons_hr.limit_base <<- cbind(medstaff.limit, nursingstaff.limit, pharmstaff.limit, labstaff.limit, dentalstaff.limit, mentalstaff.limit, nutristaff.limit, diagstaff.limit)
  cons_hr.limit <- cbind(medstaff.limit * medstaff.scale, nursingstaff.limit * nursestaff.scale, pharmstaff.limit * pharmstaff.scale, labstaff.limit * labstaff.scale, dentalstaff.limit * dentalstaff.scale, mentalstaff.limit * mentalstaff.scale, nutristaff.limit * nutristaff.scale, diagstaff.limit * diagstaff.scale)
  
  colnames(cons_hr.limit) <- colnames(cons_hr)
  cons_hr.limit.saved <<- cons_hr.limit
  
  # Combine the constraints into one matrix
  #****************************************************
  # 1. HR
  #--------------------------------------
  cons_hr <<- as.matrix(cons_hr)
  cons_hr.limit <<- as.matrix(cons_hr.limit)
  dim(cons_hr) # = 111 X 8
  dim(cons_hr.limit)  # = 1 X 8
  
  # 2. Drug
  #--------------------------------------
  cons_drug <<-as.matrix(cons_drug)
  cons_drug.limit <<- as.matrix(cons_drug.limit)
  dim(cons_drug) # = 111 X 1
  dim(cons_drug.limit) # = 1 X 1
  
  # 3. Max coverage
  #--------------------------------------
  #cons.feascov <- diag(n)
  cons.feascov <<- diag(x = cases, n, n)
  if (use_feasiblecov_constraint == 1){
    cons.feascov.limit <<- as.matrix(pmin(maxcoverage * feascov_scale * cases, cases)) # changed the constraint on 12May (multiplied by cases)
  }
  else if (use_feasiblecov_constraint == 0){
    cons.feascov.limit <<- as.matrix(cases) # changed the constraint on 12May (multiplied by cases)
  }
  else{
    print('ERROR: use_feasiblecov_constraint can take values 0 or 1')
  }  
  
  nonneg.lim <<- as.matrix(rep(0,n))
  dim(cons.feascov) # 111 X 111
  dim(cons.feascov.limit) # 111 X 1
  dim(nonneg.lim) # 111 X 1
  
  # % Update this to refer to intervention codes rather than row number % 
  # 4. Compulsory interventions
  #--------------------------------------
  if (length(compulsory_interventions) > 0){
    comp.count <- length(compulsory_interventions)
    cons_compulsory <<- matrix(0L, length(compulsory_interventions), ncol = n)
    cons_compulsory.limit <<- matrix(0L, length(compulsory_interventions), ncol = 1)
    for (i in 1:length(compulsory_interventions)){
      a <- which(data.frame$intcode == compulsory_interventions[i])
      b <- data.frame$intervention[a]
      #print(paste("Compulsory intervention: ",b, "; Code: ", compulsory_interventions[i], "; Number ",a ))
      cons_compulsory[i,a] <<- cases[a]
      # CHECK THIS CHANGE MADE on 26Aug21
      cons_compulsory.limit[i] <<- min(cases[a] * maxcoverage[a] * feascov_scale * compcov_scale, cases[a]) # changed on 12May to maxcoverage because cons.feascov.limit is now maximum number of cases rather than maximum % coverage 
    }
    dim(cons_compulsory)
  }
  else if(length(compulsory_interventions) == 0){
    comp.count<- 1
    cons_compulsory <<- matrix(0L, 1, ncol = n)
    cons_compulsory.limit <<- matrix(0L, 1, ncol = 1)
  }  
  cons_compulsory <<- t(cons_compulsory) 
  
  # 5. Complementary interventions
  # Nested complement is delivered to a proportion of those covered by the base intervention (this proportion can be 100%)
  complements_nested = complements_nested
  complements.count <- length(complements_nested)
  cons_complements.limit <<- matrix(0L, length(complements_nested), ncol = 1)
  cons_complements <<- matrix(0L, length(complements_nested), ncol = n) 
  
  if (complements.count > 0){
    print("Nested complements: Constraints added")
    counter = 1
    for (i in 1:complements.count){
      print(paste("Nested complements group", i))
      print("------------------------------------------------------------")  
      base <- which(data.frame$intcode == complements_nested[[i]][1])
      base_intervention <- data.frame$intervention[base]
      cases_base <- cases[base]
      
      nested_intervention_location <- which(data.frame$intcode == complements_nested[[i]][2])
      nested_intervention <- data.frame$intervention[nested_intervention_location]
      print(paste("Base intervention:", base_intervention , cases_base, "Intervention: ", nested_intervention, "; Code: ", complements_nested[[1]][2] , "; (Proportion: ",as.numeric(complements_nested[[i]][3]), ")"))
      cons_complements[counter,base] <<- cases_base * as.numeric(complements_nested[[i]][3])
      cons_complements[counter,nested_intervention_location] <<- - cases[nested_intervention_location]
      
      counter = counter + 1
    } 
    cons_complements <<- t(cons_complements)
   }else{cons_complements <<- t(cons_complements)}
  
  # 6. Substitute interventions
  #--------------------------------------
  substitutes = substitutes
  subs.count <- length(substitutes)
  cons_substitutes.limit <<- matrix(0L, length(substitutes), ncol = 1)
  cons_substitutes <<- matrix(0L, length(substitutes), ncol = n) 
  
  # First find the maximum number of feasible cases among the substitute interventions
  subsgrp_casesmax = matrix(0L, length(substitutes), ncol = 1)
  for (i in 1:subs.count){
    for (j in substitutes[i]){
      subsgrp_cases <- 0
      for (k in j){
        a <- which(data.frame$intcode == k)
        if (use_feasiblecov_constraint == 1){
          cases_max <- min(cases[a] * maxcoverage[a] * feascov_scale, cases[a])
        }
        else if (use_feasiblecov_constraint == 0){
          cases_max <- cases[a]
        }
        subsgrp_cases = cbind(subsgrp_cases,cases_max) 
      }
      subsgrp_casesmax[i] = max(subsgrp_cases)
      #print(paste("Group", i, "Cases max", subsgrp_casesmax[i]))
    }
  }
  
  # Next define the constraint such that the sum of the cases for each substitute interventions is less than or equal to the maxumum feasible cases derived above
  # print("Substitutes")
  for (i in 1:subs.count){
    # print(paste("Substitute group", i))
    # print("------------------------------------------------------------")
    for (j in substitutes[i]){
      for (k in j){
        a <- which(data.frame$intcode == k)
        b <- data.frame$intervention[a]
        #     print(paste("Intervention: ",b, "; Code: ", k, "; Maximum cases for intervention:", cons.feascov.limit[a],"; Number: ",a))
        cons_substitutes[i,a] <<- cases[a] # changed on 12May from 1 to cases
        cons_substitutes.limit[i] <<- subsgrp_casesmax[i] # changed on 12May to maxcoverage because cons.feascov.limit is now maximum number of cases rather than maximum % coverage 
      }
    }
    #cons_substitutes.limit[i] <- cons_substitutes.limit[i]/lengths(substitutes)[i]  # removed on 12May
    # print(paste("Maximum combined cases for group ",i, "= ", subsgrp_casesmax[i])) # print suppressed
  }  
  cons_substitutes <<- t(cons_substitutes)
  
  
  # Changes to constraints if task-shifting of pharmacist responsibility is allowed  
  #--------------------------------------------------------------------------------
  # Update the constraint matrices if task shifting is allowed
  if (task_shifting_pharm == 0){
    print("No task shifting of pharmaceutical tasks")
  }
  else if (task_shifting_pharm == 1){
    #1. Objective
    objective <<- duplicate_matrix_horizontally(reps, as.matrix(objective))
    #2. Drug budget constraint (cons_drug.limit does not need to be changed)
    cons_drug <<- duplicate_matrix_horizontally(reps, as.matrix(cons_drug))
    #3. Feasible coverage constraint
    cons.feascov <<- duplicate_matrix_horizontally(reps,as.matrix(cons.feascov))
    #4. Compulsory interventions
    cons_compulsory <<- duplicate_matrix_horizontally(reps,as.matrix(cons_compulsory))
    #6. Nested complements
    cons_complements <<- duplicate_matrix_horizontally(reps,as.matrix(cons_complements))
    #6. Substitutes
    cons_substitutes <<- duplicate_matrix_horizontally(reps,as.matrix(cons_substitutes))
  }
  else{
    print('ERROR: task_shifting_pharm can take values 0 or 1')
  }
  
  #   
  # COMBINE ALL
  print(dim(t(cons_drug)))
  print(dim(t(cons_hr)))
  print(dim(t(cons.feascov)))
  print(dim(t(cons_compulsory)))
  print(dim(t(cons_substitutes)))
  print(dim(t(cons_complements)))
  cons.mat <- rbind(t(cons_drug), t(cons_hr), t(cons.feascov), t(cons.feascov), t(cons_compulsory), t(cons_substitutes), t(cons_complements)) 
  dim(cons.mat) # (1+ 8 + N + N + 1 + No. of substitutes + No. of nested complements) X N
  cons.mat.limit <- rbind(cons_drug.limit, t(cons_hr.limit), cons.feascov.limit, nonneg.lim, cons_compulsory.limit, cons_substitutes.limit, cons_complements.limit)
  dim(cons.mat.limit) # (1+ 8 + N + N + 1 + No. of substitutes + No. of nested complements) X 1
  print(dim(cons.mat))
  print(dim(cons.mat.limit))  
  
  # Direction of relationship
  cons.dir <- rep("<=",1+8+n)
  cons.dir <- c(cons.dir,rep(">=",n), rep(">=",comp.count))
  cons.dir <- c(cons.dir,rep("<=",length(substitutes)))
  cons.dir <- c(cons.dir, rep(">=", length(complements_nested)))
  length(cons.dir)
  length(cons.dir) = dim(cons.mat.limit)[1] # Assert that the length of the directions list is the same as that of the constraints matrix
  
  
  ###################################
  # 3.2 - Run LPP
  ###################################
  solution.class <<- lp("max", objective, cons.mat, cons.dir, cons.mat.limit, compute.sens = TRUE)
  
  ###################################
  # 3.3 - Outputs	
  ###################################
  # Export solution to a .csv file
  #------------------------------------
  solution <<- as.data.frame(solution.class$solution)
  solution_hr <<- as.data.frame(solution.class$solution) # use this uncollapsed version of the dataframe for HR use calculations below
  # Collapse solution by intervention
  if (task_shifting_pharm == 1){
    for (i in 1:length(dalys)){
      for (j in 1:(reps-1)){
        solution[i,1] <<- solution[i,1] + solution[i+length(dalys)*j,1]
      }
    }
    solution <<- as.data.frame(solution[1:length(dalys),1])
  }
  
  # Number of interventions with a positive net health impact
  pos_nethealth.count <<- sum(nethealth > 0) # this seems to be one less than the figure in the excel
  
  # Number of interventions in the optimal package
  intervention.count <<- sum(solution != 0)
  
  # DALY burden averted as a % of avertible DALY burden
  solution_dalysaverted <<- solution * cases * dalys # Dalys averted per intervention
  dalysavertible = cases * dalys # Total DALYs that can be averted at maximum coverage
  dalys_averted <<- round(sum(unlist(lapply(solution_dalysaverted, sum))),2)
  dalys_averted.prop <<- sum(unlist(lapply(solution_dalysaverted, sum)))/sum(unlist(lapply(dalysavertible, sum)))
  
  # Drugs and Commodities cost (% of budget available)
  solution_drugexp <<- solution*cons_drug[1:length(dalys),] # Total drug budget required per intervention for the  the optimal solution
  total_drug_exp <<- round(sum(unlist(lapply(solution_drugexp, sum))),2) # Total drug budget required for the  the optimal solution
  drug_exp.prop <<- total_drug_exp/cons_drug.limit_base
  
  # Total HR use (% of capacity)
  hr_cadres <- c("Medical staff", "Nurse", "Pharmacist", "Lab", "Dental", "Mental", "Nutrition", "Diagnostic")
  solution_hruse <<- unlist(solution_hr) * cons_hr  # Number of minutes per health worker cadre and intervention utlitised by the optimal solution
  if (task_shifting_pharm == 1){
    for (i in 1:length(dalys)){
      for (j in 1:(reps-1)){
        solution_hruse[i,] <<- solution_hruse[i,] + solution_hruse[i+length(dalys)*j,]
      }
    }
    solution_hruse <<- solution_hruse[1:length(dalys),]
  }
  total_hruse <<- colSums(solution_hruse, na.rm = FALSE, dims = 1) # Number of minutes per health worker cadre utlitised by the optimal solution
  hruse.prop <<- round(total_hruse/cons_hr.limit_base, 2)
  colnames(hruse.prop) <<- hr_cadres
  
  # Cost-effectiveness Threshold
  icer <- fullcost/dalys
  temp <- cbind.data.frame(icer, solution, data.frame$intervention)
  temp['solution.class$solution'] =  as.numeric(temp[[2]])
  temp['icer'] =  as.numeric(temp[[1]])
  cet_soln <<- round(max(temp['icer'][temp['solution.class$solution'] > 0]),2) # previoiusly temp$icer[temp$solution > 0]
  a <- which(icer == max(temp['icer'][temp['solution.class$solution'] > 0])) # to check which included intervention has the highest ICER
  least.ce.intervention <- data.frame$intervention[a]
  
  # Collapse above outputs so that each intervention appears once in the list irrespective of task-shifting
  #pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres])
  
  outputs <- list("Total number of interventions in consideration" = length(dalys), 
                  "Number of interventions with positive net health impact" = pos_nethealth.count, 
                  "Number of interventions in the optimal package" = intervention.count,
                  "Net DALYs averted" = solution.class$objval,
                  "Total DALYs averted" = sum(unlist(lapply(solution_dalysaverted, sum))), 
                  "Proportion of DALY burden averted" = dalys_averted.prop , 
                  "Proportion of drug budget used" = drug_exp.prop, 
                  "Proportion of HR capacity used by cadre" = hruse.prop,
                  "CET based on solution" =  cet_soln
  )
  return(outputs)
}

#############################################################
# Function to generate resource use stacked bar charts
#############################################################
# Note that in order to run this function, find_optimal_package needs to be run first
gen_resourceuse_graphs <- function(plot_title, file_name){
  #pal <- viridisLite::viridis(10) # Create a viridis palette for the graph
  pal <- rainbow(10)
  
  ## Generate matrix representing HR and Drug budget use by the HBP solution run above
  #***********************************************************************************
  # HR Resource Use
  data_hr <- sweep(solution_hruse, 2, cons_hr.limit_base, FUN = '/')
  hr_cadres <- c("Doctor/\nClinical officer", "Nursing \nstaff", "Pharmaceutical \nstaff", "Laboratory \nstaff", 
                 "Dental \nstaff", "Mental Health \nstaff", "Nutrition \nstaff", "Diagnostic \nstaff")
  #colnames(data_hr) <- hr_cadres
  
  # Drug budget Use
  data_drug <- as.matrix(solution_drugexp)/cons_drug.limit_base
  #colnames(data_drug) <- 'Drug \nbudget'
  
  length(data_drug) = dim(data_hr)[1] # Assert that the length of the directions list is the same as that of the constraints matrix
  
  # Combine all resource use matrices into one matrix
  data <- cbind(data_hr,data_drug)
  data <- as.matrix(data)
  # Drop Dental and Diagnostic staff from the matrix
  data <- data[,-c(4,5,8)] 
  data <- cbind(category,data)
  
  ## Convert to long form in order to apply ggplot 
  #***********************************************************************************
  data <- as.data.frame(data)
  colnames(data) <- c('category', hr_cadres[-c(4, 5,8)], 'Consumables \nbudget')
  data_long <<- gather(data, resource, use, 2:'Consumables \nbudget', factor_key=TRUE)
  data_long$use <<- as.numeric(data_long$use) # convert use data to numeric
  
  # Arrange/sort and compute cumulative sums to position labels on each stacked portion
  #data_long <- data_long %>%
  #  group_by(resource) %>%
  #  arrange(resource, desc(category)) %>%
  #  mutate(lab_ypos = cumsum(use) - 0.5 * use) 
  
  ## Generate graph
  #***********************************************************************************
  p <- ggplot(data = data_long, aes(x = resource, y = use)) +
    geom_col(aes(fill = category), width = 0.7)
  #+geom_text(aes(y = lab_ypos, label = intcode, group =intcode), color = "white") # add data labels
  p <- p + guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    scale_y_continuous(labels = percent)+ 
    geom_text(aes(label = stat(sprintf("%1.1f%%", round(100*y, digits = 2))), group = resource), stat = 'summary', fun = sum, vjust = -1, size=4)+
    theme_classic()# show total labels on the top
  titleformats <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (20)),  
                        legend.text = element_text(face = "italic", colour="black",family = "Helvetica"),
                        legend.title = element_blank(), # remove legend title
                        axis.title = element_text(family = "Helvetica", size = (15), colour = "black"),
                        axis.text.x = element_text(face="bold", color="black", size=10, angle=0),
                        axis.text.y = element_text(face="bold", color="black", size=10, angle=0),
                        legend.position="bottom") 
  
  print(p + titleformats + labs( title= plot_title, 
                                 x="Resource", y = "Percentage of resource required") + scale_fill_manual(values = pal))
  
  # Save graph with the assigned title
  ggsave(file_name, width = 20, height = 20, units = "cm")
}