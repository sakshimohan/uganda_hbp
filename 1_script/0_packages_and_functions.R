##############################
# 0 - Load libraries
##############################
# Note the following packages need to be installed  readxl, lpSolveAPI, checkData, fmsb
library(readxl)
library(lpSolve)
library(fmsb) # for radar chart
library(dplyr)
library(ggplot2)
library(tidyverse)
library(forcats) # to reorder plots
library(xtable) # for LaTeX tables
library(tidyr)
library(scales) # to formal axis labels
library(viridis) # load viridis colour palette

###################################
# 3. Define customizable LPP/optimization function
###################################
find_optimal_package <- function(input_data_file, # path to excel sheet which contains all data 
                                 objective_input = "nethealth", # what is being maximised
                                 cet_input = 165, # chosen cost effectiveness threshold (only relevant if objective_input = "nethealth")
                                 drug_budget_input, # size of consumables budget
                                 drug_budget.scale = 1,  # use this to scale consumables budget up or down (1 -> no scaling applied)
                                 hr.scale,  # use this to scale health workforce size up or down individually fo each cadre (1 -> no scaling applied)
                                 allow_demand_constraint = 0, # whether maximum feasible coverage constraints should be applied (default set to 0)
                                 allow_other_modes_delivery = 0, # whether other modes of delivery should be allowed 
                                 max_feasible_coverage_scale = 1, # whether maximum feasible coverage constraints should be scaled up or down
                                 compulsory_intervention_coverage_scale = 1, # use this to scale maximum feasible coverage constraints for compulsory interventions up or down (1 -> no scaling applied) - this is applied to maximum feasible coverage if use_feasiblecov_constraint = 1
                                 allow_task_shifting_pharm = 0) # whether task shifting is allowed (from pharmacists and nutrition officers to nurses)
{ 
  ## Load data
  #######################################################################################
  # Load intervention data - cost-effectiveness + drug cost + target population + coverage constraints
  df <- read_excel(input_data_file, sheet = "intervention list",col_names = TRUE,col_types=NULL,na="",skip=0)
  # Load HR availability data set
  df_hr <- read_excel(input_data_file, sheet = "hr constraint",col_names = TRUE,col_types=NULL,na="",skip=0)
  # Load compulsory intervention list
  df_compulsory <- read_excel(input_data_file, sheet = "compulsory int",col_names = TRUE,col_types=NULL,na="",skip=0)
  # Load substitute intervention list
  df_substitutes <- read_excel(input_data_file, sheet = "substitute int",col_names = TRUE,col_types=NULL,na="",skip=0)
  # Load complementary intervention list
  df_complements <- read_excel(input_data_file, sheet = "complementary int",col_names = TRUE,col_types=NULL,na="",skip=0)
  
  # Clean dataframes
  df <- na.omit(df) # drop rows containing missing values #df[!is.na(df$`DALYs averted per patient (Uganda)`)]
  colnames(df_hr) = df_hr[1,] #set the columns name based on first row
  
  df_hr <- df_hr %>% 
    slice(-1) #remove the first row
  
  # Extract .csv versions of input data to keep track of changes
  #write.csv(df, file = "3_processing/uganda_intervention_data.csv")
  #write.csv(df_hr, file = "3_processing/uganda_hr_data.csv")
  #write.csv(df_complements, file = "4_processing/uganda_hr_data.csv")
  #write.csv(df_substitutes, file = "5_processing/uganda_hr_data.csv")
  #write.csv(df_compulsory, file = "6_processing/uganda_hr_data.csv")
  
  # Set up HR constraint data frames
  # Patient-facing time needed per case per year
  hr_minutes <- df_hr %>% 
    mutate(`Total patient-facing time per year (minutes)` = as.numeric(`Total patient-facing time per year (minutes)`)) %>% 
    pull(`Total patient-facing time per year (minutes)`)
  # Size of the health workforce
  hr_size <- df_hr %>% 
    mutate(`Total staff` = as.numeric(`Total staff`)) %>% 
    pull(`Total staff`)
  
  # Generate relevant lists from data set
  #--------------------------------------------------------
  # Rename columns
  df <- df %>% dplyr::rename(
    dalys = `DALYs averted per patient (Uganda)`, 
    drugcost = `Average drugs and commodities cost (2023 USD)`,
    maxcovchw = `Community health workers_coverage_2024`,
    maxcovprivate = `Private health workers_coverage_2024`,
    maxcov =`Maximum coverage`, 
    fullcost = `Cost per case (Uganda) - 2023 USD`,
    intervention = `intervention_name`,
    cases = `Cases_full_2023`,
    feasconstchw = `Feasibility constraint community health workers`,
    feasconstprivate = `Feasibility constraint private health workers`,
    intcode = `code`,
    category = `Category`
  )
  
  N <- length(df$dalys) # total number of interventions included in the analysis
  
  # Convert columns to numeric
  df <- df %>% mutate_at(c('drugcost', 'dalys', 'maxcovchw', 'maxcovprivate', 'feasconstchw', 'maxcov', 'feasconstprivate', 'fullcost', 'cases'), as.numeric)
  str(df) # ^^ check format of all columns ^^	
  
  intervention <<- df$intervention
  intcode <<- df$intcode # list of intervention codes
  category <<- df$category # program/category of intervention
  dalys <<- df$dalys # Per case DALYs averted based on CE evidence
  fullcost <<- df$fullcost # Full cost per patient based on CE evidence ('full' because this captures all costs - from the CE study -and not only drugs and commodities cost which are used as a constraint in our model)
  drugcost <<- df$drugcost #  Per case cost of drugs and commodities
  maxcoveragechw <<- df$maxcovchw # Maximum percentage of cases which can be covered by CHW
  maxcoverageprivate <<- df$maxcovprivate # Maximum percentage of cases which can be covered by private pharmacists
  maxcoverage <<- df$maxcov # Maximum possible coverage of all eligible cases
  cases <<- df$cases # Total number of eligible cases
  hrneed <<- as.data.frame(apply(df[,c(8:17)],2,as.numeric)) # Number of minutes of health worker time requires per intervention per person
  use_feas_constraint_chw <<- df$feasconstchw
  use_feas_constraint_private <<- df$feasconstprivate
  
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
  } else if (objective_input == 'dalys'){
    objective <<- dalys * cases
  } else{
    stop('ERROR: objective_input must be either "dalys" or "nethealth".')
  }
  
  # Constraints - 1. Drug Budget, 2. HR Requirements
  #****************************************************
  # 1. Drug Budget
  #----------------
  cons_drug <<- drugcost * cases # Cost of drugs for the number of cases covered
  cons_drug.limit <<- drug_budget_input * drug_budget.scale
  cons_drug.limit_base <<- drug_budget_input # un-scaled drug budget
  
  # 2. HR Constraints
  #---------------------
  hr_minutes_need <- hrneed * cases[row(hrneed)] # HR minutes required to deliver intervention to all cases in need
  
  # Extract individual arrays for each HR constraint by cadre (Number of minutes of staff time needed per intervention)
  medstaffmins <- hr_minutes_need %>% pull(`Medicalstaff`)
  nursingstaffmins <- hr_minutes_need %>% pull (`Nursingstaff`) 
  pharmstaffmins <- hr_minutes_need %>% pull (`Pharmaceuticalstaff`)
  labstaffmins <- hr_minutes_need %>% pull (`Labstaff`)
  dentalstaffmins <- hr_minutes_need %>% pull (`Dentalstaff`) 
  mentalstaffmins <- hr_minutes_need %>% pull (`Mentalhealthstaff`) 
  nutristaffmins <- hr_minutes_need %>% pull (`Nutritionstaff`) 
  diagstaffmins <- hr_minutes_need %>% pull (`Radiographystaff`)
  chwstaffmins <- hr_minutes_need %>% pull (`Community health workers`)
  pvtpharmstaffmins <- hr_minutes_need %>% pull(`Private pharmacists staff`) 
  
  # Clean total minutes available per cadre  
  cons_hr.limit <- as.data.frame(hr_minutes)
  medstaffmins.limit <<- cons_hr.limit %>% slice(1) %>% pull() 
  nursingstaffmins.limit <<- cons_hr.limit %>% slice(2) %>% pull () 
  pharmstaffmins.limit <<- cons_hr.limit %>% slice(3) %>% pull ()
  labstaffmins.limit <<- cons_hr.limit %>% slice(4) %>% pull () 
  dentalstaffmins.limit <<- cons_hr.limit %>% slice(5) %>% pull () 
  mentalstaffmins.limit <<- cons_hr.limit %>% slice(6) %>% pull () 
  nutristaffmins.limit <<- cons_hr.limit %>% slice(7) %>% pull ()
  diagstaffmins.limit <<- cons_hr.limit %>% slice(8) %>% pull ()
  chwstaffmins.limit <<- cons_hr.limit %>% slice(9) %>% pull ()
  pvtpharmstaffmins.limit <<- cons_hr.limit %>% slice(10) %>% pull()
  
  # Define a function which duplicates a matrix vertically
  duplicate_matrix_vertically <- function(reps, matrix){
    matrix <- do.call(rbind, replicate(reps, matrix, simplify=FALSE))
  }
  
  #Scenario: Task shifting and other modes of health service delivery
  if (allow_other_modes_delivery == 0) {
    if (allow_task_shifting_pharm == 0) {
      nursingstaff <- nursingstaffmins
      medstaff <- medstaffmins
      pharmstaff <- pharmstaffmins
      labstaff <- labstaffmins
      dentalstaff <- dentalstaffmins
      mentalstaff <- mentalstaffmins
      nutristaff <- nutristaffmins
      diagstaff <- diagstaffmins
      chwstaff <- as.matrix(rep(0,N)) # will create a null matrix 
      pvtpharmstaff <- as.matrix(rep(0,N)) # will create a null matrix 
    } else if (allow_task_shifting_pharm == 1){
      reps <- 4 # set the number of times that the matrix of interventions is duplicated
      nursingstaff <- rbind(as.matrix(nursingstaffmins), as.matrix(nursingstaffmins + pharmstaffmins), as.matrix(nursingstaffmins + nutristaffmins), as.matrix(nursingstaffmins + nutristaffmins + pharmstaffmins))
      medstaff <- duplicate_matrix_vertically(reps,as.matrix(medstaffmins))
      pharmstaff <- rbind(as.matrix(pharmstaffmins), as.matrix(rep(0,N)), as.matrix(pharmstaffmins), as.matrix(rep(0,N)))
      labstaff <- duplicate_matrix_vertically(reps,as.matrix(labstaffmins))
      dentalstaff <- duplicate_matrix_vertically(reps,as.matrix(dentalstaffmins))
      mentalstaff <- duplicate_matrix_vertically(reps,as.matrix(mentalstaffmins))
      nutristaff <- rbind(as.matrix(nutristaffmins), as.matrix(nutristaffmins), as.matrix(rep(0,N)), as.matrix(rep(0,N)))
      diagstaff <- duplicate_matrix_vertically(reps,as.matrix(diagstaffmins))  
      chwstaff <- duplicate_matrix_vertically(reps,as.matrix(rep(0,N))) # will replicate a null matrix 
      pvtpharmstaff <- duplicate_matrix_vertically(reps,as.matrix(rep(0,N))) # will replicate a null matrix 
    } 
  } else if (allow_other_modes_delivery == 1){
    if (allow_task_shifting_pharm == 0) {
      # Estimate the total time spent for HF based cadres and time to be redistributed by CHW involvement 
      total_time_spent <- nursingstaffmins + medstaffmins + pharmstaffmins + labstaffmins + dentalstaffmins + mentalstaffmins + nutristaffmins + diagstaffmins
      #Estimates remaining time to re-distribute. If remaining time is greater than zero, then it redistributes, otherwise if negative (CHW time is higher than total time spent), then there is no need for redistribution
      time_to_redistribute <- ifelse(total_time_spent - chwstaffmins > 0, total_time_spent - chwstaffmins, 0)
      
      # set the number of times that the matrix of interventions is duplicated
      reps <- 3 
      nursingstaff <- rbind(as.matrix(nursingstaffmins), as.matrix((nursingstaffmins/total_time_spent)*time_to_redistribute), as.matrix(nursingstaffmins))
      nursingstaff <- ifelse(is.nan(nursingstaff), 0, nursingstaff)
      medstaff <- rbind(as.matrix(medstaffmins), as.matrix((medstaffmins/total_time_spent)*time_to_redistribute), as.matrix(medstaffmins))
      medstaff <- ifelse(is.nan(medstaff), 0, medstaff)
      pharmstaff <- rbind(as.matrix(pharmstaffmins), as.matrix((pharmstaffmins/total_time_spent)*time_to_redistribute), as.matrix(pharmstaffmins*(1-use_feas_constraint_private)))
      pharmstaff <- ifelse(is.nan(pharmstaff), 0, pharmstaff)
      labstaff <- rbind(as.matrix(labstaffmins), as.matrix((labstaffmins/total_time_spent)*time_to_redistribute), as.matrix(labstaffmins))
      labstaff <- ifelse(is.nan(labstaff), 0, labstaff)
      dentalstaff <- duplicate_matrix_vertically(reps,as.matrix(dentalstaffmins))
      dentalstaff <- ifelse(is.nan(dentalstaff), 0, dentalstaff)
      mentalstaff <-  rbind(as.matrix(mentalstaffmins), as.matrix((mentalstaffmins/total_time_spent)*time_to_redistribute), as.matrix(mentalstaffmins))
      mentalstaff <- ifelse(is.nan(mentalstaff), 0, mentalstaff)
      nutristaff <-  rbind(as.matrix(nutristaffmins), as.matrix((nutristaffmins/total_time_spent)*time_to_redistribute), as.matrix(nutristaffmins))
      nutristaff <- ifelse(is.nan(nutristaff), 0, nutristaff)
      diagstaff <- rbind(as.matrix(diagstaffmins), as.matrix((diagstaffmins/total_time_spent)*time_to_redistribute), as.matrix(diagstaffmins))
      diagstaff <- ifelse(is.nan(diagstaff), 0, diagstaff)
      chwstaff <- rbind(as.matrix(rep(0,N)), as.matrix(chwstaffmins), as.matrix(rep(0,N))) # will include time spent only for the CHW version of the intervention 
      chwstaff <- ifelse(is.nan(chwstaff), 0, chwstaff)
      pvtpharmstaff <- rbind(as.matrix(rep(0,N)), as.matrix(rep(0,N)), as.matrix(pvtpharmstaffmins*use_feas_constraint_private)) # substituting for facility based pharmacist staff (assumption); rest of the other cadres remain unchanged in terms of need. 
      pvtpharmstaff <- ifelse(is.nan(pvtpharmstaff), 0, pvtpharmstaff)
    } else if (allow_task_shifting_pharm == 1){
      # Estimate the total time spent for HF based cadres and time to be redistributed by CHW involvement
      total_time_spent <- nursingstaffmins + medstaffmins + pharmstaffmins + labstaffmins + dentalstaffmins + mentalstaffmins + nutristaffmins + diagstaffmins
      time_to_redistribute <- ifelse(total_time_spent - chwstaffmins > 0, total_time_spent - chwstaffmins, 0)
      # set the number of times that the matrix of interventions is duplicated
      reps <- 6  
      nursingstaff <- rbind(as.matrix(nursingstaffmins), as.matrix(nursingstaffmins + pharmstaffmins), as.matrix(nursingstaffmins + nutristaffmins), as.matrix(nursingstaffmins + nutristaffmins + pharmstaffmins), as.matrix((nursingstaffmins/total_time_spent)*time_to_redistribute), as.matrix(nursingstaffmins))
      nursingstaff <- ifelse(is.nan(nursingstaff), 0, nursingstaff)
      medstaff <- rbind(as.matrix(medstaffmins),as.matrix(medstaffmins),as.matrix(medstaffmins),as.matrix(medstaffmins), as.matrix((medstaffmins/total_time_spent)*time_to_redistribute), as.matrix(medstaffmins))
      medstaff <- ifelse(is.nan(medstaff), 0, medstaff)
      pharmstaff <- rbind(as.matrix(pharmstaffmins), as.matrix(rep(0,N)), as.matrix(pharmstaffmins), as.matrix(rep(0,N)), as.matrix((pharmstaffmins/total_time_spent)*time_to_redistribute), as.matrix(pharmstaffmins*(1-use_feas_constraint_private)))
      pharmstaff <- ifelse(is.nan(pharmstaff), 0, pharmstaff)
      labstaff <- rbind(as.matrix(labstaffmins),as.matrix(labstaffmins),as.matrix(labstaffmins),as.matrix(labstaffmins), as.matrix((labstaffmins/total_time_spent)*time_to_redistribute), as.matrix(labstaffmins))
      labstaff <- ifelse(is.nan(labstaff), 0, labstaff)
      dentalstaff <- duplicate_matrix_vertically(reps,as.matrix(dentalstaffmins))
      dentalstaff <- ifelse(is.nan(dentalstaff), 0, dentalstaff)
      mentalstaff <- rbind(as.matrix(mentalstaffmins),as.matrix(mentalstaffmins),as.matrix(mentalstaffmins),as.matrix(mentalstaffmins), as.matrix((mentalstaffmins/total_time_spent)*time_to_redistribute), as.matrix(mentalstaffmins))
      mentalstaff <- ifelse(is.nan(mentalstaff), 0, mentalstaff)
      nutristaff <- rbind(as.matrix(nutristaffmins), as.matrix(nutristaffmins), as.matrix(rep(0,N)), as.matrix(rep(0,N)), as.matrix((nutristaffmins/total_time_spent)*time_to_redistribute), as.matrix(nutristaffmins))
      nutristaff <- ifelse(is.nan(nutristaff), 0, nutristaff)
      diagstaff <- rbind(as.matrix(diagstaffmins), as.matrix(diagstaffmins), as.matrix(diagstaffmins), as.matrix(diagstaffmins), as.matrix((diagstaffmins/total_time_spent)*time_to_redistribute), as.matrix(diagstaffmins))
      diagstaff <- ifelse(is.nan(diagstaff), 0, diagstaff)
      chwstaff <- rbind(as.matrix(rep(0,N)), as.matrix(rep(0,N)), as.matrix(rep(0,N)), as.matrix(rep(0,N)), as.matrix(chwstaffmins), as.matrix(rep(0,N))) # will include time spent only for the CHW version of the intervention 
      chwstaff <- ifelse(is.nan(chwstaff), 0, chwstaff)
      pvtpharmstaff <- rbind(as.matrix(rep(0,N)), as.matrix(rep(0,N)), as.matrix(rep(0,N)), as.matrix(rep(0,N)),as.matrix(rep(0,N)), as.matrix(pvtpharmstaffmins*use_feas_constraint_private)) # will include time spent only for the Private pharmacist version of the intervention
      pvtpharmstaff <- ifelse(is.nan(pvtpharmstaff), 0, pvtpharmstaff)
    } 
  } else {
    stop('ERROR: ERROR: allow_other_modes_delivery and allow_task_shifting_pharm take values 0 or 1')
  }
  
  
  # Clean total workforce size per cadre   
  hr_size.limit <- as.data.frame(hr_size)
  medstaff.limit <- hr_size.limit %>% slice(1) %>% pull()
  nursingstaff.limit <- hr_size.limit %>% slice(2) %>% pull()
  pharmstaff.limit <- hr_size.limit %>% slice(3) %>% pull() 
  labstaff.limit <- hr_size.limit %>% slice(4) %>% pull()
  dentalstaff.limit <- hr_size.limit %>% slice(5) %>% pull()
  mentalstaff.limit <- hr_size.limit %>% slice(6) %>% pull()
  nutristaff.limit <- hr_size.limit %>% slice(7) %>% pull()
  diagstaff.limit <- hr_size.limit %>% slice(8) %>% pull()
  chwstaff.limit <- hr_size.limit %>% slice(9) %>% pull()
  pvtpharmstaff.limit <- hr_size.limit %>% slice(10) %>% pull()
  
  hr.scale <- as.data.frame(hr.scale)
  medstaff.scale <- hr.scale %>% slice(1) %>% pull()
  nursestaff.scale <- hr.scale %>% slice(2) %>% pull()
  pharmstaff.scale <- hr.scale %>% slice(3) %>% pull()
  labstaff.scale <- hr.scale %>% slice(4) %>% pull()
  dentalstaff.scale <- hr.scale %>% slice(5) %>% pull()
  mentalstaff.scale <- hr.scale %>% slice(6) %>% pull()
  nutristaff.scale <- hr.scale %>% slice(7) %>% pull()
  diagstaff.scale <- hr.scale %>% slice(8) %>% pull() 
  chwstaff.scale <- hr.scale %>% slice(9) %>% pull()
  pvtpharmstaff.scale <- hr.scale %>% slice(10) %>% pull()
  
  # Each list here represents the number of staff (of each cadre) needed to deliver each intervention to all cases in need. 
  # E.g. for each cesarean section, 45 minutes of medical staff's time is needed (or 104,200 minutes for 2316 cases). On average 39,900 minutes are available per medical staff each year (257.3 million minutes in total divided by 6,400 medical staff). This means that for 2136 cases, 2.16 medical staff are needed (2316*45/(257.3m/6400))
  
  cons_hr <- cbind(medstaff/(medstaffmins.limit/medstaff.limit), nursingstaff/(nursingstaffmins.limit/nursingstaff.limit), pharmstaff/(pharmstaffmins.limit/pharmstaff.limit), labstaff/(labstaffmins.limit/labstaff.limit), dentalstaff/(dentalstaffmins.limit/dentalstaff.limit), mentalstaff/(mentalstaffmins.limit/mentalstaff.limit), nutristaff/(nutristaffmins.limit/nutristaff.limit),diagstaff/(diagstaffmins.limit/diagstaff.limit), chwstaff/(chwstaffmins.limit/chwstaff.limit), pvtpharmstaff/(pvtpharmstaffmins.limit/pvtpharmstaff.limit))
  cons_hr.saved <<- cons_hr
  
  cons_hr.limit_base <- cbind(medstaff.limit, nursingstaff.limit, pharmstaff.limit, labstaff.limit, dentalstaff.limit, mentalstaff.limit, nutristaff.limit, diagstaff.limit, chwstaff.limit, pvtpharmstaff.limit)
  cons_hr.limit <- cbind(medstaff.limit * medstaff.scale, nursingstaff.limit * nursestaff.scale, pharmstaff.limit * pharmstaff.scale, labstaff.limit * labstaff.scale, dentalstaff.limit * dentalstaff.scale, mentalstaff.limit * mentalstaff.scale, nutristaff.limit * nutristaff.scale, diagstaff.limit * diagstaff.scale, chwstaff.limit * chwstaff.scale, pvtpharmstaff.limit * pvtpharmstaff.scale)
  
  colnames(cons_hr.limit) <- colnames(cons_hr)
  cons_hr.limit.saved <<- cons_hr.limit
  
  # Combine the constraints into one matrix
  #****************************************************
  # 1. HR
  #--------------------------------------
  cons_hr <<- as.matrix(cons_hr)
  cons_hr.limit <<- as.matrix(cons_hr.limit)
  # use dim(cons_hr) to ensure that the dimensions of the matrix are as expected
  
  # 2. Drug
  #--------------------------------------
  cons_drug <<-as.matrix(cons_drug)
  cons_drug.limit <<- as.matrix(cons_drug.limit)
  
  # 3. Decision variable constraints
  #--------------------------------------
  cons.feascov <<- diag(x = cases, n, n)
  
  if (allow_demand_constraint == 1) {
    if (allow_other_modes_delivery == 1) {
      cons.feascov.limit <<- rbind(
        as.matrix(pmin(maxcoverage * max_feasible_coverage_scale * cases, cases)),
        as.matrix(cases * use_feas_constraint_chw * maxcoveragechw),
        as.matrix(cases * use_feas_constraint_private * maxcoverageprivate)
      )
    } else if (allow_other_modes_delivery == 0) {
      cons.feascov.limit <<- as.matrix(pmin(maxcoverage * max_feasible_coverage_scale * cases, cases))
    }
  } else if (allow_demand_constraint == 0) {
    if (allow_other_modes_delivery == 1) {
      cons.feascov.limit <<- rbind(
        as.matrix(cases),
        as.matrix(cases * use_feas_constraint_chw * maxcoveragechw),
        as.matrix(cases * use_feas_constraint_private * maxcoverageprivate)
      )
    } else if (allow_other_modes_delivery == 0) {
      cons.feascov.limit <<- as.matrix(cases)
    }
  } else {
    print('ERROR: allow_demand_constraint and allow_other_modes_delivery can take values 0 or 1')
  }
  
  nonneg.lim <<- as.matrix(rep(0,n))
  
  # 4. Compulsory interventions  
  #--------------------------------------
  if (nrow(df_compulsory) > 0) {
    # Get the number of compulsory interventions
    comp.count <- nrow(df_compulsory)
    # Initialize the constraint matrices for compulsory interventions
    cons_compulsory <<- matrix(0L, comp.count, ncol = n)
    cons_compulsory.limit <<- matrix(0L, comp.count, ncol = 1)
    # Loop through the compulsory interventions  
    for (i in 1:comp.count) {
      # Find the index of the intervention in the data.frame
      a <- which(df$intcode == df_compulsory$`compulsory intervention`[i])
      b <- df$intervention[a]
      # Update the constraint matrix
      cons_compulsory[i, a] <<- cases[a]
      # Add the new conditional logic for the limit
      if (allow_demand_constraint == 1) {
        cons_compulsory.limit[i] <<- min(cases[a] * maxcoverage[a] * max_feasible_coverage_scale * compulsory_intervention_coverage_scale, cases[a])
      } else {
        cons_compulsory.limit[i] <<- cases[a]
      }
    }
    
  } else if (nrow(df_compulsory) == 0) {
    # If no compulsory intervention exists, create a 1 x N matrix of zeroes.
    comp.count <- 1
    cons_compulsory <<- matrix(0L, 1, ncol = n)
    cons_compulsory.limit <<- matrix(0L, 1, ncol = 1)
  }
  
  # Transpose the matrix at the end 
  cons_compulsory <<- t(cons_compulsory)
  
  # 5. Complementary interventions
  #--------------------------------------
  # Nested complement is delivered to a proportion of those covered by the base intervention (this proportion can be 100%)
  complements.count <- nrow(df_complements)
  cons_complements.limit <<- matrix(0L, complements.count, ncol = 1)
  cons_complements <<- matrix(0L, complements.count, ncol = n) 
  
  if (complements.count > 0){
    print("Nested complements: Constraints added")
    counter = 1
    for (i in 1:complements.count){
      print(paste("Nested complements group", i))
      print("------------------------------------------------------------")  
      # Retrieve base intervention codes from the data frame 
      base <- which(df$intcode == df_complements$`Base intervention`[i])
      base_intervention <- df$intervention[base]
      cases_base <- cases[base]
      # Retrieve nested intervention codes from the data frame
      nested_intervention_location <- which(df$intcode == df_complements$`Nested intervention`[i])
      nested_intervention <- df$intervention[nested_intervention_location]
      #Print information 
      print(paste("Base intervention:", base_intervention , cases_base, "Intervention: ", nested_intervention, "; Code: ", df_complements$`Nested intervention`[i] , "; (Proportion: ",as.numeric(df_complements$Proportion[i]), ")"))
      #Apply the proportion for the base intervention and set complement constraint 
      cons_complements[counter,base] <<- cases_base * as.numeric(df_complements$Proportion[i])
      cons_complements[counter,nested_intervention_location] <<- - cases[nested_intervention_location]
      
      counter = counter + 1
    } 
    cons_complements <<- t(cons_complements)
  }else{cons_complements <<- t(cons_complements)}
  
  
  # 6. Substitute interventions
  #--------------------------------------
  subs.count <- length(unique(df_substitutes$Group)) 
  cons_substitutes.limit <<- matrix(0L, subs.count, ncol = 1)
  cons_substitutes <<- matrix(0L, subs.count, ncol = n) 
  
  # First find the maximum number of feasible cases among the substitute interventions
  subsgrp_casesmax = matrix(0L, subs.count, ncol = 1)
  
  for (i in 1:subs.count){
    subsgrp_cases <- 0
    # Filter the data frame by group
    current_group <- df_substitutes[df_substitutes$Group == i, ]
    substitutes <- current_group$Substitute  # Get the intervention codes for this group
    
    for (k in substitutes){
      a <- which(df$intcode == k)
      
      if (allow_demand_constraint == 1){
        cases_max <- min(cases[a] * maxcoverage[a] * max_feasible_coverage_scale, cases[a])
      }
      else if (allow_demand_constraint == 0){
        cases_max <- cases[a]
      }
      subsgrp_cases = cbind(subsgrp_cases,cases_max) 
    }
    subsgrp_casesmax[i] = max(subsgrp_cases)
  }
  
  # Next define the constraint such that the sum of the cases for each substitute interventions is less than or equal to the maxumum feasible cases derived above
  print("Substitutes")
  for (i in 1:subs.count){
    print(paste("Substitute group", i))
    print("------------------------------------------------------------")
    # Filter the data frame by group
    current_group <- df_substitutes[df_substitutes$Group == i, ]
    substitutes <- current_group$Substitute  # Get the intervention codes for this group
    for (k in substitutes){
      a <- which(df$intcode == k)
      b <- df$intervention[a]
      
      cons_substitutes[i,a] <<- cases[a] 
      cons_substitutes.limit[i] <<- subsgrp_casesmax[i]
      print(paste("Intervention: ",b, "; Code: ", k, "; Maximum cases for group:", cons_substitutes.limit[i]))
    }
  }
  
  cons_substitutes <<- t(cons_substitutes)
  
  # Changes to constraints if other modes of delivery and task shifting are allowed  
  #--------------------------------------------------------------------------------
  # Update the constraint matrices if other modes of delivery is allowed
  
  if (allow_other_modes_delivery == 0){
    if ( allow_task_shifting_pharm == 0){
      print("No task shifting of pharmaceutical tasks or other modes of delivery")
      
    } else if (allow_task_shifting_pharm == 1){
      reps <- 4
      #1. Objective
      objective <<- duplicate_matrix_vertically(reps, as.matrix(objective))
      #2. Drug budget constraint (cons_drug.limit does not need to be changed)
      cons_drug <<- duplicate_matrix_vertically(reps, as.matrix(cons_drug))
      #3. Feasible coverage constraint
      cons.feascov <<- duplicate_matrix_vertically(reps,as.matrix(cons.feascov))
      #4. Non-negativity limit for feasible coverage constraint (facility based cadres)
      cons.feascov.nonneg <<- rbind(diag(x = cases, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n))
      #5. Non-negativity limit for tasking from pharmacists to nurses 
      cons.feascov.ts1.nonneg <<- rbind(diag(x = 0, n, n), diag(x = cases, n, n), diag(x = 0, n, n), diag(x = 0, n, n))
      #6. Non-negativity limit for tasking from nutritionists to nurses 
      cons.feascov.ts2.nonneg <<- rbind(diag(x = 0, n, n), diag(x = 0, n, n), diag(x = cases, n, n), diag(x = 0, n, n))
      #6. Non-negativity limit for tasking from pharmacists and nutritionists to nurses
      cons.feascov.ts3.nonneg <<- rbind(diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = cases, n, n))
      #4. Compulsory interventions
      cons_compulsory <<- duplicate_matrix_vertically(reps,as.matrix(cons_compulsory))
      #6. Nested complements
      cons_complements <<- duplicate_matrix_vertically(reps,as.matrix(cons_complements))
      #6. Substitutes
      cons_substitutes <<- duplicate_matrix_vertically(reps,as.matrix(cons_substitutes))
    }
    
  } else if (allow_other_modes_delivery == 1){
    if (allow_task_shifting_pharm == 0) {
      reps <- 3
      #1. Objective
      objective <<- duplicate_matrix_vertically(reps, as.matrix(objective))
      #2. Drug budget constraint (cons_drug.limit does not need to be changed)
      cons_drug <<- duplicate_matrix_vertically(reps, as.matrix(cons_drug))
      #3. Feasible coverage constraint
      cons.feascov <<- duplicate_matrix_vertically(reps,as.matrix(cons.feascov))
      #4. Feasible coverage constraint for Community health workers 
      cons.feascov.chw <<- rbind(diag(x = 0, n, n), diag(x = cases, n, n), diag(x = 0, n, n))
      #5. Feasible coverage constraint for Private health workers 
      cons.feascov.private <<- rbind(diag(x = 0, n, n), diag(x = 0, n, n), diag(x = cases, n, n))
      #6. Non-negativity limit for feasible coverage constraint (facility based cadres)
      cons.feascov.nonneg <<- rbind(diag(x = cases, n, n), diag(x = 0, n, n), diag(x = 0, n, n))
      #7. Non-negativity limit for feasible coverage constraint (CHWs cadres)
      cons.feascov.chw.nonneg <<- rbind(diag(x = 0, n, n), diag(x = cases, n, n), diag(x = 0, n, n))
      #8. Non-negativity limit for feasible coverage constraint (Private cadres) 
      cons.feascov.private.nonneg <<- rbind(diag(x = 0, n, n), diag(x = 0, n, n), diag(x = cases, n, n))
      #9. Compulsory interventions
      cons_compulsory <<- duplicate_matrix_vertically(reps,as.matrix(cons_compulsory))
      #10. Nested complements
      cons_complements <<- duplicate_matrix_vertically(reps,as.matrix(cons_complements))
      #11. Substitutes
      cons_substitutes <<- duplicate_matrix_vertically(reps,as.matrix(cons_substitutes))
      
    } else if (allow_task_shifting_pharm == 1){
      reps <- 6
      #1. Objectives
      objective <<- duplicate_matrix_vertically(reps, as.matrix(objective))
      #2. Drug budget constraint (cons_drug.limit does not need to be changed)
      cons_drug <<- duplicate_matrix_vertically(reps, as.matrix(cons_drug))
      #3. Feasible coverage constraint
      cons.feascov <<- duplicate_matrix_vertically(reps,as.matrix(cons.feascov))
      #4. Feasible coverage constraint for Community health workers 
      cons.feascov.chw <<- rbind(diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = cases, n, n), diag(x = 0, n, n))
      #5. Feasible coverage constraint for Private health workers 
      cons.feascov.private <<- rbind(diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = cases, n, n))
      #6. Non-negativity limit for feasible coverage constraint (facility based cadres)
      cons.feascov.nonneg <<- rbind(diag(x = cases, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n))
      #7. Non-negativity limit for feasible coverage constraint (CHWs cadres)
      cons.feascov.chw.nonneg <<- rbind(diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = cases, n, n), diag(x = 0, n, n))
      #8. Non-negativity limit for feasible coverage constraint (Private cadres) 
      cons.feascov.private.nonneg <<- rbind(diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = cases, n, n))
      #9. Non-negativity limit for tasking from pharmacists to nurses 
      cons.feascov.ts1.nonneg <<- rbind(diag(x = 0, n, n), diag(x = cases, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n))
      #10. Non-negativity limit for tasking from nutritionists to nurses  
      cons.feascov.ts2.nonneg <<- rbind(diag(x = 0, n, n), diag(x = 0, n, n), diag(x = cases, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n))
      #11.Non-negativity limit for tasking from pharmacists and nutritionists to nurses 
      cons.feascov.ts3.nonneg <<- rbind(diag(x = 0, n, n), diag(x = 0, n, n), diag(x = 0, n, n), diag(x = cases, n, n), diag(x = 0, n, n), diag(x = 0, n, n))
      #12. Compulsory interventions
      cons_compulsory <<- duplicate_matrix_vertically(reps,as.matrix(cons_compulsory))
      #13. Nested complements
      cons_complements <<- duplicate_matrix_vertically(reps,as.matrix(cons_complements))
      #14. Substitutes
      cons_substitutes <<- duplicate_matrix_vertically(reps,as.matrix(cons_substitutes))
    } 
    
  } else {
    stop('ERROR: allow other modes of delivery and task_shifting_pharm can take values 0 or 1')
  }
  
  # Combine all the above constraints into one matrix
  # Print dimensions of individual matrices to check that the dimensions of LHS and RHS are the same
  print(paste("Dimension - Drug constraint:", paste(unlist(dim(t(cons_drug))), collapse=' ')))
  print(paste("Dimension - HR constraint:", paste( unlist(dim(t(cons_hr))), collapse=' ')))
  print(paste("Dimension - Feasible coverage constraint:", paste( unlist(dim(t(cons.feascov))), collapse=' ')))
  print(paste("Dimension - Compulsory interventions constraint:", paste( unlist(dim(t(cons_compulsory))), collapse=' ')))
  print(paste("Dimension - Substitutes constraint:", paste( unlist(dim(t(cons_substitutes))), collapse=' ')))
  print(paste("Dimension - Complements constraint:", paste( unlist(dim(t(cons_complements))), collapse=' ')))
  
  # Generate cons.mat based on whether other modes of delivery or task shifting is allowed (LHS)
  if (allow_other_modes_delivery == 1) {
    if (allow_task_shifting_pharm == 1) {
      cons.mat <<- rbind(t(cons_drug), t(cons_hr), t(cons.feascov), t(cons.feascov.chw), t(cons.feascov.private), t(cons.feascov.chw.nonneg), t(cons.feascov.private.nonneg), t(cons.feascov.nonneg),  t(cons.feascov.ts1.nonneg),  t(cons.feascov.ts2.nonneg),  t(cons.feascov.ts3.nonneg), t(cons_compulsory), t(cons_substitutes), t(cons_complements)) # LHS
    } else if (allow_task_shifting_pharm == 0) {
      cons.mat <<- rbind(t(cons_drug), t(cons_hr), t(cons.feascov), t(cons.feascov.chw), t(cons.feascov.private), t(cons.feascov.chw.nonneg), t(cons.feascov.private.nonneg), t(cons.feascov.nonneg), t(cons_compulsory), t(cons_substitutes), t(cons_complements)) # LHS
    }
  } else if (allow_other_modes_delivery == 0) {
    if (allow_task_shifting_pharm == 1) {
      cons.mat <<- rbind(t(cons_drug), t(cons_hr), t(cons.feascov), t(cons.feascov.nonneg), t(cons.feascov.ts1.nonneg), t(cons.feascov.ts2.nonneg), t(cons.feascov.ts3.nonneg), t(cons_compulsory), t(cons_substitutes), t(cons_complements)) # LHS
    } else if (allow_task_shifting_pharm == 0) {
      cons.mat <<- rbind(t(cons_drug), t(cons_hr), t(cons.feascov), t(cons.feascov), t(cons_compulsory), t(cons_substitutes), t(cons_complements)) # LHS
    }
  }
  
  # Generate constraints matrix (limits) based on whether other modes of delivery or task shifting is allowed (RHS)
  if (allow_other_modes_delivery == 1) {
    if (allow_task_shifting_pharm == 1) {
      cons.mat.limit <<- rbind(cons_drug.limit, t(cons_hr.limit), cons.feascov.limit, nonneg.lim, nonneg.lim, nonneg.lim, nonneg.lim, nonneg.lim, nonneg.lim, cons_compulsory.limit, cons_substitutes.limit, cons_complements.limit) # RHS
    } else if (allow_task_shifting_pharm == 0) {
      cons.mat.limit <<- rbind(cons_drug.limit, t(cons_hr.limit), cons.feascov.limit, nonneg.lim, nonneg.lim, nonneg.lim, cons_compulsory.limit, cons_substitutes.limit, cons_complements.limit) # RHS
    }
  } else if (allow_other_modes_delivery == 0) {
    if (allow_task_shifting_pharm == 1) {
      cons.mat.limit <<- rbind(cons_drug.limit, t(cons_hr.limit), cons.feascov.limit, nonneg.lim, nonneg.lim, nonneg.lim, nonneg.lim, cons_compulsory.limit, cons_substitutes.limit, cons_complements.limit)
    } else if (allow_task_shifting_pharm == 0) {
      cons.mat.limit <<- rbind(cons_drug.limit, t(cons_hr.limit), cons.feascov.limit, nonneg.lim, cons_compulsory.limit, cons_substitutes.limit, cons_complements.limit) # RHS
    }
  }
  
  #Check dimensions of LHS and RHS
  print(paste("Dimension of LHS", paste( unlist(dim(cons.mat)), collapse=' '))) # (1+ 8 + N + N + 1 + No. of substitutes + No. of nested complements) X N
  print(paste("Dimension of RHS", paste( unlist(dim(cons.mat.limit)), collapse=' ')))  # (1+ 8 + N + N + 1 + No. of substitutes + No. of nested complements) X 1
  
  # Direction of relationship
  # Store the number of cadres 
  hr_n <- length(hr_size)
  # Generates directions for drug budget, HR cadres, and cons.feas (cases)
  cons.dir <- rep("<=",1+hr_n+n)
  #Add additional feasibility constraints for CHWs and private
  if (allow_other_modes_delivery == 1) {
    cons.dir <- c(cons.dir,rep("<=",n), rep("<=", n))
  } else {
    cons.dir <- cons.dir
  }
  #Add non-negativity limit directions based on whether tasking shifting or other modes of delivery is allowed or not 
  if (allow_other_modes_delivery == 1) {
    if (allow_task_shifting_pharm == 1) {
      cons.dir <- c(cons.dir,rep(">=",n), rep(">=",n), rep(">=",n), rep(">=",n), rep(">=",n), rep(">=",n), rep(">=",comp.count))
    } else if (allow_task_shifting_pharm == 0) {
      cons.dir <- c(cons.dir,rep(">=",n), rep(">=",n), rep(">=",n), rep(">=",comp.count))
    }
  } else if (allow_other_modes_delivery == 0) {
    if (allow_task_shifting_pharm == 1) {
      cons.dir <- c(cons.dir,rep(">=",n), rep(">=",n), rep(">=",n), rep(">=",n), rep(">=",comp.count))
    } else if (allow_task_shifting_pharm == 0) {
      cons.dir <- c(cons.dir,rep(">=",n), rep(">=",comp.count))
    }
  }
  #Add directions for substitutable interventions 
  cons.dir <- c(cons.dir,rep("<=",subs.count))
  #Add directions for complementary interventions 
  cons.dir <- c(cons.dir, rep(">=", complements.count))
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
  solution_hr <<- as.data.frame(solution.class$solution) # use this un-collapsed version of the data-frame for HR use calculations below
  # Collapse solution by intervention
  solution_reps <- nrow(solution)/length(intcode)
  intcode.matrix <- rep(intcode, solution_reps)
  solution.df <<- data.frame(intcode = intcode.matrix, solution = solution)
  colnames(solution.df) <<- c("intcode", "solution")
  solution <<- as.data.frame(solution.df %>%
                               group_by(intcode) %>%
                               summarise(solution = sum(solution)) %>% select(solution))
  
  #solution_hf <- solution.df[1:length(intcode), "solution"]
  #solution_chw <- solution.df[(length(intcode) + 1):(2 * length(intcode)), "solution"]
  #solution_pvt <- solution.df[(2 * length(intcode) + 1):(3 * length(intcode)), "solution"]
  #solution_alt_models <- df(intcode, solution_hr, solution_chw, solution_pvt)
  
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
  hr_cadres <- c("Medical staff", "Nurse", "Pharmacist", "Lab", "Dental", "Mental", "Nutrition", "Diagnostic", "Community", "Pvt Pharmacist" )
  
  solution_hruse <<- unlist(solution_hr) * cons_hr  # Number of minutes per health worker cadre and intervention utlitised by the optimal solution
  # Collapse solution HR by intervention
  solution.df_hruse <<- data.frame(intcode = intcode.matrix, solution = solution_hruse)
  
  colnames(solution.df_hruse) <<- c("intcode", "solution_medstaff", "solution_nurse", "solution_pharmacist", "solution_lab", "solution_dental", "solution_mental", "solution_nutrition", "solution_diagnostic", "solution_community", "solution_pvtpharmacist")
  
  solution_hruse <<- as.data.frame(solution.df_hruse %>%
                                     group_by(intcode) %>%
                                     summarise(solution_medstaff = sum(solution_medstaff),
                                               solution_nurse = sum(solution_nurse),
                                               solution_pharmacist = sum(solution_pharmacist),
                                               solution_lab = sum(solution_lab),
                                               solution_dental = sum(solution_dental),
                                               solution_mental = sum(solution_mental),
                                               solution_nutrition = sum(solution_nutrition),
                                               solution_diagnostic = sum(solution_diagnostic),
                                               solution_community = sum(solution_community),
                                               solution_pvtpharmacist = sum(solution_pvtpharmacist)) %>% 
                                     select(-(intcode)))
  
  #solution_hruse.hf <- solution.df_hruse[ 1:(length(intcode)), c("solution_medstaff", "solution_nurse", "solution_pharmacist", "solution_lab", "solution_dental", "solution_mental", "solution_nutrition", "solution_community", "solution_diagnostic")]
  #solution_hruse.chw <- solution.df_hruse[(length(intcode) + 1):(2 * length(intcode)), c("solution_medstaff", "solution_nurse", "solution_pharmacist", "solution_lab", "solution_dental", "solution_mental", "solution_nutrition", "solution_community", "solution_diagnostic")]
  #solution_hruse.pvt <- solution.df_hruse[(length(intcode) + 1):(2 * length(intcode)), c("solution_medstaff", "solution_nurse", "solution_pharmacist", "solution_lab", "solution_dental", "solution_mental", "solution_nutrition", "solution_community", "solution_diagnostic")]
  
  total_hruse <<- colSums(solution_hruse, na.rm = FALSE, dims = 1) # Number of minutes per health worker cadre utlitised by the optimal solution
  hruse.prop <<- round(total_hruse/cons_hr.limit_base, 2)
  colnames(hruse.prop) <<- hr_cadres
  
  #hruse.prop_hf <- round(colSums(solution_hruse.hf, na.rm = FALSE, dims = 1)/cons_hr.limit_base,2)
  #hruse.prop_chw <- round(colSums(solution_hruse.chw, na.rm = FALSE, dims = 1)/cons_hr.limit_base,2)
  #hruse.prop_pvt <- round(colSums(solution_hruse.pvt, na.rm = FALSE, dims = 1)/cons_hr.limit_base,2)
  
  #colnames(hruse.prop_hf) <- hr_cadres
  #colnames(hruse.prop_chw) <- hr_cadres
  #colnames(hruse.prop_pvt) <- hr_cadres
  
  # Cost-effectiveness Threshold
  icer <- fullcost/dalys
  temp <- cbind.data.frame(icer, solution, df$intervention)
  temp['solution'] =  as.numeric(temp[[2]])
  temp['icer'] =  as.numeric(temp[[1]])
  cet_soln <<- round(max(temp['icer'][temp['solution'] > 0]),2) # previously temp$icer[temp$solution > 0]
  a <- which(icer == max(temp['icer'][temp['solution'] > 0])) # to check which included intervention has the highest ICER
  least.ce.intervention <- df$intervention[a]
  
  # Summarised list of outputs printed upon running the fucntion
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
# 4. Function to generate resource use stacked bar charts
#############################################################
# Note that in order to run this function, find_optimal_package needs to be run first
gen_resourceuse_graphs <- function(plot_title, file_name){
  pal <- viridisLite::viridis(10) # Create a viridis palette for the graph
  #pal <- rainbow(10)
  
  ## Generate matrix representing HR and Drug budget use by the HBP solution run above
  #--------------------------------------------------------------------------------------
  # HR Resource Use
  data_hr <- sweep(solution_hruse, 2, cons_hr.limit_base, FUN = '/')
  
  if (allow_other_modes_delivery==1){
    hr_cadres <- c("Doctor/\nClinical officer", "Nursing \nstaff", "Pharmaceutical \nstaff", "Laboratory \nstaff", 
                   "Dental \nstaff", "Mental Health \nstaff", "Nutrition \nstaff", "Diagnostic \nstaff", "Community \nhealth \nworkers", "Private \nPharmacists")
  } else {
    hr_cadres <- c("Doctor/\nClinical officer", "Nursing \nstaff", "Pharmaceutical \nstaff", "Laboratory \nstaff", 
                   "Dental \nstaff", "Mental Health \nstaff", "Nutrition \nstaff", "Diagnostic \nstaff")
  }
  # Drug budget Use
  data_drug <- as.matrix(solution_drugexp)/cons_drug.limit_base
  
  length(data_drug) = dim(data_hr)[1] # Assert that the length of the directions list is the same as that of the constraints matrix
  
  # Combine all resource use matrices into one matrix
  data <- cbind(data_hr,data_drug)
  data <- as.matrix(data)
  # Drop Dental staff from the matrix
  data <- data[,-c(5)] 
  data <- cbind(category,data)
  
  ## Convert to long form in order to apply ggplot 
  #--------------------------------------------------------------------------------------
  data <- as.data.frame(data)
  colnames(data) <- c('Category', hr_cadres[-c(5)], 'Consumables \nbudget')
  data_long <<- gather(data, resource, use, 2:'Consumables \nbudget', factor_key=TRUE)
  data_long$use <<- as.numeric(data_long$use) # convert use data to numeric
  
  ## Generate graph
  #--------------------------------------------------------------------------------------
  p <- ggplot(data = data_long, aes(x = resource, y = use)) +
    geom_col(aes(fill = Category), width = 0.7)
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






