#############################################################
## Uganda's Health Benefits Package - All scenarios

## Created by: Sakshi Mohan; 15/12/20

## Updated by: Megha Rao: 16/09/2024

## This file sets the inputs for the linear constrained optimisation function to be used in
# scenario generation for Uganda's HBP analysis
#############################################################

##########################################################
# 1 - Set Working Directory & and Run LP function Script
##########################################################
# Point to where the repo is stored on your machine
setwd ("C:/Users/crw571/OneDrive - University of York/Desktop/uganda_hbp")
#setwd("/Users/sm2511/Dropbox/York/Research Projects/Uganda EHP/Analysis/repo/uganda_hbp") 

# Run R script which generates LP function
source("1_script/0_packages_and_functions.R")

###################################
# 2 - Set up inputs for LPP
###################################
## Pre-code main inputs, complements, and substitutes for the scenarios that follow 
#######################################################################################
#  Common function inputs
#------------------------
chosen_data_file <- "2_data/chbp_2023_full_dataset.xls"
chosen_df <- df
base.cet <- 165 # This value is in 2023 USD 
base.drugbudget <- 560823263 #new Uganda drugs and consumables budget
base.hr <- rep(1,10)
no.hr.limit <- rep(9999999999,10) # set an arbitrarily high scaling figure to represent no constraint
nurse.limit <- c(999999999, 1, 999999999, 999999999, 999999999, 999999999,999999999,999999999,999999999,999999999)
no.drugbudget.limit <- 9999999999
no.cet <- 9999999999

########################################################################################################
# 3 - Run optimisation under a variety of constraint scenarios
#-------------------------------------------------------------------------------------------------------
########################################################################################################
# Scenario 1: No constraints
#----------------------------------------------------------------------------------------------------------------------------

visible_cadres = c(1:3,5:10) # showing all cadres except the dental staff


capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = no.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = no.drugbudget.limit, 
                       hr_scale = no.hr.limit, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                       allow_task_shifting = 0)  
)
drug_exp.prop = drug_exp.prop * no.drugbudget.limit/base.drugbudget
scen1 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted,  dalys_averted.prop,  cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen1_coverage = solution


# Scenario 2: CET = $165
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = no.drugbudget.limit, 
                       hr_scale = no.hr.limit, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                       allow_task_shifting = 0)
)
drug_exp.prop = drug_exp.prop * no.drugbudget.limit/base.drugbudget
scen3 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted,  dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen3_coverage = solution

# Scenario 3: CET = $165 + Drug budget constraint
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = no.hr.limit, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                       allow_task_shifting = 0)
)
scen4 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted,  dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen4_coverage = solution


# Scenario 4: CET = $165 + Drug budget constraint + HR capacity constraint 
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 1, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                       allow_task_shifting = 0)
)
scen5 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen5_coverage = solution


# Scenario 5: CET = $165 + Drug budget constraint + CHW delivery + HR capacity constraint 
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                       allow_task_shifting = 0)
)
scen6 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen6_coverage = solution


#Scenario 6: CET = $165 + Drug budget constraint + Private Pharmacist + HR capacity constraint  
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                       allow_task_shifting = 0)
)
scen7 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted,  dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen7_coverage = solution


# Scenario 7: CET = $165 + Drug budget constraint + CHW delivery + Private pharmacist + HR capacity constraint 
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                       allow_task_shifting = 0)
)
scen7 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted,  dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen7_coverage = solution

# Scenario 8: CET = $165 + Drug budget constraint + HR capacity constraint (WITH TASKSHIFTING)
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                       allow_task_shifting = 1)
)
scen8 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted,  dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen8_coverage = solution


# Scenario 9: CET = $165 + Drug budget constraint + CHW delivery + Private pharmacist + HR capacity constraint (WITH TASKSHIFTING)
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                       allow_task_shifting = 1)
)
scen9 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted,  dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen9_coverage = solution

# Scenario 10: CET = $165 + Drug budget constraint + Private pharmacist + Mark up drug cost + HR capacity constraint 
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                       allow_task_shifting = 0)
)
scen10 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted,  dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen10_coverage = solution

# Scenario 11: CET = $165 + Drug budget constraint + CHW Delivery + Private pharmacist + Mark up drug cost + HR capacity constraint 
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                       allow_task_shifting = 0)
)
scen11 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted,  dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen11_coverage = solution

# Scenario 12: CET = $165 + Drug budget constraint + CHW Delivery + Private pharmacist + Mark up drug cost + HR capacity constraint (with TASKSHIFTING)
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                       allow_task_shifting = 1)
)
scen12 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted,  dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen12_coverage = solution


# Extract scenario results to .csv
#----------------------------------------------------------------------------------------------------------------------------
scenarios = c("No constraints",
              "CET ($165)",
              "CET ($165) + Drug Budget",
              "CET ($165) + Drug budget + HR capacity",
              "CET ($165) + Drug budget + CHW delivery + HR capacity",
              "CET ($165) + Drug budget + private pharmacist + HR capacity",
              "CET ($165) + Drug budget + CHW delivery + private pharmacist + HR capacity",
              "CET ($165) + Drug budget + HR capacity + taskshifting",
              "CET ($165) + Drug budget + CHW delivery + private pharmacist + HR capacity + taskshifting",
              "CET ($165) + Drug budget + private pharmacist + mark up drug cost + HR capacity",
              "CET ($165) + Drug budget + CHW delivery + private pharmacist + mark up drug cost + HR capacity",
              "CET ($165) + Drug budget + CHW delivery + private pharmacist + mark up drug cost + HR capacity + taskshifting"
)

# Ovrall summary results
summary = rbind(scen1, scen2, scen3, scen4, scen5, scen6, scen7, scen8, scen9, scen10, scen11, scen12)
summary = cbind(scenarios, summary)
colnames(summary) = c("Constraints applied", 
                      "Number of interventions with positive NHB", 
                      "Number of interventions in the optimal package", 
                      "Total DALYs averted", 
                      "Proportion of DALYs averted",
                      "Highest ICER in the HBP", 
                      "% of drug budget required",
                      "% of Doctor/Clinical officer capacity required", "% of Nursing staff capacity required",
                      "% of Pharmaceutical staff capacity required", 
                      "% of Lab staff capacity required",
                      "% of Mental health staff capacity required", 
                      "% of Nutrition staff capacity required",
                      "% of Diagnostic staff capacity required",
                      "% of Community health workers capacity required",
                      "% of Private pharmacist capacity required")

#print(xtable(summary, type = "latex"), file = "4_outputs/tables/scanario_summaries.tex")
#write.csv(t(summary), file = "4_outputs/tables/all_scenarios_results.csv")

# Results on chosen package/coverage under various scenarios
coverage_byscenario = cbind(category, intcode, intervention, scen1_coverage, scen2_coverage, scen3_coverage, scen4_coverage, scen5_coverage, scen6_coverage, scen7_coverage, scen8_coverage, scen9_coverage, scen10_coverage, scen11_coverage, scen12_coverage)
colnames(coverage_byscenario) = c("Program", "Intervention code", "Intervention", scenarios)
#write.csv(coverage_byscenario, file = "4_outputs/tables/all_scenarios_coverage_results.csv")

#---------------------------------------------------------------------------------------------------------------------------------------------
