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
#setwd ("C:/Users/crw571/OneDrive - University of York/Desktop/R files for constrained optimization/")
setwd("/Users/sm2511/Dropbox/York/Research Projects/Uganda EHP/Analysis/repo/uganda_hbp") 

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
base.drugbudget <- 600000000 #subject to change after MakSPH inputs
base.hr <- rep(1,10)
no.hr.limit <- rep(9999999999,10) # set an arbitrarily high scaling figure to represent no constraint
nurse.limit <- c(999999999, 1, 999999999, 999999999, 999999999, 999999999,999999999,999999999,999999999,999999999)
no.drugbudget.limit <- 9999999999
no.cet <- 9999999999
gdp_pc = 964 # GDP per capita current or constant?

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
                       hr_scale = no.hr.limit, allow_other_modes_delivery = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1, compulsory_intervention_coverage_scale = 1,
                       allow_task_shifting_pharm = 0)  
)
drug_exp.prop = drug_exp.prop * no.drugbudget.limit/base.drugbudget
scen1 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted,  dalys_averted.prop,  cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen1_coverage = solution


# Scenario 2: CET = 0.5 X GDP per capita
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = 0.5 * gdp_pc, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = no.drugbudget.limit, 
                       hr_scale = no.hr.limit, allow_other_modes_delivery = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1, compulsory_intervention_coverage_scale = 1,
                       allow_task_shifting_pharm = 0)
)
drug_exp.prop = drug_exp.prop * no.drugbudget.limit/base.drugbudget
scen2 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted,  dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen2_coverage = solution

# Scenario 3: CET = $165
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = no.drugbudget.limit, 
                       hr_scale = no.hr.limit, allow_other_modes_delivery = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1, compulsory_intervention_coverage_scale = 1,
                       allow_task_shifting_pharm = 0)
)
drug_exp.prop = drug_exp.prop * no.drugbudget.limit/base.drugbudget
scen3 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted,  dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen3_coverage = solution

# Scenario 4: CET = $165 + Drug budget constraint
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = no.hr.limit, allow_other_modes_delivery = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1, compulsory_intervention_coverage_scale = 1,
                       allow_task_shifting_pharm = 0)
)
scen4 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted,  dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen4_coverage = solution


# Scenario 5: CET = $165 + Drug budget constraint + HR capacity constraint 
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_other_modes_delivery = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1, compulsory_intervention_coverage_scale = 1,
                       allow_task_shifting_pharm = 0)
)
scen5 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen5_coverage = solution


# Scenario 6: CET = $165 + Drug budget constraint + Other modes of delivery + HR capacity constraint 
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_other_modes_delivery = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1, compulsory_intervention_coverage_scale = 1,
                       allow_task_shifting_pharm = 0)
)
scen6 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen6_coverage = solution


#Scenario 7: CET = $165 + Drug budget constraint + HR capacity (with task shifting) 
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_other_modes_delivery = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1, compulsory_intervention_coverage_scale = 1,
                       allow_task_shifting_pharm = 1)
)
scen7 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted,  dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen7_coverage = solution


# Scenario 8: CET = $165 + Drug budget constraint + Other modes of delivery + HR capacity constraint (with task shifting) 
#----------------------------------------------------------------------------------------------------------------------------
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_other_modes_delivery = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1, compulsory_intervention_coverage_scale = 1, 
                       allow_task_shifting_pharm = 1)
)
scen8 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted,  dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen8_coverage = solution

# Scenario 9: CET = $165 + Other modes of delivery + HR capacity constraint (with task shifting) (Unlimited Budget)
#----------------------------------------------------------------------------------------------------------------------------
#capture.output(
#  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
#                       drug_budget_input = no.drugbudget.limit, drug_budget_scale = 1, 
#                       hr_scale = base.hr, allow_other_modes_delivery = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1, compulsory_intervention_coverage_scale = 1, 
#                       allow_task_shifting_pharm = 1)
#)
#scen9 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted,  dalys_averted.prop, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
#scen9_coverage = solution

# Scenario 9: CET = $165 + Drug budget constraint + Other modes of delivery + Demand constraint [Optional]
#----------------------------------------------------------------------------------------------------------------------------
#capture.output(
#  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
#                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
#                       hr_scale = base.hr, allow_other_modes_delivery = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1, compulsory_intervention_coverage_scale = 1, 
#                       allow_task_shifting_pharm = 1)
#)
#scen9 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
#scen9_coverage = solution

# Scenario 10: CET = $165 + Drug budget constraint + Other modes of delivery +  Demand constraint + HR capacity constraint (No pharmacist constraint) [Optional]
#----------------------------------------------------------------------------------------------------------------------------------------------------------------
#capture.output(
#  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
#                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
#                       hr_scale = base.hr, allow_other_modes_delivery = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1, compulsory_intervention_coverage_scale = 1, 
#                       allow_task_shifting_pharm = 1)
#)
#scen10 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
#scen10_coverage = solution

# Scenario 11: CET = $165 + Drug budget constraint + Other modes of delivery + Demand constraint + HR capacity constraint [Optional]
#--------------------------------------------------------------------------------------------------------------------------------------------------------
#capture.output(
#  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
#                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
#                       hr_scale = base.hr, allow_other_modes_delivery = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1, compulsory_intervention_coverage_scale = 1, 
#                       allow_task_shifting_pharm = 1)
#)
#scen11 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
#scen11_coverage = solution

# Scenario 12: CET = $165 + Drug budget constraint + Other modes of delivery + Demand constraint + HR capacity constraint (with task shifting) [Optional]
#--------------------------------------------------------------------------------------------------------------------------------------------------------
#capture.output(
#  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
#                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
#                       hr_scale = base.hr, allow_other_modes_delivery = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1, compulsory_intervention_coverage_scale = 1, 
#                       allow_task_shifting_pharm = 1)
#)
#scen12 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
#scen12_coverage = solution


# Extract scenario results to .csv
#----------------------------------------------------------------------------------------------------------------------------
scenarios = c("No constraints",
              "CET (0.5 X GDP per capita)",
              "CET ($165)",
              "CET ($165) + Drug Budget",
              "CET ($165) + Drug budget + HR capacity",
              "CET ($165) + Drug budget + Other modes of delivery + HR capacity",
              "CET ($165) + Drug budget + HR capacity(with task shifting)",
              "CET ($165) + Drug budget + Other modes of delivery + HR capacity(with task shifting to nurses)"
              #,"CET ($165) + Drug budget + Other modes of delivery + Demand constraint",
              #"CET ($165) + Drug budget + Other modes of delivery + Demand constraint + HR capacity(except pharmacists)",
              #"CET ($165) + Drug budget + Other modes of delivery + Demand constraint + HR capacity",
              #"CET ($165) + Drug budget + Other modes of delivery + Demand constraint + HR capacity(with task shifting to nurses)"
)

# Overall summary results
summary = rbind(scen1, scen2, scen3, scen4, scen5, scen6, scen7, scen8)
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
coverage_byscenario = cbind(category, intcode, intervention, scen1_coverage, scen2_coverage, scen3_coverage, scen4_coverage, scen5_coverage, scen6_coverage, scen7_coverage, scen8_coverage)
colnames(coverage_byscenario) = c("Program", "Intervention code", "Intervention", scenarios)
#write.csv(coverage_byscenario, file = "4_outputs/tables/all_scenarios_coverage_results.csv")

#---------------------------------------------------------------------------------------------------------------------------------------------
