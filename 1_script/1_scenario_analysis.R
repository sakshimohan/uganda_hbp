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
setwd ("/Users/crw571/Desktop/uganda_hbp")
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
base.cet <- 165 # This value is in 2023 USD 
base.drugbudget <- 560823263 #new Uganda drugs and consumables budget
base.hr <- rep(1,10)
no.hr.limit <- rep(9999999999,10) # set an arbitrarily high scaling figure to represent no constraint
no.drugbudget.limit <- 9999999999
no.cet <- 9999999999

########################################################################################################
# 3 - Run optimisation under a variety of constraint scenarios
#-------------------------------------------------------------------------------------------------------
########################################################################################################
# Scenario 1: No constraints
#----------------------------------------------------------------------------------------------------------------------------

visible_cadres = c(1:4,6:10) # showing all cadres except the dental staff

scenarios = c("Baseline: facility-based delivery", "Inclusion of VHTs only", "Inclusion of medicine retailers only", "Inclusion of both VHTs and medicine retailers", 
              "Allowing mark up for inclusion of medicine retailers", "VHTs and allowing mark up for medicine retailers", "Allowing taskshifting (to baseline)", 
              "Allowing taskshifting & inclusion of VHTs", "Allowing taskshifting & inclusion of medicine retailers", 
              "Allowing taskshifting & inclusion of VHTs & medicine retailers", "Takshifting with markup for medicine retailers", 
              "Taskshifting with VHTs and medicine retailers and markup", "Baseline: facility-based delivery unconstrained", "Inclusion of VHTs only unconstrained", 
              "Inclusion of medicine retailers only unconstrained", "Inclusion of both VHTs and medicine retailers unconstrained", 
              "Allowing mark up for inclusion of medicine retailers unconstrained", "VHTs and allowing mark up for medicine retailers unconstrained")  # for file names

scenario_labels = c("Baseline scenario", "Standalone VHT Integration", "Standalone Medicine Retailers Integration", "Joint Integration", 
                    "Standalone Medicine Retailers Integration with markup", "Joint Integration with markup", "Taskshifting", "Taskshifting and Standalone VHT Integration", 
                    "Taskshifting and Standalone Medicine Retailers Integration", 
                    "Taskshifting and Joint Integration", "Taskshifting and Standalone Medicine Retailers Integration with Markup", 
                    "Taskshifting and Joint Integration with Markup", "Standalone VHT Integration unconstrained", "Standalone Medicine Retailers Integration unconstrained", "Joint Integration unconstrained", 
                    "Standalone Medicine Retailers Integration with markup unconstrained", "Joint Integration with markup unconstrained") # for table headers

#1.Baseline: facility-based delivery
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_base = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_base = solution


#2. Inclusion of VHTs only
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_vht = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_vht = solution

#3. Inclusion of medicine retailers only
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_mr = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_mr = solution


#4. Inclusion of both VHTs and medicine retailers 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_vht_and_mr = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_chw_and_pvtpharm = solution

#5. #Allowing mark up with inclusion of medicine retailers 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_mr_markup = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_mr_markup = solution

#6. Allowing mark up with inclusion of VHTs and medicine retailers 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_mr_vht_markup = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_mr_vht_markup = solution

#7. Sensitivity analysis: Allowing facility based taskshifting to the baseline scenario 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)

summary_taskshifting = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_taskshifting = solution

#8. Sensitivity analysis: Allowing facility based taskshifting with inclusion of VHTs  
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)  

summary_taskshifting_vht = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_taskshifting_vht = solution 

#9. Sensitivity analysis: Allowing facility based taskshifting with inclusion of medicine retailers 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)  

summary_taskshifting_mr = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_taskshifting_mr = solution

#10. Sensitivity analysis: Allowing facility based taskshifting with inclusion of VHTs and medicine retailers 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)  

summary_taskshifting_with_both = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_taskshifting_with_both = solution

#11. Allowing mark up and facility based taskshifting with inclusion of medicine retailers  
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)  

summary_mr_taskshift_markup = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_mr_taskshift_markup = solution

#12. Allowing mark up and facility based taskshifting with inclusion of VHTs and medicine retailers  
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)  

summary_mr_vht_taskshift_markup = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_mr_vht_taskshift_markup = solution

#  Common function inputs
#------------------------
chosen_data_file <- "2_data/chbp_2023_full_dataset_SA.xls"
chosen_df <- df
base.cet <- 165 # This value is in 2023 USD 
base.drugbudget <- 560823263 #new Uganda drugs and consumables budget
base.hr <- rep(1,10)
no.hr.limit <- rep(9999999999,10) # set an arbitrarily high scaling figure to represent no constraint
no.drugbudget.limit <- 9999999999
no.cet <- 9999999999

########################################################################################################
# 3 - Run optimisation under a variety of constraint scenarios
#-------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------

visible_cadres = c(1:4,6:10) # showing all cadres except the dental staff

#2. Inclusion of VHTs only (no constraints)
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_vht_sa = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_vht_sa = solution

#3. Inclusion of medicine retailers only (no constraints)
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_mr_sa = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_mr_sa = solution

#4. Inclusion of both VHTs and medicine retailers (no constraints)
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_vht_and_mr_sa = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_vht_and_mr_sa = solution

#5. #Allowing mark up with inclusion of medicine retailers (no constraints)
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_mr_markup_sa = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_mr_markup_sa = solution

#6. Allowing mark up with inclusion of VHTs and medicine retailers (no constraints)
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_mr_vht_markup_sa = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_mr_vht_markup_sa = solution

----------------------------------------------------------------------------

summary = rbind(summary_base, summary_vht, summary_mr, summary_vht_and_mr, 
                summary_mr_markup, summary_mr_vht_markup, summary_taskshifting, summary_taskshifting_vht, summary_taskshifting_mr, 
                summary_taskshifting_with_both, summary_mr_taskshift_markup, summary_mr_vht_taskshift_markup, 
                summary_vht_sa, summary_mr_sa, summary_vht_and_mr_sa, 
                summary_mr_markup_sa, summary_mr_vht_markup_sa)
summary = cbind(scenario_labels, summary)
colnames(summary) = c("Scenario", 
                      "Number of interventions with a positive Net Health Benefit", 
                      "Number of interventions in the optimal package", 
                      "Total DALYs averted",
                      "Percentage of the total DALYs averted",
                      "Net DALYs averted", 
                      "Highest ICER in the optimal package", 
                      "Percentage of drug budget required",
                      "Percentage of Doctor/Clinical officer capacity required", 
                      "Percentage of Nursing staff capacity required",
                      "Percentage of Pharmaceutical staff capacity required", 
                      "Percentage of Laboratory staff capacity required",
                      "Percentage of Mental health staff capacity required",
                      "Percentage of Nutrition staff capacity required", 
                      "Percentage of Radiography staff capacity required",
                      "Percentage of Village health team capacity required",
                      "Percentage of Medicine retailers capacity required")

summary$`Percentage of the total DALYs averted` <- sprintf("%.2f%%", (summary$`Percentage of the total DALYs averted`) * 100)
summary$`Percentage of drug budget required` <- sprintf("%.2f%%", (summary$`Percentage of drug budget required`) * 100)
summary$`Percentage of Doctor/Clinical officer capacity required` <- sprintf("%.2f%%", (summary$`Percentage of Doctor/Clinical officer capacity required`) * 100)
summary$`Percentage of Nursing staff capacity required` <- sprintf("%.2f%%", (summary$`Percentage of Nursing staff capacity required`) * 100)
summary$`Percentage of Pharmaceutical staff capacity required` <- sprintf("%.2f%%", (summary$`Percentage of Pharmaceutical staff capacity required`) * 100)
summary$`Percentage of Laboratory staff capacity required` <- sprintf("%.2f%%", (summary$`Percentage of Laboratory staff capacity required`) * 100)
summary$`Percentage of Mental health staff capacity required` <- sprintf("%.2f%%", (summary$`Percentage of Mental health staff capacity required`) * 100)
summary$`Percentage of Nutrition staff capacity required` <- sprintf("%.2f%%", (summary$`Percentage of Nutrition staff capacity required`) * 100)
summary$`Percentage of Radiography staff capacity required` <- sprintf("%.2f%%", (summary$`Percentage of Radiography staff capacity required`) * 100)
summary$`Percentage of Villa health team capacity required` <- sprintf("%.2f%%", (summary$`Percentage of Village health team capacity required`) * 100)
summary$`Percentage of Medicine retailers capacity required` <- sprintf("%.2f%%", (summary$`Percentage of Medicine retailers capacity required`) * 100)

write.csv(summary, file = "4_outputs/tables/all_scenarios_coverage_results.csv")

#---------------------------------------------------------------------------------------------------------------------------------------------
