#############################################################
## Linear Programming Function to optimize Uganda's Health Benefits Package

## Created by: Sakshi Mohan; 01/2021

## Updated by: Megha Rao; 02/2025

## This file generates the outputs for the new community based health benefits package for Uganda 
#############################################################


##########################################################
# 1 - Set Working Directory & and Run LP function Script
##########################################################
setwd ("C:/Users/crw571/OneDrive - University of York/Desktop/uganda_hbp")
#setwd("/Users/sm2511/Dropbox/York/Research Projects/Uganda EHP/Analysis/repo/uganda_hbp") 

# Run R script which generates LP function
source("1_script/0_packages_and_functions.R")

##########################################################
# 2 - Set up common inputs for scenarios
##########################################################
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
#----------------------------------------------------------------------------------------------------------------------------

visible_cadres = c(1:4,6:10) # showing all cadres except the dental staff

# Scenarios
##########################################################
scenarios = c("baseline: facility-based delivery", "Inclusion of CHWs only", "Inclusion of private pharmacists only", "Inclusion of both CHWs and private pharmacists", 
              "Allowing taskshifting (to baseline)", "Allowing taskshifting & inclusion of CHWs", "Allowing taskshifting & inclusion of private pharmacists", 
              "Allowing taskshifting & inclusion of CHWs & private pharmacists", "Allowing mark up for inclusion of private pharmacists", 
              "CHWs and allowing mark up for private pharmacists", "Takshifting with markup for private pharmacists", "Taskshifting with both modes and markup") # for file names
scenario_labels = c("Baseline scenario", "CHW scenario", "Private pharmacy scenario", "CHW and Private pharmacists scenario", 
                    "Taskshifting scenario", "Taskshifting with CHW scenario", "Taskshifting with Private pharmacists scenario", 
                    "Takshifting with CHWs & private pharmacists scenario", "Inclusion of private pharmacists with mark up", "Inclusion of both CHWs and private pharmacists with mark up",
                    "Taskshifting with Private pharmacists scenario with markup", "Takshifting with CHWs & private pharmacists scenario with markup" ) # for table headers

#Baseline: facility-based delivery
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_base = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_base = solution


#Inclusion of CHWs only
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_chw = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_chw = solution

#Inclusion of private pharmacies only
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_pvtpharm = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_pvtpharm = solution


#Inclusion of both CHWs and private pharmacies
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_chw_and_pvtpharm = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_chw_and_pvtpharm = solution

#Allowing taskshifting (to baseline)
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                     allow_task_shifting = 1)

summary_taskshifting = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_taskshifting = solution


#Allowing task-shifting and inclusion of CHW
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                     allow_task_shifting = 1)

summary_taskshifting_chw = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_taskshifting_chw = solution

#Allowing task-shifting and inclusion of private pharmacists 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                     allow_task_shifting = 1)

summary_taskshifting_pvt_pharm = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_taskshifting_pvt_pharm = solution

#Allowing taskshifting & inclusion of CHWs & private pharmacies
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                     allow_task_shifting = 1)

summary_taskshifting_with_other_modes = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_taskshifting_with_othermodes = solution



###########################################################################################################################################################
### Optional: Scenarios involving mark up ###

#Allowing private pharmacists and mark up (no task shifting) 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_pvtpharm_markup = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_pvtpharm_markup = solution

#Allowing CHWs and private pharmacists and mark up (no task shifting) 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_pvtpharm_chw_markup = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_pvtpharm_chw_markup = solution

#Allowing private pharmacists and mark up (with task shifting) 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)

summary_pvtpharm_taskshift_markup = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_pvtpharm_taskshift_markup = solution

#Allowing CHWs and private pharmacists and mark up (with task shifting) 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)

summary_pvtpharm_chw_taskshift_markup = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_pvtpharm_chw_taskshift_markup = solution

# Table 2: Result Summary
##########################################################
summary = rbind(summary_base, summary_chw, summary_pvtpharm, summary_chw_and_pvtpharm, summary_taskshifting, summary_taskshifting_chw, summary_taskshifting_pvt_pharm, summary_taskshifting_with_other_modes, 
                summary_pvtpharm_markup, summary_pvtpharm_chw_markup, summary_pvtpharm_taskshift_markup, summary_pvtpharm_chw_taskshift_markup)
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
                      "Percentage of Community Health Workers required",
                      "Percentage of Private Pharmacies required")

summary$`Percentage of the total DALYs averted` <- sprintf("%.2f%%", (summary$`Percentage of the total DALYs averted`) * 100)
summary$`Percentage of drug budget required` <- sprintf("%.2f%%", (summary$`Percentage of drug budget required`) * 100)
summary$`Percentage of Doctor/Clinical officer capacity required` <- sprintf("%.2f%%", (summary$`Percentage of Doctor/Clinical officer capacity required`) * 100)
summary$`Percentage of Nursing staff capacity required` <- sprintf("%.2f%%", (summary$`Percentage of Nursing staff capacity required`) * 100)
summary$`Percentage of Pharmaceutical staff capacity required` <- sprintf("%.2f%%", (summary$`Percentage of Pharmaceutical staff capacity required`) * 100)
summary$`Percentage of Laboratory staff capacity required` <- sprintf("%.2f%%", (summary$`Percentage of Laboratory staff capacity required`) * 100)
summary$`Percentage of Mental health staff capacity required` <- sprintf("%.2f%%", (summary$`Percentage of Mental health staff capacity required`) * 100)
summary$`Percentage of Nutrition staff capacity required` <- sprintf("%.2f%%", (summary$`Percentage of Nutrition staff capacity required`) * 100)
summary$`Percentage of Radiography staff capacity required` <- sprintf("%.2f%%", (summary$`Percentage of Radiography staff capacity required`) * 100)
summary$`Percentage of Community Health Workers required` <- sprintf("%.2f%%", (summary$`Percentage of Community Health Workers required`) * 100)
summary$`Percentage of Private Pharmacies required` <- sprintf("%.2f%%", (summary$`Percentage of Private Pharmacies required`) * 100)


write.csv(t(summary), file = "4_outputs/tables/table_2_result_summary.csv")

-------------------------------------------------------------------------------------------
  ####Some hypothetical scenarios - constraints related for task-shifting#####  

#Task-shifting no drug budget constraint 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = no.drugbudget.limit, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)

summary_ts_nodrugbudget = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_ts_nodrugbudget = solution

#Task-shifting no drug budget constraint with CHW
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = no.drugbudget.limit, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)

summary_ts_chw_nodrugbudget = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_ts_chw_nodrugbudget = solution

#Task-shifting no pharmacist, no doctor, and no lab constraint 
no.limit <- c(999999999, 1, 9999999999, 99999999999, 1, 9999999999, 9999999999, 1, 1, 1)
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = no.drugbudget.limit, 
                     hr_scale = no.limit, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)

summary_ts_nohr_nochw = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_ts_nohr_nochw = solution

#Task-shifting no pharmacist, no doctor, and no lab constraint with CHW
no.limit <- c(999999999, 1, 9999999999, 99999999999, 1, 9999999999, 9999999999, 1, 1, 1)
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = no.drugbudget.limit, 
                     hr_scale = no.limit, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)

summary_ts_nohr_chw = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_ts_nohr_chw = solution

summary_hyp = rbind(summary_taskshifting, summary_taskshifting_chw, summary_ts_nodrugbudget, summary_ts_chw_nodrugbudget,
                    summary_ts_nohr_nochw, summary_ts_nohr_chw)

scenario_labels_hyp = c( "Taskshifting scenario", "Taskshifting with CHW scenario", "Taskshifting with no drug budget limit scenario", 
                         "Takshifting with no drug budget and with CHWs scenario", "Taskshifting with no doctor, lab, and pharm limit", "Takshifting with no doctor, lab, and pharm limit and with CHWs scenario" ) # for table headers

summary_hyp = cbind(scenario_labels_hyp, summary_hyp)
colnames(summary_hyp) = c("Scenario", 
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
                          "Percentage of Community Health Workers required",
                          "Percentage of Private Pharmacies required")

summary_hyp$`Percentage of the total DALYs averted` <- sprintf("%.2f%%", (summary_hyp$`Percentage of the total DALYs averted`) * 100)
summary_hyp$`Percentage of drug budget required` <- sprintf("%.2f%%", (summary_hyp$`Percentage of drug budget required`) * 100)
summary_hyp$`Percentage of Doctor/Clinical officer capacity required` <- sprintf("%.2f%%", (summary_hyp$`Percentage of Doctor/Clinical officer capacity required`) * 100)
summary_hyp$`Percentage of Nursing staff capacity required` <- sprintf("%.2f%%", (summary_hyp$`Percentage of Nursing staff capacity required`) * 100)
summary_hyp$`Percentage of Pharmaceutical staff capacity required` <- sprintf("%.2f%%", (summary_hyp$`Percentage of Pharmaceutical staff capacity required`) * 100)
summary_hyp$`Percentage of Laboratory staff capacity required` <- sprintf("%.2f%%", (summary_hyp$`Percentage of Laboratory staff capacity required`) * 100)
summary_hyp$`Percentage of Mental health staff capacity required` <- sprintf("%.2f%%", (summary_hyp$`Percentage of Mental health staff capacity required`) * 100)
summary_hyp$`Percentage of Nutrition staff capacity required` <- sprintf("%.2f%%", (summary_hyp$`Percentage of Nutrition staff capacity required`) * 100)
summary_hyp$`Percentage of Radiography staff capacity required` <- sprintf("%.2f%%", (summary_hyp$`Percentage of Radiography staff capacity required`) * 100)
summary_hyp$`Percentage of Community Health Workers required` <- sprintf("%.2f%%", (summary_hyp$`Percentage of Community Health Workers required`) * 100)
summary_hyp$`Percentage of Private Pharmacies required` <- sprintf("%.2f%%", (summary_hyp$`Percentage of Private Pharmacies required`) * 100)

write.csv(t(summary_hyp), file = "4_outputs/tables/table_2_1_result_summary.csv")

# Table 3: Summary by program
##########################################################
# Baseline scenario: Only facility based delivery
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                       allow_task_shifting = 0))

program_detailed_base = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt1 <- program_detailed_base %>% group_by(category) %>% summarize_all(sum)
program_summary_pt1[,4] <- program_summary_pt1[,3]/program_summary_pt1[,2]
colnames(program_summary_pt1)[4] <- "solution_pt1"
program_summary_pt1 <- program_summary_pt1 %>% rename(total.dalys.avertible = `cases * dalys`)

#Inclusion of CHWs only 
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                                    allow_task_shifting = 0))

program_detailed_chw = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt2 <- program_detailed_chw %>% group_by(category) %>% summarize_all(sum)
program_summary_pt2[,4] <- program_summary_pt2[,3]/program_summary_pt2[,2]
colnames(program_summary_pt2)[4] <- "solution_pt2"
program_summary_pt2 <- program_summary_pt2 %>% rename(total.dalys.avertible = `cases * dalys`)

#Inclusion of private pharmacies only
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                                    allow_task_shifting = 0))

program_detailed_pvtpharm = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt3 <- program_detailed_pvtpharm %>% group_by(category) %>% summarize_all(sum)
program_summary_pt3[,4] <- program_summary_pt3[,3]/program_summary_pt3[,2]
colnames(program_summary_pt3)[4] <- "solution_pt3"
program_summary_pt3 <- program_summary_pt3 %>% rename(total.dalys.avertible = `cases * dalys`)

#Inclusion of both CHWs and private pharmacies
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                                    allow_task_shifting = 0))

program_detailed_chw_pvtpharm = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt4 <- program_detailed_chw_pvtpharm %>% group_by(category) %>% summarize_all(sum)
program_summary_pt4[,4] <- program_summary_pt4[,3]/program_summary_pt4[,2]
colnames(program_summary_pt4)[4] <- "solution_pt4"
program_summary_pt4 <- program_summary_pt4 %>% rename(total.dalys.avertible = `cases * dalys`)

#Allowing taskshifting (to baseline)
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                                    allow_task_shifting = 1))

program_detailed_taskshifting = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt5 <- program_detailed_taskshifting %>% group_by(category) %>% summarize_all(sum)
program_summary_pt5[,4] <- program_summary_pt5[,3]/program_summary_pt5[,2]
colnames(program_summary_pt5)[4] <- "solution_pt5"
program_summary_pt5 <- program_summary_pt5 %>% rename(total.dalys.avertible = `cases * dalys`)

#Allowing taskshifting & inclusion of CHWs
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                                    allow_task_shifting = 1))

program_detailed_taskshifting_othermodes = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt6 <- program_detailed_taskshifting_othermodes %>% group_by(category) %>% summarize_all(sum)
program_summary_pt6[,4] <- program_summary_pt6[,3]/program_summary_pt6[,2]
colnames(program_summary_pt6)[4] <- "solution_pt6"
program_summary_pt6 <- program_summary_pt6 %>% rename(total.dalys.avertible = `cases * dalys`)


#Allowing task-shifting & inclusion of private pharmacists
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                                    allow_task_shifting = 1))

program_detailed_taskshifting_othermodes = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt7 <- program_detailed_taskshifting_othermodes %>% group_by(category) %>% summarize_all(sum)
program_summary_pt7[,4] <- program_summary_pt7[,3]/program_summary_pt7[,2]
colnames(program_summary_pt7)[4] <- "solution_pt7"
program_summary_pt7 <- program_summary_pt7 %>% rename(total.dalys.avertible = `cases * dalys`)


#Allowing task-shifting & inclusion of CHWs & private pharmacies
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                                    allow_task_shifting = 1))

program_detailed_taskshifting_othermodes = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt8 <- program_detailed_taskshifting_othermodes %>% group_by(category) %>% summarize_all(sum)
program_summary_pt8[,4] <- program_summary_pt8[,3]/program_summary_pt8[,2]
colnames(program_summary_pt8)[4] <- "solution_pt8"
program_summary_pt8 <- program_summary_pt8 %>% rename(total.dalys.avertible = `cases * dalys`)

#Merging all the program summaries of all scenarios. 
program_summary <- merge(program_summary_pt1, program_summary_pt2, by = c("category", "total.dalys.avertible"))
program_summary <- merge(program_summary, program_summary_pt3, by = c("category", "total.dalys.avertible"))
program_summary <- merge(program_summary, program_summary_pt4, by = c("category", "total.dalys.avertible"))
program_summary <- merge(program_summary, program_summary_pt5, by = c("category", "total.dalys.avertible"))
program_summary <- merge(program_summary, program_summary_pt6, by = c("category", "total.dalys.avertible"))
program_summary <- merge(program_summary, program_summary_pt7, by = c("category", "total.dalys.avertible"))
program_summary <- merge(program_summary, program_summary_pt8, by = c("category", "total.dalys.avertible"))


colnames(program_summary) = c("Program", "Total DALYs averted overall",
                              "Total DALYs averted in the optimal package (Base)", "Percentage of total DALYs averted in the optimal package (Base)",
                              "Total DALYs averted in the optimal package (CHW)", "Percentage of total DALYs averted in the optimal package (CHW)", 
                              "Total DALYs averted in the optimal package (Pvt Pharm)", "Percentage of total DALYs averted in the optimal package (Pvt Pharm)",
                              "Total DALYs averted in the optimal package (CHW and Pvt Pharm)", "Percentage of total DALYs averted in the optimal package (CHW and Pvt Pharm)",
                              "Total DALYs averted in the optimal package (Task-shifting)", "Percentage of total DALYs averted in the optimal package (Task-shifting)",
                              "Total DALYs averted in the optimal package (Task-shifting & CHWs)", "Percentage of total DALYs averted in the optimal package (Task-shifting & CHWs)",
                              "Total DALYs averted in the optimal package (Task-shifting & Pvt Pharm)", "Percentage of total DALYs averted in the optimal package (Task-shifting & Pvt Pharm)",
                              "Total DALYs averted in the optimal package (Taskshifting and other modes)", "Percentage of total DALYs averted in the optimal package (Taskshifting and other modes")

write.csv(program_summary, file = "4_outputs/tables/table_3_program_summary.csv")

#For creating a stacked bar graph on DALYs averted by programs. 

colnames(program_summary_pt1)[4] <- "solution.prop"
colnames(program_summary_pt2)[4] <- "solution.prop"
colnames(program_summary_pt3)[4] <- "solution.prop"
colnames(program_summary_pt4)[4] <- "solution.prop"
colnames(program_summary_pt5)[4] <- "solution.prop"
colnames(program_summary_pt6)[4] <- "solution.prop"
colnames(program_summary_pt7)[4] <- "solution.prop"
colnames(program_summary_pt8)[4] <- "solution.prop"


program_summary_pt1$scenario <- rep("Baseline", nrow(program_summary_pt1))
program_summary_pt2$scenario <- rep("CHW", nrow(program_summary_pt2))
program_summary_pt3$scenario <- rep("Pvt Pharm", nrow(program_summary_pt3))
program_summary_pt4$scenario <- rep("CHW & Pvt Pharm", nrow(program_summary_pt4))
program_summary_pt5$scenario <- rep("Task \nshifting", nrow(program_summary_pt5))
program_summary_pt6$scenario <- rep("Takshifting \nwith \nCHWs", nrow(program_summary_pt6))
program_summary_pt7$scenario <- rep("Takshifting \nwith \nPvt Pharm", nrow(program_summary_pt7))
program_summary_pt8$scenario <- rep("Takshifting \nwith \nother modes", nrow(program_summary_pt8))

## Create a stacked bar chart - no task-shifting allowed 
tmap_tab <- rbind(program_summary_pt1, program_summary_pt2, program_summary_pt3, program_summary_pt4)

data_melted <- reshape2::melt(tmap_tab, id.vars = c("category", "scenario"), measure.vars = "solution.prop")

# Create the stacked bar chart with a custom rainbow palette of 13 colors
library(ggplot2)

prog_sum_plot_no_taskshift <- ggplot(data_melted, aes(factor(scenario, 
                                                             levels = c("Baseline", "CHW", "Pvt Pharm", "CHW & Pvt Pharm",
                                                                        setdiff(unique(scenario), c("Baseline", "CHW", "Pvt Pharm", "CHW & Pvt Pharm")))), 
                                                      y = value, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +  # Stack the bars
  geom_text(aes(label = paste0(round(value * 100, 0), "%")), 
            position = position_stack(vjust = 0.5),  # Position text in the middle of each segment
            color = "black", size = 4) +  # Add percentage text
  scale_fill_manual(values = rainbow (13), name = "Disease Program Area") +  # Set legend title
  ggtitle("Scenario wise: Percentage of Total DALYs averted by disease program areas") +  # Add plot title
  theme_minimal() +  # Use minimal theme
  labs(x = "Scenario", y = "Program proportion") +  # Set x and y axis labels
  theme(axis.text.y = element_blank(),
        legend.position = "bottom")  # Remove y-axis labels

# Check plot
prog_sum_plot_no_taskshift

# Open a PNG device with a fixed filename
png("4_outputs/figures/prog_sum_no_taskshift.png", width = 600, height = 600)

print(prog_sum_plot_no_taskshift)

# Close the device to save the file
dev.off()


#Create a stacked bar chart - with taskshifting allowed 
tmap_tab <- rbind(program_summary_pt5, program_summary_pt6, program_summary_pt7, program_summary_pt8)

data_melted <- reshape2::melt(tmap_tab, id.vars = c("category", "scenario"), measure.vars = "solution.prop")

# Create the stacked bar chart with a custom rainbow palette of 13 colors
library(ggplot2)

prog_sum_plot_w_taskshift <- ggplot(data_melted, aes(factor(scenario, levels = c("Task \nshifting", "Takshifting \nwith \nCHWs", "Takshifting \nwith \nPvt Pharm" , "Takshifting \nwith \nother modes",
                                                                                 setdiff(unique(scenario), c("Task \nshifting", "Takshifting \nwith \nCHWs", "Takshifting \nwith \nPvt Pharm" , "Takshifting \nwith \nother modes")))), 
                                                     y = value, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +  # Stack the bars
  geom_text(aes(label = paste0(round(value * 100, 0), "%")), 
            position = position_stack(vjust = 0.5),  # Position text in the middle of each segment
            color = "black", size = 4) +  # Add percentage text
  scale_fill_manual(values = rainbow(13), name = "Disease Program Area") +  # Set legend title
  ggtitle("Scenario wise: Percentage of Total DALYs averted by disease program areas") +  # Add plot title
  theme_minimal() +  # Use minimal theme
  labs(x = "Scenario", y = "Program proportion") +  # Set x and y axis labels
  theme(axis.text.y = element_blank(),
        legend.position = "bottom")  # Remove y-axis labels

# Check plot
prog_sum_plot_w_taskshift

# Open a PNG device with a fixed filename
png("4_outputs/figures/prog_sum_w_taskshift.png", width = 600, height = 600)


print(prog_sum_plot_w_taskshift)

# Close the device to save the file
dev.off()


# Table 2/3 - Solution and resource use along with optimal coverage for each mode of delivery
##########################################################
# Results on chosen package/coverage under various scenarios (Appendix files)

#Baseline scenario
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

detailed_results <- cbind(category, intervention, solution, solution*cases, 
                          solution_dalysaverted, solution_drugexp, solution_hruse[,visible_cadres])
detailed_results <- detailed_results[order(detailed_results[,1],detailed_results[,2],decreasing=FALSE),]
int_code_new <- 1:length(intcode)
detailed_results_table <- cbind(int_code_new, detailed_results)

colnames(detailed_results_table) <- c("No.", "Program", "Intervention", "Percentage of cases in need covered under optimal package",
                                      "Total number of cases covered under the optimal package", 
                                      "DALYs averted", "Consumable expenditure required",
                                      "Doctors/Clinical Officers", "Nursing staff",
                                      "Pharmaceutical staff", "Laboratory staff", "Mental health staff", 
                                      "Nutrition staff", "Radiography staff", "Community health workers", "Private pharmacies") 

detailed_results_table$prop_dalysaverted <- sprintf("%.2f%%", (detailed_results_table$`DALYs averted` / dalys_averted) * 100)
detailed_results_table$prop_drugbudget_used <- sprintf("%.2f%%", (detailed_results_table$`Consumable expenditure required` / base.drugbudget) * 100)
detailed_results_table$prop_doctors_used <- sprintf("%.2f%%", (detailed_results_table$`Doctors/Clinical Officers` / cons_hr.limit[1]) * 100)
detailed_results_table$prop_nurses_used <- sprintf("%.2f%%", (detailed_results_table$`Nursing staff` / cons_hr.limit[2]) * 100)
detailed_results_table$prop_pharma_used <- sprintf("%.2f%%", (detailed_results_table$`Pharmaceutical staff` / cons_hr.limit[3]) * 100)
detailed_results_table$prop_lab_used <- sprintf("%.2f%%", (detailed_results_table$`Laboratory staff` / cons_hr.limit[4]) * 100)
detailed_results_table$prop_mental_used <- sprintf("%.2f%%", (detailed_results_table$`Mental health staff` / cons_hr.limit[6]) * 100)
detailed_results_table$prop_nutrition_used <- sprintf("%.2f%%", (detailed_results_table$`Nutrition staff` / cons_hr.limit[7]) * 100)
detailed_results_table$prop_radiography_used <- sprintf("%.2f%%", (detailed_results_table$`Radiography staff` / cons_hr.limit[8]) * 100)
detailed_results_table$prop_chws_used <- sprintf("%.2f%%", (detailed_results_table$`Community health workers` / cons_hr.limit[9]) * 100)
detailed_results_table$prop_pvtpharm_used <- sprintf("%.2f%%", (detailed_results_table$`Private pharmacies` / cons_hr.limit[10]) * 100)

detailed_results_table$`Percentage of cases in need covered under optimal package`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package`) * 100)

filename <- "4_outputs/tables/stable2_data.csv"
write.csv(detailed_results_table, file = filename)

#CHW Scenario 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

solution_1 <- solution.df[1:length(intcode), "solution"]
solution_2 <- solution.df[(1 * length(intcode) + 1):(2 * length(intcode)), "solution"]

detailed_results <- cbind(category, intervention, solution, solution*cases, 
                          solution_dalysaverted, solution_drugexp, solution_hruse[,visible_cadres], solution_1, solution_2)
detailed_results <- detailed_results[order(detailed_results[,1],detailed_results[,2],decreasing=FALSE),]
int_code_new <- 1:length(intcode)
detailed_results_table <- cbind(int_code_new, detailed_results)

colnames(detailed_results_table) <- c("No.", "Program", "Intervention", "Percentage of cases in need covered under optimal package",
                                      "Total number of cases covered under the optimal package", 
                                      "DALYs averted", "Consumable expenditure required",
                                      "Doctors/Clinical Officers", "Nursing staff",
                                      "Pharmaceutical staff", "Laboratory staff", "Mental health staff", 
                                      "Nutrition staff", "Radiography staff", "Community health workers", "Private pharmacies",
                                      "Percentage of cases in need covered under optimal package by facility based delivery",
                                      "Percentage of cases in need covered under optimal package by CHW delivery") 

detailed_results_table$prop_dalysaverted <- sprintf("%.2f%%", (detailed_results_table$`DALYs averted` / dalys_averted) * 100)
detailed_results_table$prop_drugbudget_used <- sprintf("%.2f%%", (detailed_results_table$`Consumable expenditure required` / base.drugbudget) * 100)
detailed_results_table$prop_doctors_used <- sprintf("%.2f%%", (detailed_results_table$`Doctors/Clinical Officers` / cons_hr.limit[1]) * 100)
detailed_results_table$prop_nurses_used <- sprintf("%.2f%%", (detailed_results_table$`Nursing staff` / cons_hr.limit[2]) * 100)
detailed_results_table$prop_pharma_used <- sprintf("%.2f%%", (detailed_results_table$`Pharmaceutical staff` / cons_hr.limit[3]) * 100)
detailed_results_table$prop_lab_used <- sprintf("%.2f%%", (detailed_results_table$`Laboratory staff` / cons_hr.limit[4]) * 100)
detailed_results_table$prop_mental_used <- sprintf("%.2f%%", (detailed_results_table$`Mental health staff` / cons_hr.limit[6]) * 100)
detailed_results_table$prop_nutrition_used <- sprintf("%.2f%%", (detailed_results_table$`Nutrition staff` / cons_hr.limit[7]) * 100)
detailed_results_table$prop_radiography_used <- sprintf("%.2f%%", (detailed_results_table$`Radiography staff` / cons_hr.limit[8]) * 100)
detailed_results_table$prop_chws_used <- sprintf("%.2f%%", (detailed_results_table$`Community health workers` / cons_hr.limit[9]) * 100)
detailed_results_table$prop_pvtpharm_used <- sprintf("%.2f%%", (detailed_results_table$`Private pharmacies` / cons_hr.limit[10]) * 100)

detailed_results_table$`Percentage of cases in need covered under optimal package`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by CHW delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by CHW delivery`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`) * 100)

filename <- "4_outputs/tables/stable3_data.csv"
write.csv(detailed_results_table, file = filename) 

#Private pharmacist Scenario 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

solution_1 <- solution.df[1:length(intcode), "solution"]
solution_2 <- solution.df[(1 * length(intcode) + 1):(2 * length(intcode)), "solution"]

detailed_results <- cbind(category, intervention, solution, solution*cases, 
                          solution_dalysaverted, solution_drugexp, solution_hruse[,visible_cadres], solution_1, solution_2)
detailed_results <- detailed_results[order(detailed_results[,1],detailed_results[,2],decreasing=FALSE),]
int_code_new <- 1:length(intcode)
detailed_results_table <- cbind(int_code_new, detailed_results)

colnames(detailed_results_table) <- c("No.", "Program", "Intervention", "Percentage of cases in need covered under optimal package",
                                      "Total number of cases covered under the optimal package", 
                                      "DALYs averted", "Consumable expenditure required",
                                      "Doctors/Clinical Officers", "Nursing staff",
                                      "Pharmaceutical staff", "Laboratory staff", "Mental health staff", 
                                      "Nutrition staff", "Radiography staff", "Community health workers", "Private pharmacies",
                                      "Percentage of cases in need covered under optimal package by facility based delivery",
                                      "Percentage of cases in need covered under optimal package by private pharmacist delivery") 

detailed_results_table$prop_dalysaverted <- sprintf("%.2f%%", (detailed_results_table$`DALYs averted` / dalys_averted) * 100)
detailed_results_table$prop_drugbudget_used <- sprintf("%.2f%%", (detailed_results_table$`Consumable expenditure required` / base.drugbudget) * 100)
detailed_results_table$prop_doctors_used <- sprintf("%.2f%%", (detailed_results_table$`Doctors/Clinical Officers` / cons_hr.limit[1]) * 100)
detailed_results_table$prop_nurses_used <- sprintf("%.2f%%", (detailed_results_table$`Nursing staff` / cons_hr.limit[2]) * 100)
detailed_results_table$prop_pharma_used <- sprintf("%.2f%%", (detailed_results_table$`Pharmaceutical staff` / cons_hr.limit[3]) * 100)
detailed_results_table$prop_lab_used <- sprintf("%.2f%%", (detailed_results_table$`Laboratory staff` / cons_hr.limit[4]) * 100)
detailed_results_table$prop_mental_used <- sprintf("%.2f%%", (detailed_results_table$`Mental health staff` / cons_hr.limit[6]) * 100)
detailed_results_table$prop_nutrition_used <- sprintf("%.2f%%", (detailed_results_table$`Nutrition staff` / cons_hr.limit[7]) * 100)
detailed_results_table$prop_radiography_used <- sprintf("%.2f%%", (detailed_results_table$`Radiography staff` / cons_hr.limit[8]) * 100)
detailed_results_table$prop_chws_used <- sprintf("%.2f%%", (detailed_results_table$`Community health workers` / cons_hr.limit[9]) * 100)
detailed_results_table$prop_pvtpharm_used <- sprintf("%.2f%%", (detailed_results_table$`Private pharmacies` / cons_hr.limit[10]) * 100)

detailed_results_table$`Percentage of cases in need covered under optimal package`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by private pharmacist delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by private pharmacist delivery`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`) * 100)

filename <- "4_outputs/tables/stable4_data.csv"
write.csv(detailed_results_table, file = filename) 

#Both CHW and private scenario 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

solution_1 <- solution.df[1:length(intcode), "solution"]
solution_2 <- solution.df[(1 * length(intcode) + 1):(2 * length(intcode)), "solution"]
solution_3 <- solution.df[(2 * length(intcode) + 1):(3 * length(intcode)), "solution"]

detailed_results <- cbind(category, intervention, solution, solution*cases, 
                          solution_dalysaverted, solution_drugexp, solution_hruse[,visible_cadres], solution_1, solution_2, solution_3)

detailed_results <- detailed_results[order(detailed_results[,1],detailed_results[,2],decreasing=FALSE),]
int_code_new <- 1:length(intcode)
detailed_results_table <- cbind(int_code_new, detailed_results)

colnames(detailed_results_table) <- c("No.", "Program", "Intervention", "Percentage of cases in need covered under optimal package",
                                      "Total number of cases covered under the optimal package", 
                                      "DALYs averted", "Consumable expenditure required",
                                      "Doctors/Clinical Officers", "Nursing staff",
                                      "Pharmaceutical staff", "Laboratory staff", "Mental health staff", 
                                      "Nutrition staff", "Radiography staff", "Community health workers", "Private pharmacies",
                                      "Percentage of cases in need covered under optimal package by facility based delivery",
                                      "Percentage of cases in need covered under optimal package by CHW delivery",
                                      "Percentage of cases in need covered under optimal package by private pharmacist delivery") 

detailed_results_table$prop_dalysaverted <- sprintf("%.2f%%", (detailed_results_table$`DALYs averted` / dalys_averted) * 100)
detailed_results_table$prop_drugbudget_used <- sprintf("%.2f%%", (detailed_results_table$`Consumable expenditure required` / base.drugbudget) * 100)
detailed_results_table$prop_doctors_used <- sprintf("%.2f%%", (detailed_results_table$`Doctors/Clinical Officers` / cons_hr.limit[1]) * 100)
detailed_results_table$prop_nurses_used <- sprintf("%.2f%%", (detailed_results_table$`Nursing staff` / cons_hr.limit[2]) * 100)
detailed_results_table$prop_pharma_used <- sprintf("%.2f%%", (detailed_results_table$`Pharmaceutical staff` / cons_hr.limit[3]) * 100)
detailed_results_table$prop_lab_used <- sprintf("%.2f%%", (detailed_results_table$`Laboratory staff` / cons_hr.limit[4]) * 100)
detailed_results_table$prop_mental_used <- sprintf("%.2f%%", (detailed_results_table$`Mental health staff` / cons_hr.limit[6]) * 100)
detailed_results_table$prop_nutrition_used <- sprintf("%.2f%%", (detailed_results_table$`Nutrition staff` / cons_hr.limit[7]) * 100)
detailed_results_table$prop_radiography_used <- sprintf("%.2f%%", (detailed_results_table$`Radiography staff` / cons_hr.limit[8]) * 100)
detailed_results_table$prop_chws_used <- sprintf("%.2f%%", (detailed_results_table$`Community health workers` / cons_hr.limit[9]) * 100)
detailed_results_table$prop_pvtpharm_used <- sprintf("%.2f%%", (detailed_results_table$`Private pharmacies` / cons_hr.limit[10]) * 100)

detailed_results_table$`Percentage of cases in need covered under optimal package`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by private pharmacist delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by private pharmacist delivery`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by CHW delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by CHW delivery`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`) * 100)

filename <- "4_outputs/tables/stable5_data.csv"
write.csv(detailed_results_table, file = filename) 

#Task shifting scenario 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)

solution_1 <- solution.df[1:length(intcode), "solution"]
solution_2 <- solution.df[(1 * length(intcode) + 1):(2 * length(intcode)), "solution"]
solution_3 <- solution.df[(2 * length(intcode) + 1):(3 * length(intcode)), "solution"]
solution_4 <- solution.df[(3 * length(intcode) + 1):(4 * length(intcode)), "solution"]

detailed_results <- cbind(category, intervention, solution, solution*cases, 
                          solution_dalysaverted, solution_drugexp, solution_hruse[,visible_cadres], solution_1, solution_2, solution_3, solution_4)

detailed_results <- detailed_results[order(detailed_results[,1],detailed_results[,2],decreasing=FALSE),]
int_code_new <- 1:length(intcode)
detailed_results_table <- cbind(int_code_new, detailed_results)

colnames(detailed_results_table) <- c("No.", "Program", "Intervention", "Percentage of cases in need covered under optimal package",
                                      "Total number of cases covered under the optimal package", 
                                      "DALYs averted", "Consumable expenditure required",
                                      "Doctors/Clinical Officers", "Nursing staff",
                                      "Pharmaceutical staff", "Laboratory staff", "Mental health staff", 
                                      "Nutrition staff", "Radiography staff", "Community health workers", "Private pharmacies",
                                      "Percentage of cases in need covered under optimal package by facility based delivery",
                                      "Percentage of cases in need covered under optimal package by taskshifting from pharmacist to nurse",
                                      "Percentage of cases in need covered under optimal package by taskshifting from nutritionist to nurse",
                                      "Percentage of cases in need covered under optimal package by taskshifting from pharmacist and nutritionist to nurse")

detailed_results_table$prop_dalysaverted <- sprintf("%.2f%%", (detailed_results_table$`DALYs averted` / dalys_averted) * 100)
detailed_results_table$prop_drugbudget_used <- sprintf("%.2f%%", (detailed_results_table$`Consumable expenditure required` / base.drugbudget) * 100)
detailed_results_table$prop_doctors_used <- sprintf("%.2f%%", (detailed_results_table$`Doctors/Clinical Officers` / cons_hr.limit[1]) * 100)
detailed_results_table$prop_nurses_used <- sprintf("%.2f%%", (detailed_results_table$`Nursing staff` / cons_hr.limit[2]) * 100)
detailed_results_table$prop_pharma_used <- sprintf("%.2f%%", (detailed_results_table$`Pharmaceutical staff` / cons_hr.limit[3]) * 100)
detailed_results_table$prop_lab_used <- sprintf("%.2f%%", (detailed_results_table$`Laboratory staff` / cons_hr.limit[4]) * 100)
detailed_results_table$prop_mental_used <- sprintf("%.2f%%", (detailed_results_table$`Mental health staff` / cons_hr.limit[6]) * 100)
detailed_results_table$prop_nutrition_used <- sprintf("%.2f%%", (detailed_results_table$`Nutrition staff` / cons_hr.limit[7]) * 100)
detailed_results_table$prop_radiography_used <- sprintf("%.2f%%", (detailed_results_table$`Radiography staff` / cons_hr.limit[8]) * 100)
detailed_results_table$prop_chws_used <- sprintf("%.2f%%", (detailed_results_table$`Community health workers` / cons_hr.limit[9]) * 100)
detailed_results_table$prop_pvtpharm_used <- sprintf("%.2f%%", (detailed_results_table$`Private pharmacies` / cons_hr.limit[10]) * 100)

detailed_results_table$`Percentage of cases in need covered under optimal package`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from pharmacist to nurse`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from pharmacist to nurse`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from nutritionist to nurse`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from nutritionist to nurse`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from pharmacist and nutritionist to nurse`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from pharmacist and nutritionist to nurse`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`) * 100)

filename <- "4_outputs/tables/stable6_data.csv"
write.csv(detailed_results_table, file = filename) 


#Task shifting with chw 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)

solution_1 <- solution.df[1:length(intcode), "solution"]
solution_2 <- solution.df[(1 * length(intcode) + 1):(2 * length(intcode)), "solution"]
solution_3 <- solution.df[(2 * length(intcode) + 1):(3 * length(intcode)), "solution"]
solution_4 <- solution.df[(3 * length(intcode) + 1):(4 * length(intcode)), "solution"]
solution_5 <- solution.df[(4 * length(intcode) + 1):(5 * length(intcode)), "solution"]

detailed_results <- cbind(category, intervention, solution, solution*cases, 
                          solution_dalysaverted, solution_drugexp, solution_hruse[,visible_cadres],
                          solution_1, solution_2, solution_3, solution_4, solution_5)

detailed_results <- detailed_results[order(detailed_results[,1],detailed_results[,2],decreasing=FALSE),]
int_code_new <- 1:length(intcode)
detailed_results_table <- cbind(int_code_new, detailed_results)

colnames(detailed_results_table) <- c("No.", "Program", "Intervention", "Percentage of cases in need covered under optimal package",
                                      "Total number of cases covered under the optimal package", 
                                      "DALYs averted", "Consumable expenditure required",
                                      "Doctors/Clinical Officers", "Nursing staff",
                                      "Pharmaceutical staff", "Laboratory staff", "Mental health staff", 
                                      "Nutrition staff", "Radiography staff", "Community health workers", "Private pharmacies",
                                      "Percentage of cases in need covered under optimal package by facility based delivery",
                                      "Percentage of cases in need covered under optimal package by taskshifting from pharmacist to nurse",
                                      "Percentage of cases in need covered under optimal package by taskshifting from nutritionist to nurse",
                                      "Percentage of cases in need covered under optimal package by taskshifting from pharmacist and nutritionist to nurse",
                                      "Percentage of cases in need covered under optimal package by CHW delivery")

detailed_results_table$prop_dalysaverted <- sprintf("%.2f%%", (detailed_results_table$`DALYs averted` / dalys_averted) * 100)
detailed_results_table$prop_drugbudget_used <- sprintf("%.2f%%", (detailed_results_table$`Consumable expenditure required` / base.drugbudget) * 100)
detailed_results_table$prop_doctors_used <- sprintf("%.2f%%", (detailed_results_table$`Doctors/Clinical Officers` / cons_hr.limit[1]) * 100)
detailed_results_table$prop_nurses_used <- sprintf("%.2f%%", (detailed_results_table$`Nursing staff` / cons_hr.limit[2]) * 100)
detailed_results_table$prop_pharma_used <- sprintf("%.2f%%", (detailed_results_table$`Pharmaceutical staff` / cons_hr.limit[3]) * 100)
detailed_results_table$prop_lab_used <- sprintf("%.2f%%", (detailed_results_table$`Laboratory staff` / cons_hr.limit[4]) * 100)
detailed_results_table$prop_mental_used <- sprintf("%.2f%%", (detailed_results_table$`Mental health staff` / cons_hr.limit[6]) * 100)
detailed_results_table$prop_nutrition_used <- sprintf("%.2f%%", (detailed_results_table$`Nutrition staff` / cons_hr.limit[7]) * 100)
detailed_results_table$prop_radiography_used <- sprintf("%.2f%%", (detailed_results_table$`Radiography staff` / cons_hr.limit[8]) * 100)
detailed_results_table$prop_chws_used <- sprintf("%.2f%%", (detailed_results_table$`Community health workers` / cons_hr.limit[9]) * 100)
detailed_results_table$prop_pvtpharm_used <- sprintf("%.2f%%", (detailed_results_table$`Private pharmacies` / cons_hr.limit[10]) * 100)

detailed_results_table$`Percentage of cases in need covered under optimal package`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from pharmacist to nurse`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from pharmacist to nurse`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from nutritionist to nurse`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from nutritionist to nurse`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from pharmacist and nutritionist to nurse`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from pharmacist and nutritionist to nurse`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by CHW delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by CHW delivery`) * 100)

filename <- "4_outputs/tables/stable7_data.csv"
write.csv(detailed_results_table, file = filename) 

#Task shifting with private pharmacies 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)

solution_1 <- solution.df[1:length(intcode), "solution"]
solution_2 <- solution.df[(1 * length(intcode) + 1):(2 * length(intcode)), "solution"]
solution_3 <- solution.df[(2 * length(intcode) + 1):(3 * length(intcode)), "solution"]
solution_4 <- solution.df[(3 * length(intcode) + 1):(4 * length(intcode)), "solution"]
solution_5 <- solution.df[(4 * length(intcode) + 1):(5 * length(intcode)), "solution"]

detailed_results <- cbind(category, intervention, solution, solution*cases, 
                          solution_dalysaverted, solution_drugexp, solution_hruse[,visible_cadres],
                          solution_1, solution_2, solution_3, solution_4, solution_5)

detailed_results <- detailed_results[order(detailed_results[,1],detailed_results[,2],decreasing=FALSE),]
int_code_new <- 1:length(intcode)
detailed_results_table <- cbind(int_code_new, detailed_results)

colnames(detailed_results_table) <- c("No.", "Program", "Intervention", "Percentage of cases in need covered under optimal package",
                                      "Total number of cases covered under the optimal package", 
                                      "DALYs averted", "Consumable expenditure required",
                                      "Doctors/Clinical Officers", "Nursing staff",
                                      "Pharmaceutical staff", "Laboratory staff", "Mental health staff", 
                                      "Nutrition staff", "Radiography staff", "Community health workers", "Private pharmacies",
                                      "Percentage of cases in need covered under optimal package by facility based delivery",
                                      "Percentage of cases in need covered under optimal package by taskshifting from pharmacist to nurse",
                                      "Percentage of cases in need covered under optimal package by taskshifting from nutritionist to nurse",
                                      "Percentage of cases in need covered under optimal package by taskshifting from pharmacist and nutritionist to nurse",
                                      "Percentage of cases in need covered under optimal package by private pharmacist delivery")

detailed_results_table$prop_dalysaverted <- sprintf("%.2f%%", (detailed_results_table$`DALYs averted` / dalys_averted) * 100)
detailed_results_table$prop_drugbudget_used <- sprintf("%.2f%%", (detailed_results_table$`Consumable expenditure required` / base.drugbudget) * 100)
detailed_results_table$prop_doctors_used <- sprintf("%.2f%%", (detailed_results_table$`Doctors/Clinical Officers` / cons_hr.limit[1]) * 100)
detailed_results_table$prop_nurses_used <- sprintf("%.2f%%", (detailed_results_table$`Nursing staff` / cons_hr.limit[2]) * 100)
detailed_results_table$prop_pharma_used <- sprintf("%.2f%%", (detailed_results_table$`Pharmaceutical staff` / cons_hr.limit[3]) * 100)
detailed_results_table$prop_lab_used <- sprintf("%.2f%%", (detailed_results_table$`Laboratory staff` / cons_hr.limit[4]) * 100)
detailed_results_table$prop_mental_used <- sprintf("%.2f%%", (detailed_results_table$`Mental health staff` / cons_hr.limit[6]) * 100)
detailed_results_table$prop_nutrition_used <- sprintf("%.2f%%", (detailed_results_table$`Nutrition staff` / cons_hr.limit[7]) * 100)
detailed_results_table$prop_radiography_used <- sprintf("%.2f%%", (detailed_results_table$`Radiography staff` / cons_hr.limit[8]) * 100)
detailed_results_table$prop_chws_used <- sprintf("%.2f%%", (detailed_results_table$`Community health workers` / cons_hr.limit[9]) * 100)
detailed_results_table$prop_pvtpharm_used <- sprintf("%.2f%%", (detailed_results_table$`Private pharmacies` / cons_hr.limit[10]) * 100)

detailed_results_table$`Percentage of cases in need covered under optimal package`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from pharmacist to nurse`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from pharmacist to nurse`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from nutritionist to nurse`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from nutritionist to nurse`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from pharmacist and nutritionist to nurse`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from pharmacist and nutritionist to nurse`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by private pharmacist delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by private pharmacist delivery`) * 100)

filename <- "4_outputs/tables/stable8_data.csv"
write.csv(detailed_results_table, file = filename) 

#Task shifting with chw and private pharmacies 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)

solution_1 <- solution.df[1:length(intcode), "solution"]
solution_2 <- solution.df[(1 * length(intcode) + 1):(2 * length(intcode)), "solution"]
solution_3 <- solution.df[(2 * length(intcode) + 1):(3 * length(intcode)), "solution"]
solution_4 <- solution.df[(3 * length(intcode) + 1):(4 * length(intcode)), "solution"]
solution_5 <- solution.df[(4 * length(intcode) + 1):(5 * length(intcode)), "solution"]
solution_6 <- solution.df[(5 * length(intcode) + 1):(6 * length(intcode)), "solution"]

detailed_results <- cbind(category, intervention, solution, solution*cases, 
                          solution_dalysaverted, solution_drugexp, solution_hruse[,visible_cadres],
                          solution_1, solution_2, solution_3, solution_4, solution_5, solution_6)

detailed_results <- detailed_results[order(detailed_results[,1],detailed_results[,2],decreasing=FALSE),]
int_code_new <-1:length(intcode)
detailed_results_table <- cbind(int_code_new, detailed_results)

colnames(detailed_results_table) <- c("No.", "Program", "Intervention", "Percentage of cases in need covered under optimal package",
                                      "Total number of cases covered under the optimal package", 
                                      "DALYs averted", "Consumable expenditure required",
                                      "Doctors/Clinical Officers", "Nursing staff",
                                      "Pharmaceutical staff", "Laboratory staff", "Mental health staff", 
                                      "Nutrition staff", "Radiography staff", "Community health workers", "Private pharmacies",
                                      "Percentage of cases in need covered under optimal package by facility based delivery",
                                      "Percentage of cases in need covered under optimal package by taskshifting from pharmacist to nurse",
                                      "Percentage of cases in need covered under optimal package by taskshifting from nutritionist to nurse",
                                      "Percentage of cases in need covered under optimal package by taskshifting from pharmacist and nutritionist to nurse",
                                      "Percentage of cases in need covered under optimal package by CHW delivery",
                                      "Percentage of cases in need covered under optimal package by private pharmacist delivery")

detailed_results_table$prop_dalysaverted <- sprintf("%.2f%%", (detailed_results_table$`DALYs averted` / dalys_averted) * 100)
detailed_results_table$prop_drugbudget_used <- sprintf("%.2f%%", (detailed_results_table$`Consumable expenditure required` / base.drugbudget) * 100)
detailed_results_table$prop_doctors_used <- sprintf("%.2f%%", (detailed_results_table$`Doctors/Clinical Officers` / cons_hr.limit[1]) * 100)
detailed_results_table$prop_nurses_used <- sprintf("%.2f%%", (detailed_results_table$`Nursing staff` / cons_hr.limit[2]) * 100)
detailed_results_table$prop_pharma_used <- sprintf("%.2f%%", (detailed_results_table$`Pharmaceutical staff` / cons_hr.limit[3]) * 100)
detailed_results_table$prop_lab_used <- sprintf("%.2f%%", (detailed_results_table$`Laboratory staff` / cons_hr.limit[4]) * 100)
detailed_results_table$prop_mental_used <- sprintf("%.2f%%", (detailed_results_table$`Mental health staff` / cons_hr.limit[6]) * 100)
detailed_results_table$prop_nutrition_used <- sprintf("%.2f%%", (detailed_results_table$`Nutrition staff` / cons_hr.limit[7]) * 100)
detailed_results_table$prop_radiography_used <- sprintf("%.2f%%", (detailed_results_table$`Radiography staff` / cons_hr.limit[8]) * 100)
detailed_results_table$prop_chws_used <- sprintf("%.2f%%", (detailed_results_table$`Community health workers` / cons_hr.limit[9]) * 100)
detailed_results_table$prop_pvtpharm_used <- sprintf("%.2f%%", (detailed_results_table$`Private pharmacies` / cons_hr.limit[10]) * 100)

detailed_results_table$`Percentage of cases in need covered under optimal package`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from pharmacist to nurse`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from pharmacist to nurse`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from nutritionist to nurse`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from nutritionist to nurse`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from pharmacist and nutritionist to nurse`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by taskshifting from pharmacist and nutritionist to nurse`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by private pharmacist delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by private pharmacist delivery`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by CHW delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by CHW delivery`) * 100)

filename <- "4_outputs/tables/stable9_data.csv"
write.csv(detailed_results_table, file = filename) 


--------------------------------------------------------------------------------------------------------------
  # Figure 1: Resource use graphs across scenarios (for each resource --- doctors, drugs and consumables etc)
  ##########################################################
# Define values for each parameter
allow_chw_delivery_values <- c(0, 1)
allow_pvtpharm_delivery_values <- c(0, 1)
allow_task_shifting_values <- c(0, 1)

# Create a matrix with all combinations of the parameters
param_combinations <- expand.grid(
  allow_chw_delivery = allow_chw_delivery_values,
  allow_pvtpharm_delivery = allow_pvtpharm_delivery_values,
  allow_task_shifting = allow_task_shifting_values
)

# Define a list of specific scenario names
scenario_names <- c("Baseline Scenario", "CHW Scenario", "Pvt Pharmacist Scenario", "CHW and Pvt Pharmacist Scenario", "Taskshifting only", 
                    "Taskshifting with CHW", "Taskshifting with Pvt Pharm", "Taskshifting with both modes")

# Initialize an empty list to store results
all_data <- list()

# Define a function to process and plot the results for each resource
plot_resource_usage <- function(resource_column, plot_title, output_filename) {
  # Loop over the parameter combinations
  for (params in 1:nrow(param_combinations)) {
    # Extract the current combination of parameters
    allow_chw <- param_combinations$allow_chw_delivery[params]
    allow_pvtpharm <- param_combinations$allow_pvtpharm_delivery[params]
    allow_task_shifting <- param_combinations$allow_task_shifting[params]
    
    # Call find_optimal_package
    solution <- find_optimal_package(
      input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
      drug_budget_input = base.drugbudget, drug_budget_scale = 1, hr_scale = base.hr, 
      allow_chw_delivery = allow_chw, allow_pvtpharm_delivery = allow_pvtpharm, 
      allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
      allow_task_shifting = allow_task_shifting
    )
    
    # Normalize HR usage
    data_hr <- sweep(solution_hruse, 2, cons_hr.limit_base, FUN = '/')
    data_drug <- as.matrix(solution_drugexp)/cons_drug.limit_base
    # Combine all resource use matrices into one matrix
    data <- cbind(data_hr, data_drug)
    data <- as.matrix(data)
    data <- data[,-c(5)] 
    
    # Create a new data frame using `Category` from `chosen_df`
    data_temp <- data.frame(
      Category = category,  # Use `Category` column from chosen_df
      Resource_Use = data[, resource_column],  # Use the dynamic resource column
      Scenario = scenario_names[params]  # Add scenario name
    )
    
    # Append to the list
    all_data[[params]] <- data_temp
  }
  
  # Combine all stored results into a single data frame
  final_data <- do.call(rbind, all_data)
  
  # Load required libraries
  library(ggplot2)
  library(scales)
  library(viridisLite)
  
  # Create a viridis color palette
  pal <- magma(n = 12)
  
  # Calculate the total Resource_Use per Scenario
  total_data <- final_data %>%
    group_by(Scenario) %>%
    summarize(total_resource_use = sum(Resource_Use))
  
  # Ensure that Scenario is a factor with levels in the desired order
  final_data$Scenario <- factor(final_data$Scenario, levels = scenario_names)
  
  # Create the plot
  p <- ggplot(data = final_data, aes(x = Scenario, y = Resource_Use)) +
    geom_col(aes(fill = Category), width = 0.7) +
    
    # Format legend
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +  
    
    # Scale y-axis to show percentage
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  
    
    # Add total resource use percentage on top of each stacked bar
    geom_text(
      data = total_data,  # Use the total data frame for total Resource_Use
      aes(x = Scenario, y = total_resource_use, label = sprintf("%1.1f%%", total_resource_use * 100)),  # Total label
      vjust = -0.5, size = 4, color = "black"
    ) +
    
    # Apply classic theme
    theme_classic() +  
    
    # Formatting for titles, axis labels, and legend
    theme(
      plot.title = element_text(family = "Helvetica", face = "bold", size = 20),
      legend.text = element_text(face = "italic", colour = "black", family = "Helvetica"),
      legend.title = element_blank(),  # Remove legend title
      axis.title = element_text(family = "Helvetica", size = 15, colour = "black"),
      axis.text.x = element_text(face = "bold", color = "black", size = 10, angle = 45, hjust = 1),  # Rotate x-axis labels
      axis.text.y = element_text(face = "bold", color = "black", size = 10),
      legend.position = "bottom"
    ) +
    
    # Add title and labels
    labs(
      x = "Scenario",
      y = "Percentage of Resource Required",
      title = plot_title  # Use dynamic title
    ) +
    
    # Apply the Viridis color palette, assuming pal is defined
    scale_fill_manual(values = pal)
  
  # Print the plot
  print(p)
  
  # Saving the plot 
  ggsave(output_filename, plot = p, width = 10, height = 8, dpi = 300)
}

# Plot for Resource Use - Doctors/Medical Officer
plot_resource_usage(resource_column = 1, plot_title = "Doctor/Medical Officer Use Across Scenarios", output_filename = "4_outputs/figures/resource_use_plot_1.png")
# Plot for Resource Use - Nursing Staff
plot_resource_usage(resource_column = 2, plot_title = "Nursing Staff Time Use Across Scenarios", output_filename = "4_outputs/figures/resource_use_plot_2.png")
# Plot for Resource Use - Pharmaceutical Staff 
plot_resource_usage(resource_column = 3, plot_title = "Pharmaceutical Staff Time Use Across Scenarios", output_filename = "4_outputs/figures/resource_use_plot_3.png")
# Plot for Resource Use - Laboratory Staff 
plot_resource_usage(resource_column = 4, plot_title = "Laboratory Staff Time Use Across Scenarios", output_filename = "4_outputs/figures/resource_use_plot_4.png")
# Plot for Resource Use - Mental Health Staff
plot_resource_usage(resource_column = 5, plot_title = "Mental Health Staff Time Use Across Scenarios", output_filename = "4_outputs/figures/resource_use_plot_5.png")
# Plot for Resource Use - Nutrition Staff
plot_resource_usage(resource_column = 6, plot_title = "Nutrition Staff Time Use Across Scenarios", output_filename = "4_outputs/figures/resource_use_plot_6.png")
# Plot for Resource Use - Diagnostic Staff
plot_resource_usage(resource_column = 7, plot_title = "Diagnostic Staff Time Use Across Scenarios", output_filename = "4_outputs/figures/resource_use_plot_7.png")
# Plot for Resource Use - Community Health Workers Staff
plot_resource_usage(resource_column = 8, plot_title = "Community Health Workers Time Use Across Scenarios", output_filename = "4_outputs/figures/resource_use_plot_8.png")
# Plot for Resource Use - Private pharmacists Staff
plot_resource_usage(resource_column = 9, plot_title = "Private Pharmacists Time Use Across Scenarios", output_filename = "4_outputs/figures/resource_use_plot_9.png")
# Plot for Resource Use - Drugs and consumables budget
plot_resource_usage(resource_column = 10, plot_title = "Drugs and Consumables Budget Use Across Scenarios", output_filename = "4_outputs/figures/resource_use_plot_10.png")


# Resource use graphs (All resource use in one graph and created separately for each scenario - not used in the paper)
##########################################################
# Define values for each parameter
allow_chw_delivery_values <- c(0, 1)
allow_pvtpharm_delivery_values <- c(0, 1)
allow_task_shifting_values <- c(0, 1)

# Create a matrix with all combinations of the parameters
param_combinations <- expand.grid(
  allow_chw_delivery = allow_chw_delivery_values,
  allow_pvtpharm_delivery = allow_pvtpharm_delivery_values,
  allow_task_shifting = allow_task_shifting_values
)

# Define a list of specific scenario names
scenario_names <- c("Baseline Scenario", "CHW Scenario", "Pvt Pharmacist Scenario", "CHW and Pvt Pharmacist Scenario", "Taskshifting only", 
                    "Taskshifting with CHW", "Taskshifting with Pvt Pharm", "Takshifting with both modes")

# Loop over the parameter combinations
for (params in 1:nrow(param_combinations)) {
  # Extract the current combination of parameters
  allow_chw <- param_combinations$allow_chw_delivery[params]
  allow_pvtpharm <- param_combinations$allow_pvtpharm_delivery[params]
  allow_task_shifting <- param_combinations$allow_task_shifting[params]
  
  # Call find_optimal_package
  solution <- find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                   drug_budget_input = base.drugbudget, drug_budget_scale = 1, hr_scale = base.hr, 
                                   allow_chw_delivery = allow_chw, allow_pvtpharm_delivery = allow_pvtpharm, 
                                   allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                                   allow_task_shifting = allow_task_shifting)
  
  # Use the scenario name from the list instead of the number
  plot_title <- paste("Resource use for", scenario_names[params])  # Scenario name from the list
  file_name <- paste0("4_outputs/figures/resourceuse_", scenario_names[params], ".png")  # File name with scenario name
  
  # Call gen_resource use_graphs to generate and save the graph
  gen_resourceuse_graphs(plot_title, file_name)
  
  # Optionally print progress for tracking
  print(paste("Iteration", params, "completed, plot saved to", file_name))
}

# Figure 2: Marginal value
##########################################################
salaries_hr_mth <- c(1363, 557, 830, 604, 1157, 930, 851, 72)
salaries_hr_annual <- salaries_hr_mth * 12
cadre_names <- c("doctor", "nurse", "pharm", "lab", "mental", "nutri", "radio", "chw")

nhb_hr_unscaled = c(NA, NA, NA, NA, NA, NA, NA, NA)
margval_table = matrix(nrow = 8, ncol = length(visible_cadres))

# Define values for each parameter
allow_chw_delivery_values <- c(0, 1)
allow_pvtpharm_delivery_values <- c(0, 1)
allow_task_shifting_values <- c(0, 1)

# Initialize the nhb_hr_unscaled vector with the correct length
nhb_hr_unscaled <- numeric(8)

# Create a matrix with all combinations of the parameters
param_combinations <- expand.grid(
  allow_chw_delivery = allow_chw_delivery_values,
  allow_pvtpharm_delivery = allow_pvtpharm_delivery_values,
  allow_task_shifting = allow_task_shifting_values
)

# Loop over the parameter combinations
i <- 1
for (params in 1:nrow(param_combinations)) {
  # Extract the current combination of parameters
  allow_chw <- param_combinations$allow_chw_delivery[params]
  allow_pvtpharm <- param_combinations$allow_pvtpharm_delivery[params]
  allow_task_shifting <- param_combinations$allow_task_shifting[params]
  
  # Capture output based on the current parameter combination
  capture.output(
    find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth',  cet_input = base.cet, drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                         hr_scale = base.hr, allow_chw_delivery = allow_chw, allow_pvtpharm_delivery = allow_pvtpharm, allow_markup = 0, allow_demand_constraint = 0, 
                         max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, allow_task_shifting = allow_task_shifting
    )
  )
  
  # Store the result in nhb_hr_unscaled
  nhb_hr_unscaled[i] <- solution.class$objval
  i <- i + 1
}

# Output the nhb_hr_unscaled values
print(nhb_hr_unscaled)


# Marginal value of consumables

# Initialize the drug_budget_marginal vector with the correct length
drug_budget_marginal <- numeric(8)

# Loop through the parameter combinations
i <- 1
for (params in 1:nrow(param_combinations)) {
  # Extract the current combination of parameters
  allow_chw <- param_combinations$allow_chw_delivery[params]
  allow_pvtpharm <- param_combinations$allow_pvtpharm_delivery[params]
  allow_task_shifting <- param_combinations$allow_task_shifting[params]
  
  # Capture output based on the current parameter combination
  capture.output(
    find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, drug_budget_input = base.drugbudget + 1, drug_budget_scale = 1,  # Adjusting drug budget as neededdrug_budget.scale = 1, 
                         hr_scale = base.hr, allow_chw_delivery = allow_chw, allow_pvtpharm_delivery = allow_pvtpharm, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                         allow_task_shifting = allow_task_shifting
    )
  )
  
  # Store the result in the drug_budget_marginal vector
  drug_budget_marginal[i] <- solution.class$objval
  
  # Increment the index
  i <- i + 1
}

# Output the drug_budget_marginal values
print(drug_budget_marginal)

value_1drug <- (drug_budget_marginal - nhb_hr_unscaled)
margval_table[, length(visible_cadres)] <- value_1drug

# Marginal Value of HR cadres

hr_marginal <- numeric((length(visible_cadres) - 1) * nrow(param_combinations))
cadres_marg <- c(1:4,6:9) # showing all cadres except the dental staff and private pharm

i <- 1
for (cadre in cadres_marg) {
  for (params in 1:nrow(param_combinations)) {
    
    # Calculate the proportion for scaling
    prop <- 1 / cons_hr.limit.saved[, cadre]  # Ensure this takes scaling into account
    
    # Adjust the HR scale marginally
    hr.scale.marginal <- base.hr
    hr.scale.marginal[cadre] <- hr.scale.marginal[cadre] + prop
    
    # Extract the current combination of parameters
    allow_chw <- param_combinations$allow_chw_delivery[params]
    allow_pvtpharm <- param_combinations$allow_pvtpharm_delivery[params]
    allow_task_shifting <- param_combinations$allow_task_shifting[params]
    
    # Capture output based on the current parameter combination
    capture.output(
      find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                           drug_budget_input = base.drugbudget,  drug_budget_scale = 1, hr_scale = hr.scale.marginal,  # Use the adjusted HR scale
                           allow_chw_delivery = allow_chw, allow_pvtpharm_delivery = allow_pvtpharm, allow_markup = 0, allow_demand_constraint = 0, 
                           max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, allow_task_shifting = allow_task_shifting
      )
    )
    
    # Store the result in the drug_budget_marginal vector
    hr_marginal[i] <- solution.class$objval
    
    # Increment the index
    i <- i + 1
  }
}
# Output the drug_budget_marginal values
print(hr_marginal)

medstaff_value <- (hr_marginal[1:8]-nhb_hr_unscaled)/salaries_hr_annual[1]
nursestaff_value <- (hr_marginal[9:16]-nhb_hr_unscaled)/salaries_hr_annual[2]
pharmstaff_value <- (hr_marginal[17:24]-nhb_hr_unscaled)/salaries_hr_annual[3]
labstaff_value <- (hr_marginal[25:32]-nhb_hr_unscaled)/salaries_hr_annual[4]
mentalstaff_value <- (hr_marginal[33:40]-nhb_hr_unscaled)/salaries_hr_annual[5]
nutristaff_value <- (hr_marginal[41:48]-nhb_hr_unscaled)/salaries_hr_annual[6]
radiostaff_value <- (hr_marginal[49:56]-nhb_hr_unscaled)/salaries_hr_annual[7]
chwstaff_value <- (hr_marginal[57:64]-nhb_hr_unscaled)/salaries_hr_annual[8]

margval_table[,1] <- medstaff_value
margval_table[,2] <- nursestaff_value
margval_table[,3] <- pharmstaff_value
margval_table[,4] <- labstaff_value
margval_table[,5] <- mentalstaff_value
margval_table[,6] <- nutristaff_value
margval_table[,7] <- radiostaff_value
margval_table[,8] <- chwstaff_value

margval_1000_table = round(margval_table*1000,2)

-----------------------------------------------------------------------------------------------------------------------
  
  cadre_labels <- c("Doctor/Clinical officer", "Nursing staff", "Pharmaceutical staff", "Laboratory staff", 
                    "Dental staff", "Mental Health staff", "Nutrition staff", "Diagnostic staff", 
                    "Community health workers")

resource_labels <- c(cadre_labels[cadres_marg], "Consumables budget")

library(ggplot2)

# Define a list of specific scenario names
scenario_names <- c("Baseline Scenario", "CHW Scenario", "Pvt Pharmacist Scenario", 
                    "CHW & Pvt Pharmacist Scenario", "Taskshifting only", 
                    "Taskshifting with CHW", "Taskshifting with Pvt Pharm", 
                    "Taskshifting with both modes")

##########################################################
#All marginal values in one plot 
library(ggplot2)
library(reshape2)

# Assuming 'cadres_marg' contains the indices or positions for 'cadre_labels'
resource_labels <- c(cadre_labels[cadres_marg], "Consumables budget")

# Convert data into long format for faceting
df_long <- melt(margval_1000_table)
colnames(df_long) <- c("Scenario", "Resource", "Value")

# Ensure Resource column has correct factor ordering for the y-axis labels
df_long$Resource <- factor(df_long$Resource, labels = resource_labels)  # Map Resource to resource_labels

# Ensure correct factor ordering for Scenario
df_long$Scenario <- factor(df_long$Scenario, labels = scenario_names)

# Create faceted bar plot
p <- ggplot(data = df_long, aes(x = Value, y = Resource, fill = Resource)) + 
  geom_col(width = 0.6, show.legend = FALSE) +  
  facet_wrap(~Scenario, ncol = 4, scales = "free_x") +  # Adjust scales per facet
  geom_text(aes(
    label = round(Value, 3), 
    hjust = ifelse(Value > (max(df_long$Value, na.rm = TRUE) * 0.85), 1.2, -0.1)  # Adjust based on value size
  ), size = 4, color = "black") +  
  scale_fill_manual(values = rainbow(length(resource_labels))) +  # Adjust colors for new Resource levels
  labs(
    title = "Marginal Value Across Scenarios",
    x = "Net DALYs Averted",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 30, 10, 10)  # Increase right margin
  ) +
  coord_cartesian(xlim = c(0, max(df_long$Value, na.rm = TRUE) * 1.1))  # Avoid cutting off

# Save the plot to a file
ggsave("4_outputs/figures/marginal_value_plot.png", plot = p, width = 12, height = 8, dpi = 300)  # Adjust filename and dimensions as needed


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #Ability to pay for chws (value based maximum price) that Uganda is able to pay for CHWs. 
  
  #Baseline: facility-based delivery
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                       allow_task_shifting = 0)

totaldalysaverted_baseline <- dalys_averted

find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

totaldalysaverted_chw <- dalys_averted

chw_ability_topay <- (totaldalysaverted_chw - totaldalysaverted_baseline) * base.cet
chw_ability_topay_pp <- round((chw_ability_topay)/(as.vector(total_hruse)[9]),2)

find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

totaldalysaverted_pvtpharm <- dalys_averted

pvtpharm_ability_topay <- (totaldalysaverted_pvtpharm - totaldalysaverted_baseline) * base.cet
pvtpharm_ability_topay_pp <- (pvtpharm_ability_topay)/(as.vector(total_hruse)[10])

#print estimates 
print(chw_ability_topay_pp)
print(pvtpharm_ability_topay_pp)

library(ggplot2)
library(dplyr)
library(scales)

# Data Preparation (Assuming you have total_hruse defined)
results <- data.frame(
  Provider = c("CHW", "Private Pharmacist"),
  Ability_to_Pay_per_Person = c(chw_ability_topay_pp, pvtpharm_ability_topay_pp)
)

# Create the Plot
p <- ggplot(results, aes(x = Provider, y = Ability_to_Pay_per_Person, fill = Provider)) +
  geom_col(width = 0.7, alpha = 0.8) +  # Adjust width and alpha for appearance
  geom_text(
    aes(label = paste0("$", scales::comma(Ability_to_Pay_per_Person, accuracy = 1))), # Use scales::comma for thousands separator and rounding
    vjust = -0.5,  # Position above the bars
    size = 5,      # Adjust size as needed
    color = "black" # Text color
  ) +
  scale_y_continuous(
    expand = c(0, 0), # Remove space between bars and y-axis
    limits = c(0, max(results$Ability_to_Pay_per_Person) * 1.2), # Adjust y-axis limit
    labels = scales::comma_format(prefix = "$")  # Format y-axis to dollars with thousands separator
  ) +
  scale_fill_manual(values = c("#4285F4", "#EA4335")) +  # Use distinct colors
  theme_classic() +  # Use a clean theme
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_text(size = 14, color = "black", margin = margin(0, 10, 0, 0)), # Adjust y-axis title
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "black"),  # Center the title
    legend.position = "none" # Remove the legend
  ) +
  labs(y = "Max Value-Based Price", title = "Maximum Value-Based Price per CHW/Pvt Pharmacist per year")

print(p)

# Optional: Save the plot
ggsave("4_outputs/figures/ability_to_pay_plot.png", p, width = 9, height = 6, dpi = 300)

-------------------------------------------------------------------------------------------------------
  ##TASKSHIFTING SCENARIOS
  #Baseline: facility-based delivery with task-shifting 
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                       allow_task_shifting = 1)

totaldalysaverted_baseline <- dalys_averted

find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)

totaldalysaverted_chw <- dalys_averted

chw_ability_topay <- (totaldalysaverted_chw - totaldalysaverted_baseline) * base.cet
chw_ability_topay_pp <- round((chw_ability_topay)/(as.vector(total_hruse)[9]),2)

find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)

totaldalysaverted_pvtpharm <- dalys_averted

pvtpharm_ability_topay <- (totaldalysaverted_pvtpharm - totaldalysaverted_baseline) * base.cet
pvtpharm_ability_topay_pp <- (pvtpharm_ability_topay)/(as.vector(total_hruse)[10])

#print estimates 
print(chw_ability_topay_pp)
print(pvtpharm_ability_topay_pp)

library(ggplot2)
library(dplyr)
library(scales)

# Data Preparation (Assuming you have total_hruse defined)
results <- data.frame(
  Provider = c("CHW", "Private Pharmacist"),
  Ability_to_Pay_per_Person = c(chw_ability_topay_pp, pvtpharm_ability_topay_pp)
)

# Create the Plot
p <- ggplot(results, aes(x = Provider, y = Ability_to_Pay_per_Person, fill = Provider)) +
  geom_col(width = 0.7, alpha = 0.8) +  # Adjust width and alpha for appearance
  geom_text(
    aes(label = paste0("$", scales::comma(Ability_to_Pay_per_Person, accuracy = 1))), # Use scales::comma for thousands separator and rounding
    vjust = -0.5,  # Position above the bars
    size = 5,      # Adjust size as needed
    color = "black" # Text color
  ) +
  scale_y_continuous(
    expand = c(0, 0), # Remove space between bars and y-axis
    limits = c(0, max(results$Ability_to_Pay_per_Person) * 1.2), # Adjust y-axis limit
    labels = scales::comma_format(prefix = "$")  # Format y-axis to dollars with thousands separator
  ) +
  scale_fill_manual(values = c("#4285F4", "#EA4335")) +  # Use distinct colors
  theme_classic() +  # Use a clean theme
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_text(size = 14, color = "black", margin = margin(0, 10, 0, 0)), # Adjust y-axis title
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "black"),  # Center the title
    legend.position = "none" # Remove the legend
  ) +
  labs(y = "Max Value-Based Price", title = "Maximum Value-Based Price per CHW/Pvt Pharmacist per year")

print(p)

# Optional: Save the plot
ggsave("4_outputs/figures/ability_to_pay_plot_taskshifting.png", p, width = 9, height = 6, dpi = 300)

#-------------------------------------------------------------------------------------------------------------------------------------















