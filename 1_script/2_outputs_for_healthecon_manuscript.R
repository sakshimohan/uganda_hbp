#############################################################
## Linear Programming Function to optimize Uganda's Health Benefits Package

## Created by: Sakshi Mohan; 01/2021

## Updated by: Megha Rao; 02/2025

## This file generates the outputs for the new community based health benefits package for Uganda 
#############################################################


##########################################################
# 1 - Set Working Directory & and Run LP function Script
##########################################################
setwd ("/Users/crw571/Desktop/uganda_hbp")
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
no.drugbudget.limit <- 9999999999
no.cet <- 9999999999

########################################################################################################
# 3 - Run optimisation under a variety of constraint scenarios
#-------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------

visible_cadres = c(1:4,6:10) # showing all cadres except the dental staff

# Scenarios
##########################################################
scenarios = c("Baseline: facility-based delivery", "Inclusion of VHTs only", "Inclusion of medicine retailers only", "Inclusion of both VHTs and medicine retailers", 
              "Allowing mark up for inclusion of medicine retailers", "VHTs and allowing mark up for medicine retailers")  # for file names

scenario_labels = c("Baseline scenario", "Standalone VHT Integration", "Standalone Medicine Retailers Integration", "Joint Integration", 
                    "Standalone Medicine Retailers Integration with markup", "Joint Integration with markup") # for table headers

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

summary_chw = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_chw = solution

#3. Inclusion of medicine retailers only
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_pvtpharm = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_pvtpharm = solution


#4. Inclusion of both VHTs and medicine retailers 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_chw_and_pvtpharm = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_chw_and_pvtpharm = solution

#5. #Allowing mark up with inclusion of medicine retailers 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_pvtpharm_markup = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_pvtpharm_markup = solution

#6. Allowing mark up with inclusion of VHTs and medicine retailers 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_pvtpharm_chw_markup = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_pvtpharm_chw_markup = solution

# Table 2: Result Summary
##########################################################
summary = rbind(summary_base, summary_vht, summary_mr, summary_vht_and_mr, 
                summary_mr_markup, summary_mr_vht_markup)
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

#Save the results 
write.csv(t(summary), file = "4_outputs/tables/table_1_result_summary.csv")

#Table 2: Summary by disease program areas 
-------------------------------------------------------------------------------------------
#1.  Baseline scenario: Only facility based delivery
capture.output(
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                       allow_task_shifting = 0))

program_detailed_base = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt1 <- program_detailed_base %>% group_by(category) %>% summarize_all(sum)
program_summary_pt1[,4] <- program_summary_pt1[,3]/sum(program_summary_pt1[,2])
colnames(program_summary_pt1)[4] <- "solution_pt1"
program_summary_pt1 <- program_summary_pt1 %>% rename(total.dalys.avertible = `cases * dalys`)

#2. Inclusion of VHTs only 
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                                    allow_task_shifting = 0))

program_detailed_chw = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt2 <- program_detailed_chw %>% group_by(category) %>% summarize_all(sum)
program_summary_pt2[,4] <- program_summary_pt2[,3]/sum(program_summary_pt2[,2])
colnames(program_summary_pt2)[4] <- "solution_pt2"
program_summary_pt2 <- program_summary_pt2 %>% rename(total.dalys.avertible = `cases * dalys`)

#3. Inclusion of medicine retailers only
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                                    allow_task_shifting = 0))

program_detailed_pvtpharm = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt3 <- program_detailed_pvtpharm %>% group_by(category) %>% summarize_all(sum)
program_summary_pt3[,4] <- program_summary_pt3[,3]/sum(program_summary_pt3[,2])
colnames(program_summary_pt3)[4] <- "solution_pt3"
program_summary_pt3 <- program_summary_pt3 %>% rename(total.dalys.avertible = `cases * dalys`)

#4. Inclusion of both VHTs and medicine retailers
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                                    allow_task_shifting = 0))

program_detailed_chw_pvtpharm = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt4 <- program_detailed_chw_pvtpharm %>% group_by(category) %>% summarize_all(sum)
program_summary_pt4[,4] <- program_summary_pt4[,3]/sum(program_summary_pt4[,2])
colnames(program_summary_pt4)[4] <- "solution_pt4"
program_summary_pt4 <- program_summary_pt4 %>% rename(total.dalys.avertible = `cases * dalys`)


#5. Allowing mark up with inclusion of medicine retailers only
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                                    allow_task_shifting = 0))

program_detailed_markup_pvtpharm = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt5 <- program_detailed_markup_pvtpharm %>% group_by(category) %>% summarize_all(sum)
program_summary_pt5[,4] <- program_summary_pt5[,3]/sum(program_summary_pt5[,2])
colnames(program_summary_pt5)[4] <- "solution_pt5"
program_summary_pt5 <- program_summary_pt5 %>% rename(total.dalys.avertible = `cases * dalys`)

#6. Allowing mark up with inclusion of both VHTs and medicine retailers
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                                    allow_task_shifting = 0))

program_detailed_markup_both = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt6 <- program_detailed_markup_both %>% group_by(category) %>% summarize_all(sum)
program_summary_pt6[,4] <- program_summary_pt6[,3]/sum(program_summary_pt6[,2])
colnames(program_summary_pt6)[4] <- "solution_pt4"
program_summary_pt6 <- program_summary_pt6 %>% rename(total.dalys.avertible = `cases * dalys`)

#Merging all the program summaries of all scenarios. Ignore the error messages 
program_summary <- merge(program_summary_pt1, program_summary_pt2, by = c("category", "total.dalys.avertible"))
program_summary <- merge(program_summary, program_summary_pt3, by = c("category", "total.dalys.avertible"))
program_summary <- merge(program_summary, program_summary_pt4, by = c("category", "total.dalys.avertible"))
program_summary <- merge(program_summary, program_summary_pt5, by = c("category", "total.dalys.avertible"))
program_summary <- merge(program_summary, program_summary_pt6, by = c("category", "total.dalys.avertible"))

colnames(program_summary) = c("Program", "Total DALYs averted overall",
                              "Total DALYs averted in the optimal package (Base)", "Percentage of total DALYs averted in the optimal package (Base)",
                              "Total DALYs averted in the optimal package (VHTs)", "Percentage of total DALYs averted in the optimal package (VHT)", 
                              "Total DALYs averted in the optimal package (Medicine retailers)", "Percentage of total DALYs averted in the optimal package (Medicine retailers)",
                              "Total DALYs averted in the optimal package (VHTs and Medicine retailers)", "Percentage of total DALYs averted in the optimal package (VHT and Medicine retailers)",
                              "Total DALYs averted in the optimal package (Medicine retailers & markup)", "Percentage of total DALYs averted in the optimal package (Medicine retailers with markup)",
                              "Total DALYs averted in the optimal package (VHTs and Medicine retailers & markup)", "Percentage of total DALYs averted in the optimal package (VHT and Medicine retailers with markup)")

write.csv(program_summary, file = "4_outputs/tables/table_2_program_summary.csv")

#Rename the columns
colnames(program_summary_pt1)[4] <- "solution.prop"
colnames(program_summary_pt2)[4] <- "solution.prop"
colnames(program_summary_pt3)[4] <- "solution.prop"
colnames(program_summary_pt4)[4] <- "solution.prop"
colnames(program_summary_pt5)[4] <- "solution.prop"
colnames(program_summary_pt6)[4] <- "solution.prop"

#Rename the scenarios
program_summary_pt1$scenario <- rep("Baseline", nrow(program_summary_pt1))
program_summary_pt2$scenario <- rep("Standalone VHT Integration", nrow(program_summary_pt2))
program_summary_pt3$scenario <- rep("Standalone Medicine Retailers Integration", nrow(program_summary_pt3))
program_summary_pt4$scenario <- rep("Joint Integration", nrow(program_summary_pt4))
program_summary_pt5$scenario <- rep("Standalone Medicine Retailers Integration \nwith markup", nrow(program_summary_pt5))
program_summary_pt6$scenario <- rep("Joint \nIntegration \nwith markup", nrow(program_summary_pt6))

#1. Plot 1 Only integration of VHTs and medicine retailers. No taskshifting or mark ups
#Prepare the data set 
tmap_tab <- rbind(program_summary_pt1, program_summary_pt2, program_summary_pt3, program_summary_pt4)
data_melted <- reshape2::melt(tmap_tab, id.vars = c("category", "scenario"), measure.vars = "solution.prop")
#Create a palette with 13 distinct colors
library(paletteer)
pal <- as.character(paletteer_d("ggsci::default_igv", n = 13)) 
# Create a label column that only includes labels for segments > 2%
library(ggplot2)
data_melted$label <- ifelse(data_melted$value > 0.001, paste0(round(data_melted$value * 100, 2), "%"), "")
prog_sum_plot_no_taskshift <- ggplot(data_melted, aes(factor(scenario, 
                                                             levels = c("Baseline", "Standalone VHT Integration", "Standalone Medicine Retailers Integration", "Joint Integration",
                                                                        setdiff(unique(scenario), c("Baseline", "Standalone VHT Integration", "Standalone Medicine Retailers Integration", "Joint Integration")))), 
                                                      y = value, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 4) +
  scale_fill_manual(values = pal, name = "Disease Program") +
  ggtitle("Scenario-wise: Program Contribution to Total DALYs Averted") +
  theme_minimal() +
  labs(x = "Scenario", y = "Proportion of Total DALYs averted by disease program") +
  theme(axis.text.y = element_blank(),
        legend.position = "bottom")
#print the plot
prog_sum_plot_no_taskshift
# Open a PNG device with a fixed file name
png("4_outputs/figures/prog_sum_no_taskshift.png", width = 600, height = 600)
print(prog_sum_plot_no_taskshift)
#To save the results from sensitivity analysis use another file name
#png("4_outputs/figures/prog_sum_SA_no_taskshift.png", width = 600, height = 600)
# Close the device to save the file
dev.off()


#2. Plot 2 Allowing mark up with integration of VHTs and medicine retailers. No taskshifting
#Prepare the dataset
tmap_tab <- rbind(program_summary_pt3, program_summary_pt4, program_summary_pt5, program_summary_pt6)
data_melted <- reshape2::melt(tmap_tab, id.vars = c("category", "scenario"), measure.vars = "solution.prop")
# Create a palette with 13 distinct colors
library(paletteer)
pal <- as.character(paletteer_d("ggsci::default_igv", n = 13)) 
# Create a label column that only includes labels for segments > 2%
library(ggplot2)
data_melted$label <- ifelse(data_melted$value > 0.001, paste0(round(data_melted$value * 100, 2), "%"), "")
prog_sum_plot_markup_no_taskshift <- ggplot(data_melted, aes(factor(scenario, 
                                                                    levels = c("Standalone Medicine Retailers Integration", "Joint Integration", "Standalone Medicine Retailers Integration with Markup", "Joint Integration with markup",
                                                                               setdiff(unique(scenario), c("Standalone Medicine Retailers Integration", "Joint Integration", "Standalone Medicine Retailers Integration with Markup", "Joint Integration with markup")))), 
                                                             y = value, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 4) +
  scale_fill_manual(values = pal, name = "Disease Program") +
  ggtitle("Scenario-wise: Program Contribution to Total DALYs Averted") +
  theme_minimal() +
  labs(x = "Scenario", y = "Proportion of Total DALYs averted by disease program") +
  theme(axis.text.y = element_blank(),
        legend.position = "bottom")
#print the plot
prog_sum_plot_markup_no_taskshift
# Open a PNG device with a fixed filename
png("4_outputs/figures/prog_sum_markup_no_taskshift.png", width = 600, height = 600)
print(prog_sum_plot_markup_no_taskshift)
#To save the results from sensitivity analysis use another file name
#png("4_outputs/figures/prog_sum_SA_markup_no_taskshift.png", width = 600, height = 600)
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
                                      "Nutrition staff", "Radiography staff", "Village health team", "Medicine retailers") 

detailed_results_table$prop_dalysaverted <- sprintf("%.2f%%", (detailed_results_table$`DALYs averted` / dalys_averted) * 100)
detailed_results_table$prop_drugbudget_used <- sprintf("%.2f%%", (detailed_results_table$`Consumable expenditure required` / base.drugbudget) * 100)
detailed_results_table$prop_doctors_used <- sprintf("%.2f%%", (detailed_results_table$`Doctors/Clinical Officers` / cons_hr.limit[1]) * 100)
detailed_results_table$prop_nurses_used <- sprintf("%.2f%%", (detailed_results_table$`Nursing staff` / cons_hr.limit[2]) * 100)
detailed_results_table$prop_pharma_used <- sprintf("%.2f%%", (detailed_results_table$`Pharmaceutical staff` / cons_hr.limit[3]) * 100)
detailed_results_table$prop_lab_used <- sprintf("%.2f%%", (detailed_results_table$`Laboratory staff` / cons_hr.limit[4]) * 100)
detailed_results_table$prop_mental_used <- sprintf("%.2f%%", (detailed_results_table$`Mental health staff` / cons_hr.limit[6]) * 100)
detailed_results_table$prop_nutrition_used <- sprintf("%.2f%%", (detailed_results_table$`Nutrition staff` / cons_hr.limit[7]) * 100)
detailed_results_table$prop_radiography_used <- sprintf("%.2f%%", (detailed_results_table$`Radiography staff` / cons_hr.limit[8]) * 100)
detailed_results_table$prop_chws_used <- sprintf("%.2f%%", (detailed_results_table$`Village health team` / cons_hr.limit[9]) * 100)
detailed_results_table$prop_pvtpharm_used <- sprintf("%.2f%%", (detailed_results_table$`Medicine retailers` / cons_hr.limit[10]) * 100)

detailed_results_table$`Percentage of cases in need covered under optimal package`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package`) * 100)

filename <- "4_outputs/tables/stable1_data.csv"
write.csv(detailed_results_table, file = filename)

#VHT Scenario 
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
                                      "Nutrition staff", "Radiography staff", "Village health team", "Medicine retailers",
                                      "Percentage of cases in need covered under optimal package by facility based delivery",
                                      "Percentage of cases in need covered under optimal package by VHT delivery") 

detailed_results_table$prop_dalysaverted <- sprintf("%.2f%%", (detailed_results_table$`DALYs averted` / dalys_averted) * 100)
detailed_results_table$prop_drugbudget_used <- sprintf("%.2f%%", (detailed_results_table$`Consumable expenditure required` / base.drugbudget) * 100)
detailed_results_table$prop_doctors_used <- sprintf("%.2f%%", (detailed_results_table$`Doctors/Clinical Officers` / cons_hr.limit[1]) * 100)
detailed_results_table$prop_nurses_used <- sprintf("%.2f%%", (detailed_results_table$`Nursing staff` / cons_hr.limit[2]) * 100)
detailed_results_table$prop_pharma_used <- sprintf("%.2f%%", (detailed_results_table$`Pharmaceutical staff` / cons_hr.limit[3]) * 100)
detailed_results_table$prop_lab_used <- sprintf("%.2f%%", (detailed_results_table$`Laboratory staff` / cons_hr.limit[4]) * 100)
detailed_results_table$prop_mental_used <- sprintf("%.2f%%", (detailed_results_table$`Mental health staff` / cons_hr.limit[6]) * 100)
detailed_results_table$prop_nutrition_used <- sprintf("%.2f%%", (detailed_results_table$`Nutrition staff` / cons_hr.limit[7]) * 100)
detailed_results_table$prop_radiography_used <- sprintf("%.2f%%", (detailed_results_table$`Radiography staff` / cons_hr.limit[8]) * 100)
detailed_results_table$prop_chws_used <- sprintf("%.2f%%", (detailed_results_table$`Village health team` / cons_hr.limit[9]) * 100)
detailed_results_table$prop_pvtpharm_used <- sprintf("%.2f%%", (detailed_results_table$`Medicine retailers` / cons_hr.limit[10]) * 100)

detailed_results_table$`Percentage of cases in need covered under optimal package`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by VHT delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by VHT delivery`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`) * 100)

filename <- "4_outputs/tables/stable2_data.csv"
write.csv(detailed_results_table, file = filename) 

#Medicine retailers scenario
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
                                      "Nutrition staff", "Radiography staff", "Village health team", "Medicine retailers",
                                      "Percentage of cases in need covered under optimal package by facility based delivery",
                                      "Percentage of cases in need covered under optimal package by medicine retailers delivery") 

detailed_results_table$prop_dalysaverted <- sprintf("%.2f%%", (detailed_results_table$`DALYs averted` / dalys_averted) * 100)
detailed_results_table$prop_drugbudget_used <- sprintf("%.2f%%", (detailed_results_table$`Consumable expenditure required` / base.drugbudget) * 100)
detailed_results_table$prop_doctors_used <- sprintf("%.2f%%", (detailed_results_table$`Doctors/Clinical Officers` / cons_hr.limit[1]) * 100)
detailed_results_table$prop_nurses_used <- sprintf("%.2f%%", (detailed_results_table$`Nursing staff` / cons_hr.limit[2]) * 100)
detailed_results_table$prop_pharma_used <- sprintf("%.2f%%", (detailed_results_table$`Pharmaceutical staff` / cons_hr.limit[3]) * 100)
detailed_results_table$prop_lab_used <- sprintf("%.2f%%", (detailed_results_table$`Laboratory staff` / cons_hr.limit[4]) * 100)
detailed_results_table$prop_mental_used <- sprintf("%.2f%%", (detailed_results_table$`Mental health staff` / cons_hr.limit[6]) * 100)
detailed_results_table$prop_nutrition_used <- sprintf("%.2f%%", (detailed_results_table$`Nutrition staff` / cons_hr.limit[7]) * 100)
detailed_results_table$prop_radiography_used <- sprintf("%.2f%%", (detailed_results_table$`Radiography staff` / cons_hr.limit[8]) * 100)
detailed_results_table$prop_chws_used <- sprintf("%.2f%%", (detailed_results_table$`Village health team` / cons_hr.limit[9]) * 100)
detailed_results_table$prop_pvtpharm_used <- sprintf("%.2f%%", (detailed_results_table$`Medicine retailers` / cons_hr.limit[10]) * 100)

detailed_results_table$`Percentage of cases in need covered under optimal package`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by medicine retailers delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by medicine retailers delivery`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`) * 100)

filename <- "4_outputs/tables/stable3_data.csv"
write.csv(detailed_results_table, file = filename) 

#Both VHT and medicine retailers scenario 
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
                                      "Nutrition staff", "Radiography staff", "Village health team", "Medicine retailers",
                                      "Percentage of cases in need covered under optimal package by facility based delivery",
                                      "Percentage of cases in need covered under optimal package by VHT delivery",
                                      "Percentage of cases in need covered under optimal package by medicine retailers delivery") 

detailed_results_table$prop_dalysaverted <- sprintf("%.2f%%", (detailed_results_table$`DALYs averted` / dalys_averted) * 100)
detailed_results_table$prop_drugbudget_used <- sprintf("%.2f%%", (detailed_results_table$`Consumable expenditure required` / base.drugbudget) * 100)
detailed_results_table$prop_doctors_used <- sprintf("%.2f%%", (detailed_results_table$`Doctors/Clinical Officers` / cons_hr.limit[1]) * 100)
detailed_results_table$prop_nurses_used <- sprintf("%.2f%%", (detailed_results_table$`Nursing staff` / cons_hr.limit[2]) * 100)
detailed_results_table$prop_pharma_used <- sprintf("%.2f%%", (detailed_results_table$`Pharmaceutical staff` / cons_hr.limit[3]) * 100)
detailed_results_table$prop_lab_used <- sprintf("%.2f%%", (detailed_results_table$`Laboratory staff` / cons_hr.limit[4]) * 100)
detailed_results_table$prop_mental_used <- sprintf("%.2f%%", (detailed_results_table$`Mental health staff` / cons_hr.limit[6]) * 100)
detailed_results_table$prop_nutrition_used <- sprintf("%.2f%%", (detailed_results_table$`Nutrition staff` / cons_hr.limit[7]) * 100)
detailed_results_table$prop_radiography_used <- sprintf("%.2f%%", (detailed_results_table$`Radiography staff` / cons_hr.limit[8]) * 100)
detailed_results_table$prop_chws_used <- sprintf("%.2f%%", (detailed_results_table$`Village health team` / cons_hr.limit[9]) * 100)
detailed_results_table$prop_pvtpharm_used <- sprintf("%.2f%%", (detailed_results_table$`Medicine retailers` / cons_hr.limit[10]) * 100)

detailed_results_table$`Percentage of cases in need covered under optimal package`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by medicine retailers delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by medicine retailers delivery`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by VHT delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by VHT delivery`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`) * 100)

filename <- "4_outputs/tables/stable4_data.csv"
write.csv(detailed_results_table, file = filename) 

--------------------------------------------------------------------------------------------------------------
  # Resource use graphs (All resource use in one graph and created separately for each scenario)
  ##########################################################

# Define values for each parameter
allow_chw_delivery_values <- c(0, 1)
allow_pvtpharm_delivery_values <- c(0, 1)

# Create a matrix with all combinations of the parameters
param_combinations <- expand.grid(
  allow_chw_delivery = allow_chw_delivery_values,
  allow_pvtpharm_delivery = allow_pvtpharm_delivery_values)

# Define a list of specific scenario names
scenario_names <- c("Baseline", "Standalone VHT Integration", "Standalone Medicine Retailers \nIntegration", "Joint Integration")        
# Loop over the parameter combinations
for (params in 1:nrow(param_combinations)) {
  # Extract the current combination of parameters
  allow_chw <- param_combinations$allow_chw_delivery[params]
  allow_pvtpharm <- param_combinations$allow_pvtpharm_delivery[params]
  #allow_task_shifting <- param_combinations$allow_task_shifting[params]
  
  # Call find_optimal_package
  solution <- find_optimal_package(
    input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
    drug_budget_input = base.drugbudget, drug_budget_scale = 1, hr_scale = base.hr, 
    allow_chw_delivery = allow_chw, allow_pvtpharm_delivery = allow_pvtpharm, 
    allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
    allow_task_shifting = 0)
  
  # Use the scenario name from the list instead of the number
  plot_title <- paste("Resource use for", scenario_names[params])  # Scenario name from the list
  file_name <- paste0("4_outputs/figures/resourceuse_", scenario_names[params], ".png")  # File name with scenario name
  
  # Call gen_resource use_graphs to generate and save the graph
  gen_resourceuse_graphs(plot_title, file_name)
  
  # Optionally print progress for tracking
  print(paste("Iteration", params, "completed, plot saved to", file_name))
}


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##. Maximum value of investment and average productivity or ACER estimates   
  
#1. Ability to pay for VHTs (value based maximum price) that Uganda is able to pay for VHTs. Calculated as additional net DALYs averted * CET 
  
#Baseline: facility-based delivery
  find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                       hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                       allow_task_shifting = 0)

netdalysaverted_baseline <- solution.class$objval

#Inclusion of VHT
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

netdalysaverted_vht <- solution.class$objval

#Maximum value of investment on VHTs
vht_ability_topay <- (netdalysaverted_vht - netdalysaverted_baseline) * base.cet
#Maximum value of investment per VHT
vht_ability_topay_pp <- round(vht_ability_topay)/ hr_size[9]
#vht_ability_topay_pp <- round((vht_ability_topay)/(as.vector(total_hruse)[9]),2)

#Inclusion of Medicine retailers 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

netdalysaverted_mr <- solution.class$objval

#Maximum value of investment on medicine retailers
mr_ability_topay <- (netdalysaverted_mr - netdalysaverted_baseline) * base.cet
#Maximum value of investment per medicine retailer
mr_ability_topay_pp <- round(mr_ability_topay)/ hr_size[10]
#mr_ability_topay_pp <- (mr_ability_topay)/(as.vector(total_hruse)[10])

#print estimates 
print(vht_ability_topay_pp)
print(mr_ability_topay_pp)

#2. Average productivity of VHTs and other health facility staff -- possible only for cadres with associated remunerations
salaries_hr_mth <- c(1363, 557, 830, 604, 1157, 930, 851, 72)
salaries_hr_annual <- salaries_hr_mth * 12
salaries_hr_total <-  salaries_hr_annual * hr_size[-c(5, 10)]

no.doc.limit <- c(0,1,1,1,1,1,1,1,1,1)
no.nurse.limit <- c(1,0,1,1,1,1,1,1,1,1)
no.pharm.limit <- c(1,1,0,1,1,1,1,1,1,1)
no.lab.limit <- c(1,1,1,0,1,1,1,1,1,1)
no.dental.limit <- c(1,1,1,1,0,1,1,1,1,1)
no.mental.limit <- c(1,1,1,1,1,0,1,1,1,1)
no.nutri.limit <- c(1,1,1,1,1,1,0,1,1,1)
no.diag.limit <-c(1,1,1,1,1,1,1,0,1,1)
no.vht.limit <- c(1,1,1,1,1,1,1,1,0,1)
no.mr.limit <-  c(1,1,1,1,1,1,1,1,1,0)

#Baseline: facility-based delivery
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)
netdalysaverted_baseline <- solution.class$objval

#ACER and average productivity for VHTs 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

netdalysaverted_vht <- solution.class$objval

avg_productivity_vht <- (netdalysaverted_vht - netdalysaverted_baseline)/salaries_hr_total[8]
acer_vht <- 1/avg_productivity_vht

#ACER and average productivity for medical doctors 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = no.doc.limit, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

netdalysaverted_doc <- solution.class$objval
avg_productivity_doc <- (netdalysaverted_baseline - netdalysaverted_doc)/salaries_hr_total[1]
acer_doc <- 1/avg_productivity_doc

#ACER and average productivity for nursing staff
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = no.nurse.limit, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

netdalysaverted_nurse <- solution.class$objval
avg_productivity_nurse <- (netdalysaverted_baseline - netdalysaverted_nurse)/salaries_hr_total[2]
acer_nurse <- 1/avg_productivity_nurse

#ACER and average productivity for pharmaceutical staff
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = no.pharm.limit, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

netdalysaverted_pharm <- solution.class$objval
avg_productivity_pharm <- (netdalysaverted_baseline - netdalysaverted_pharm)/salaries_hr_total[3]
acer_pharm <- 1/avg_productivity_pharm

#ACER and average productivity for lab staff
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = no.lab.limit, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

netdalysaverted_lab <- solution.class$objval
avg_productivity_lab <- (netdalysaverted_baseline - netdalysaverted_lab)/salaries_hr_total[4]
acer_lab <- 1/avg_productivity_lab

#ACER and average productivity for mental health staff
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = no.mental.limit, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

netdalysaverted_mental <- solution.class$objval
avg_productivity_mental <- (netdalysaverted_baseline - netdalysaverted_mental)/salaries_hr_total[5]
acer_mental <- 1/avg_productivity_mental

#ACER and average productivity for nutritionists 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = no.nutri.limit, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

netdalysaverted_nutri <- solution.class$objval
avg_productivity_nutri <- (netdalysaverted_baseline - netdalysaverted_nutri)/salaries_hr_total[6]
acer_nutri <- 1/avg_productivity_nutri

#ACER and average productivity for diagnostic staff
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = no.diag.limit, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

netdalysaverted_diag <- solution.class$objval
avg_productivity_diag <- (netdalysaverted_baseline - netdalysaverted_diag)/salaries_hr_total[7]
acer_diag <- 1/avg_productivity_diag

##########################################################################
library(ggplot2)
library(dplyr)
library(scales)

# Data Preparation (Assuming you have total_hruse defined)
results <- data.frame(
  Provider = c("Village health team members", "Medicine retailers"),
  Maximum_value_provider = c(vht_ability_topay_pp, mr_ability_topay_pp)
)

# Create the Plot
p <- ggplot(results, aes(x = Provider, y = Maximum_value_provider, fill = Provider)) +
  geom_col(width = 0.7, alpha = 0.8) +  # Adjust width and alpha for appearance
  geom_text(
    aes(label = paste0("$", scales::comma(Maximum_value_provider, accuracy = 1))), # Use scales::comma for thousands separator and rounding
    vjust = -0.5,  # Position above the bars
    size = 5,      # Adjust size as needed
    color = "black" # Text color
  ) +
  scale_y_continuous(
    expand = c(0, 0), # Remove space between bars and y-axis
    limits = c(0, max(results$Maximum_value_provider) * 1.2), # Adjust y-axis limit
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
  labs(y = "Maximum value of investment per provider", title = "Maximum Value of Investment per VHT/Medicine retailer")

print(p)

#Save the plot
ggsave("4_outputs/figures/ability_to_pay_plot.png", p, width = 9, height = 6, dpi = 300)

#Create a plot showing Average productivity for all health workers 
##########################################################################

# Data Preparation (Assuming you have total_hruse defined)
results <- data.frame(
  Provider = c("doctor", "nurse", "pharm", "lab", "mental", "nutri", "radio", "VHT"),
  ACER = c(acer_doc, acer_nurse, acer_pharm, acer_lab, acer_mental, acer_nutri, acer_diag, acer_vht)
)

# Create the Plot
p <- ggplot(results, aes(x = Provider, y = ACER, fill = Provider)) +
  geom_col(width = 0.7, alpha = 0.8) +  # Adjust width and alpha for appearance
  geom_text(
    aes(label = paste0("$", scales::comma(ACER, accuracy = 0.01))), # Use scales::comma for thousands separator and rounding
    vjust = -0.5,  # Position above the bars
    size = 5,      # Adjust size as needed
    color = "black" # Text color
  ) +
  scale_y_continuous(
    expand = c(0, 0), # Remove space between bars and y-axis
    limits = c(0, max(results$ACER) * 1.2), # Adjust y-axis limit
    labels = scales::comma_format(prefix = "$", accuracy = 0.01)  # Format y-axis to dollars with thousands separator
  ) +
  scale_fill_manual(values = c("#4285F4", "#EA4335", "#FBBC05", "#34A853", "#A142F4", "#00ACC1", "#FF6D00", "#D81B60")) +  # Use distinct colors
  theme_classic() +  # Use a clean theme
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_text(size = 14, color = "black", margin = margin(0, 10, 0, 0)), # Adjust y-axis title
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "black"),  # Center the title
    legend.position = "none" # Remove the legend
  ) +
  labs(y = "Average cost effectiveness ratio", title = "Average cost effectiveness ratio per health worker")

print(p)

# Optional: Save the plot
ggsave("4_outputs/figures/acer_plot.png", p, width = 9, height = 6, dpi = 300)


#Create a plot showing Average productivity for all health workers 
##########################################################################

# Data Preparation (Assuming you have total_hruse defined)
results <- data.frame(
  Provider = c("doctor", "nurse", "pharm", "lab", "mental", "nutri", "radio", "VHT"),
  average_prod = c(avg_productivity_doc, avg_productivity_nurse, avg_productivity_pharm, avg_productivity_lab, avg_productivity_mental, avg_productivity_nutri, avg_productivity_diag, avg_productivity_vht)
)

# Create the Plot
p <- ggplot(results, aes(x = Provider, y = average_prod, fill = Provider)) +
  geom_col(width = 0.7, alpha = 0.8) +  # Adjust width and alpha for appearance
  geom_text(
    aes(label = scales::comma(average_prod, accuracy = 0.01)), # Use scales::comma for thousands separator and rounding
    vjust = -0.5,  # Position above the bars
    size = 5,      # Adjust size as needed
    color = "black" # Text color
  ) +
  scale_y_continuous(
    expand = c(0, 0), # Remove space between bars and y-axis
    limits = c(0, max(results$average_prod) * 1.2), # Adjust y-axis limit
    labels = scales::comma_format(accuracy = 0.01)  # Format y-axis to dollars with thousands separator
  ) +
  scale_fill_manual(values = c("#4285F4", "#EA4335", "#FBBC05", "#34A853", "#A142F4", "#00ACC1", "#FF6D00", "#D81B60")) +  # Use distinct colors
  theme_classic() +  # Use a clean theme
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_text(size = 14, color = "black", margin = margin(0, 10, 0, 0)), # Adjust y-axis title
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "black"),  # Center the title
    legend.position = "none" # Remove the legend
  ) +
  labs(y = "Average productivity", title = "net DALYs averted per $ spent on salaries per health worker")

print(p)

# Optional: Save the plot
ggsave("4_outputs/figures/averageproductivity_plot.png", p, width = 9, height = 6, dpi = 300)

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

# Initialize the nhb_hr_unscaled vector with the correct length
nhb_hr_unscaled <- numeric(4)

# Create a matrix with all combinations of the parameters
param_combinations <- expand.grid(
  allow_chw_delivery = allow_chw_delivery_values,
  allow_pvtpharm_delivery = allow_pvtpharm_delivery_values
)

# Loop over the parameter combinations
i <- 1
for (params in 1:nrow(param_combinations)) {
  # Extract the current combination of parameters
  allow_chw <- param_combinations$allow_chw_delivery[params]
  allow_pvtpharm <- param_combinations$allow_pvtpharm_delivery[params]

  # Capture output based on the current parameter combination
  capture.output(
    find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth',  cet_input = base.cet, drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                         hr_scale = base.hr, allow_chw_delivery = allow_chw, allow_pvtpharm_delivery = allow_pvtpharm, allow_markup = 0, allow_demand_constraint = 0, 
                         max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, allow_task_shifting = 0
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
drug_budget_marginal <- numeric(4)

# Loop through the parameter combinations
i <- 1
for (params in 1:nrow(param_combinations)) {
  # Extract the current combination of parameters
  allow_chw <- param_combinations$allow_chw_delivery[params]
  allow_pvtpharm <- param_combinations$allow_pvtpharm_delivery[params]
  
  # Capture output based on the current parameter combination
  capture.output(
    find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, drug_budget_input = base.drugbudget + 1, drug_budget_scale = 1,  # Adjusting drug budget as neededdrug_budget.scale = 1, 
                         hr_scale = base.hr, allow_chw_delivery = allow_chw, allow_pvtpharm_delivery = allow_pvtpharm, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                         allow_task_shifting = 0
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
    
    # Capture output based on the current parameter combination
    capture.output(
      find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                           drug_budget_input = base.drugbudget,  drug_budget_scale = 1, hr_scale = hr.scale.marginal,  # Use the adjusted HR scale
                           allow_chw_delivery = allow_chw, allow_pvtpharm_delivery = allow_pvtpharm, allow_markup = 0, allow_demand_constraint = 0, 
                           max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, allow_task_shifting = 0
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

medstaff_value <- (hr_marginal[1:4]-nhb_hr_unscaled)/salaries_hr_annual[1]
nursestaff_value <- (hr_marginal[5:8]-nhb_hr_unscaled)/salaries_hr_annual[2]
pharmstaff_value <- (hr_marginal[9:12]-nhb_hr_unscaled)/salaries_hr_annual[3]
labstaff_value <- (hr_marginal[13:16]-nhb_hr_unscaled)/salaries_hr_annual[4]
mentalstaff_value <- (hr_marginal[17:20]-nhb_hr_unscaled)/salaries_hr_annual[5]
nutristaff_value <- (hr_marginal[21:24]-nhb_hr_unscaled)/salaries_hr_annual[6]
radiostaff_value <- (hr_marginal[25:28]-nhb_hr_unscaled)/salaries_hr_annual[7]
chwstaff_value <- (hr_marginal[29:32]-nhb_hr_unscaled)/salaries_hr_annual[8]

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
                    "Village Health Team")

resource_labels <- c(cadre_labels[cadres_marg], "Consumables budget")

# Define a list of specific scenario names
scenario_names <- c("Baseline", "Standalone VHT Integration", "Standalone Medicine Retailer Integration", 
                    "Joint Integration")

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

####################################################################################################################################################################################################
#FACILITY BASED TASKSHIFTING 
#######################################################
#Sensitivity Analysis for facility based taskshifting 
scenarios = c("Allowing taskshifting (to baseline)", "Allowing taskshifting & inclusion of VHTs", "Allowing taskshifting & inclusion of medicine retailers", 
                "Allowing taskshifting & inclusion of VHTs & medicine retailers", 
                "Takshifting with markup for medicine retailers", "Taskshifting with VHTs and medicine retailers and markup")  # for file names

scenario_labels = c("Taskshifting", "Taskshifting and Standalone VHT Integration", "Taskshifting and Standalone Medicine Retailers Integration", 
                    "Taskshifting and Joint Integration", "Taskshifting and Standalone Medicine Retailers Integration with Markup", 
                    "Taskshifting and Joint Integration with Markup") # for table headers

#7. Sensitivity analysis: Allowing facility based taskshifting to the baseline scenario 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)

summary_taskshifting_vht = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_taskshifting_vht = solution

#8. Sensitivity analysis: Allowing facility based taskshifting with inclusion of VHTs  
  
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)  
  
summary_taskshifting_mr = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_taskshifting_mr = solution 
  
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

summary_taskshifting_with_other_modes = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_taskshifting_with_othermodes = solution

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
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 1)  

summary_mr_vht_taskshift_markup = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_mr_vht_taskshift_markup = solution
  
  
# Table 2: Result Summary
##########################################################
summary = rbind(summary_taskshifting, summary_taskshifting_vht, summary_taskshifting_mr, 
                summary_taskshifting_with_other_modes, 
                summary_mr_taskshift_markup, summary_mr_vht_taskshift_markup)
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

write.csv(t(summary), file = "4_outputs/tables/table_1_taskshifting_result_summary.csv")

---------------------------------------------------------------------------------
#Summary of results by disease program areas 
  ------------------------------------------------
#7. Allowing taskshifting (to baseline)
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                                    allow_task_shifting = 1))

program_detailed_taskshifting = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt7 <- program_detailed_taskshifting %>% group_by(category) %>% summarize_all(sum)
program_summary_pt7[,4] <- program_summary_pt7[,3]/sum(program_summary_pt7[,2])
colnames(program_summary_pt7)[4] <- "solution_pt7"
program_summary_pt7 <- program_summary_pt7 %>% rename(total.dalys.avertible = `cases * dalys`)

#8. Allowing taskshifting & inclusion of VHTs
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                                    allow_task_shifting = 1))

program_detailed_taskshifting_vhts = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt8 <- program_detailed_taskshifting_vhts %>% group_by(category) %>% summarize_all(sum)
program_summary_pt8[,4] <- program_summary_pt8[,3]/sum(program_summary_pt8[,2])
colnames(program_summary_pt8)[4] <- "solution_pt8"
program_summary_pt8 <- program_summary_pt8 %>% rename(total.dalys.avertible = `cases * dalys`)


#9. Allowing task-shifting & inclusion of medicine retailers
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                                    allow_task_shifting = 1))

program_detailed_taskshifting_mr = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt9 <- program_detailed_taskshifting_mr %>% group_by(category) %>% summarize_all(sum)
program_summary_pt9[,4] <- program_summary_pt9[,3]/sum(program_summary_pt9[,2])
colnames(program_summary_pt9)[4] <- "solution_pt9"
program_summary_pt9 <- program_summary_pt9 %>% rename(total.dalys.avertible = `cases * dalys`)


#10. Allowing task-shifting & inclusion of VHTs & medicine retailers
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
                                    allow_task_shifting = 1))

program_detailed_taskshifting_both = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt10 <- program_detailed_taskshifting_both %>% group_by(category) %>% summarize_all(sum)
program_summary_pt10[,4] <- program_summary_pt8[,3]/sum(program_summary_pt10[,2])
colnames(program_summary_pt10)[4] <- "solution_pt10"
program_summary_pt10 <- program_summary_pt10 %>% rename(total.dalys.avertible = `cases * dalys`)

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

##########################################################
# 2 - Set up common inputs for scenarios
##########################################################
## Pre-code main inputs, complements, and substitutes for the scenarios that follow 
#######################################################################################
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

# Scenarios
##########################################################
scenarios = c("Baseline: facility-based delivery", "Inclusion of VHTs only", "Inclusion of medicine retailers only", "Inclusion of both VHTs and medicine retailers", 
              "Allowing mark up for inclusion of medicine retailers", "VHTs and allowing mark up for medicine retailers")  # for file names

scenario_labels = c("Baseline scenario", "Standalone VHT Integration", "Standalone Medicine Retailers Integration", "Joint Integration", 
                    "Standalone Medicine Retailers Integration with markup", "Joint Integration with markup") # for table headers

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

summary_chw = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_chw = solution

#3. Inclusion of medicine retailers only
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_pvtpharm = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_pvtpharm = solution


#4. Inclusion of both VHTs and medicine retailers 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_chw_and_pvtpharm = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_chw_and_pvtpharm = solution

#5. #Allowing mark up with inclusion of medicine retailers 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_pvtpharm_markup = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_pvtpharm_markup = solution

#6. Allowing mark up with inclusion of VHTs and medicine retailers 
find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                     hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                     allow_task_shifting = 0)

summary_pvtpharm_chw_markup = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, dalys_averted.prop, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_pvtpharm_chw_markup = solution

# Table 2: Result Summary
##########################################################
summary = rbind(summary_base, summary_vht, summary_mr, summary_vht_and_mr, 
                summary_mr_markup, summary_mr_vht_markup)
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

#Save the results 
write.csv(t(summary), file = "4_outputs/tables/table_1_SA_result_summary.csv")

#Table 2: Summary by disease program areas 
-------------------------------------------------------------------------------------------
#1.  Baseline scenario: Only facility based delivery
capture.output(
    find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                         drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                         hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                         allow_task_shifting = 0))

program_detailed_base = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt1 <- program_detailed_base %>% group_by(category) %>% summarize_all(sum)
program_summary_pt1[,4] <- program_summary_pt1[,3]/sum(program_summary_pt1[,2])
colnames(program_summary_pt1)[4] <- "solution_pt1"
program_summary_pt1 <- program_summary_pt1 %>% rename(total.dalys.avertible = `cases * dalys`)

#2. Inclusion of VHTs only 
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 0, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                                    allow_task_shifting = 0))

program_detailed_chw = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt2 <- program_detailed_chw %>% group_by(category) %>% summarize_all(sum)
program_summary_pt2[,4] <- program_summary_pt2[,3]/sum(program_summary_pt2[,2])
colnames(program_summary_pt2)[4] <- "solution_pt2"
program_summary_pt2 <- program_summary_pt2 %>% rename(total.dalys.avertible = `cases * dalys`)

#3. Inclusion of medicine retailers only
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                                    allow_task_shifting = 0))

program_detailed_pvtpharm = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt3 <- program_detailed_pvtpharm %>% group_by(category) %>% summarize_all(sum)
program_summary_pt3[,4] <- program_summary_pt3[,3]/sum(program_summary_pt3[,2])
colnames(program_summary_pt3)[4] <- "solution_pt3"
program_summary_pt3 <- program_summary_pt3 %>% rename(total.dalys.avertible = `cases * dalys`)

#4. Inclusion of both VHTs and medicine retailers
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                                    allow_task_shifting = 0))

program_detailed_chw_pvtpharm = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt4 <- program_detailed_chw_pvtpharm %>% group_by(category) %>% summarize_all(sum)
program_summary_pt4[,4] <- program_summary_pt4[,3]/sum(program_summary_pt4[,2])
colnames(program_summary_pt4)[4] <- "solution_pt4"
program_summary_pt4 <- program_summary_pt4 %>% rename(total.dalys.avertible = `cases * dalys`)


#5. Allowing mark up with inclusion of medicine retailers only
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 0, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                                    allow_task_shifting = 0))

program_detailed_markup_pvtpharm = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt5 <- program_detailed_markup_pvtpharm %>% group_by(category) %>% summarize_all(sum)
program_summary_pt5[,4] <- program_summary_pt5[,3]/sum(program_summary_pt5[,2])
colnames(program_summary_pt5)[4] <- "solution_pt5"
program_summary_pt5 <- program_summary_pt5 %>% rename(total.dalys.avertible = `cases * dalys`)

#6. Allowing mark up with inclusion of both VHTs and medicine retailers
capture.output(find_optimal_package(input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
                                    drug_budget_input = base.drugbudget, drug_budget_scale = 1, 
                                    hr_scale = base.hr, allow_chw_delivery = 1, allow_pvtpharm_delivery = 1, allow_markup = 1, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1,
                                    allow_task_shifting = 0))

program_detailed_markup_both = cbind(category, cases * dalys, solution_dalysaverted, as.data.frame(rep(1, length(intcode))))
program_summary_pt6 <- program_detailed_markup_both %>% group_by(category) %>% summarize_all(sum)
program_summary_pt6[,4] <- program_summary_pt6[,3]/sum(program_summary_pt6[,2])
colnames(program_summary_pt6)[4] <- "solution_pt4"
program_summary_pt6 <- program_summary_pt6 %>% rename(total.dalys.avertible = `cases * dalys`)

#Merging all the program summaries of all scenarios. Ignore the error messages 
program_summary <- merge(program_summary_pt1, program_summary_pt2, by = c("category", "total.dalys.avertible"))
program_summary <- merge(program_summary, program_summary_pt3, by = c("category", "total.dalys.avertible"))
program_summary <- merge(program_summary, program_summary_pt4, by = c("category", "total.dalys.avertible"))
program_summary <- merge(program_summary, program_summary_pt5, by = c("category", "total.dalys.avertible"))
program_summary <- merge(program_summary, program_summary_pt6, by = c("category", "total.dalys.avertible"))

colnames(program_summary) = c("Program", "Total DALYs averted overall",
                              "Total DALYs averted in the optimal package (Base)", "Percentage of total DALYs averted in the optimal package (Base)",
                              "Total DALYs averted in the optimal package (VHTs)", "Percentage of total DALYs averted in the optimal package (VHT)", 
                              "Total DALYs averted in the optimal package (Medicine retailers)", "Percentage of total DALYs averted in the optimal package (Medicine retailers)",
                              "Total DALYs averted in the optimal package (VHTs and Medicine retailers)", "Percentage of total DALYs averted in the optimal package (VHT and Medicine retailers)",
                              "Total DALYs averted in the optimal package (Medicine retailers & markup)", "Percentage of total DALYs averted in the optimal package (Medicine retailers with markup)",
                              "Total DALYs averted in the optimal package (VHTs and Medicine retailers & markup)", "Percentage of total DALYs averted in the optimal package (VHT and Medicine retailers with markup)")

write.csv(program_summary, file = "4_outputs/tables/table_2_SA_program_summary.csv")

#Rename the columns
colnames(program_summary_pt1)[4] <- "solution.prop"
colnames(program_summary_pt2)[4] <- "solution.prop"
colnames(program_summary_pt3)[4] <- "solution.prop"
colnames(program_summary_pt4)[4] <- "solution.prop"
colnames(program_summary_pt5)[4] <- "solution.prop"
colnames(program_summary_pt6)[4] <- "solution.prop"

#Rename the scenarios
program_summary_pt1$scenario <- rep("Baseline", nrow(program_summary_pt1))
program_summary_pt2$scenario <- rep("Standalone VHT Integration", nrow(program_summary_pt2))
program_summary_pt3$scenario <- rep("Standalone Medicine Retailers Integration", nrow(program_summary_pt3))
program_summary_pt4$scenario <- rep("Joint Integration", nrow(program_summary_pt4))
program_summary_pt5$scenario <- rep("Standalone Medicine Retailers Integration \nwith markup", nrow(program_summary_pt5))
program_summary_pt6$scenario <- rep("Joint \nIntegration \nwith markup", nrow(program_summary_pt6))

#1. Plot 1 Only integration of VHTs and medicine retailers. No taskshifting or mark ups
#Prepare the data set 
tmap_tab <- rbind(program_summary_pt1, program_summary_pt2, program_summary_pt3, program_summary_pt4)
data_melted <- reshape2::melt(tmap_tab, id.vars = c("category", "scenario"), measure.vars = "solution.prop")
#Create a palette with 13 distinct colors
library(paletteer)
pal <- as.character(paletteer_d("ggsci::default_igv", n = 13)) 
# Create a label column that only includes labels for segments > 2%
library(ggplot2)
data_melted$label <- ifelse(data_melted$value > 0.001, paste0(round(data_melted$value * 100, 2), "%"), "")
prog_sum_plot_no_taskshift <- ggplot(data_melted, aes(factor(scenario, 
                                                             levels = c("Baseline", "Standalone VHT Integration", "Standalone Medicine Retailers Integration", "Joint Integration",
                                                                        setdiff(unique(scenario), c("Baseline", "Standalone VHT Integration", "Standalone Medicine Retailers Integration", "Joint Integration")))), 
                                                      y = value, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 4) +
  scale_fill_manual(values = pal, name = "Disease Program") +
  ggtitle("Scenario-wise: Program Contribution to Total DALYs Averted") +
  theme_minimal() +
  labs(x = "Scenario", y = "Proportion of Total DALYs averted by disease program") +
  theme(axis.text.y = element_blank(),
        legend.position = "bottom")
#print the plot
prog_sum_plot_no_taskshift
# Open a PNG device with a fixed file name
png("4_outputs/figures/prog_sum_no_SA_taskshift.png", width = 600, height = 600)
print(prog_sum_plot_no_taskshift)
#To save the results from sensitivity analysis use another file name
#png("4_outputs/figures/prog_sum_SA_no_taskshift.png", width = 600, height = 600)
# Close the device to save the file
dev.off()


#2. Plot 2 Allowing mark up with integration of VHTs and medicine retailers. No taskshifting
#Prepare the dataset
tmap_tab <- rbind(program_summary_pt3, program_summary_pt4, program_summary_pt5, program_summary_pt6)
data_melted <- reshape2::melt(tmap_tab, id.vars = c("category", "scenario"), measure.vars = "solution.prop")
# Create a palette with 13 distinct colors
library(paletteer)
pal <- as.character(paletteer_d("ggsci::default_igv", n = 13)) 
# Create a label column that only includes labels for segments > 2%
library(ggplot2)
data_melted$label <- ifelse(data_melted$value > 0.001, paste0(round(data_melted$value * 100, 2), "%"), "")
prog_sum_plot_markup_no_taskshift <- ggplot(data_melted, aes(factor(scenario, 
                                                                    levels = c("Standalone Medicine Retailers Integration", "Joint Integration", "Standalone Medicine Retailers Integration with Markup", "Joint Integration with markup",
                                                                               setdiff(unique(scenario), c("Standalone Medicine Retailers Integration", "Joint Integration", "Standalone Medicine Retailers Integration with Markup", "Joint Integration with markup")))), 
                                                             y = value, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 4) +
  scale_fill_manual(values = pal, name = "Disease Program") +
  ggtitle("Scenario-wise: Program Contribution to Total DALYs Averted") +
  theme_minimal() +
  labs(x = "Scenario", y = "Proportion of Total DALYs averted by disease program") +
  theme(axis.text.y = element_blank(),
        legend.position = "bottom")
#print the plot
prog_sum_plot_markup_no_taskshift
# Open a PNG device with a fixed filename
png("4_outputs/figures/prog_sum_markup_no_SA_taskshift.png", width = 600, height = 600)
print(prog_sum_plot_markup_no_taskshift)
#To save the results from sensitivity analysis use another file name
#png("4_outputs/figures/prog_sum_SA_markup_no_taskshift.png", width = 600, height = 600)
# Close the device to save the file
dev.off()

# Table 2/3 - Solution and resource use along with optimal coverage for each mode of delivery
##########################################################
# Results on chosen package/coverage under various scenarios (Appendix files)

#VHT Scenario 
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
                                      "Nutrition staff", "Radiography staff", "Village health team", "Medicine retailers",
                                      "Percentage of cases in need covered under optimal package by facility based delivery",
                                      "Percentage of cases in need covered under optimal package by VHT delivery") 

detailed_results_table$prop_dalysaverted <- sprintf("%.2f%%", (detailed_results_table$`DALYs averted` / dalys_averted) * 100)
detailed_results_table$prop_drugbudget_used <- sprintf("%.2f%%", (detailed_results_table$`Consumable expenditure required` / base.drugbudget) * 100)
detailed_results_table$prop_doctors_used <- sprintf("%.2f%%", (detailed_results_table$`Doctors/Clinical Officers` / cons_hr.limit[1]) * 100)
detailed_results_table$prop_nurses_used <- sprintf("%.2f%%", (detailed_results_table$`Nursing staff` / cons_hr.limit[2]) * 100)
detailed_results_table$prop_pharma_used <- sprintf("%.2f%%", (detailed_results_table$`Pharmaceutical staff` / cons_hr.limit[3]) * 100)
detailed_results_table$prop_lab_used <- sprintf("%.2f%%", (detailed_results_table$`Laboratory staff` / cons_hr.limit[4]) * 100)
detailed_results_table$prop_mental_used <- sprintf("%.2f%%", (detailed_results_table$`Mental health staff` / cons_hr.limit[6]) * 100)
detailed_results_table$prop_nutrition_used <- sprintf("%.2f%%", (detailed_results_table$`Nutrition staff` / cons_hr.limit[7]) * 100)
detailed_results_table$prop_radiography_used <- sprintf("%.2f%%", (detailed_results_table$`Radiography staff` / cons_hr.limit[8]) * 100)
detailed_results_table$prop_chws_used <- sprintf("%.2f%%", (detailed_results_table$`Village health team` / cons_hr.limit[9]) * 100)
detailed_results_table$prop_pvtpharm_used <- sprintf("%.2f%%", (detailed_results_table$`Medicine retailers` / cons_hr.limit[10]) * 100)

detailed_results_table$`Percentage of cases in need covered under optimal package`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by VHT delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by VHT delivery`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`) * 100)

filename <- "4_outputs/tables/stable2_SA_data.csv"
write.csv(detailed_results_table, file = filename) 

#Medicine retailers scenario
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
                                      "Nutrition staff", "Radiography staff", "Village health team", "Medicine retailers",
                                      "Percentage of cases in need covered under optimal package by facility based delivery",
                                      "Percentage of cases in need covered under optimal package by medicine retailers delivery") 

detailed_results_table$prop_dalysaverted <- sprintf("%.2f%%", (detailed_results_table$`DALYs averted` / dalys_averted) * 100)
detailed_results_table$prop_drugbudget_used <- sprintf("%.2f%%", (detailed_results_table$`Consumable expenditure required` / base.drugbudget) * 100)
detailed_results_table$prop_doctors_used <- sprintf("%.2f%%", (detailed_results_table$`Doctors/Clinical Officers` / cons_hr.limit[1]) * 100)
detailed_results_table$prop_nurses_used <- sprintf("%.2f%%", (detailed_results_table$`Nursing staff` / cons_hr.limit[2]) * 100)
detailed_results_table$prop_pharma_used <- sprintf("%.2f%%", (detailed_results_table$`Pharmaceutical staff` / cons_hr.limit[3]) * 100)
detailed_results_table$prop_lab_used <- sprintf("%.2f%%", (detailed_results_table$`Laboratory staff` / cons_hr.limit[4]) * 100)
detailed_results_table$prop_mental_used <- sprintf("%.2f%%", (detailed_results_table$`Mental health staff` / cons_hr.limit[6]) * 100)
detailed_results_table$prop_nutrition_used <- sprintf("%.2f%%", (detailed_results_table$`Nutrition staff` / cons_hr.limit[7]) * 100)
detailed_results_table$prop_radiography_used <- sprintf("%.2f%%", (detailed_results_table$`Radiography staff` / cons_hr.limit[8]) * 100)
detailed_results_table$prop_chws_used <- sprintf("%.2f%%", (detailed_results_table$`Village health team` / cons_hr.limit[9]) * 100)
detailed_results_table$prop_pvtpharm_used <- sprintf("%.2f%%", (detailed_results_table$`Medicine retailers` / cons_hr.limit[10]) * 100)

detailed_results_table$`Percentage of cases in need covered under optimal package`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by medicine retailers delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by medicine retailers delivery`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`) * 100)

filename <- "4_outputs/tables/stable3_SA_data.csv"
write.csv(detailed_results_table, file = filename) 

#Both VHT and medicine retailers scenario 
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
                                      "Nutrition staff", "Radiography staff", "Village health team", "Medicine retailers",
                                      "Percentage of cases in need covered under optimal package by facility based delivery",
                                      "Percentage of cases in need covered under optimal package by VHT delivery",
                                      "Percentage of cases in need covered under optimal package by medicine retailers delivery") 

detailed_results_table$prop_dalysaverted <- sprintf("%.2f%%", (detailed_results_table$`DALYs averted` / dalys_averted) * 100)
detailed_results_table$prop_drugbudget_used <- sprintf("%.2f%%", (detailed_results_table$`Consumable expenditure required` / base.drugbudget) * 100)
detailed_results_table$prop_doctors_used <- sprintf("%.2f%%", (detailed_results_table$`Doctors/Clinical Officers` / cons_hr.limit[1]) * 100)
detailed_results_table$prop_nurses_used <- sprintf("%.2f%%", (detailed_results_table$`Nursing staff` / cons_hr.limit[2]) * 100)
detailed_results_table$prop_pharma_used <- sprintf("%.2f%%", (detailed_results_table$`Pharmaceutical staff` / cons_hr.limit[3]) * 100)
detailed_results_table$prop_lab_used <- sprintf("%.2f%%", (detailed_results_table$`Laboratory staff` / cons_hr.limit[4]) * 100)
detailed_results_table$prop_mental_used <- sprintf("%.2f%%", (detailed_results_table$`Mental health staff` / cons_hr.limit[6]) * 100)
detailed_results_table$prop_nutrition_used <- sprintf("%.2f%%", (detailed_results_table$`Nutrition staff` / cons_hr.limit[7]) * 100)
detailed_results_table$prop_radiography_used <- sprintf("%.2f%%", (detailed_results_table$`Radiography staff` / cons_hr.limit[8]) * 100)
detailed_results_table$prop_chws_used <- sprintf("%.2f%%", (detailed_results_table$`Village health team` / cons_hr.limit[9]) * 100)
detailed_results_table$prop_pvtpharm_used <- sprintf("%.2f%%", (detailed_results_table$`Medicine retailers` / cons_hr.limit[10]) * 100)

detailed_results_table$`Percentage of cases in need covered under optimal package`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by medicine retailers delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by medicine retailers delivery`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by VHT delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by VHT delivery`) * 100)
detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`<- sprintf("%.2f%%", (detailed_results_table$`Percentage of cases in need covered under optimal package by facility based delivery`) * 100)

filename <- "4_outputs/tables/stable4_SA_data.csv"
write.csv(detailed_results_table, file = filename) 

# Resource use graphs (All resource use in one graph and created separately for each scenario)
##########################################################

# Define values for each parameter
allow_chw_delivery_values <- c(0, 1)
allow_pvtpharm_delivery_values <- c(0, 1)

# Create a matrix with all combinations of the parameters
param_combinations <- expand.grid(
  allow_chw_delivery = allow_chw_delivery_values,
  allow_pvtpharm_delivery = allow_pvtpharm_delivery_values)

# Define a list of specific scenario names
scenario_names <- c("Baseline", "Standalone VHT Integration", "Standalone Medicine Retailers \nIntegration", "Joint Integration")        
# Loop over the parameter combinations
for (params in 1:nrow(param_combinations)) {
  # Extract the current combination of parameters
  allow_chw <- param_combinations$allow_chw_delivery[params]
  allow_pvtpharm <- param_combinations$allow_pvtpharm_delivery[params]
  #allow_task_shifting <- param_combinations$allow_task_shifting[params]
  
  # Call find_optimal_package
  solution <- find_optimal_package(
    input_data_file = chosen_data_file, objective_input = 'nethealth', cet_input = base.cet, 
    drug_budget_input = base.drugbudget, drug_budget_scale = 1, hr_scale = base.hr, 
    allow_chw_delivery = allow_chw, allow_pvtpharm_delivery = allow_pvtpharm, 
    allow_markup = 0, allow_demand_constraint = 0, max_feasible_coverage_scale = 1,  compulsory_intervention_coverage_scale= 1, 
    allow_task_shifting = 0)
  
  # Use the scenario name from the list instead of the number
  plot_title <- paste("Resource use for", scenario_names[params])  # Scenario name from the list
  file_name <- paste0("4_outputs/figures/resourceuse_SA_", scenario_names[params], ".png")  # File name with scenario name
  
  # Call gen_resource use_graphs to generate and save the graph
  gen_resourceuse_graphs(plot_title, file_name)
  
  # Optionally print progress for tracking
  print(paste("Iteration", params, "completed, plot saved to", file_name))
}


  
  
  
  
  
  
  
  
  
  























  
  
  


#-------------------------------------------------------------------------------------------------------------------------------------




#









