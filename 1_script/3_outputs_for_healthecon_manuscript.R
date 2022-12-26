#############################################################
## Linear Programming Function to optimize Uganda's Health Benefits Package

## Created by: Sakshi Mohan; 01/2021

## This file generates the outputs for the Health Economics submission 
#############################################################

# Extract resource use and case numbers for scenario 7 and 8
# solution_hruse, solution_drugexp

feascov_assumption = 0
feascov_scale_assumption = 1
base.cet = 161
drugbg_assumption = 1
base.hr = rep(1,8)

# Scenarios
##########################################################
scenarios = c("base", "taskshifting") # for file names
scenario_labels = c("Base scenario", "Task-shifting scenario") # for table headers

# Base scenario
find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = base.cet, 
                                       drug_budget_input = base.drugbudget, drug_budget.scale = drugbg_assumption, 
                                       hr.scale = base.hr, use_feasiblecov_constraint = feascov_assumption, feascov_scale = feascov_scale_assumption, compcov_scale = 1,
                                       compulsory_interventions = NULL, substitutes = subs_list, complements_nested = comp_nested_list,
                                       task_shifting_pharm = 0)
summary_base = cbind.data.frame(intervention.count, dalys_averted, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_base = solution

# Task-shifting scenario
task_shifting_scenario  <- find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = base.cet, 
                                                drug_budget_input = base.drugbudget, drug_budget.scale = drugbg_assumption, 
                                                hr.scale = base.hr, use_feasiblecov_constraint = feascov_assumption, feascov_scale = feascov_scale_assumption, compcov_scale = 1,
                                                compulsory_interventions = NULL, substitutes = subs_list, complements_nested = comp_nested_list,
                                                task_shifting_pharm = 1)
summary_taskshifting = cbind.data.frame(intervention.count, dalys_averted, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
optimal_coverage_taskshifting = solution

# STable 1: Data Summary
##########################################################
cols.num <- c('Medical Officer / Specialist','Clinical Officer / Technician',
              'Med. Assistant', 'Nurse Officer', 'Nurse Midwife Technician',
              'Pharmacist', 'Pharm Technician', 'Pharm Assistant',
              'Mental Health Staff',
              'Nutrition Staff')
chosen_df[cols.num] <- sapply(chosen_df[cols.num],as.numeric)

data <- cbind(chosen_df$category, chosen_df$intervention, 
              chosen_df$dalys, chosen_df$fullcost, chosen_df$fullcost/chosen_df$dalys, 
              chosen_df$cases, chosen_df$drugcost, 
              chosen_df$'Medical Officer / Specialist' + chosen_df$'Clinical Officer / Technician',
              chosen_df$'Med. Assistant' + chosen_df$'Nurse Officer' + chosen_df$'Nurse Midwife Technician',
              chosen_df$'Pharmacist' + chosen_df$'Pharm Technician' + chosen_df$'Pharm Assistant',
              chosen_df$'Mental Health Staff',
              chosen_df$'Nutrition Staff',
              chosen_df$maxcoverage)


data <- data[order(data[,1],data[,2],decreasing=FALSE),]
int_code_new <- 1:dim(chosen_df)[1]
data <- cbind(int_code_new, data)

colnames(data) <- c("No.", "Program", "Intervention", 
                    "DALYs averted per patient (Uganda)", "Cost per case (Uganda), 2019 USD*", "ICER*",
                    "Total number of cases in need", 
                    "Annual consumables cost, 2019 USD",
                    "Doctors/Clinical Officers", "Nursing staff",
                    "Pharmaceutical staff", "Mental health staff", 
                    "Nutrition staff",
                    "Maximum feasible coverage (%)") 


filename <- "4_outputs/tables/stable1_data.csv"
write.csv(data, file = filename)

# Table 2: Result Summary
##########################################################
summary = rbind(summary_base, summary_taskshifting)
summary = cbind(scenario_labels, summary)
colnames(summary) = c("Scenario", "Number of interventions with a positive Net Health Benefit", "Number of interventions in the optimal package", 
                      "Total DALYs averted","Net DALYs averted", "Highest ICER in the optimal pacakge", "Percentage of drug budget required",
                      "Percentage of Doctor/Clinical officer capacity required", "Percentage of Nursing staff capacity required",
                      "Percentage of Pharmaceutical staff capacity required", 
                      "Percentage of Mental health staff capacity required", "Percentage of Nutrition staff capacity required")

write.csv(t(summary), file = "4_outputs/tables/table_2_result_summary.csv")


# Table 3: Summary by program
##########################################################
# Base
capture.output(
  find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = drugbg_assumption, 
                       hr.scale = base.hr, use_feasiblecov_constraint = feascov_assumption, feascov_scale = feascov_scale_assumption, compcov_scale = 1,
                       compulsory_interventions = NULL, substitutes = subs_list, complements_nested = comp_nested_list,
                       task_shifting_pharm = 1)
)
intervention_included <- solution != 0
program_detailed_base = cbind(chosen_df$category, as.data.frame(rep(1, dim(chosen_df)[1])), intervention_included)

program_summary_pt1 <- program_detailed_base %>% group_by(chosen_df$category) %>%
  summarize_all(sum)

program_summary_pt1[,4] <- program_summary_pt1[,3]/program_summary_pt1[,2]

# Task_shifting
capture.output(
  find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = drugbg_assumption, 
                       hr.scale = base.hr, use_feasiblecov_constraint = feascov_assumption, feascov_scale = feascov_scale_assumption, compcov_scale = 1,
                       compulsory_interventions = NULL, substitutes = subs_list, complements_nested = comp_nested_list,
                       task_shifting_pharm = 1)
)
intervention_included <- solution != 0
program_detailed_taskshifting = cbind(chosen_df$category, as.data.frame(rep(1, dim(chosen_df)[1])), intervention_included)
#program_detailed_taskshifting[,2] = as.logical(program_detailed_taskshifting[,2])
#colnames()
#transform(program_detailed_taskshifting, char = as.numeric(char))

program_summary_pt2 <- program_detailed_taskshifting %>% group_by(chosen_df$category) %>%
  summarize_all(sum)
program_summary_pt2[,4] <- program_summary_pt2[,3]/program_summary_pt2[,2]

program_summary <- cbind(program_summary_pt1, program_summary_pt2[,c(3,4)])
colnames(program_summary) = c("Program", "Number of interventions included in the analysis",
                              "Number of interventions in the optimal package (Base)", "Percentage of interventions in the optimal package (Base)",
                              "Number of interventions in the optimal package (Task-shifting)", "Percentage of interventions in the optimal package (Task-shifting)")

write.csv(program_summary, file = "4_outputs/tables/table_3_program_summary.csv")

# STable 2/3 - Solution and resource use 
##########################################################
# Results on chosen package/coverage under various scenarios
for (task_shifting_scen in c(1,2)){
  find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = drugbg_assumption, 
                       hr.scale = base.hr, use_feasiblecov_constraint = feascov_assumption, feascov_scale = feascov_scale_assumption, compcov_scale = 1,
                       compulsory_interventions = NULL, substitutes = subs_list, complements_nested = comp_nested_list,
                       task_shifting_pharm = task_shifting_scen-1)
  
  detailed_results <- cbind(chosen_df$category, chosen_df$intervention, solution, solution*cases, 
                            solution_dalysaverted, solution_drugexp, solution_hruse[,visible_cadres])
  detailed_results <- detailed_results[order(detailed_results[,1],detailed_results[,2],decreasing=FALSE),]
  int_code_new <- 1:dim(chosen_df)[1]
  detailed_results_table <- cbind(int_code_new, detailed_results)
  
  colnames(detailed_results_table) <- c("No.", "Program", "Intervention", "Percentage of cases in need covered under optimal package",
                                        "Total number of cases covered under the optimal package", 
                                        "DALYs averted", "Consumable expenditure required",
                                        "Doctors/Clinical Officers", "Nursing staff",
                                        "Pharmaceutical staff", "Mental health staff", 
                                        "Nutrition staff") 
  
  
  filename <- paste0("4_outputs/tables/stable_", as.character(task_shifting_scen+1), scenarios[task_shifting_scen], ".csv") 
  write.csv(detailed_results_table, file = filename)
}


# STable 8 - Solution and resource use
##########################################################
scenario_labels_ext = c(scenario_labels, "Only consumables budget constraint scenario", 
                        "Only consumables budget and maximum feasible coverage constraint scenario") # for table headers

# Only consumables budget constraint scenario
capture.output(
  find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = 1, 
                       hr.scale = no.hr.limit, use_feasiblecov_constraint = 0, feascov_scale = 1, compcov_scale = 1,
                       compulsory_interventions = NULL, substitutes = subs_list, complements_nested = comp_nested_list,
                       task_shifting_pharm = 1)
)
summary_onlydrug = cbind.data.frame(intervention.count, dalys_averted, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))

# Only consumables budget and maximum feasible coverage constraint scenario
capture.output(
  find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = 1, 
                       hr.scale = no.hr.limit, use_feasiblecov_constraint = 1, feascov_scale = 1, compcov_scale = 1,
                       compulsory_interventions = NULL, substitutes = subs_list, complements_nested = comp_nested_list,
                       task_shifting_pharm = 1)
)
summary_onlydrugandfeascov = cbind.data.frame(intervention.count, dalys_averted, solution.class$objval, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))

summary_4scens = rbind(summary_base, summary_taskshifting, summary_onlydrug, summary_onlydrugandfeascov)
summary_4scens = cbind(scenario_labels_ext, summary_4scens)
colnames(summary_4scens) = colnames(summary) 

write.csv(t(summary_4scens), file = "4_outputs/tables/stable8_result_summary.csv")


# Figure 1: Resource use graphs
##########################################################
for (task_shifting_scen in c(1,2)){
  find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = drugbg_assumption, 
                       hr.scale = base.hr, use_feasiblecov_constraint = feascov_assumption, feascov_scale = feascov_scale_assumption, compcov_scale = 1,
                       compulsory_interventions = NULL, substitutes = subs_list, complements_nested = comp_nested_list,
                       task_shifting_pharm = task_shifting_scen-1)
  gen_resourceuse_graphs("", paste0("4_outputs/figures/resourceuse_", scenarios[task_shifting_scen], ".png"))
}

# Figure 2: Marginal value
##########################################################
salaries_hr_mth <- c(567, 166, 230, 230, 230, 166)
salaries_hr_annual <- salaries_hr_mth * 12
cadre_names <- c("doctor", "nurse", "pharm", "lab", "dental", "mental", "nutri", "diag")

nhb_hr_unscaled = c(NA, NA)
margval_table = matrix(nrow = 2, ncol = length(visible_cadres)+1)
  
for (task_shifting_scen in c(1,2)){
  print(scenarios[task_shifting_scen])
  
  capture.output(
    find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = base.cet, 
                         drug_budget_input = base.drugbudget, drug_budget.scale = drugbg_assumption, 
                         hr.scale = base.hr, use_feasiblecov_constraint = feascov_assumption, feascov_scale = feascov_scale_assumption, compcov_scale = 1,
                         compulsory_interventions = NULL, substitutes = subs_list, complements_nested = comp_nested_list,
                         task_shifting_pharm = task_shifting_scen - 1)
  
  )
  
  nhb_hr_unscaled[task_shifting_scen] = solution.class$objval
  
  # Marginal value of consumables
  capture.output(
    find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = base.cet, 
                         drug_budget_input = base.drugbudget+1, drug_budget.scale = drugbg_assumption, 
                         hr.scale = base.hr, use_feasiblecov_constraint = feascov_assumption, feascov_scale = feascov_scale_assumption, compcov_scale = 1,
                         compulsory_interventions = NULL, substitutes = subs_list, complements_nested = comp_nested_list,
                         task_shifting_pharm = task_shifting_scen - 1)
    
  )
  value_1drug <- (solution.class$objval - nhb_hr_unscaled[task_shifting_scen])
  margval_table[task_shifting_scen, length(visible_cadres)+1] <- value_1drug
  
  
  # Marginal Value of HR cadres
  i = 1
  for (cadre in visible_cadres){ 
    print(cadre_names[cadre])
    #noquote(paste0("prop_1",  cadre_names[cadre]))
    
    prop <- 1/cons_hr.limit.saved[,cadre] # Note that this already takes into account scaling

    hr.scale.marginal <- base.hr
    hr.scale.marginal[cadre] <- hr.scale.marginal[cadre] + prop
    
    capture.output(
      find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = base.cet, 
                           drug_budget_input = base.drugbudget, drug_budget.scale = drugbg_assumption, 
                           hr.scale = hr.scale.marginal, use_feasiblecov_constraint = feascov_assumption, feascov_scale = feascov_scale_assumption, compcov_scale = 1,
                           compulsory_interventions = NULL, substitutes = subs_list, complements_nested = comp_nested_list,
                           task_shifting_pharm = task_shifting_scen - 1)

    )
    
    a <- (solution.class$objval - nhb_hr_unscaled[task_shifting_scen])/salaries_hr_annual[i]
    assign(paste0("value_1",  cadre_names[cadre], "_", scenarios[task_shifting_scen]), a)
    margval_table[task_shifting_scen, i] <- a
    
    i = i+1
  }
}

margval_1000_table = round(margval_table*1000,2)

cadre_labels <- c("Doctor/\nClinical officer", "Nursing \nstaff", "Pharmaceutical \nstaff", "Laboratory \nstaff", 
               "Dental \nstaff", "Mental Health \nstaff", "Nutrition \nstaff", "Diagnostic \nstaff")
resource_labels <- c(cadre_labels[visible_cadres], "Consumables budget")


for (task_shifting_scen in c(1,2)){
  if (task_shifting_scen == 1){
    xlim_scen = 5000
    label_placement = 500
  } else{
    xlim_scen = 30
    label_placement = 2
  }
  
  margval <- margval_1000_table[task_shifting_scen,]
  filename <- paste0("4_outputs/figures/margval_", scenarios[task_shifting_scen], ".pdf")
  pdf(filename)
  par(mar=c(4,11,4,4))
  
  y <- barplot(height = rev(t(margval)), names = rev(resource_labels), width = 1 ,
               space=NULL, las = 1, horiz=T,
               xlim=c(0,xlim_scen), ylab = "", xlab = "Net DALYs averted",
               main="")
  x <-as.matrix(rev(t(margval)))
  text(x+label_placement,y,labels=as.character(round(x,3)))
  dev.off()
}

# Figure for responses: Fully unconstrained scenario
##########################################################
find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = no.cet, 
                     drug_budget_input = base.drugbudget, drug_budget.scale = no.drugbudget.limit, 
                     hr.scale = no.hr.limit, use_feasiblecov_constraint = 0, feascov_scale = 1, compcov_scale = 1,
                     compulsory_interventions = NULL, substitutes = subs_list, complements_nested = comp_nested_list,
                     task_shifting_pharm = 1)
gen_resourceuse_graphs("", "4_outputs/figures/resourceuse_noconstraints.png")
