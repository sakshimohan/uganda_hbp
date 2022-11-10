##########################################################
# 1 - Set Working Directory & and Run LP function Script
##########################################################
setwd("C:/Users/sm2511/Dropbox/York/Research Projects/Uganda EHP/Analysis/repo/uganda_hbp/")

# Run R script which generates LP function
source("1_script/0_packages_and_functions.R")

##########################################################
# 2 - Set up common inputs for scenarios
##########################################################

## Pre-code complements and substitutes for the optimizations that follow 
##########################################################################
#  BASE FUNCTION INPUTS
chosen_df <- df
base.cet <- 161 # This value is in 2019 USD (2017 USD 154)
base.drugbudget <- 374300000
#base.hr <- rep(1,8)
pharm_scale = 1
base.hr <- c(1,1,pharm_scale,rep(1,5))
no.hr.limit <- rep(9999999999,8)
no.nurse.limit <- c(1,9999999999,1,1,1,1,1,1)
no.pharm.limit <- c(1,1,9999999999,1,1,1,1,1)
only.nurse.limit <- c(9999999999,1,9999999999,9999999999,1,1,1,1)
only.pharm.limit <- c(9999999999,9999999999,1, 9999999999,1,1,1,1)
no.drugbudget.limit <- 9999999999
no.cet <- 9999999999


# Currently all interventions are combined into one group pre-optimization

# SUBSTITUTES #
subs_list = list(subs1 = c("112","113"), # Viral load testing, CD4 testing - removed from the analysis
                 # subs3 = c("187", "188"), # Treatment of injuries (Fracture and dislocation - reduction), Treatment of injuries (Fracture and dislocation - fixation)
                 subs2 = c("217", "218"), # Prevention of cardiovascular disease, Prevention and treatment of cardiovascular disease
                 subs3 c("316", "317"), # Xpert test (Full), Xpert test (targeted)**
                 subs4 = c("185", "186"), # Colorectoral cancer (screening + treatment), Colorectoral cancer (treatment)**
                 subs5 = c("321", "322"), # ART (Second-Line Treatment) for adults without intensive monitoring, ART (Second-Line Treatment) for adults with intensive monitoring
                 subs6 = c("096", "097"), # Isoniazid preventive therapy for HIV+ pregnant women, Isoniazid preventive therapy for HIV+ people
                 subs7 = c("223", "224"), # Breast Cancer - active screening (biannual clinical breast examination of asymptomatic women aged 40-69 years) + treatment of stages I-IV, Breast Cancer - active screening (biannual mammography of asymptomatic women aged 50-69 years) + treatment of stages I-IV
                 subs8 = c("229", "233"), # Basic psychosocial support, advice, and follow-up (including antidepressants), Treatment of depression - antidepressants
                 subs9 = c("350", "351")) # Smear test at age 40 for cervical cancer detection + cancer treatment, Smear test at age 40 for cervical cancer detection + HPV vaccinations starting at age 12 + cancer treatment
# The constraint for substitutes currently is sum of coverage of substitutes <= mean (coverage); But the case numbers might be different (example targeted Xpert)

# Nested complements
comp_nested_list = list(comp_nested1 = c("006","007","059","061","303"), # ANC and its complements
                        comp_nested2 = c("003","004"), # safe abortion, post abortion care
                        comp_nested3 = c("104", "106"), # smear positive TB, smear negative TB
                        comp_nested4 = c("105", "106")) # Smear negative TB, MDR TB

########################################################################################################
# OUTPUT 1: DALYs averted and resource use under various scenarios with constraints added incrementally
#--------------------------------------------------------------------------------------------------------------
########################################################################################################
gdp_pc = 794
visible_cadres = c(1:4,6,7)

# Scenario 1: No constraints
capture.output(
  find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = no.cet, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = no.drugbudget.limit, 
                       hr.scale = no.hr.limit, use_feasiblecov_constraint = 0, feascov_scale = 1, compcov_scale = 1,
                       compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 0)
)
drug_exp.prop = drug_exp.prop * no.drugbudget.limit/base.drugbudget
scen1 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen1_coverage = solution

# Scenario 2: CET = 0.5 X GDP per capita
capture.output(
  find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = 0.5 * gdp_pc, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = no.drugbudget.limit, 
                       hr.scale = no.hr.limit, use_feasiblecov_constraint = 0, feascov_scale = 1, compcov_scale = 1,
                       compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 0)
)
drug_exp.prop = drug_exp.prop * no.drugbudget.limit/base.drugbudget
scen2 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen2_coverage = solution

# Scenario 3: CET = $161
capture.output(
  find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = no.drugbudget.limit, 
                       hr.scale = no.hr.limit, use_feasiblecov_constraint = 0, feascov_scale = 1, compcov_scale = 1,
                       compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 0)
)
drug_exp.prop = drug_exp.prop * no.drugbudget.limit/base.drugbudget
scen3 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen3_coverage = solution


# Scenario 4: CET = $161 + Drug budget constraint
capture.output(
  find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = 1, 
                       hr.scale = no.hr.limit, use_feasiblecov_constraint = 0, feascov_scale = 1, compcov_scale = 1,
                       compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 0)
)
scen4 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen4_coverage = solution

# Scenario 5: CET = $161 + Drug budget constraint + Feasible coverage constraint
capture.output(
  find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = 1, 
                       hr.scale = no.hr.limit, use_feasiblecov_constraint = 1, feascov_scale = 1, compcov_scale = 1,
                       compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 0)
)
scen5 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen5_coverage = solution

# Scenario 6: CET = $161 + Drug budget constraint + Feasible coverage constraint + HR capacity constraint (No pharmacist constraint)
capture.output(
  find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = 1, 
                       hr.scale = no.pharm.limit, use_feasiblecov_constraint = 1, feascov_scale = 1, compcov_scale = 1,
                       compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 0)
)
scen6 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen6_coverage = solution

# Scenario 7: CET = $161 + Drug budget constraint + Feasible coverage constraint + HR capacity constraint
capture.output(
  find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = 1, 
                       hr.scale = base.hr, use_feasiblecov_constraint = 1, feascov_scale = 1, compcov_scale = 1,
                       compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 0)
)
scen7 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen7_coverage = solution

# Scenario 8: CET = $161 + Drug budget constraint + Feasible coverage constraint + HR capacity constraint (with task shifting)
capture.output(
  find_optimal_package(data.frame = chosen_df, objective_input = 'nethealth', cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = 1, 
                       hr.scale = base.hr, use_feasiblecov_constraint = 1, feascov_scale = 1, compcov_scale = 1,
                       compulsory_interventions = NULL, substitutes = subs_list, complements_nested = comp_nested_list,
                       task_shifting_pharm = 1)
)
scen8 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, t(hruse.prop[,visible_cadres]))
scen8_coverage = solution

scenarios = c("No constraints",
              "CET (0.5 X GDP per capita)",
              "CET ($161)",
              "CET ($161) + Drug Budget",
              "CET ($161) + Drug budget + Feasible coverage",
              "CET ($161) + Drug budget + Feasible coverage + HR capacity(except pharmacists)",
              "CET ($161) + Drug budget + Feasible coverage + HR capacity",
              "CET ($161) + Drug budget + Feasible coverage + HR capacity(with task shifting to nurses)")

summary = rbind(scen1, scen2, scen3, scen4, scen5, scen6, scen7, scen8)
summary = cbind(scenarios, summary)
colnames(summary) = c("Constraints applied", "Number of interventions with positive NHB", "Number of interventions in the optimal package", 
                      "Total DALYs averted", "Highest ICER in the HBP", "% of drug budget required",
                      "% of Medical/Clinical officer capacity required", "% of Nursing staff capacity required",
                      "% of Pharmaceutical staff capacity required", "% of Lab staff capacity required",
                      "% of Mental health staff capacity required", "% of Nutrition staff capacity required")

print(xtable(summary, type = "latex"), file = "4_outputs/tables/scanario_summaries.tex")
write.csv(t(summary), file = "4_outputs/tables/scenarios_results.csv")

coverage_byscenario = cbind(category, intervention, scen1_coverage, scen2_coverage, scen3_coverage, scen4_coverage, scen5_coverage, scen6_coverage, scen7_coverage, scen8_coverage)
colnames(coverage_byscenario) = c("Program", "Intervention", scenarios)
write.csv(coverage_byscenario, file = "4_outputs/tables/scenarios_coverage_results.csv")