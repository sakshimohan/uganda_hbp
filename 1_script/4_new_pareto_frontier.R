setwd("/Users/sm2511/Dropbox/York/Research Projects/NIH Eswatini/HBP/")
source("/Users/sm2511/Dropbox/York/Research Projects/Uganda EHP/Community HBP_2024/uganda_hbp/1_script/0_packages_and_functions.R")


# Try the updated function
chosen_df <- df
base.cet <- 2612 # This value is in 2023 USD (Lomas et al. 2022)
base.drugbudget <-38269715 
base.hr <- rep(1,8)
base.hr1 <- rep(0.95,8) # if want to reduce the HR avaliability to 95% (X%) of total capacity
no.hr.limit <- rep(9999999999,8) # set an arbitrarily high scaling figure to represent no constraint
no.nurse.limit <- c(1,9999999999,1,1,1,1,1,1)
no.pharm.limit <- c(1,1,9999999999,1,1,1,1,1)
only.nurse.limit <- c(9999999999,1,9999999999,9999999999,1,1,1,1)
only.pharm.limit <- c(9999999999,9999999999,1, 9999999999,1,1,1,1)
no.drugbudget.limit <- 9999999999
no.cet <- 9999999999
gdp_pc = 4184 # GDP per capita

# Substitutes
#--------------
substitutes = list(subs1 = c("34","35"), # B
                   subs3 = c("27", "79"), # 
                   subs4 = c("218", "106"), # 
                   subs6 = c("293", "290"), # 
                   subs7 = c("293", "271"), # 
                   subs8 = c("41", "8"), # 
                   subs9 = c("41", "6"), # 
                   subs10 = c("41", "5")) #  # list of substitutable interventions 

# Nested complements
#--------------------
comp_nested_list = list(comp_nested1 = c("003", "004", 0.05), # Safe abortion, post abortion case management
                        comp_nested2 = c("289", "221", 1), # BCG vaccination and re-vaccination
                        comp_nested3 = c("280", "285", 1), # Stroke and post-stroke care
                        comp_nested4 = c("131","136", 1), # Screening: clinical breast exam and palliative care
                        comp_nested5 = c("131","132", 1), # Screening: clinical breast exam and mammography 
                        comp_nested6 = c("156","160", 1), # Screening: Colonoscopy and palliative care
                        comp_nested7 = c("141","142", 1), # Visual inspection with acetic acid (VIA) and pap smear 
                        comp_nested8 = c("141", "149", 1), # Visual inspection with acetic acid (VIA) and palliative care
                        comp_nested9 = c("198", "197", 1), # Cesearian Section with indication and with complication
                        comp_nested10 = c("317", "318", 1), # TB diagnosis and drug sensitivity test
                        comp_nested11 = c("317", "319", 1), # TB diagnosis and first-line treatment 
                        comp_nested12 = c("317", "320", 1) # TB diagnosis and second-line treatment
) 

# Compulsory interventions
#--------------------
compulsory_list = list(compulsory1 = c("92"), # ART for men
                       compulsory2 = c("93") # ART for women
)

## Find extreme solutions

# Only DALYs averted
find_optimal_package(data.frame = chosen_df, objective_input = 'dalys', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget.scale = 1, 
                     hr.scale = base.hr, use_feasiblecov_constraint = 0, feascov_scale = 1, compcov_scale = 1,
                     compulsory_interventions = compulsory_list, substitutes = substitutes, complements_nested = comp_nested_list,
                     task_shifting_pharm = 0)

# Only CHE cases averted
find_optimal_package(data.frame = chosen_df, objective_input = 'che10', cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget.scale = 1, 
                     hr.scale = base.hr, use_feasiblecov_constraint = 0, feascov_scale = 1, compcov_scale = 1,
                     compulsory_interventions = compulsory_list, substitutes = substitutes, complements_nested = comp_nested_list,
                     task_shifting_pharm = 0)

# Run some tests to check if middle solutions make sense

# Both CHE and DALYs (1 DALY = 1 CHE)
find_optimal_package(data.frame = chosen_df, objective_input = 'dalys_and_frp_che10', weight_dalys_per_1_che = 1, cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget.scale = 1, 
                     hr.scale = base.hr, use_feasiblecov_constraint = 0, feascov_scale = 1, compcov_scale = 1,
                     compulsory_interventions = compulsory_list, substitutes = substitutes, complements_nested = comp_nested_list,
                     task_shifting_pharm = 0)

# Both CHE and DALYs (1 DALY = 1 CHE, CHE at 25%)
find_optimal_package(data.frame = chosen_df, objective_input = 'dalys_and_frp_che25', weight_dalys_per_1_che = 1, cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget.scale = 1, 
                     hr.scale = base.hr, use_feasiblecov_constraint = 0, feascov_scale = 1, compcov_scale = 1,
                     compulsory_interventions = compulsory_list, substitutes = substitutes, complements_nested = comp_nested_list,
                     task_shifting_pharm = 0)

# Both CHE and DALYs (100 DALYs = 1 CHE)
find_optimal_package(data.frame = chosen_df, objective_input = 'dalys_and_frp_che10', weight_dalys_per_1_che = 100, cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget.scale = 1, 
                     hr.scale = base.hr, use_feasiblecov_constraint = 0, feascov_scale = 1, compcov_scale = 1,
                     compulsory_interventions = compulsory_list, substitutes = substitutes, complements_nested = comp_nested_list,
                     task_shifting_pharm = 0)

# Both CHE and DALYs (0.1 DALYs = 1 CHE)
find_optimal_package(data.frame = chosen_df, objective_input = 'dalys_and_frp_che10', weight_dalys_per_1_che = 0.1, cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget.scale = 1, 
                     hr.scale = base.hr, use_feasiblecov_constraint = 0, feascov_scale = 1, compcov_scale = 1,
                     compulsory_interventions = compulsory_list, substitutes = substitutes, complements_nested = comp_nested_list,
                     task_shifting_pharm = 0)

# Generate the Frontier

# Define the range of k values (how many DALYs are equivalent to 1 CHE)
# The higher the k, the more the weight given to CHE compared to DALYs
k_values <- seq(0, 100, by = 1) # c(seq(0, 1, by = 0.1), seq(2, 100, by = 1)) 

# Create an empty list to collect output rows
results_list <- list()

# Loop through k values and run the function
for (k in k_values) {
  
  outputs <- find_optimal_package(
    data.frame = chosen_df,
    objective_input = 'dalys_and_frp_che10',
    weight_dalys_per_1_che = k,
    cet_input = base.cet,
    drug_budget_input = base.drugbudget,
    drug_budget.scale = 1,
    hr.scale = base.hr,
    use_feasiblecov_constraint = 0,
    feascov_scale = 1,
    compcov_scale = 1,
    compulsory_interventions = compulsory_list,
    substitutes = substitutes,
    complements_nested = comp_nested_list,
    task_shifting_pharm = 0
  )
  
  # Extract the relevant values
  daly_averted <- outputs$`Total DALYs averted` # `Proportion of DALY burden averted`
  che10_averted <- outputs$`Total CHE cases (10% threshold) averted` #`Proportion of CHE cases (10% threshold) averted`
  
  # Store in list as a data frame row
  results_list[[length(results_list) + 1]] <- data.frame(
    k = k,
    daly_averted = daly_averted/1000000,
    che10_averted = che10_averted
  )
}

# Combine into a single data frame
results_df <- do.call(rbind, results_list)

library(ggplot2)

ggplot(results_df, aes(x = daly_averted, y = che10_averted, color = k)) +
  geom_point(size = 2) +
  scale_color_viridis_c() +
  labs(
    x = "DALY Burden Averted, millions",
    y = "CHE Cases Averted (10%)",
    color = "k (DALYs per CHE)",
    title = "Trade-off Between Health and Financial Risk Protection"
  ) +
  theme_minimal()
