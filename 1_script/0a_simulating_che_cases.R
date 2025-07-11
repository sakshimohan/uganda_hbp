## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

## Analysis with Dual Objectives: Health Maximisation & FRP ##

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

#############################################
# 1 - Set up and simulating household incomes
#############################################

## N.B: This code has been run before the scripts '0_...' and '1_...' to generate the CHE cases at intervention level and plug into the excel files.

# Remove all objects in the global environment
rm(list = ls())
# To clear the console
cat("\014")

##Generating the household income draws
#setwd("/Users/finn/Documents/Work/York/NIH HIV in Eswatini NYU/Analysis/")
setwd("/Users/sm2511/Dropbox/York/Research Projects/NIH Eswatini/HBP/")
#file <- "/Users/finn/Documents/Work/York/NIH HIV in Eswatini NYU/Analysis/2_data/Eswatini_HBP_Tool_for_R_script_v2_increm.xlsx"
file <- "/Users/sm2511/Dropbox/York/Research Projects/NIH Eswatini/HBP/2_data/Eswatini_HBP_Tool_for_R_script_v2_increm.xlsx"

## income parameters
gini <- 0.493 ## From Eswatini Household Income and Expenditure Survey 2016/17 Final Report (2019). Report gives figure of 49.3 so assume this should be 0.493.
ave_hh_consumption <-  896.50 ## Mean per person household consumption (US$2023) -- From Eswatini Household Income and Expenditure Survey 2016/17 Final Report (2019).
## Could also use Mean per person household income (US$2023) = 1,378.14 from same source. Original figures given in 2019 SZL, see excel for conversion.

total.draws <- 100000 ## number of household income's to generate - note this is arbitrary because we calculate CHE cases by pulling incomes from this distribution with replacement later. 

## define income cutoffs
cutoffs <- c(0.1,0.25)

## define marginal productivity of health care expenditure in Eswatini (cost-effectiveness threshold)
base.cet <- 2612 # This value is in 2023 USD (Lomas et al. 2022)

##Drug and Supplies buget
base.drugbudget <-38269715

## set seed
set.seed(21052025)

## generate hh income draws
fgamma <- function(phi){gini-(1/(phi*4^phi))*1/beta(phi,phi + 1)} #income distribution function
phi <- uniroot(fgamma,lower=0.000001,upper=100)$root
beta <- (1/phi)*(ave_hh_consumption)

gen.draws <- function(y){rgamma(y,shape=phi,scale=beta)}
draws <- gen.draws(total.draws)

## save file with per person HH income draws
write.table(draws,"3_processing/income_draws.csv",sep=",",row.names=F)

## Load per person HH income draws data
hh.incomes <- read.table("3_processing/income_draws.csv",sep=",",header=T)
names(hh.incomes) <- "hh.incomes"

## Verify that this simulated income distribution represents the same gini coefficient
estimate_gini <- function(x) {
  x <- sort(x)
  n <- length(x)
  index <- 1:n
  G <- sum((2 * index - n - 1) * x)
  G / (n * sum(x))
}
# Note that the above is an approximate formula from pg 101 of Cowell, F.A. (2000). Measuring Inequality. 3rd Edition. Oxford University Press.
# This is used because using the original formula has large memory requirements

simulated_gini = estimate_gini(hh.incomes$hh.incomes)
stopifnot(0.99 < simulated_gini / gini, simulated_gini / gini < 1.01) # This makes sure that the gini coefficient of the simulated income levels is within 1% of the actual gini coefficient

## Load frp parameter data 
print("USING FILE:")
print(file)
data <- read_excel(path = file, sheet = "frp")

###################################
# 2 - Calculating CHE cases averted  
###################################

## set seed
set.seed(21052025)

## Change is CHE is only for individuals who would utilise interventions under the exclusion scenario - i.e. don't count incremental patients who utilise based on free provision in CHE cases averted
## loop by row
for(r in 1:dim(data)[1]) {
  
  ## draw subset of incomes
  i <- runif(n=round(data$excl_case_numbers[r]),min=1,max=total.draws) 
  tx.incomes <- hh.incomes[i,]
  
  ## calculate the percent of income OOP represents
  i.per <- data$oop_cost[r]/tx.incomes
  
  ## generate counts of CHE averted at all cutoffs (10% HH income & 25% HH income)
  che.counts <- vector()
  
  index <- 0
  for(c in cutoffs) {
    index <- index + 1
    che.counts[index] <- length(which(i.per >= c))
  }
  data$che.10[r] <- che.counts[1]
  data$che.25[r] <- che.counts[2]
}  # close row loop

## Calculating Net Health Benefit
data$nhb <- data$incl_increm_dalys_avert - data$total_cost_orig_study / base.cet

## save output
write.csv(data, file = "3_processing/frp.csv")

## Once run, this information has been manually added into the script used for the subsequent analysis i.e. already in excel data file.

