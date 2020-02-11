###################################
# Oil and Gas Risk Simulation
# David Andexler
# v1.1
###################################
# 
# install.packages("graphics")
# install.packages("triangle")
# install.packages("ks")
# install.packages("MASS")
# install.packages("rgl")

library(graphics)
library(triangle)
library(ks)
library(MASS)
library(rgl)
library(readxl)
library(dplyr)

simulation_size <- 100

setwd('C:/Users/dande/Desktop/MAIN/Documents/NCState Advanced Analytics/Spring 2020/Spring 1/Simulation and Risk/Homework/data')

df <- read_xlsx('Analysis_Data.xlsx', sheet = "Price Projections", )

analysis_data <- as.data.frame(df[3:nrow(df), ]) 

colnames(analysis_data) <-  c("year", "high_oil_price", "low_oil_price", "aeo2018_ref")

# Standardize function
standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

# Destandardize function
destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}



##############################################
# Main Simulation Logic
##############################################

# Production risk occurs outside of main loop to improve performance
####################################
# Production Risk
# LogNormal Distribution 
# Mean = 420 BOPD, std = 120 BOPD
# Rate of decline is uniformly distributed between 15 and 32%
# Correlation coefficient of 0.64 between IP and decline rate that are drawn from respective distributions during each trial of simulation
# Reference Production Risk - Continued above the 1-15 loop
####################################

cor_simulation_size <- 1000000
sample_size = 100
correlation_coef <- 0.64

# Building the correlation matrix, Choleski decomp, and correlating the X's. #
R <- matrix(data=cbind(1,correlation_coef, correlation_coef, 1), nrow=2)
U <- t(chol(R))

set.seed(1234)
production_rate_year1_IP <- rlnorm(cor_simulation_size, meanlog = 6, sdlog = .28)

set.seed(1234)
production_decline_rate <- runif(cor_simulation_size, min = .15, max = 0.32)
X2u <- rnorm(sample_size*cor_simulation_size, mean = 0, sd = 1)

Both <- cbind(standardize(production_rate_year1_IP), standardize(production_decline_rate))
correlated_IP_decline <- U %*% t(Both)
correlated_IP_decline <- t(correlated_IP_decline)

correlated_IP_decline <- destandardize(correlated_IP_decline, cbind(production_rate_year1_IP, production_decline_rate) )

all_oil_volume <- c()

# Distribution of Net Present Value for simulated oil wells over 15 years
for (i in 1:simulation_size) {
  # Year 0 costs
  
  cost.drilling <- NA #NEED DRILLING COST
  
  
  ####################################
  # Lease Costs
  # Normal Distribution, 
  # Mean = 600 acres
  # Standard deviation = 50 acres
  # Price per acre = $960
  ####################################
  
  # Lease Costs
  set.seed(1234)
  cost.lease <- rnorm(1, mean = 600, sd = 50)*960
  
  
  ####################################
  # Seismic Sections Cost
  # Normal Distribution, 
  # Mean = 3 seismic sections per well
  # Standard deviation = 0.35 sections
  # Price per acre = $43000
  ####################################
  # Seismic Sections Per Well
  set.seed(1234)
  cost.seismic <- rnorm(1, mean = 3, sd = 0.35)*43000
  
  
  ####################################
  # Professional Overhead - Year 0
  # Triangle Distribution 
  # 172,000/215,000/279,500
  # Stops after Year 0 if dry well.
  # Same costs incurred for drilling. - CHECK THIS
  ####################################
  
  set.seed(1234)
  cost.yr_0_professional <- rtriangle(n = 1, a = 172000, b = 279500, c = 215000)
  
  
  ####################################################
  # Total Cost of a Dry Well
  ####################################################
  cost.total_dry_well <- cost.drilling + cost.lease + cost.seismic + cost.yr_0_professional
  
  
  
  
  
  # Costs and Revenue for Years 1-15
  
  ####################################
  # Completion Costs
  # Normal Distribution, 
  # Mean = $390,000
  # Standard deviation = $50,000
  # Assumes not a dry well
  ####################################
  
  # Completion Costs
  set.seed(1234)
  cost.completion <- rnorm(1, mean = 390000, sd = 50000)
  
  
  ####################################
  # Production Risk Continued
  # LogNormal Distribution 
  # Mean = 420 BOPD, std = 120 BOPD
  # Rate of decline is uniformly distributed between 15 and 32%
  # Correlation coefficient of 0.64 between IP and decline rate that are drawn from respective distributions during each trial of simulation
  # Reference Production Risk outside of main loop
  ####################################
  
  prod_rate_decline <- destandardize(correlated_IP_decline[i, 2])
  rate_yr_begin <- destandardize(correlated_IP_decline[i, 1])
  
  
  # Barrel production rate at the end of the year in BOPD
  rate_yr_end <-(1-prod_rate_decline)*rate_yr_begin
  
  # Yearly production volumes in barrels of oil
  # This is an average daily rate to estimate the total volume over the year
  oil_volume <- 365 * (rate_yr_begin+rate_yr_end)/2
  all_oil_volume <- c(all_oil_volume, oil_volume) # This is for testing only
  
  ####################################
  # Operating expenses
  # Build distributions for the next 15 years of the project 
  # Using Price Projections worksheet in Analysis_Data
  # Net revenue interest. NRI is the percentage of oil
  # revenue retained after paying royalties
  # Normal distribution
  # Mean = 75%
  # std dev = 2%
  # Done per well for the life of the well
  # Annual revenues = oil price * annual production
  ####################################
  
  set.seed(1234)
  revenue.interest = rnorm(simulation_size, mean = .75, sd = .02)
  

  
  #####################################################################################################
  # 15 years simulated for each well
  #####################################################################################################
  
  revenue.annual <- c()
  
  for (j in 1:15){
    
    
    ####################################
    # Professional Overhead - Year j
    # Triangle Distribution 
    # 172,000/215,000/279,500
    # Same costs incurred for drilling.
    ####################################
    
    set.seed(1234)
    cost.yr_j_professional <- rtriangle(n = 1, a = 172000, b = 279500, c = 215000) #CHECK: Does it mean costs will remain constant 
    # dist will remain constant?
    
    # NEED TO CLARIFY ASSUMPTIONS HERE
    
    
    ####################################
    # Revenue Risk
    # Build distributions for the next 15 years of the project 
    # Using Price Projections worksheet in Analysis_Data
    # Use Triangle Distribution with best case, worst case, typical
    # Net revenue interest. NRI is the percentage of oil
    # revenue retained after paying royalties
    # Normal distribution
    # Mean = 75%
    # std dev = 2%
    # Done per well for the life of the well
    # Annual revenues = oil price * annual production
    ####################################
    
    revenue.per_barrel <- rtriangle(1, a=analysis_data$low_oil_price[j], b=analysis_data$high_oil_price[j], c=analysis_data$aeo2018_ref[j])
    revenue.net_revenue_interest <- oil_volume * revenue.per_barrel * revenue.interest
    
    
    
    
    
    
    ####################################
    # Operating Expenses
    # Build distributions for the next 15 years of the project 
    # Using Price Projections worksheet in Analysis_Data
    # Use Triangle Distribution with best case, worst case, typical
    # Net revenue interest. NRI is the percentage of oil
    # revenue retained after paying royalties
    # Normal distribution
    # Mean = 75%
    # std dev = 2%
    # Done per well for the life of the well
    # Annual revenues = oil price * annual production
    ####################################
    
    
  }
  
  
  
} 



