###################################
# Oil and Gas Risk Simulation
# David Andexler
# v0.1
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

simulation_size <- 10000

setwd('C:/Users/dande/Desktop/MAIN/Documents/NCState Advanced Analytics/Spring 2020/Spring 1/Simulation and Risk/Homework/data')

df <- read_xlsx('Analysis_Data.xlsx', sheet = "Price Projections", )

analysis_data <- as.data.frame(df[3:nrow(df), ]) 

colnames(analysis_data) <-  c("year", "high_oil_price", "low_oil_price", "aeo2018_ref")

# Will eventually need simulated drilling costs from Phase 1

####################################
# Lease Costs
# Normal Distribution, 
# Mean = 600 acres
# Standard deviation = 50 acres
# Price per acre = $960
####################################
set.seed(1234)
# Lease Costs
Units <- rnorm(simulation_size, mean = 600, sd = 50)

Var.Cost <- 960*Units

hist(Var.Cost, main = 'Variable Leasing Costs', xlab = 'Leasing Costs')

####################################
# Seismic Sections Cost
# Normal Distribution, 
# Mean = 3 seismic sections per well
# Standard deviation = 0.35 sections
# Price per acre = $43000
####################################
# Seismic Sections Per Well
set.seed(1234)
Units <- rnorm(simulation_size, mean = 3, sd = 0.35)

Var.Cost <- 43000*Units

hist(Var.Cost, main = 'Seismic Sections Costs', xlab = 'Costs')

####################################
# Completion Costs
# Normal Distribution, 
# Mean = $390,000
# Standard deviation = $50,000
# Assumes not a dry well
####################################

# Completion Costs
set.seed(1234)
Units <- rnorm(simulation_size, mean = 390000, sd = 50000)

hist(Units, main = 'Completion Costs', xlab = 'Costs')

####################################
# Professional Overhead
# Triangle Distribution 
# 172,000/215,000/279,500
# Assumes costs remain constant
# Stops after Year 0 if dry well.
# Same costs incurred for drilling.
####################################

# Difficulties getting rtriangle to work
set.seed(1234)
Units <- rtriangle(n = simulation_size, a = 172000, b = 279500, c = 215000)

hist(Units, main = 'Professional Overhead Costs', xlab = 'Costs', breaks = 50)

####################################
# Production Risk
# LogNormal Distribution 
# Mean = 420 BOPD, std = 120 BOPD
# Rate of decline is uniformly distributed between 15 and 32%
# Correlation coefficient of 0.64 between IP and decline rate that are drawn from respective distributions during each trial of simulation
# How to get this?
####################################

rate_yr_begin <- 'INSERT RATE HERE'

# Production rates in BOPD for our model
rate_yr_end <-(1-decline_rate)*rate_yr_begin

# Yearly production volumes in barrels of oil
# Approximation

oil_volume <- 365 * (rate_yr_begin+rate_yr_end)/2
set.seed(1234)
IP <- rlnorm(simulation_size, meanlog = ln(420), sdlog = ln(120))

####################################
# Revenue Risk
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
Units = rnorm(simulation_size, mean = .75, sd = .02)

# NEEDS COMPLETED





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


