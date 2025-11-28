################################################################################
##          ECONOMETRICS 4b: EMPIRICAL MACROECONOMICS - HT2025                ##
################################################################################
################################################################################
##                          Code for Assignment 3                             ##
################################################################################

####################### Setting the work environment ###########################

# Clear the environment
rm(list = ls())

# Loading the necessary packages
required_packages <- c("readxl", "urca", "vars", "bvarsv" )

#Installs the packages that have not been installed
installed <- required_packages %in% installed.packages()
if (any(!installed)) {
  install.packages(required_packages[!installed])
}
invisible(lapply(required_packages, library, character.only = TRUE))

##################### Loading and preparing the data ###########################
# Load the data
data_raw <- read.csv(file = "Data3.csv", sep = ";") # ; is used to separate columns

col_data <- 2:4
ts_names <- c("OP", "dY", "dP")

ts_list <- lapply(col_data,
                  function(i) ts(as.numeric(data_raw[,i]), 
                                 start = c(1990,1), frequency = 4))

names(ts_list) <- ts_names

list2env(ts_list, envir = .GlobalEnv)
data <- cbind(dP, dY, OP)


############################### Run TVP-VAR ####################################

# Set random seed to get identical results each time (including when smaller
# simulations are run)
set.seed(100) # This starts the random number generator at 100 so that you
# always get the same results 

# Code is from Krueger (2015)
# https://cran.r-project.org/web/packages/bvarsv/index.html

fit <- bvar.sv.tvp(data,
                   p = 2,    # Number of lags in the VAR
                   tau = 20, # TVP-BVAR estimated with 5-year training sample
                   k_Q = 0.03, # This is the starting value for the variances of
			       # shocks to the parameters
                   k_W = 0.1,
                   k_S = 0.1, # scaling coefficients
                   nrep = 3000, # 3000 = value for testing the code. Then set to 30000 
                   nburn = 1000, # 1000 = value for testing the code. Then set to 10000 
                   thinfac = 1)  # This can be used to break potential autocorrelation.
				 # Setting it to 10 picks every 10th draw

##################### Impulse response functions ###############################

# Producing equivalents of Figure 2a and Figure 3a in Primiceri (2005):

# Dates in string format for use below
tml <- paste0(floor(time(data)), "Q", (1+4*(time(data)-floor(time(data)))))

# Dates for which we want IRF: 1998 Q1
# Create an index, subtract the lost observations (2 lags + 5 years training)
t <- which(tml == "1998Q1") -22

# Computing the impusle response function
png(filename = "irf_1998Q1.png", width = 600, height = 400, units = "px")
irf_1998 <- impulse.responses(fit,
                              impulse.variable = 3, # oil price
                              response.variable = 1, # CPI
                              nhor = 18,
                              t = t,
                              scenario = 3)$irf
dev.off()
# The png() command creates a png file of the printed graph. Use dev.off() afterwards.

# Dates for which we want IRF: 2010 Q1
# Create an index, subtract the lost observations (2 lags + 5 years training)
t <- which(tml == "2010Q1") -22

# Compute the impusle response function
png(filename = "irf_2010Q1.png", width = 600, height = 400, units = "px")
irf_2010 <- impulse.responses(fit,
                              impulse.variable = 3, # oil price
                              response.variable = 1, # CPI
                              nhor = 18,
                              t = t,
                              scenario = 3)$irf
dev.off()

# Dates for which we want IRF: 2022 Q2
# Create an index, subtract the lost observations (2 lags + 5 years training)
t <- which(tml == "2022Q2") - 22

# Compute the impusle response function
png(filename = "irf_2022Q1.png", width = 600, height = 400, units = "px")
irf_2022 <- impulse.responses(fit,
                              impulse.variable = 3, # oil price
                              response.variable = 1, # CPI
                              nhor = 18,
                              t = t,
                              scenario = 3)$irf
dev.off()

############################### Run TVP-VAR [2] ####################################

fit <- bvar.sv.tvp(data,
                   p = 1,    # (i) number of lags decreased to 1
                   tau = 24, # (ii) 6-year training sample = 24 quarters
                   k_Q = 0.1, #(iii) increased variance of parameter evolution
                   k_W = 0.1,
                   k_S = 0.1, 
                   nrep = 3000, 
                   nburn = 1000,  
                   thinfac = 1)  

##################### Impulse response functions ###############################

# Producing equivalents of Figure 2a and Figure 3a in Primiceri (2005):

# Dates in string format for use below
tml <- paste0(floor(time(data)), "Q", (1+4*(time(data)-floor(time(data)))))

# Dates for which we want IRF: 1998 Q1
# Create an index, subtract the lost observations (2 lags + 5 years training)
t <- which(tml == "1998Q1") -22

# Computing the impusle response function
png(filename = "irf_1998Q1(2).png", width = 600, height = 400, units = "px")
irf_1998 <- impulse.responses(fit,
                              impulse.variable = 3, # oil price
                              response.variable = 1, # CPI
                              nhor = 18,
                              t = t,
                              scenario = 3)$irf
dev.off()
# The png() command creates a png file of the printed graph. Use dev.off() afterwards.

# Dates for which we want IRF: 2010 Q1
# Create an index, subtract the lost observations (2 lags + 5 years training)
t <- which(tml == "2010Q1") -22

# Compute the impusle response function
png(filename = "irf_2010Q1(2).png", width = 600, height = 400, units = "px")
irf_2010 <- impulse.responses(fit,
                              impulse.variable = 3, # oil price
                              response.variable = 1, # CPI
                              nhor = 18,
                              t = t,
                              scenario = 3)$irf
dev.off()

# Dates for which we want IRF: 2022 Q2
# Create an index, subtract the lost observations (2 lags + 5 years training)
t <- which(tml == "2022Q2") - 22

# Compute the impusle response function
png(filename = "irf_2022Q1(2).png", width = 600, height = 400, units = "px")
irf_2022 <- impulse.responses(fit,
                              impulse.variable = 3, # oil price
                              response.variable = 1, # CPI
                              nhor = 18,
                              t = t,
                              scenario = 3)$irf
dev.off()


######### Posterior means of the standard deviation of the residuals ###########

# Producing equivalents of Figure 1 a-c in Primiceri (2005)

# The time varying variances of variables 1, 2, 3 are organized into a vector 
# like this: h1(obs1) h2(obs1) h3(obs1) h1(obs2) h2(obs2) h3(obs2) h1(obs3) 
# h2(obs3) h3(obs3) etc. (2nd dim of H.draws matrix in the output of fit)
#
# Now, to get h1(obs1) h1(obs2) h1(obs3) we need to extract every third number 
# starting at 1. This is done using the following sequence, where 107 is the 
# number of obs once we have removed the training sample and lags:

aux <- seq(1, 3*114, 3)

# Standard deviation of the CPI residuals
# Get posterior draws: a 107x3000 matrix
sd_cpi <- sqrt(fit$H.draws[1, aux, ])

# Now get the credibility intervals by taking the 16th and 84th percentile: for 
# each time period, this computes the mean and quantiles of the SD over 3000 draws
sd_cpi <- t(apply(sd_cpi, 1, function(z) c(mean(z), 
                                   quantile(z, c(0.16, 0.84)))[c(2, 1, 3)]))

# Compute time series
mean_sd_cpi <- ts(sd_cpi[,2], start = c(1995,3), frequency = 4)
sd_u_cpi <- ts(sd_cpi[,1], start = c(1995,3), frequency = 4)
sd_l_cpi <- ts(sd_cpi[,3], start = c(1995,3), frequency = 4)

# Construct a dataset and plot
h1 <- ts.intersect(mean_sd_cpi, sd_u_cpi, sd_l_cpi)  

png(filename = "ts_cpi.png", width = 600, height = 400, units = "px")
ts.plot(h1, col = c("blue","red","red"), lty = c(1,2,2))
dev.off()
# Standard deviation of GDP residuals
# Get posterior draws
sd_gdp <- sqrt(fit$H.draws[2, aux+1, ])
sd_gdp <- t(apply(sd_gdp, 1, function(z) c(mean(z), 
                                           quantile(z, c(0.16, 0.84)))[c(2, 1, 3)]))

mean_sd_gdp <- ts(sd_gdp[,2], start = c(1995,3), frequency = 4)
sd_u_gdp <- ts(sd_gdp[,1], start = c(1995,3), frequency = 4)
sd_l_gdp <- ts(sd_gdp[,3], start = c(1995,3), frequency = 4)


h2 <- ts.intersect(mean_sd_gdp, sd_u_gdp, sd_l_gdp)  
png(filename = "ts_gdp.png", width = 600, height = 400, units = "px")
ts.plot(h2, col = c("blue","red","red"), lty = c(1,2,2))
dev.off()
# Standard deviation of oil price residuals
# Get posterior draws
sd_oilp <- sqrt(fit$H.draws[3, aux+2, ])
sd_oilp <- t(apply(sd_oilp, 1, function(z) c(mean(z), 
                                           quantile(z, c(0.16, 0.84)))[c(2, 1, 3)]))

mean_sd_oilp <- ts(sd_oilp[,2], start = c(1995,3), frequency = 4)
sd_u_oilp <- ts(sd_oilp[,1], start = c(1995,3), frequency = 4)
sd_l_oilp <- ts(sd_oilp[,3], start = c(1995,3), frequency = 4)

h3 <- ts.intersect(mean_sd_oilp, sd_u_oilp, sd_l_oilp)  
png(filename = "ts_oil.png", width = 600, height = 400, units = "px")
ts.plot(h3, col = c("blue","red","red"), lty = c(1,2,2))
dev.off()

