# Term paper ECON4310 - autumn 2023
#w Wealth in the utility function
#Load packages
library(WDI)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(countrycode)
library(psych)
library(Hmisc)
library(car)
library(vtable)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
#Data from WDI (World Development Indicators )

data <- WDI(indicator = c("NY.ADJ.NNTY.PC.CD", "FR.INR.RINR", "NE.CON.PRVT.ZS", "NY.ADJ.SVNG.GN.ZS", "SE.XPD.TOTL.GD.ZS", "SL.UEM.TOTL.ZS", "SP.POP.1564.TO.ZS"),
            country = "all",
            start = 1980, 
            end = 2020,
            language = "en")

data <- na.omit(data)



#Descriptive statistics

df <- subset(data, select = c("NY.ADJ.NNTY.PC.CD", "FR.INR.RINR", "NE.CON.PRVT.ZS", 
                              "NY.ADJ.SVNG.GN.ZS", "SE.XPD.TOTL.GD.ZS", "SL.UEM.TOTL.ZS", 
                              "SP.POP.1564.TO.ZS"))


#Renaming variables in table
labs <- c("Adjusted net national income per capita (current US$)", 
          "Real interest rate (%)", "Final consumption expenditure, etc. (% of GDP)", 
          "Adjusted net savings, excluding particulate emission damage (% of GNI)", 
          "Government expenditure on education, total (% of GDP)", 
          "Unemployment, total (% of total labor force) (modeled ILO estimate)", 
          "Population ages 15-64" )

# The table 
st(df, labels=labs, title = "Descriptive statistics:", 
   summ = list( c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'), c('notNA(x)','mean(x)') ), 
   summ.names = list( c('N','Mean','SD','Min','Max'), c('','') ))



# Try Norway
data_norway <- WDI(indicator = c("NY.ADJ.NNTY.PC.CD", "FR.INR.RINR", "NE.CON.PRVT.ZS", "NY.ADJ.SVNG.GN.ZS", "SE.XPD.TOTL.GD.ZS", "SL.UEM.TOTL.ZS", "SP.POP.1564.TO.ZS"),
            country = "NO",
            start = 1980, 
            end = 2020,
            language = "en")

data_norway <- na.omit(data_norway)

data_norway <- subset(data_norway, select = c("NY.ADJ.NNTY.PC.CD", "FR.INR.RINR", "NE.CON.PRVT.ZS", 
                              "NY.ADJ.SVNG.GN.ZS", "SE.XPD.TOTL.GD.ZS", "SL.UEM.TOTL.ZS", 
                              "SP.POP.1564.TO.ZS"))


st(data_norway, labels = labs, title = "Descriptive statistics: Norway", 
   summ = list(c('notNA(x)', 'mean(x)', 'sd(x)', 'min(x)', 'max(x)'), c('notNA(x)', 'mean(x)')),
   summ.names = list(c('N', 'Mean', 'SD', 'Min', 'Max'), c('', '')))


# Regression analysis

model <- lm(NE.CON.PRVT.ZS ~ NY.ADJ.NNTY.PC.CD + FR.INR.RINR + 
              NY.ADJ.SVNG.GN.ZS + SE.XPD.TOTL.GD.ZS + SL.UEM.TOTL.ZS + 
              SP.POP.1564.TO.ZS, data = data)
model

#Endre varibelnavn 
pl <- c(
  `(Intercept)` = "Intercept",
  "NE.CON.PRVT.ZS" = "Final consumption expenditure, etc. (% of GDP)",
  "NY.ADJ.NNTY.PC.CD" = "Adjusted net national income per capita (current US$)", 
  "FR.INR.RINR" = "Real interest rate (%)", 
  "NY.ADJ.SVNG.GN.ZS" = "Adjusted net savings, excluding particulate emission damage (% of GNI)",
  "SE.XPD.TOTL.GD.ZS" = "Government expenditure on education, total (% of GDP)",
  "SL.UEM.TOTL.ZS" = "Unemployment, total (% of total labor force)",
  "SP.POP.1564.TO.ZS" = "Population ages 15-64"
  
)


regression <- tab_model(model, 
                        p.style = "stars",
                        title = "Regression analysis",
                        dv.labels ="Final consumption expenditure, etc. (% of GDP)" , 
                        pred.labels = pl
)
regression

#Multi collernarity
vif(model)

vif_values <- vif(model)

# Create a data frame with variable names and VIF values
vif_table <- data.frame(vif_values)

library(kableExtra)
kbl(vif_table,
    caption = "VIF values") %>%
  kable_classic(full_width = F,
                html_font = "Cambria")


#testing with parameters
alpha <- 0.5 # You can adjust this value 
beta <- 0.5 #You can adjust this value 
r <- 0.08  # Example interest rate, adjust as needed 
y <- 8778.046 # Example income, adjust as needed 
b0 <- 7.75 # Initial wealth (meanvalue from the descreptiv table) 
beta_val <- 0.92592593 # Discount factor, adjust as needed

#from regression analysis
consumption_function <- function(adjusted_net_income, real_interest_rate, adjusted_net_savings, govt_expenditure, unemployment, population_15_64) { 
  intercept <- 107.32 
  coef_adjusted_net_income <- -0.00
  coef_real_interest_rate <- 0.07 
  coef_adjusted_net_savings <- -0.44 
  coef_govt_expenditure <- -0.68 
  coef_unemployment <- 0.40 
  coef_population_15_64 <- -0.60


c <- intercept + coef_adjusted_net_income * adjusted_net_income + 
  coef_real_interest_rate * real_interest_rate + 
  coef_adjusted_net_savings * adjusted_net_savings + 
  coef_govt_expenditure * govt_expenditure + 
  coef_unemployment * unemployment + 
  coef_population_15_64 * population_15_64 
return(c) 
}



utility_function <- function(params, predictors) { 
  c1 <- consumption_function(predictors$adjusted_net_income, 
                             predictors$real_interest_rate, 
                             predictors$adjusted_net_savings, 
                             predictors$govt_expenditure, 
                             predictors$unemployment, 
                             predictors$population_15_64) 
  b1 <- params[1] 
  c2 <- consumption_function(predictors$adjusted_net_income, 
                                             predictors$real_interest_rate, 
                                             predictors$adjusted_net_savings, 
                                             predictors$govt_expenditure, 
                                             predictors$unemployment, 
                                             predictors$population_15_64) 
  b2 <- params[2]

u <- c1^alpha + b1^beta + beta_val * (c2^alpha + b2^beta)

return(-u) # We return negative because optim() minimizes by default 
}

initial_guess <- c(500, 500) # Initial guesses for b1, b2

# Example predictor values, adjust as needed

predictors <- list(adjusted_net_income = 10000, 
                   real_interest_rate = 0.05, 
                   adjusted_net_savings = 0.1, 
                   govt_expenditure = 0.2, 
                   unemployment = 0.05, 
                   population_15_64 = 0.6 )

result <- optim(par = initial_guess, 
                fn = utility_function, 
                predictors = predictors, 
                method = "L-BFGS-B",
                lower = c(0, 0), # Lower bounds for b1, b2
                upper = c(Inf, Inf) # Upper bounds for b1, b2
)


# Extract the optimal values of b1 and b2
optimal_b1 <- result$par[1]
optimal_b2 <- result$par[2]

# Print the optimal values
result_b1 <- cat("Optimal b1:", optimal_b1, "\n")
result_b2 <- cat("Optimal b2:", optimal_b2, "\n")

# Alternativ model where wealth doesn´t contribute to utiltiy function

u_alt <- function(c, alpha) {
  return(c^alpha)
}


utility_function_alt <- function(params, predictors) { 
  c1 <- consumption_function(predictors$adjusted_net_income, 
                             predictors$real_interest_rate, 
                             predictors$adjusted_net_savings, 
                             predictors$govt_expenditure, 
                             predictors$unemployment, 
                             predictors$population_15_64) 
  c2 <- consumption_function(predictors$adjusted_net_income, 
                             predictors$real_interest_rate, 
                             predictors$adjusted_net_savings, 
                             predictors$govt_expenditure, 
                             predictors$unemployment, 
                             predictors$population_15_64) 
  b1 <- params[1] 
  b2 <- params[2]
  
  u_original <- c1^alpha + b1^beta + beta_val * (c2^alpha + b2^beta)
  u_alternative <- c1^alpha + beta_val * c2^alpha
  
  # Return the difference between the original and alternative utilities
  return(- (u_original - u_alternative))
}


# Run optimization for the original model
result_original <- optim(par = initial_guess, 
                         fn = utility_function, 
                         predictors = predictors, 
                         method = "L-BFGS-B",
                         lower = c(0, 0), 
                         upper = c(Inf, Inf))

# Extract the optimal values of b1 and b2 for the original model
optimal_b1_original <- result_original$par[1]
optimal_b2_original <- result_original$par[2]

# Run optimization for the alternative model
result_alternative <- optim(par = initial_guess, 
                            fn = utility_function_alt, 
                            predictors = predictors, 
                            method = "L-BFGS-B",
                            lower = c(0, 0), 
                            upper = c(Inf, Inf))


# Extract the optimal values of b1 and b2 for the alternative model
optimal_b1_alternative <- result_alternative$par[1]
optimal_b2_alternative <- result_alternative$par[2]

# Create a data frame with column names
df_result <- data.frame(
  Original_b1 = optimal_b1_original,
  Original_b2 = optimal_b2_original,
  Alternative_b1 = optimal_b1_alternative,
  Alternative_b2 = optimal_b2_alternative)

df_result <- gather(df_result, Model, Value) 
df_result


kbl(df_result,
    caption = "<b>b1 and b2 values</b>",
    escape = FALSE) %>%
  kable_classic(full_width = F,
                html_font = "Cambria")

