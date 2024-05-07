#*******************************************************************************************************************
#
# 0. Identification ---------------------------------------------------
# Title: Data analysis EDUMER Survey Wave 1
# Author: Andreas Laffert            
# Overview: EDUMER Survey Wave 1 Average Treatment Effect          
# Date: 02-05-2024            
#
#******************************************************************************************************************

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjmisc, 
               sjPlot,
               psych,
               estimatr,
               Matching,
               ebal,
               cobalt,
               WeightIt,
               rstatix,
               summarytools,
               here,
               vtable,
               texreg)

options(scipen=999)
rm(list = ls())

# 2. Data --------------------------------------------------------------

load(file = here("output", "db_proc.RData"))
glimpse(db)

# 3. Analysis --------------------------------------------------------------

# Descriptive

db %>%
  mutate(aleatorio = factor(aleatorio, levels = 0:1, labels = c("Controles", "Tratados")),
         check_comprension = factor(check_comprension, levels = 0:1, labels = c("No", "Si"))) %>% 
  vtable::st()

db <- db %>% 
  select(-id) %>% 
  mutate(aleatorio = if_else(aleatorio == 1, 1, 0),
         across(
           .cols = c(12:30),
           .fns = ~ as.numeric(.)
         ))

summary(db)

frq(db$check_comprension)

# Balance

summary(
  lm(aleatorio ~ sexo + edad + etni + religion + 
     nivel_curso + dep_escuela + check_comprension,
     data = db)
)

# religion creyente no adherente it´s significant: use as control

df_bal <- db %>% select(aleatorio, religion)

bal1 <- MatchBalance(aleatorio ~.,
                     data = df_bal,
                     match.out = NULL, 
                     ks =TRUE)

bal1.label <- c("Evangelico", "Protestante", "Judio", "Creyente no adherente", "Ateo", "Agnostico", "Ninguna")

# Funcion de ebal
bal1.m1  <- baltest.collect(matchbal.out= bal1, var.names = bal1.label, after=FALSE)
round(bal1.m1,2)  # Desbalance con relig creyente no adeherente (16.3) --> controlar por relig en modelos

# ATE

m1 <- lm_robust(percep_desigualdad ~ aleatorio,
                data = db, se_type = "HC2", alpha = 0.05)

m2 <- lm_robust(percep_desigualdad ~ aleatorio + religion, 
          data = db, se_type = "HC2", alpha = 0.05)
  
screenreg(l = list(m1, m2), stars = c(0.05,0.01,0.001), digits = 2)

# eficiencia sin controles y luego con controles

db %>% 
  lm_robust(formula = dep_var ~ aleatorio + religion, 
            se_type = "HC2", alpha = 0.05) %>% 
  screenreg(l = ., stars = c(0.05,0.01,0.001), digits = 2)


dep_vars <- colnames(db[12:30])  # Replace with your actual dependent variables

results_list <- list()

for(dep_var in dep_vars) {
  formula <- as.formula(paste(dep_var, "~ aleatorio + religion"))
  result <- lm_robust(formula = formula, data = db, se_type = "HC2", alpha = 0.05)
  results_list[[dep_var]] <- result
}

# Function to check if aleatorio coefficient is significant in a single regression result
is_aleatorio_significant <- function(result) {
  summary_result <- summary(result)
  p_value <- summary_result$coefficients["aleatorio", "Pr(>|t|)"]
  return(p_value < 0.05)  # Adjust the significance threshold if needed
}

# Check if aleatorio coefficient is significant in any of the regression results
significant_results <- sapply(results_list, is_aleatorio_significant)

# Check if any regression result had a significant aleatorio coefficient
is_aleatorio_significant_overall <- any(significant_results)

# Output
is_aleatorio_significant_overall  # TRUE if significant in at least one regression, FALSE otherwise
