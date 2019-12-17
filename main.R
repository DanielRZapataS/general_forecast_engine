## Starting 

# Clean environment
rm(list=ls())
gc()
# Disable scientific notation
options(scipen=999)
# Change prompt
options(prompt="Demanda Transaccional> ", continue=" ") 
cat("\014")
source("scripts/utiles.R")
set_environment()
# ejecutar el modelo 
demanda_transaccional()