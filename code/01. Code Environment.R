# Project: Can Dense Places Still Build? 
# File Description: code environment

# Author: Jess Remington
#Creation Date: 04/21/25
# last update: 04/21/25

##########################################################################################################

#Clear current environment
rm(list = ls())

# Load packages
library(here)
library(readxl)
library(openxlsx)
library(writexl)
library(dplyr)
library(foreign)
library(readr)
library(stringr)
library(ggplot2)
library(tidyr)
library(scales)

#Create function (decimals) to set all numeric values to 2 decimal places
decimals <- function(df, digits = 2) {
   df %>% mutate(across(where(is.numeric), ~ round(.x, digits)))
}
