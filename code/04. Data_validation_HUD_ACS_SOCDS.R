##########################################################################################################
# Project: COMPARE AND VALIDATE DATA - HUD USPS, RESIDENTIAL CONSTRUCTION, AND ACS HOUSING UNITS (B25001) 
# File Description: 

# Author: Jess Remington
#Creation Date: 04/15/25
# last update: 

##########################################################################################################
#Clear current environment
rm(list = ls())

# Set base project path
base_path <- "ENTER YOUR PROJECT PATH HERE"
inputs <- file.path(base_path, "inputs")
outputs <- file.path(base_path, "outputs")

#import source code
source(file.path(base_path, "code/01.\ Code\ Environment.R"))

# Load packages
library(readr)
library(readxl)
library(dplyr)

##########################################################################################################
# IMPORTS -----------------------------------

# HUD USPS DATA (uncleaned)
hud <- read_csv(file.path(outputs, "hud_usps_15to23.csv"))

# ACS 5-YR
acs5 <- read_csv(file.path(outputs, "ACS5_15to23.csv"))

# HUD USPS DATA (EIG - cleaned)
hud <- read_csv(file.path(outputs, "hud14to24.csv"))


##########################################################################################################
#Residential Construction Permits data (SOCDC) ------------------------

#IMPORT
construction_import <- readxl::read_excel(file.path(inputs,"Residential_Construction_Permits_by_County.xlsx"))

#CLEAN
construction_db <- construction_import %>%
   mutate(
      ALL_PERMITS_TOTAL = rowSums(select(., ALL_PERMITS_2014, ALL_PERMITS_2015, ALL_PERMITS_2016, ALL_PERMITS_2017,
                                         ALL_PERMITS_2018, ALL_PERMITS_2019, ALL_PERMITS_2020,
                                         ALL_PERMITS_2021, ALL_PERMITS_2022), na.rm = TRUE),
      
      SF_PERMITS_TOTAL = rowSums(select(., SINGLE_FAMILY_PERMITS_2014, SINGLE_FAMILY_PERMITS_2015, SINGLE_FAMILY_PERMITS_2016,
                                        SINGLE_FAMILY_PERMITS_2017, SINGLE_FAMILY_PERMITS_2018,
                                        SINGLE_FAMILY_PERMITS_2019, SINGLE_FAMILY_PERMITS_2020,
                                        SINGLE_FAMILY_PERMITS_2021, SINGLE_FAMILY_PERMITS_2022), na.rm = TRUE),
      
      MF_PERMITS_TOTAL = rowSums(select(., ALL_MULTIFAMILY_PERMITS_2015, ALL_MULTIFAMILY_PERMITS_2016,
                                        ALL_MULTIFAMILY_PERMITS_2017, ALL_MULTIFAMILY_PERMITS_2018,
                                        ALL_MULTIFAMILY_PERMITS_2019, ALL_MULTIFAMILY_PERMITS_2020,
                                        ALL_MULTIFAMILY_PERMITS_2021, ALL_MULTIFAMILY_PERMITS_2022), na.rm = TRUE)
   )

construction <- construction_db %>%
   select(GEOID, NAME, STATE_NAME, ALL_PERMITS_TOTAL, SF_PERMITS_TOTAL, MF_PERMITS_TOTAL)

#Export to csv
write.csv(construction, "~/Documents/Can Dense Areas Still Build/Residential_construction_15to23.csv", row.names = FALSE)


##########################################################################################################

#MERGE ALL DATASETS

compare1 <- construction %>%
   left_join(hud, by = "GEOID")

compare2 <- compare1 %>%
   left_join(acs5, by = "GEOID")

#ANALYZE

# differences

#Calc diff between HUD and ACS 5-yr housing unit growth from 2015 to 2023
compare3 <- compare2 %>%
   mutate(HUD_ACS_Diff = HUD_HU_chg_15to23 - ACS_HU_chg_15to23)

   #Median diff between HUD and ACS housing unit growth from 2015 to 2023 (should not exceed 1 percentage point)
   median(compare3$HUD_ACS_Diff, na.rm = TRUE)
   #result: 7.07935 percentage points

#Calc diff between HUD and ACS total units - 2015
   compare4 <- compare3 %>%
   mutate(HUD_ACS_Diff_15 = HUD_housingunits_15 - ACS_housingunits_15)
   
   #Median difference between HUD and ACS total housing units in 2015
   median(compare4$HUD_ACS_Diff_15, na.rm = TRUE)
   #result: 428 more HUD units recorded in 2015 - this is to be expected since it is using granular 1-yr or quarterly data rather than 5-yr average. But what if we compare to the ACS 1-year?

#Calculate the difference between HUD and ACS total units - 2023
   compare5 <- compare4 %>%
   mutate(HUD_ACS_Diff_23 = HUD_housingunits_23 - ACS_housingunits_23)

   #Median difference between HUD and ACS total housing units in 2023
   median(compare5$HUD_ACS_Diff_23, na.rm = TRUE)
   #result: 1217 more HUD units recorded in 2023

#HUD housing unit growth (2015 to 2023)
   compare6 <- compare5 %>%
   mutate(HUD_Diff_15to23 = HUD_housingunits_23 - HUD_housingunits_15)

#ACS housing unit growth (2015 to 2023)
   compare7 <- compare6 %>%
   mutate(ACS_Diff_15to23 = ACS_housingunits_23 - ACS_housingunits_15)

#Compare HUD to New Construction
   compare8 <- compare7 %>%
   mutate(HUD_construction_Diff = HUD_Diff_15to23 - ALL_PERMITS_TOTAL)
   
   #Median difference between HUD and New Construction
   median(compare8$HUD_construction_Diff, na.rm = TRUE)
   #result: 446

#Compare ACS to New Construction
   compare9 <- compare8 %>%
   mutate(ACS_construction_Diff = ACS_Diff_15to23 - ALL_PERMITS_TOTAL)

   #Median difference between ACS and New Construction
   median(compare9$ACS_construction_Diff, na.rm = TRUE)
   #result: -326 


# DO IT AGAIN - COMPARE TO THE ACS 1-YEAR -------------------------------------

   #ACS HOUSING UNITS DATA 1yr (B25001)
   
   #2015
   
   #IMPORT
   acs1_import <- readxl::read_excel("~/Documents/Can Dense Areas Still Build/ACS_Housingunits_1yr_2015.xlsx")

   #CLEAN
   acs1_15 <- acs1_import %>%
      rename (
         GEOID = GEO_ID,
         ACS_housingunits_15 = Estimate) %>%
      mutate(GEOID = substr(GEOID, 10, 14))
   
   #2023
   
   #IMPORT
   acs1_import <- readxl::read_excel("~/Documents/Can Dense Areas Still Build/ACS_Housingunits_1yr_2023.xlsx")
   
   #CLEAN
   acs1_23 <- acs1_import %>%
      rename (
         GEOID = GEO_ID,
         ACS_housingunits_23 = Estimate) %>%
      mutate(GEOID = substr(GEOID, 10, 14)) %>%
      select(GEOID, ACS_housingunits_23)
   
   #MERGE 2015 and 2023
   #Merge
   acs1_15to23 <- acs1_15 %>%
      left_join(acs1_23, by = "GEOID")
   
   #Calc housing units percent change
   acs1_15to23 <- acs1_15to23 %>%
      mutate(ACS_HU_chg_15to23 = 100 * (ACS_housingunits_23 - ACS_housingunits_15) / ACS_housingunits_15)
   
   #Calc HUD housing unit numerical growth
   acs1_15to23 <- acs1_15to23 %>%
      mutate(ACS_Diff_15to23 = ACS_housingunits_23 - ACS_housingunits_15)
   
   #Filter out missing values due to changed FIPS codes
   acs1_15to23 <- acs1_15to23 %>%
      filter(!is.na(ACS_HU_chg_15to23))
   
   #Export to csv
   write.csv(acs1_15to23, "~/Documents/Can Dense Areas Still Build/ACS1_15to23.csv", row.names = FALSE)
   
 ##########################################################################################################
 
# MERGE ALL DATASETS ---------------------------

#merge ACS 1-yr with Construction and HUD USPS
compare1a <- acs1_15to23 %>%
   left_join(hud, by = "GEOID")
   
compare2a <- compare1a %>%
   left_join(
      construction %>% select (GEOID, ALL_PERMITS_TOTAL, SF_PERMITS_TOTAL, MF_PERMITS_TOTAL),
      by = "GEOID")


#ANALYZE

# differences

#Calc diff between HUD and ACS housing unit growth from 2015 to 2023
compare3a <- compare2a %>%
   mutate(HUD_ACS_Diff = HUD_HU_chg_15to23 - ACS_HU_chg_15to23)

   #Median diff between HUD and ACS housing unit growth from 2015 to 2023 (should not exceed 1 percentage point)
   median(compare3a$HUD_ACS_Diff, na.rm = TRUE)
   #result: 1.056242 percentage points - A BIG IMPROVEMENT

#Calc diff between HUD and ACS total units - 2015
compare4a <- compare3a %>%
   mutate(HUD_ACS_Diff_15 = HUD_housingunits_15 - ACS_housingunits_15)

   #Median difference between HUD and ACS total housing units in 2015
   median(compare4a$HUD_ACS_Diff_15, na.rm = TRUE)
   #result: 7071 more HUD units recorded in 2015, median per county - huge difference, way bigger than the 5-yr

#Calculate the difference between HUD and ACS total units - 2023
compare5a <- compare4a %>%
   mutate(HUD_ACS_Diff_23 = HUD_housingunits_23 - ACS_housingunits_23)

   #Median difference between HUD and ACS total housing units in 2023
   median(compare5a$HUD_ACS_Diff_23, na.rm = TRUE)
   #result: 8861 more HUD units recorded in 2023, median per county

#HUD housing unit growth (2015 to 2023)
compare6a <- compare5a %>%
   mutate(HUD_Diff_15to23 = HUD_housingunits_23 - HUD_housingunits_15)

#ACS housing unit growth (2015 to 2023)
compare7a <- compare6a %>%
   mutate(ACS_Diff_15to23 = ACS_housingunits_23 - ACS_housingunits_15)

#Compare HUD to New Construction
compare8a <- compare7a %>%
   mutate(HUD_construction_Diff = HUD_Diff_15to23 - ALL_PERMITS_TOTAL)

   #Median difference between HUD and New Construction
   median(compare8a$HUD_construction_Diff, na.rm = TRUE)
   #result: 1373

#Compare ACS to New Construction
compare9a <- compare8a %>%
   mutate(ACS_construction_Diff = ACS_Diff_15to23 - ALL_PERMITS_TOTAL)

   #Median difference between ACS and New Construction
   median(compare9a$ACS_construction_Diff, na.rm = TRUE)
   #result: -197. ACS more closely matches new construction data than HUD USPS 

