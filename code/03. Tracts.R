# Project: Can Dense Places Still Build? 
# File Description: Census tracts

# Author: Jess Remington
#Creation Date: 04/21/25
# last update: 05/13/25

##########################################################################################################

#Clear current environment
rm(list = ls())

# Set base project path
base_path <- "ENTER YOUR PROJECT PATH HERE"
inputs <- file.path(base_path, "inputs")
outputs <- file.path(base_path, "outputs")
graphs <- file.path(base_path, "graphs")

#import source code
source(file.path(base_path, "code/01.\ Code\ Environment.R"))

# POPULATION & CROSSWALK --------------------------------------------------------
#import
pop_tract_14_import <- read.csv(file.path(inputs, "ACSDT5Y2014.B01003-Data.csv"))
pop_tract_23_import <- read.csv(file.path(inputs, "ACSDT5Y2023.B01003-Data.csv"))

#clean
pop_tract_14 <- pop_tract_14_import[-1, ] %>%
   rename(pop14 = B01003_001E) %>%
   mutate(GEOID = str_sub(GEO_ID, -11))%>%
   select(-1, -4, -5)%>%
   mutate(pop14 = as.numeric(pop14))%>%
   select(GEOID, everything())

pop_tract_23 <- pop_tract_23_import[-1, ] %>%
   rename(pop23 = B01003_001E) %>%
   mutate(GEOID = str_sub(GEO_ID, -11))%>%
   select(-1, -4, -5)%>%
   mutate(pop23 = as.numeric(pop23))%>%
   select(GEOID, everything())

#import crosswalk
# Note: this crosswalk only links 2010 tracts and 2020 tracts. However, the tracts for 2023 population
# are based on 2022 coding, which includes the changes in Connecticut \(CT census tracts' codes changed
# due to redrawing of county boundaries, but their shapes remain the same\). As a result, our population
# change data from 2014 to 2023 excludes Connecticut entirely.
### NOTE: This does not need to be corrected, for we are not using tract-level CT data in our analysis. ###
crosswalk <- read_excel(file.path(inputs, "CENSUS_TRACT_CROSSWALK_2010_to_2020.xlsx"))

#join pop_tract_14 with crosswalk using 2010 GEOIDs
pop14_weighted <- crosswalk %>%
   left_join(pop_tract_14, by = c("GEOID_2010" = "GEOID")) %>%
   mutate(pop14_weighted = round(pop14 * RES_RATIO))

#aggregate weighted pop14 to 2020 tracts
pop14_GEOID2020 <- pop14_weighted %>%
   group_by(GEOID_2020) %>%
   summarize(pop14 = sum(pop14_weighted, na.rm = TRUE), .groups = "drop")%>%
   rename(GEOID=GEOID_2020)

#merge
pop_tract_14to23 <- pop_tract_23 %>%
   left_join(pop14_GEOID2020, by = "GEOID")%>%
   select(1:2, pop14, pop23)%>%
   mutate(pop_change_pct = (pop23 - pop14) / pop14 * 100)

#qc
sum(pop_tract_14to23$pop14, na.rm = TRUE)
sum(pop_tract_14$pop14, na.rm = TRUE)
sum(pop_tract_23$pop23, na.rm = TRUE)


# DENSITY ------------------------------------------------------------
#import
sqmi_tract <- read_excel(file.path(inputs, "LandArea_Censustract.xlsx"))
sqmi_tract <- sqmi_tract %>%
   select(GEOID, ALAND_SQMI)

#merge
density_sqmi_tract <- pop_tract_14to23 %>%
   left_join(sqmi_tract %>% select(GEOID, ALAND_SQMI))

#analyze (calculate density)
densitydb_tract <- density_sqmi_tract %>%
   mutate(
      pop14 = as.numeric(gsub(",", "", pop14)),
      pop23 = as.numeric(gsub(",", "", pop23)),
      ALAND_SQMI = as.numeric(ALAND_SQMI)) %>%
   filter(!is.na(ALAND_SQMI)) %>%
   mutate(
      density14 = pop14 / ALAND_SQMI,
      density23 = pop23 / ALAND_SQMI)

#rank density in deciles and 100ths
density_analyze1 <- densitydb_tract %>%
   mutate(density_rank_10_start = ntile(density14, 10))%>%
   mutate(density_rank_10_end = ntile(density23, 10))%>%
   mutate(density_rank_100_start = ntile(density14, 100)) %>%
   mutate(density_rank_100_end = ntile(density23, 100))

#calc relative and absolute pop change
density_analyze2 <- density_analyze1 %>%
   mutate(pop_pctchg14to23 = 100 * (pop23 - pop14) / pop14,
          pop_14to23 = pop23 - pop14)

#rank density in 20ths
density_20ths <- density_analyze2 %>%
   mutate(density_rank_20_start = ntile(density14, 20))%>%
   mutate(density_rank_20_end = ntile(density23, 20))

#rename database
density <- density_20ths


# HOUSING UNITS ------------------------------------------------------

#import (Source: HUD USPS (EIG-validated)
   hud_import_14 <- read_excel(file.path(inputs, "TRACT20_122014.xlsx"))
   hud_import_24 <- read_excel(file.path(inputs, "TRACT20_122024.xlsx"))

#clean
   hud14_tract <- hud_import_14 %>%
      select(TRACT20, TOTAL_RESIDENTIAL_ADDRESSES, NO_STAT_RESIDENTIAL_ADDRESSES)
   
   hud24_tract <- hud_import_24 %>%
      select(TRACT20,  TOTAL_RESIDENTIAL_ADDRESSES, NO_STAT_RESIDENTIAL_ADDRESSES)

   #remove no status addresses (these are mostly PO boxes) and sum from tract-level to county-level
   hud14 <- hud14_tract %>%
      mutate(housingunits14 = TOTAL_RESIDENTIAL_ADDRESSES - NO_STAT_RESIDENTIAL_ADDRESSES) %>%
      group_by(TRACT20) %>%
      summarize(housingunits14 = sum(housingunits14, na.rm = TRUE))
   
   hud24 <- hud24_tract %>%
      mutate(housingunits24 = TOTAL_RESIDENTIAL_ADDRESSES - NO_STAT_RESIDENTIAL_ADDRESSES) %>%
      group_by(TRACT20) %>%
      summarize(housingunits24 = sum(housingunits24, na.rm = TRUE))

#merge
   hud14to24 <- hud14 %>%
      left_join(hud24, by = "TRACT20")

#analyze: calculate housing units percent change
   hud14to24 <- hud14to24 %>%
      mutate(HU_pctchg14to24 = 100 * (housingunits24 - housingunits14) / housingunits14,
             HU_14to24 = housingunits24 - housingunits14) %>%
      rename(GEOID=TRACT20)

#merge DENSITY with HUD USPS HOUSING UNITS
   density_hud <- density %>%
      left_join(hud14to24, by = "GEOID")

#clean: filter out missing and infinite values
density_hud <- density_hud %>%
   filter(
      !is.na(density_rank_10_start),
      !is.na(HU_pctchg14to24),
      !is.na(pop_pctchg14to23),
      is.finite(density_rank_10_start),
      is.finite(HU_pctchg14to24),
      is.finite(pop_pctchg14to23)
   )

density <- density_hud

# VACANCY -----------------------------------------------------------------

#tract-level
vacancies14_tract <- hud_import_14 %>%
   select(TRACT20, STATE_NAME, COUNTY_NAME, TOTAL_RESIDENTIAL_ADDRESSES, NO_STAT_RESIDENTIAL_ADDRESSES, C_SUM_S_VAC_RES, C_SUM_L_VAC_RES)       %>%
   mutate(housingunits14 = TOTAL_RESIDENTIAL_ADDRESSES - NO_STAT_RESIDENTIAL_ADDRESSES, vacancies14 = C_SUM_S_VAC_RES+C_SUM_L_VAC_RES)%>%
   rename(GEOID=TRACT20, shortvac14=C_SUM_S_VAC_RES, longvac14=C_SUM_L_VAC_RES)%>%
   select(-TOTAL_RESIDENTIAL_ADDRESSES,-NO_STAT_RESIDENTIAL_ADDRESSES) %>%
   mutate(vacancypct14 = (vacancies14 / housingunits14) * 100)

vacancies24_tract <- hud_import_24 %>%
   select(TRACT20, STATE_NAME, COUNTY_NAME, TOTAL_RESIDENTIAL_ADDRESSES, NO_STAT_RESIDENTIAL_ADDRESSES, C_SUM_S_VAC_RES, C_SUM_L_VAC_RES)       %>%
   mutate(housingunits24 = TOTAL_RESIDENTIAL_ADDRESSES - NO_STAT_RESIDENTIAL_ADDRESSES, vacancies24 = C_SUM_S_VAC_RES+C_SUM_L_VAC_RES)%>%
   rename(GEOID=TRACT20, shortvac24=C_SUM_S_VAC_RES, longvac24=C_SUM_L_VAC_RES)%>%
   select(-TOTAL_RESIDENTIAL_ADDRESSES,-NO_STAT_RESIDENTIAL_ADDRESSES) %>%
   mutate(vacancypct24 = (vacancies24 / housingunits24) * 100)

vacancies_tract <- vacancies14_tract %>%
   inner_join(vacancies24_tract %>% select(-STATE_NAME, -COUNTY_NAME),by = "GEOID")

#merge
vacancy <- density %>%
   left_join(vacancies_tract %>% select(-housingunits14, -housingunits24),
             by = "GEOID")

density <- vacancy %>%
   select(GEOID, STATE_NAME, COUNTY_NAME, everything())


# CROWDING/HOUSEHOLD SIZE -----------------------------------------------------------------
   hhsize <- density %>%
      mutate(occ_housingunits14 = housingunits14 - vacancies14,
             occ_housingunits24 = housingunits24 - vacancies24,
             hhsize14 = pop14/occ_housingunits14,
             hhsize24 = pop23/occ_housingunits24,
             hhsize_pctchg14to24 = (hhsize24-hhsize14)/hhsize14 * 100)
   
   hhsizetest <- hhsize %>%
      filter(hhsize14 <= 8, hhsize24 <= 8, occ_housingunits24 > 200)


# NEW HOMES PER 1,000 RESIDENTS -----------------------------------------------------------------
homes_per_resident <- hhsize %>%
   mutate(new_housing_per_res = (HU_14to24/pop14)*1000)
#obs: 82,411


# MULTIFAMILY -------------------------------------------------------------
   #import
   structure_import <- read.csv(file.path(inputs, "ACSDP5Y2023.DP04-Data.csv"))

   #clean
   structure <- structure_import[-1, ] %>%
      rename(all_housing = DP04_0006E,
             SFH_detached = DP04_0007E,
             SFH_attached = DP04_0008E,
             duplex = DP04_0009E,
             tri_quadplex = DP04_0010E,
             apartment_small = DP04_0011E,
             apartment_med = DP04_0012E,
             apartment_large = DP04_0013E,
             mobile_home = DP04_0014E) %>%
      mutate(GEOID = str_sub(GEO_ID, -11))%>%
      select(-1, -4, -5)%>%
      mutate(all_housingn = as.numeric(all_housing),
             SFH_detachedn = as.numeric(SFH_detached),
             SFH_attachedn = as.numeric(SFH_attached),
             duplexn = as.numeric(duplex),
             tri_quadplexn = as.numeric(tri_quadplex),
             apartment_smalln = as.numeric(apartment_small),
             apartment_medn = as.numeric(apartment_med),
             apartment_largen = as.numeric(apartment_large),
             mobile_homen = as.numeric(mobile_home)) %>%
      select(GEOID, NAME, all_housingn, SFH_detachedn, SFH_attachedn, duplexn, tri_quadplexn, apartment_smalln, apartment_medn, apartment_largen, mobile_homen)
   
   structure <- structure %>%
      mutate(missing_middlen = SFH_attachedn+duplexn+tri_quadplexn,
             apartmentsn = apartment_smalln+ apartment_medn + apartment_largen)
   
   structure_pct <- structure %>%
      mutate(
             SFH_detached = round((SFH_detachedn/all_housingn) * 100),
             SFH_attached = round((SFH_attachedn/all_housingn) * 100),
             duplex = round((duplexn/all_housingn) * 100),
             tri_quadplex = round((tri_quadplexn/all_housingn) * 100),
             apartment_small = round((apartment_smalln/all_housingn) * 100),
             apartment_med = round((apartment_medn/all_housingn) * 100),
             apartment_large = round((apartment_largen/all_housingn) * 100),
             mobile_home = round((mobile_homen/all_housingn) * 100),
             missing_middle = round((missing_middlen/all_housingn) * 100),
             apartments = round((apartmentsn/all_housingn) * 100)
             )

   #merge
   density_structure <- homes_per_resident %>%
      left_join(structure_pct, by = "GEOID") %>%
      select (-NAME.y)

   #rename
   density_final <- density_structure
   
# EXPORT FINAL -------------------------------------------------------

   #filter out unrealistic/ridiculous data
   density_final <- density_final %>%
      filter(hhsize14 <= 5, hhsize24 <= 5)
   #obs: 79,927

   density_final <- density_final %>%
      mutate(GEOID = as.character(GEOID))
   
   write.csv(density_final, file = file.path(outputs,"density_final.csv"),row.names = FALSE)

#Combine with county-level data
   #import
   county_data_import <- read_excel(file.path(outputs, "county_density14to24.xlsx"))
   
   #clean
   county_data <- county_data_import %>%
      rename_with(~ paste0(.x, "county"), .cols = -c(1:3)) %>%
      rename(STCTY=GEOID)
   
   density_stcty <- density_final %>%
      mutate(STCTY = substr(GEOID, 1, 5))
   
   #merge
   density_final_withcounty <- density_stcty %>%
      inner_join(county_data, by = "STCTY")
   
   sum(density_final_withcounty$pop14, na.rm = TRUE)
   
   write.csv(density_final_withcounty, file = file.path(outputs,"density_final_withcounty.csv"),row.names = FALSE)

# ANALYSIS & TOP 100 DATABASES -------------------------------------------------------

   #Measure the correlation between pre-existing density and HU growth
   cor(density$density_rank_10_start, density$HU_pctchg14to24, use = "complete.obs")
   #SEER result: -0.003. no relationship between pre-existing density and HU growth at the tract level
   #ACS result: -0.017 no relationship between pre-existing density and HU growth at the tract level
   
   #Measure the correlation between pop growth and HU growth
   cor(density$pop_pctchg14to23, density$HU_pctchg14to24, use = "complete.obs")
   #SEER result: 0.026. no relationship between pop growth and HU growth at the tract level 
   #ACS result: 0.402. moderately strong relationship between pop growth and HU growth at the tract level 
   
   #Calculate median tract relative HU growth from 2014 to 2024
   median(density$HU_pctchg14to24, na.rm = TRUE)
   #SEER result: 2.85%
   #ACS result: 3.86%
   
   #Calculate median tract absolute HU growth from 2014 to 2024
   median(density$HU_14to24, na.rm = TRUE)
   #SEER result: 41
   #ACS result: 53

#POPULATION change (PERCENT)   
   #Measure the correlation between pre-existing density and relative pop growth
   cor(density_final$density_rank_10_start, density_final$pop_pctchg14to23, use = "complete.obs")
   #SEER result: -0.115 tracts in higher density deciles saw slightly less relative pop growth from 2014 to 2024
   #ACS result: -0.025 almost no relationship between tract density and relative pop growth from 2014 to 2023
   
   #Calculate median tract pop growth from 2014 to 2024
   median(density_final$pop_pctchg14to23, na.rm = TRUE)
   #SEER result: 1.7%. The median tract barely grew at all in population.
   #ACS result: 2.6%
   
   #Sort by top 100 pop change (percent)
   top_100_pop_pct <- density_final %>%
      arrange(desc(pop_pctchg14to23)) %>%
      slice(1:100)
   median10_top <- median(top_100_pop_pct$density_rank_10_start, na.rm = TRUE)
   median100_top <- median(top_100_pop_pct$density_rank_100_start, na.rm = TRUE)
   #SEER result: 3, 29
   #ACS result: 2, 17
   
   #Sort by bottom 100 pop change (percent)
   bottom_100_pop_pct <- density_final %>%
      arrange(pop_pctchg14to23) %>%
      slice(1:100)
   median10_bottom_pct <- median(bottom_100_pop_pct$density_rank_10_start, na.rm = TRUE)
   median100_botom_pct <- median(bottom_100_pop_pct$density_rank_100_start, na.rm = TRUE)
   #SEER result: 2, 12   
   #ACS result: 1, 4

#POPULATION change (NUMBER)   
   #Measure the correlation between pre-existing density and absolute pop growth for tracts
   cor(density_final$density_rank_10_start, density_final$pop_14to23, use = "complete.obs")
   #SEER result: -0.105 tracts in higher density deciles saw slightly less absolute pop growth from 2014 to 2023
   #ACS result" -0.092. no relationship
   
   #Calculate median tract pop growth from 2014 to 2023
   median(density_final$pop_14to23, na.rm = TRUE)
   #SEER result: 6 people - the median tract barely grew at all
   #ACS result: 90 people - the median tract grew a little bit, but barely at all
   
   #Sort by top 100 pop change (number)
   top_100_pop_num <- density_final %>%
      arrange(desc(pop_14to23)) %>%
      slice(1:100)
   median10_top_num <- median(top_100_pop_num$density_rank_10_start, na.rm = TRUE)
   median100_top_num <- median(top_100_pop_num$density_rank_100_start, na.rm = TRUE)
   #SEER result: 4, 36
   #ACS result: 3, 28.5
   
   #Sort by bottom 100 pop change (number)
   bottom_100_pop_num <- density_final %>%
      arrange(pop_14to23) %>%
      slice(1:100)
   median10_bottom_num <- median(bottom_100_pop_num$density_rank_10_start, na.rm = TRUE)
   median100_botom_num <- median(bottom_100_pop_num$density_rank_100_start, na.rm = TRUE)
   #SEER result: 3.5, 30.5    
   #ACS result: 6, 57.5

#HOUSING UNIT change (PERCENT)
   
   #Measure the correlation between pre-existing density and relative HU growth
   cor(density_final$density_rank_10_start, density_final$HU_pctchg14to24, use = "complete.obs")
   #SEER result: -0.003. almost no relationship between pre-existing density and relative housing unit growth from 2014 to 2024
   #ACS result: -0.021. almost no relationship between pre-existing density and relative housing unit growth from 2014 to 2024
   
   #Calculate median county HU growth from 2014 to 2024
   median(density_final$HU_pctchg14to24, na.rm = TRUE)
   #SEER and ACS result: 3.9%. The median tract barely grew at all in population.
   
   #sort by top
   top_100_HU_pct <- density_final %>%
      arrange(desc(HU_pctchg14to24)) %>%
      slice(1:100)
   
   #calc medians top
   median_10 <- median(top_100_HU_pct$density_rank_10_start, na.rm = TRUE)
   median_100 <- median(top_100_HU_pct$density_rank_100_start, na.rm = TRUE)
   #SEER result: 3, 21
   #ACS result: 2, 15
   
   #sort by bottom
   bottom_100_HU_pct <- density_final %>%
      arrange(HU_pctchg14to24) %>%
      slice(1:100)
   
   #calc medians bottom
   median_10 <- median(bottom_100_HU_pct$density_rank_10_start, na.rm = TRUE)
   median_100 <- median(bottom_100_HU_pct$density_rank_100_start, na.rm = TRUE)
   #SEER result: 4, 36
   #ACS result: 5, 44.5
   
   #median density of top 100 vs all
   median_density_top_pct <- median(top_100_HU_pct$density14, na.rm = TRUE)
   median_density_all_pct <- median(density_final$density14, na.rm = TRUE)
   #ACS result: The top percentage growth areas had MUCH lower density than the median density for the country as a whole
   
   #HU change (NUMBER)
   #sort by top
   top_100_HU_num <- density_final %>%
      arrange(desc(HU_14to24)) %>%
      slice(1:100)
   
   #calc medians top
   median_10 <- median(top_100_HU_num$density_rank_10_start, na.rm = TRUE)
   median_100 <- median(top_100_HU_num$density_rank_100_start, na.rm = TRUE)
   #SEER and ACS result: 3, 24.5
   
   #sort by bottom
   bottom_100_HU_num <- density_final %>%
      arrange(HU_14to24) %>%
      slice(1:100)
   
   #calc medians bottom
   median_10 <- median(bottom_100_HU_num$density_rank_10_start, na.rm = TRUE)
   median_100 <- median(bottom_100_HU_num$density_rank_100_start, na.rm = TRUE)
   #SEER result: 5, 49
   #ACS result: 6, 53.5
   
   #median density of top 100 vs all
   median_density_top_num <- median(top_100_HU_num$density14, na.rm = TRUE)
   median_density_all_num <- median(density_final$density14, na.rm = TRUE)
   #result: HUGE difference in the initial density between top growth areas (by number) and the country as a whole. MUCH LOWER
   
   #Population by density decile
   population_by_density <- decimals(density_final) %>%
      group_by(density_rank_10_start) %>%
      summarize(
         pop14_decile = sum(pop14, na.rm = TRUE),
         pop23_decile = sum(pop23, na.rm = TRUE),
         .groups = "drop"
      ) %>%
      mutate(
         pop14_decile_pct = pop14_decile / sum(pop14_decile) * 100,
         pop23_decile_pct = pop23_decile / sum(pop23_decile) * 100
      )

# MEDIAN DECILE DENSITY VALUES ---------------------------------------------------------------------
   
   #Median density per decile
   density_deciles <- density_final %>%
      filter(!is.na(density14)) %>%
      mutate(
         density_decile = ntile(density14, 10)
      ) %>%
      group_by(density_decile) %>%
      summarize(
         min_density14 = min(density14, na.rm = TRUE),
         median_density14 = median(density14, na.rm = TRUE),
         max_density14 = max(density14, na.rm = TRUE),
         .groups = "drop"
      )
   
   density_deciles <- decimals(density_deciles)
   
   #Median density per 20ths
   density_20ths_meds <- density_final %>%
      filter(!is.na(density14)) %>%
      mutate(
         density_20ths = ntile(density14, 20)
      ) %>%
      group_by(density_20ths) %>%
      summarize(
         min_density14 = min(density14, na.rm = TRUE),
         median_density14 = median(density14, na.rm = TRUE),
         max_density14 = max(density14, na.rm = TRUE),
         allhousing_sum = sum(all_housingn, na.rm = TRUE), 
         SFHdetached_sum = sum(SFH_detachedn, na.rm = TRUE),
         missingmiddle_sum = sum(missing_middlen, na.rm=TRUE),
         apartments_sum = sum(apartmentsn, na.rm=TRUE),
         aptslarge_sum = sum(apartment_largen, na.rm=TRUE),
         .groups = "drop"
      ) %>%
      mutate(SFH_detached = (SFHdetached_sum/allhousing_sum) * 100,
             missingmiddle = (missingmiddle_sum/allhousing_sum) * 100,
             apartments = (apartments_sum/allhousing_sum) * 100,
             aptslarge = (aptslarge_sum/allhousing_sum) * 100
      ) %>%
      select(-allhousing_sum,SFHdetached_sum,missingmiddle_sum,apartments_sum,aptslarge_sum)
             
   density_20ths_meds <- decimals(density_20ths_meds)

# GRAPHS AND VISUALS ------------------------------------------------------

   #-------bar charts-------#
   #Housing Growth by Density
   hu_totals <- density_final %>%
      mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
      group_by(density_rank_10_start) %>%
      summarize(total_HU_14to24 = sum(HU_14to24, na.rm = TRUE)) %>%
      ungroup()
   overall_median <- median(hu_totals$total_HU_14to24, na.rm = TRUE)
   ggplot(hu_totals, aes(x = density_rank_10_start, y = total_HU_14to24)) +
      geom_col(fill = "orange") +
      geom_hline(yintercept = overall_median, color = "black", size = 0.7) +
      scale_y_continuous(labels = comma_format()) +
      scale_x_discrete(drop = FALSE) +
      labs(
         title = "New Housing by Initial Density Rank (2014–2024)",
         subtitle = "Census Tracts",
         x = "Density Rank in 2014",
         y = "Net Change in Housing Units") +
      theme_minimal() +
      theme(
         plot.title = element_text(face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 10, hjust = 0.5),
         plot.background = element_rect(fill = "white", color = NA),
         panel.background = element_rect(fill = "white", color = NA))
   
   ggsave(filename = file.path(graphs,"housingunits_density_added.png"),width = 8, height = 6, dpi = 300)
   
   #Housing Growth by Density - where County Density > 8
   hu_totals2 <- density_final_withcounty %>%
      filter(density_rank_10_startcounty > 8) %>%
      mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
      group_by(density_rank_10_start) %>%
      summarize(total_HU_14to24 = sum(HU_14to24, na.rm = TRUE)) %>%
      ungroup()
   
   overall_median <- median(hu_totals$total_HU_14to24, na.rm = TRUE)
   
   ggplot(hu_totals2, aes(x = density_rank_10_start, y = total_HU_14to24)) +
      geom_col(fill = "orange") +
      geom_hline(yintercept = overall_median, color = "black", size = 0.7) +
      scale_y_continuous(labels = comma_format()) +
      scale_x_discrete(drop = FALSE) +
      labs(
         title = "New Housing by Initial Density Rank (2014–2024)",
         subtitle = "Census Tracts (High-Density Counties Only)",
         x = "Density Rank in 2014",
         y = "Net Change in Housing Units"
      ) +
      theme_minimal() +
      theme(
         plot.title = element_text(face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 10, hjust = 0.5),
         plot.background = element_rect(fill = "white", color = NA),
         panel.background = element_rect(fill = "white", color = NA)
      )
   
   ggsave(filename = file.path(graphs, "housingunits_density_added_tracts_high_density_counties.png"),width = 8, height = 6, dpi = 300)
   #export to datawrapper
   write.csv(hu_totals2, file = file.path(outputs,"chart2data.csv"),row.names = FALSE)
   
# Graph: New Housing per 1000 Residents ----------------------------------
   
   #New Housing per 1000 Residents (10ths)
   pop_hu_total <- density_final %>%
      mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
      group_by(density_rank_10_start) %>%
      summarize(
         total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
         total_pop14 = sum(pop14, na.rm = TRUE)
      ) %>% 
      mutate(new_housing_per_res = (total_HU_14to24/total_pop14)*1000) %>%
      ungroup()
         
   ggplot(pop_hu_total, aes(x = density_rank_10_start, y = new_housing_per_res)) +
      geom_col(fill = "coral2") +
      scale_y_continuous(labels = comma_format()) +
      scale_x_discrete(drop = FALSE) +
      labs(
         title = "New Housing per Resident by Initial Density Rank (2014–2024)",
         subtitle = "Census Tracts",
         x = "Density Rank in 2014",
         y = "New Housing Units per 1,000 Residents") +
      theme_minimal() +
      theme(
         plot.title = element_text(face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 10, hjust = 0.5),
         plot.background = element_rect(fill = "white", color = NA),
         panel.background = element_rect(fill = "white", color = NA))
   ggsave(filename = file.path(graphs, "housingunitsperresident.png"),width = 8, height = 6, dpi = 300)

   
   #New Housing per 1000 Residents (20ths)
      #Rank density in 20ths
      density_20ths <- density_final %>%
         mutate(density_rank_20_start = ntile(density14, 20))%>%
         mutate(density_rank_20_end = ntile(density23, 20))
   
   newhousing_5pct <- density_20ths %>%
      mutate(density_rank_20_start = factor(density_rank_20_start, levels = 1:20)) %>%
      group_by(density_rank_20_start) %>%
      summarize(
         total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
         total_pop14 = sum(pop14, na.rm = TRUE),
         new_housing_per_res = round((total_HU_14to24 / total_pop14) * 1000),
         avg_density14 = mean(density14, na.rm = TRUE),
         .groups = "drop"
      )
   
   ggplot(newhousing_5pct, aes(x = density_rank_20_start, y = new_housing_per_res)) +
      geom_col(fill = "coral2") +
      scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.1))) +
      scale_x_discrete(labels = function(x) {
         case_when(
            x == "1"  ~ "5%\n(Least Dense)",
            x == "10" ~ "50%",
            x == "20" ~ "95%\n(Densest)",
            TRUE      ~ ""
         )
      }) +
      labs(
         title = "New Housing Built per 1,000 Residents by Pre-Existing Density",
         x = "National Density Rank",
         y = "Housing Units Built from 2014 to 2024"
      ) +
      theme_minimal() +
      theme(
         plot.title = element_text(face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 10, hjust = 0.5),
         plot.background = element_rect(fill = "white", color = NA),
         panel.background = element_rect(fill = "white", color = NA),
         axis.text.x = element_text(size = 9)
      )
   
   ggsave(filename = file.path(graphs, "New Housing Units per 1,000 Residents by Pre-Existing Density 20ths.png"),width = 10, height = 8, dpi = 300)
   write.csv(newhousing_5pct, file = file.path(outputs,"chart3data.csv"),row.names = FALSE)



# Graph: Housing Growth - both relative and total -----------------
# 1. Median values
hu_medians <- density_final %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE), .groups = "drop")

# 2. Total values (for point sizes)
hu_totals <- density_final %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(total_HU_14to24 = sum(HU_14to24, na.rm = TRUE), .groups = "drop")

# 3. Combine both into one dataframe
hu_combined <- left_join(hu_medians, hu_totals, by = "density_rank_10_start")

# 4. Overall median line
overall_median <- median(density_final$HU_pctchg14to24, na.rm = TRUE)

# 5. Plot
ggplot(hu_combined, aes(x = density_rank_10_start, y = median_hu_pctchg)) +
   geom_col(fill = "orange") +
   geom_hline(yintercept = overall_median, color = "black", size = 0.6) +
   geom_point(aes(size = total_HU_14to24), color = "darkred", shape = 21, fill = "white", stroke = 1.2, 
              position = position_nudge(y = 0.01)) +  
   scale_size_continuous(range = c(3, 10), name = "New housing", labels = scales::comma) +
   scale_x_discrete(drop = FALSE) +
   scale_y_continuous(
      breaks = pretty_breaks(),
      labels = label_percent(scale = 1, accuracy = 1)) +
   labs(
      title = "Median % Change in Housing Units by Density Rank (2014–2024)",
      subtitle = "Census Tracts",
      x = "Density Rank in 2014",
      y = "Median % Change in Housing") +
   theme_minimal() +
   theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "right")
ggsave(filename = file.path(graphs, "housing_growth.png"),width = 10, height = 7, dpi = 300)
 
#------------------------don't use these graphs------------------------#
   
   #-------box plots-------#
   #Population Growth vs Density Rank 
   plot(density_final$density_rank_10_start, density_final$pop_pctchg14to23,
        xlab = "Density Rank (1 = Least Dense, 10 = Most Dense)",
        ylab = "Population Change % (2014 to 2023)",
        main = "Population Growth vs. Density Rank")
   abline(lm(pop_pctchg14to23 ~ density_rank_10_start, data = density), col = "red")
   
   #Housing Growth vs. Density Rank 
   plot(density_hud$density_rank_10_start, density_hud$HU_pctchg14to24,
        xlab = "Density Rank (1 = Least Dense, 10 = Most Dense)",
        ylab = "Housing Unit Growth % (2014 to 2024)",
        main = "Housing Growth vs. Density Rank")
   abline(lm(HU_pctchg14to24 ~ density_rank_10_start, data = density_hud), col = "red")
 
   
   #-------line graphs-------#
   #Population Growth by Density (Line graph)
   pop_medians <- density_final %>%
      mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
      group_by(density_rank_10_start) %>%
      summarize(median_pop_pctchg = median(pop_pctchg14to23, na.rm = TRUE)) %>%
      ungroup()
   
   ggplot(pop_medians, aes(x = density_rank_10_start, y = median_pop_pctchg, group = 1)) +
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue") +
      scale_x_discrete(drop = FALSE) +  # force showing all factor levels
      labs(
         title = "Median Population % Change (2014–2023) by Density Rank",
         subtitle = "Tracts",
         x = "Density Rank in 2014",
         y = "Median % Change in Population"
      ) +
      theme_minimal()
   
   
   
   #Housing Growth by Density (Line graph)
   hu_medians <- density_final %>%
      mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
      group_by(density_rank_10_start) %>%
      summarize(median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE)) %>%
      ungroup()
   
   ggplot(hu_medians, aes(x = density_rank_10_start, y = median_hu_pctchg, group = 1)) +
      geom_line(color = "orange") +
      geom_point(color = "orange") +
      scale_x_discrete(drop = FALSE) +  # force showing all factor levels
      labs(
         title = "Median % Change in Housing Units (2014–2024) by Density Decile",
         x = "Density Rank in 2014",
         y = "Median % Change in Housing"
      ) +
      theme_minimal()+
      theme(
         plot.title = element_text(face = "bold", hjust = 0.5),
         plot.background = element_rect(fill = "white", color = NA),
         panel.background = element_rect(fill = "white", color = NA)
      )
   ggsave("graphs/density_growth_line.png")
   
   
   #COMBINED  - Population and Housing by Density (Line graph)
   combined_medians <- left_join(pop_medians, hu_medians, by = "density_rank_10_start") %>%
      pivot_longer(
         cols = c(median_pop_pctchg, median_hu_pctchg),
         names_to = "metric",
         values_to = "percent_change")
   
   ggplot(combined_medians, aes(x = density_rank_10_start, y = percent_change, color = metric, group = metric)) +
      geom_line() +
      geom_point() +
      geom_hline(yintercept = 0, color = "black", size = .5) +
      scale_color_manual(
         values = c("median_pop_pctchg" = "steelblue", "median_hu_pctchg" = "darkorange"),
         labels = c(
            "median_pop_pctchg" = "Population % Change",
            "median_hu_pctchg" = "Housing Unit % Change")) +
      scale_y_continuous(
         limits = c(-5, NA),  
         labels = label_percent(scale = 1)) +
      labs(
         title = "Median % Change by Density at the Census Tract (2014–2024)",
         x = "Density Rank in 2014",
         y = "Median % Change",
         color = NULL) +
      theme_minimal() +
      theme(
         plot.title = element_text(face = "bold", hjust = 0.5),
         plot.background = element_rect(fill = "white", color = NA),
         panel.background = element_rect(fill = "white", color = NA))
   ggsave("graphs/density_housing_pop_tracts.png", width = 8, height = 6, dpi = 300)
   
   #Housing Growth by Density (pct)
   hu_medians <- density_final %>%
      mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
      group_by(density_rank_10_start) %>%
      summarize(median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE)) %>%
      ungroup()
   overall_median <- median(density_final$HU_pctchg14to24, na.rm = TRUE)
   
   ggplot(hu_medians, aes(x = density_rank_10_start, y = median_hu_pctchg)) +
      geom_col(fill = "orange") +
      geom_hline(yintercept = overall_median, color = "black", size = 0.6) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(
         breaks = pretty_breaks(),
         labels = label_percent(scale = 1, accuracy = 1)) +
      labs(
         title = "Median % Change in Housing Units (2014–2024) by Density Rank",
         subtitle = "Census Tracts",
         x = "Density Rank in 2014",
         y = "Median % Change in Housing") +
      theme_minimal() +
      theme(
         plot.title = element_text(face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 10, hjust = 0.5),
         plot.background = element_rect(fill = "white", color = NA),
         panel.background = element_rect(fill = "white", color = NA))
   
   
   ggplot(newhousing_5pct, aes(x = as.integer(as.character(density_rank_20_start)), 
                               y = new_housing_per_res)) +
      geom_point(color = "coral2", size = 3) +
      geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) +
      scale_y_continuous(labels = comma_format()) +
      labs(
         title = "New Housing per 1,000 Residents by Pre-Existing Density",
         subtitle = "Census Tracts, 2014-2024",
         x = "National Density Rank",
         y = "Housing Units Built per 1,000 Residents"
      ) +
      theme_minimal() +
      theme(
         plot.title = element_text(face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 10, hjust = 0.5),
         plot.background = element_rect(fill = "white", color = NA),
         panel.background = element_rect(fill = "white", color = NA)
      )
   ggsave(filename = file.path(graphs, "scatterplot.png"),width = 8, height = 6, dpi = 300)
# LEGACY - DON'T USE ---------------------------------------------------------------
   #POPULATION - DONT USE
      
      
      # pop_tract_14 <- read_fwf(
      #    file = "inputs/population/pop_tract_14.txt",
      #    fwf_widths(c(4, 2, 2, 3, 6, 1, 1, 2, 9),
      #               col_names = c("year", "state_post", "state", "county", "tract", "race", "sex", "age", "population")),
      #    progress = TRUE
      # )
      # 
      # pop_tract_14_totals <- pop_tract_14 %>%
      #    mutate(GEOID = paste0(state, county, tract),
      #       population = as.character(population),
      #       population = sub("^([0-9]{4})([0-9]+)$", "\\1.\\2", population),
      #       population = as.numeric(population)
      #    ) %>%
      #    group_by(GEOID) %>%
      #    summarize(
      #       pop14 = round(sum(population, na.rm = TRUE)),
      #       .groups = "drop"
      #    )
      # 
      # pop_tract_23 <- read_fwf(
      #    file = "inputs/population/pop_tract_24.txt",
      #    fwf_widths(c(4, 2, 2, 3, 6, 1, 1, 2, 9),
      #               col_names = c("year", "state_post", "state", "county", "tract", "race", "sex", "age", "population")),
      #    progress = TRUE
      # )
      # 
      # pop_tract_23_totals <- pop_tract_24 %>%
      #    mutate(GEOID = paste0(state, county, tract),
      #           population = as.character(population),
      #           population = sub("^([0-9]{4})([0-9]+)$", "\\1.\\2", population),
      #           population = as.numeric(population)
      #    ) %>%
      #    group_by(GEOID) %>%
      #    summarize(
      #       pop23 = round(sum(population, na.rm = TRUE)),
      #       .groups = "drop"
      #    )
      
      # #merge
      # pop_tract_14to23 <- pop_tract_14_totals %>%
      #    inner_join(pop_tract_23_totals, by = "GEOID")
      
   
   # #1pct - DO NOT USE
   # top_79to100<- density_final %>%
   #    filter(density_rank_100_start %in% 80:100) %>%
   #    group_by(density_rank_100_start) %>%
   #    summarize(
   #       total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
   #       total_pop14 = sum(pop14, na.rm = TRUE),
   #       new_housing_per_res = (total_HU_14to24 / total_pop14) * 1000,
   #       .groups = "drop"
   #    )
   # 
   # ggplot(top_79to100, aes(x = density_rank_100_start, y = total_HU_14to24)) +
   #    geom_col(fill = "orange") +
   #    labs(
   #       title = "New Housing Built in Highest Density Areas (2014–2024)",
   #       subtitle = "Census Tracts",
   #       x = "Density Rank in 2014",
   #       y = "Housing Units Built") +
   #    theme_minimal() +
   #    theme(
   #       plot.title = element_text(face = "bold", hjust = 0.5),
   #       plot.subtitle = element_text(size = 10, hjust = 0.5),
   #       plot.background = element_rect(fill = "white", color = NA),
   #       panel.background = element_rect(fill = "white", color = NA),
   #       legend.position = "right")


# #SCATTERPLOT OF ALL CENSUS TRACTS - NOT HELPFUL
#       # Step 1: Create tract-level housing per resident
#       density_tracts_plot <- density_final %>%
#          filter(!is.na(density14), !is.na(HU_14to24), !is.na(pop14), pop14 > 0) %>%
#          mutate(
#             new_housing_per_res = (HU_14to24 / pop14) * 1000 
#             ) %>%
#                filter(new_housing_per_res <= 50000)
#       
#       # Step 2: Plot scatterplot
#       ggplot(density_tracts_plot, aes(x = density14, y = new_housing_per_res)) +
#          geom_point(color = "coral2", size = 0.7, alpha = 0.5) +
#          geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
#          scale_x_continuous(trans = "log10", labels = comma_format()) +
#          scale_y_continuous(labels = comma_format()) +
#          labs(
#             title = "New Housing Units per 1,000 Residents by Census Tract Density",
#             subtitle = "Each point represents one tract (log scale on x-axis)",
#             x = "Population Density (log scale)",
#             y = "New Housing Units per 1,000 Residents"
#          ) +
#          theme_minimal() +
#          theme(
#             plot.title = element_text(face = "bold", hjust = 0.5),
#             plot.subtitle = element_text(hjust = 0.5),
#             plot.background = element_rect(fill = "white", color = NA),
#             panel.background = element_rect(fill = "white", color = NA)
#          )
   
   # #New Housing Added (20ths)
   # ggplot(newhousing_5pct, aes(x = density_rank_20_start, y = total_HU_14to24)) +
   #    geom_col(fill = "coral2") +
   #    geom_text(
   #       aes(label = ifelse(density_rank_20_start %in% c(3, 6, 15, 18, 19, 20),
   #                          comma(avg_density14),
   #                          NA)),
   #       vjust = -0.5,
   #       size = 3.5
   #    ) +
   #    scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.1))) +
   #    scale_x_discrete(breaks = c("1", "5", "10", "15", "20")) +
   #    labs(
   #       title = "Total New Housing by Pre-Existing Density",
   #       subtitle = "Census tracts",
   #       x = "National Density Percentile (20ths)",
   #       y = "New Housing Units"
   #    ) +
   #    theme_minimal() +
   #    theme(
   #       plot.title = element_text(face = "bold", hjust = 0.5),
   #       plot.subtitle = element_text(size = 10, hjust = 0.5),
   #       plot.background = element_rect(fill = "white", color = NA),
   #       panel.background = element_rect(fill = "white", color = NA)
   #    )
   # ggsave("graphs/New Housing Units Added by Pre-Existing Density 20ths.png", width = 9, height = 10, dpi = 300)


