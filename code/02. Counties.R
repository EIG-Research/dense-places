# Project: Can Dense Places Still Build? 
# File Description: counties
 
# Author: Jess Remington
# Creation Date: 04/14/25
# last update: 04/21/25

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

# POPULATION --------------------------------------------------------

#import (Source: Population Estimates Program) 
pop10to19import <- read_excel(file.path(inputs, "PEP_pop_county_2014.xlsx"))
pop20to24import <- read_excel(file.path(inputs, "PEP_pop_county_2024.xlsx"))

#clean
pop10to19db <- pop10to19import %>%
   select(STATE, COUNTY, STNAME, CTYNAME, starts_with("POPESTIMATE")) %>%
   rename_with(
      .cols = matches("POPESTIMATE20\\d{2}"),
      .fn = ~ str_replace(., "POPESTIMATE20", "pop")
   ) %>%
   mutate(GEOID = paste0(STATE, COUNTY)) %>%
   select(GEOID, everything(), -STATE, -COUNTY)


pop20to24db <- pop20to24import %>%
   select(STATE, COUNTY, STNAME, CTYNAME, starts_with("POPESTIMATE")) %>%
   rename_with(
      .cols = matches("POPESTIMATE20\\d{2}"),
      .fn = ~ str_replace(., "POPESTIMATE20", "pop")
   ) %>%
   mutate(GEOID = paste0(STATE, COUNTY)) %>%
   select(GEOID, everything(), -STATE, -COUNTY)

#merge
pop_all_export <- pop10to19db %>%
   left_join(pop20to24db %>% select(GEOID, pop20, pop21, pop22, pop23, pop24), by = "GEOID")

pop_all <- pop_all_export %>%
   select(GEOID, STNAME, CTYNAME, pop14, pop24)%>%
   rename(STATE=STNAME, COUNTY=CTYNAME)%>%
   filter(STATE != COUNTY| STATE == "District of Columbia",
          GEOID != "11000")

sum(pop_all$pop14, na.rm=TRUE)

#export
write.csv(pop_all_export,
   file = file.path(outputs, "PEP_population_county_2010to2024.csv"),
   row.names = FALSE
)

# DENSITY ------------------------------------------------------------
#import (Source: Gazeteer)
sqmi <- read_excel(file.path(inputs, "LandArea_County.xlsx"))

#clean
sqmi <- sqmi %>%
   select(GEOID, NAME, ALAND_SQMI)

#merge
density_sqmi <- pop_all %>%
   left_join(sqmi %>% select(GEOID, ALAND_SQMI))

#analyze
#calculate density
densitydb <- density_sqmi %>%
   mutate(
      pop14 = as.numeric(gsub(",", "", pop14)),
      pop24 = as.numeric(gsub(",", "", pop24)),
      ALAND_SQMI = as.numeric(ALAND_SQMI)) %>%
      filter(!is.na(ALAND_SQMI)) %>%
         mutate(
         density14 = pop14 / ALAND_SQMI,
         density24 = pop24 / ALAND_SQMI)

#rank density in deciles and 100ths
density_analyze1 <- densitydb %>%
   mutate(density_rank_10_start = ntile(density14, 10))%>%
   mutate(density_rank_10_end = ntile(density24, 10))%>%
   mutate(density_rank_100_start = ntile(density14, 100)) %>%
   mutate(density_rank_100_end = ntile(density24, 100))

#calculate relative and absolute population change
density_analyze2 <- density_analyze1 %>%
   mutate(pop_pctchg14to24 = 100 * (pop24 - pop14) / pop14,
          pop_14to24 = pop24 - pop14)

#rename database
density <- density_analyze2

# HOUSING UNITS ------------------------------------------------------

#import (Source: HUD USPS (EIG-validated))
   hud_import_14 <- read_excel(file.path(inputs, "TRACT20_122014.xlsx"))
   hud_import_24 <- read_excel(file.path(inputs, "TRACT20_122024.xlsx"))

#clean
   hud14_tract <- hud_import_14 %>%
      rename(GEOID=STCNTY) %>%
      select(GEOID, TOTAL_RESIDENTIAL_ADDRESSES, NO_STAT_RESIDENTIAL_ADDRESSES)
   
   hud24_tract <- hud_import_24 %>%
      rename(GEOID=STCNTY) %>%
      select(GEOID, TOTAL_RESIDENTIAL_ADDRESSES, NO_STAT_RESIDENTIAL_ADDRESSES)

   #Remove no status addresses (these are mostly PO boxes) and sum from tract-level to county-level
   hud14 <- hud14_tract %>%
      mutate(housingunits14 = TOTAL_RESIDENTIAL_ADDRESSES - NO_STAT_RESIDENTIAL_ADDRESSES) %>%
      group_by(GEOID) %>%
      summarize(housingunits14 = sum(housingunits14, na.rm = TRUE))
   
   hud24 <- hud24_tract %>%
      mutate(housingunits24 = TOTAL_RESIDENTIAL_ADDRESSES - NO_STAT_RESIDENTIAL_ADDRESSES) %>%
      group_by(GEOID) %>%
      summarize(housingunits24 = sum(housingunits24, na.rm = TRUE))

#merge
   hud14to24 <- hud14 %>%
      left_join(hud24, by = "GEOID")
   
#analyze
   #Calculate housing units percent change
      hud14to24 <- hud14to24 %>%
         mutate(HU_pctchg14to24 = 100 * (housingunits24 - housingunits14) / housingunits14,
                HU_14to24 = housingunits24 - housingunits14)
   
# merge DENSITY with HUD USPS HOUSING UNITS
   density_hud <- density %>%
      left_join(hud14to24, by = "GEOID")

#clean
   #filter out missing and infinite values
   density_hud <- density_hud %>%
      filter(
         !is.na(density_rank_10_start),
         !is.na(HU_pctchg14to24),
         is.finite(density_rank_10_start),
         is.finite(HU_pctchg14to24)
   )
   
#export
   write_xlsx(density_hud,path = file.path(outputs, "county_density14to24.xlsx"))
   
   #rename database
   density <- density_hud

# VACANCY -----------------------------------------------------------------

   #tract-level
   vacancies14_tract <- hud_import_14 %>%
      rename(GEOID=STCNTY) %>%
      select(TRACT20, GEOID, STATE_NAME, COUNTY_NAME, TOTAL_RESIDENTIAL_ADDRESSES, NO_STAT_RESIDENTIAL_ADDRESSES, C_SUM_S_VAC_RES, C_SUM_L_VAC_RES)       %>%
      mutate(housingunits14 = TOTAL_RESIDENTIAL_ADDRESSES - NO_STAT_RESIDENTIAL_ADDRESSES, vacancies14 = C_SUM_S_VAC_RES+C_SUM_L_VAC_RES)%>%
      rename(shortvac14=C_SUM_S_VAC_RES, longvac14=C_SUM_L_VAC_RES)%>%
      select(-TOTAL_RESIDENTIAL_ADDRESSES,-NO_STAT_RESIDENTIAL_ADDRESSES) %>%
      mutate(vacancypct14 = (vacancies14 / housingunits14) * 100)
   
   vacancies24_tract <- hud_import_24 %>%
      rename(GEOID=STCNTY) %>%
      select(TRACT20, GEOID, STATE_NAME, COUNTY_NAME, TOTAL_RESIDENTIAL_ADDRESSES, NO_STAT_RESIDENTIAL_ADDRESSES, C_SUM_S_VAC_RES, C_SUM_L_VAC_RES)       %>%
      mutate(housingunits24 = TOTAL_RESIDENTIAL_ADDRESSES - NO_STAT_RESIDENTIAL_ADDRESSES, vacancies24 = C_SUM_S_VAC_RES+C_SUM_L_VAC_RES)%>%
      rename(shortvac24=C_SUM_S_VAC_RES, longvac24=C_SUM_L_VAC_RES)%>%
      select(-TOTAL_RESIDENTIAL_ADDRESSES,-NO_STAT_RESIDENTIAL_ADDRESSES) %>%
      mutate(vacancypct24 = (vacancies24 / housingunits24) * 100)
   
   vacancies_tract <- vacancies14_tract %>%
      inner_join(vacancies24_tract %>% select(-GEOID, -STATE_NAME, -COUNTY_NAME),by = "TRACT20")
   
   #county-level
   vacancies14_county <- vacancies14_tract %>%
      group_by(GEOID) %>%
      summarize(
         STATE_NAME = first(STATE_NAME),
         COUNTY_NAME = first(COUNTY_NAME),
         housingunits14 = sum(housingunits14, na.rm = TRUE),
         vacancies14 = sum(vacancies14, na.rm = TRUE),
         .groups = "drop") %>%
      mutate(vacancypct14 = (vacancies14 / housingunits14) * 100)
   
   vacancies24_county <- vacancies24_tract %>%
      group_by(GEOID) %>%
      summarize(
         STATE_NAME = first(STATE_NAME),
         COUNTY_NAME = first(COUNTY_NAME),
         housingunits24 = sum(housingunits24, na.rm = TRUE),
         vacancies24 = sum(vacancies24, na.rm = TRUE),
         .groups = "drop") %>%
      mutate(vacancypct24 = (vacancies24 / housingunits24) * 100)
   
   vacancies_county <- vacancies14_county %>%
      inner_join(vacancies24_county %>% select(-STATE_NAME, -COUNTY_NAME),by = "GEOID") %>%
      mutate(vac_14to24 = vacancies24 - vacancies14, 
             vac_pctchg14to24 = ((vacancypct24 - vacancypct14)/vacancypct14 *100))
   
   #merge
   vacancy <- density %>%
      left_join(vacancies_county %>% select(-STATE_NAME, -COUNTY_NAME, -housingunits14, -housingunits24),
                  by = "GEOID")
   
   #rename dataset
   density <- vacancy

# CROWDING -----------------------------------------------------------------
   crowding <- density %>%
      mutate(occ_housingunits14 = housingunits14 - vacancies14,
             occ_housingunits24 = housingunits24 - vacancies24,
             crowding14 = pop14/occ_housingunits14,
             crowding24 = pop24/occ_housingunits24,
             crowding_pctchg14to24 = (crowding24-crowding14)/crowding14 * 100)
   
   crowdingtest <- crowding %>%
      filter(crowding14 <= 8, crowding24 <= 8, occ_housingunits24 > 200)
   
   #rename dataset
   density <- crowding
   
# CONSTRUCTION--------------
   
   #import 
   construction <- read_excel(file.path(inputs, "Residential_construction_15to23.xlsx"))
   
   #analyze
   construction <- construction %>%
      mutate(SF_pct = 100 * SF_PERMITS_TOTAL/ALL_PERMITS_TOTAL,
             MF_pct = 100 * MF_PERMITS_TOTAL/ALL_PERMITS_TOTAL)
   
   #merge
   density_final <- decimals(density) %>%
      left_join(
         construction %>% select(GEOID, ALL_PERMITS_TOTAL, SF_PERMITS_TOTAL, MF_PERMITS_TOTAL, SF_pct, MF_pct), 
         by = "GEOID")
   
# ANALYSIS & TOP 100 DATABASES -------------------------------------------------------
   
   #Measure the correlation between pre-existing density and HU growth
   cor(density_hud$density_rank_10_start, density_hud$HU_pctchg14to24, use = "complete.obs")
   #result: 0.09100416. no relationship between pre-existing density and HU growth
   
   #Measure the correlation between pop growth and HU growth
   cor(density_hud$pop_pctchg14to24, density_hud$HU_pctchg14to24, use = "complete.obs")
   #result: 0.4181851. moderate positive relationship between pop growth and HU growth 
   
   #Calculate median county relative HU growth from 2014 to 2024
   median(density_hud$HU_pctchg14to24, na.rm = TRUE)
   #result: 6.237959%
   
   #Calculate median county absolute HU growth from 2014 to 2024
   median(density_hud$HU_pctchg14to24, na.rm = TRUE)
   #result: 6.237959%
   
#POPULATION change (PERCENT)   
   #Measure the correlation between pre-existing density and relative pop growth
   cor(density_final$density_rank_10_start, density_final$pop_pctchg14to24, use = "complete.obs")
   #result: 0.3676125 counties in higher density deciles saw moderately more relative pop growth from 2014 to 2024
   
   #Calculate median county pop growth from 2014 to 2024
   median(density_final$pop_pctchg14to24, na.rm = TRUE)
   #result: 0.8%. The median county barely grew at all in population.
   
   #Sort by top 100 pop change (percent)
   top_100_pop_pct <- density_final %>%
      arrange(desc(pop_pctchg14to24)) %>%
      slice(1:100)
   median10_top <- median(top_100_pop_pct$density_rank_10_start, na.rm = TRUE)
   median100_top <- median(top_100_pop_pct$density_rank_100_start, na.rm = TRUE)
   #result: 8, 78
   
   #Sort by bottom 100 pop change (percent)
   bottom_100_pop_pct <- density_final %>%
      arrange(pop_pctchg14to24) %>%
      slice(1:100)
   median10_bottom_pct <- median(bottom_100_pop_pct$density_rank_10_start, na.rm = TRUE)
   median100_botom_pct <- median(bottom_100_pop_pct$density_rank_100_start, na.rm = TRUE)
   #result: 3, 22    
   
#POPULATION change (NUMBER)   
   #Measure the correlation between pre-existing density and absolute pop growth
   cor(density_final$density_rank_10_start, density_final$pop_14to24, use = "complete.obs")
   #result: 0.3087454 counties in higher density deciles saw moderately more absolute pop growth from 2014 to 2024
   
   #Calculate median county pop growth from 2014 to 2024
   median(density_final$pop_14to24, na.rm = TRUE)
   #result: 126 people - the median county barely grew at all.
   
   #Sort by top 100 pop change (number)
   top_100_pop_num <- density_final %>%
      arrange(desc(pop_14to24)) %>%
      slice(1:100)
   median10_top_num <- median(top_100_pop_num$density_rank_10_start, na.rm = TRUE)
   median100_top_num <- median(top_100_pop_num$density_rank_100_start, na.rm = TRUE)
   #result: 10, 93
   
   #Sort by bottom 100 pop change (number)
   bottom_100_pop_num <- density_final %>%
      arrange(pop_14to24) %>%
      slice(1:100)
   median10_bottom_num <- median(bottom_100_pop_num$density_rank_10_start, na.rm = TRUE)
   median100_botom_num <- median(bottom_100_pop_num$density_rank_100_start, na.rm = TRUE)
   #result: 8, 78    
   
#HOUSING UNIT change (PERCENT)
   
   #Measure the correlation between pre-existing density and relative HU growth
   cor(density_final$density_rank_10_start, density_final$HU_pctchg14to24, use = "complete.obs")
   #result: 0.09100581 almost no relationship between pre-existing density and relative housing unit growth from 2014 to 2024
   
   #Calculate median county HU growth from 2014 to 2024
   median(density_final$HU_pctchg14to24, na.rm = TRUE)
   #result: 6.24%. The median county grew somewhat in population.
   
   #sort by top
      top_100_HU_pct <- density_final %>%
         arrange(desc(HU_pctchg14to24)) %>%
         slice(1:100)
      
   #calc medians top
      median_10 <- median(top_100_HU_pct$density_rank_10_start, na.rm = TRUE)
      median_100 <- median(top_100_HU_pct$density_rank_100_start, na.rm = TRUE)
      #result: 6, 54
      
   #sort by bottom
      bottom_100_HU_pct <- density_final %>%
         arrange(HU_pctchg14to24) %>%
         slice(1:100)
      
   #calc medians bottom
      median_10 <- median(bottom_100_HU_pct$density_rank_10_start, na.rm = TRUE)
      median_100 <- median(bottom_100_HU_pct$density_rank_100_start, na.rm = TRUE)
      #result: 2, 17
      
   #count top states
      top_100_HU_pct %>%
         count(STATE) %>%
         arrange(desc(n))
      
   #count bottom states
      bottom_100_HU_pct %>%
         count(STATE) %>%
         arrange(desc(n))
      
   #median density of top 100 vs all
      median_density_top_pct <- median(top_100_HU_pct$density14, na.rm = TRUE)
      median_density_final_pct <- median(density_final$density14, na.rm = TRUE)
      #result: Virtually no difference in the density between top percentage growth areas and the country as a whole. 49 ppl/sqmi vs 45 ppl/sqmi
      
#HU change (NUMBER)
      #sort by top
      top_100_HU_num <- density_final %>%
         arrange(desc(HU_14to24)) %>%
         slice(1:100)
      
      #calc medians top
      median_10 <- median(top_100_HU_num$density_rank_10_start, na.rm = TRUE)
      median_100 <- median(top_100_HU_num$density_rank_100_start, na.rm = TRUE)
      #result: 10, 94.5
      
      #sort by bottom
      bottom_100_HU_num <- density_final %>%
         arrange(HU_14to24) %>%
         slice(1:100)
      
      #calc medians bottom
      median_10 <- median(bottom_100_HU_num$density_rank_10_start, na.rm = TRUE)
      median_100 <- median(bottom_100_HU_num$density_rank_100_start, na.rm = TRUE)
      #result: 6, 50
      
      #count top states
      top_100_HU_num %>%
         count(STATE) %>%
         arrange(desc(n))
      
      #count bottom states
      bottom_100_HU_num %>%
         count(STATE) %>%
         arrange(desc(n))
      
      #median density of top 100 vs all
      median_density_top_num <- median(top_100_HU_num$density14, na.rm = TRUE)
      median_density_final_num <- median(density_final$density14, na.rm = TRUE)
      #result: HUGE difference in the initial density between top growth areas (by number) and the country as a whole. 740 ppl/sqmi vs 45  ppl/sqmi
      
#HU change - multifamily (NUMBER)
      top_100_HU_MF <- density_final %>%
         arrange(desc(HU_14to24)) %>%
         slice(1:100)
      
      median_MF_all <- median(density_final$MF_pct, na.rm = TRUE)
      median_MF_top <- median(top_100_HU_MF$MF_pct, na.rm = TRUE)
      #result: 10% multifamily development for the whole country vs 36% multifamily for the greatest in overall housing growth (numerically, not as a percent)
      
      #Sort by bottom 100 HU change
      bottom_100_HU_MF <- density_final %>%
         arrange(HU_14to24) %>%
         slice(1:100)
      
      median_MF_bottom <- median(bottom_100_HU_MF$MF_pct, na.rm = TRUE)
      #result: 5% MF development in the places with the least housing growth (numerically, not as a percent)
      
   
# RESULTS -----------------------------------------------------------------

#Correlation between pre-existing density and pop growth = 0.37. Higher-density counties had moderately more pop growth
#Correlation between pre-existing density and HU growth = 0.091. No relationship between pre-existing density and HU growth
#Correlation between pop growth and HU growth = 0.42. High pop-growth counties had moderately more HU growth from 2014 to 2024 
   
#Median county pop growth from 2014 to 2024 = 0.7%. The median county barely grew its population at all.
#Median county HU growth from 2014 to 2024 = 6.2%. The median county had some HU growth. The discrepancy is explained more by the fact that housing units are destroyed less often than populations decline. 

#Median density rank of top 100 counties with most pop growth = 8, 78
#Median density rank of bottom 100 counties with least pop growth = 3, 21
#Median density rank of top 100 counties with most HU growth = 6, 53.5
#Median density rank of bottom 100 counties with least HU growth = 2, 17
      
#Median initial density rank of top 100 counties with 

# GRAPHS AND VISUALS ------------------------------------------------------

#Housing Growth by Density (Totals)
      hu_totals <- density_final %>%
         mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
         group_by(density_rank_10_start) %>%
         summarize(
            total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
            num_counties = n()
         ) %>%
         ungroup() %>%
         mutate(
            share_of_total = round(total_HU_14to24 / sum(total_HU_14to24, na.rm = TRUE) * 100, 1)
         )
      
      overall_median <- median(hu_totals$total_HU_14to24, na.rm = TRUE)
      
      ggplot(hu_totals, aes(x = density_rank_10_start, y = total_HU_14to24)) +
         geom_col(fill = "forestgreen") +
         geom_hline(yintercept = overall_median, color = "black", size = 0.7) +
         scale_y_continuous(labels = comma_format()) +
         scale_x_discrete(drop = FALSE) +
         labs(
            title = "Net Housing Units Added (2014–2024) by Density Rank",
            subtitle = "Counties",
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
      
      ggsave(filename = file.path(graphs,"housingunits_density_added_counties.png"),width = 8, height = 6, dpi = 300)
      
      #export data for datawrapper chart
      write.csv(hu_totals, file = file.path(outputs,"chart1data.csv"),row.names = FALSE)
      

         
#-----------------------------Don't use these graphs-----------------------------#

         
#Population Growth vs Density Rank 
         plot(density$density_rank_10_start, density$pop_pctchg14to24,
              xlab = "Density Rank (1 = Least Dense, 10 = Most Dense)",
              ylab = "Population Change % (2014 to 2024)",
              main = "Population Growth vs. Density Rank")
         abline(lm(pop_pctchg14to24 ~ density_rank_10_start, data = density), col = "red")
         
#Housing Growth vs. Density Rank 
         plot(density_hud$density_rank_10_start, density_hud$HU_pctchg14to24,
              xlab = "Density Rank (1 = Least Dense, 10 = Most Dense)",
              ylab = "Housing Unit Growth % (2014 to 2024)",
              main = "Housing Growth vs. Density Rank")
         abline(lm(HU_pctchg14to24 ~ density_rank_10_start, data = density_hud), col = "red")
         
#Population Growth by Density (Line graph)
pop_medians <- density_final %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(median_pop_pctchg = median(pop_pctchg14to24, na.rm = TRUE)) %>%
   ungroup()


ggplot(pop_medians, aes(x = density_rank_10_start, y = median_pop_pctchg, group = 1)) +
   geom_line(color = "steelblue") +
   geom_point(color = "steelblue") +
   scale_x_discrete(drop = FALSE) +  # force showing all factor levels
   labs(
      title = "Median Population % Change (2014–2024) by Pre-Existing Density",
      x = "Density Rank in 2014",
      y = "Median % Change in Population"
   ) +
   theme_minimal()


#Housing Growth by Density (pct)
hu_medians <- density_final %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE)) %>%
   ungroup()
overall_median <- median(density_final$HU_pctchg14to24, na.rm = TRUE)

ggplot(hu_medians, aes(x = density_rank_10_start, y = median_hu_pctchg)) +
   geom_col(fill = "forestgreen") +
   geom_hline(yintercept = overall_median, color = "black", size = 0.6) +
   scale_x_discrete(drop = FALSE) +
   scale_y_continuous(
      breaks = pretty_breaks(),
      labels = label_percent(scale = 1, accuracy = 1)
   ) +
   labs(
      title = "Median % Change in Housing Units (2014–2024) by Density Rank",
      subtitle = "Counties",
      x = "Density Rank in 2014",
      y = "Median % Change in Housing"
   ) +
   theme_minimal() +
   theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
   )


#Population and Housing by Density (Line graph)
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
         "median_hu_pctchg" = "Housing Unit % Change"
      )
   ) +
   scale_y_continuous(
      limits = c(-5, NA),  # show y-axis starting at -5
      labels = label_percent(scale = 1)  # display as %, assuming values are already in percent units
   ) +
   labs(
      title = "Median % Change by Density (2014–2024)",
      x = "Density Rank in 2014",
      y = "Median % Change",
      color = NULL
   ) +
   theme_minimal()

# COUNTY-SPECIFIC ANALYSIS ------------------------------------------------------

#Essex County NJ
essex <- decimals(vacancies_tract) %>% 
   filter(GEOID == "34013") 

#Denver County CO
denver <- decimals(vacancies_county) %>% 
   filter(GEOID == "08031") 

#Harris County TX
harris <-  decimals(vacancies_county) %>% 
   filter(GEOID == "48201") 





