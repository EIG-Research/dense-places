# Project: Can Dense Places Still Build? 
# File Description: Data analysis

# Author: Jess Remington
#Creation Date: 04/21/25
# last update: 05/13/25


##########################################################################################################


#Clear current environment
rm(list = ls())

# Set base project path
base_path <- "ENTER YOUR PROJECT PATH HERE"
outputs <- file.path(base_path, "outputs")
graphs <- file.path(base_path, "graphs")

#import source code
source(file.path(base_path, "code/01.\ Code\ Environment.R"))

#import density datasets
density_counties <- read_excel(file.path(outputs, "county_density14to24.xlsx"))
density_final <- read.csv(file.path(outputs, "density_final.csv"))
density_final_withcounty <- read.csv(file.path(outputs, "density_final_withcounty.csv"))

# Analysis by tract density, counties -----------------------------------------------


#New Housing per 1000 Residents (20ths)
#Rank density in 20ths
density_20ths <- density_final %>%
   mutate(density_rank_20_start = ntile(density14, 20))%>%
   mutate(density_rank_20_end = ntile(density23, 20))

#percent share in counties with density 10
bin_share_county10 <- density_final_withcounty %>%
   group_by(density_rank_10_start) %>%
   summarize(
      total = n(),
      in_county10 = sum(density_rank_10_startcounty == 10, na.rm = TRUE),
      percent_in_county10 = in_county10 / total * 100
   ) %>%
   ungroup()

#tracts of density 3 by county density
tract3 <- density_final_withcounty %>%
   filter(density_rank_10_start == 3)

tract3summary <- tract3 %>%
  group_by(density_rank_10_startcounty) %>%
  summarize(
    n = n(),
    median_pop_change_pct = median(pop_change_pct, na.rm = TRUE),
    mean_pop_change_pct = mean(pop_change_pct, na.rm = TRUE),
    median_pop_14to23 = median(pop_14to23, na.rm = TRUE),
    mean_pop_14to23 = mean(pop_14to23, na.rm = TRUE),
    median_HU_pctchg14to24 = median(HU_pctchg14to24, na.rm = TRUE),
    mean_HU_pctchg14to24 = mean(HU_pctchg14to24, na.rm = TRUE),
    median_HU_14to24 = median(HU_14to24, na.rm = TRUE),
    mean_HU_14to24 = mean(HU_14to24, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    percent = n / sum(n) * 100
  ) %>%
  arrange(desc(n))


#median county density for each census tract
median_county_ranks <- density_final_withcounty %>%
   group_by(density_rank_10_start) %>%
   summarize(
      median_county_rank = median(density_rank_10_startcounty, na.rm = TRUE),
      .groups = "drop")

#percent of pop that lives in densest 10% of counties
pop_summary23 <- density_final_withcounty %>%
   summarize(
      total_pop = sum(pop23, na.rm = TRUE),
      pop_in_top_10_counties = sum(pop23[density_rank_10_startcounty == 10], na.rm = TRUE)
   ) %>%
   mutate(percent_in_top_10 = pop_in_top_10_counties / total_pop * 100)
#59% of Americans lived in the densest 10% of counties in 2023

pop_summary14 <- density_final_withcounty %>%
   summarize(
      total_pop = sum(pop14, na.rm = TRUE),
      pop_in_top_10_counties = sum(pop14[density_rank_10_startcounty == 10], na.rm = TRUE)
   ) %>%
   mutate(percent_in_top_10 = pop_in_top_10_counties / total_pop * 100)
#59% of Americans lived in the densest 10% of counties in 2014


dense_growth <- density_final %>%
   filter(density_rank_20_start > 17, !is.na(HU_14to24))

correlation <- dense_growth %>%
   filter(!is.na(HU_14to24), !is.na(SFH_detached)) %>%
   summarize(correlation = cor(HU_14to24, SFH_detached, method = "pearson"))
correlation

cor.test(dense_growth$HU_14to24, dense_growth$SFH_detached)

within_bucket_correlations <- density_final %>%
   filter(!is.na(HU_14to24), !is.na(missing_middle), !is.na(density_rank_20_start)) %>%
   group_by(density_rank_20_start) %>%
   summarize(
      correlation = cor(HU_14to24, missing_middle, method = "pearson"),
      .groups = "drop"
   )

within_bucket_correlations

# STATES ------------------------------------------------------------------


#Median State-level density
state_density_medians <- density_final %>%
   group_by(STATE_NAME) %>%
   summarize(
      median_density23 = median(density23, na.rm = TRUE),
      .groups = "drop"
   ) %>%
   arrange(desc(median_density23))

#Texas
texas_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "Texas"], na.rm = TRUE)
texas_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "Texas"], na.rm = TRUE)
texas_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "Texas"], na.rm = TRUE)

texas <- density_final %>%
   filter(STATE_NAME == "Texas") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / texas_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / texas_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / texas_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )
texas <- decimals(texas)

#New York
newyork_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "New York"], na.rm = TRUE)
newyork_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "New York"], na.rm = TRUE)
newyork_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "New York"], na.rm = TRUE)

newyork <- density_final %>%
   filter(STATE_NAME == "New York") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / newyork_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / newyork_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / newyork_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )
newyork <- decimals(newyork)

#Arizona
arizona_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "Arizona"], na.rm = TRUE)
arizona_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "Arizona"], na.rm = TRUE)
arizona_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "Arizona"], na.rm = TRUE)

arizona <- density_final %>%
   filter(STATE_NAME == "Arizona") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / arizona_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / arizona_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / arizona_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )
arizona <- decimals(arizona)

#DC
dc_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "District of Columbia"], na.rm = TRUE)
dc_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "District of Columbia"], na.rm = TRUE)
dc_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "District of Columbia"], na.rm = TRUE)

dc <- density_final %>%
   filter(STATE_NAME == "District of Columbia") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / dc_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / dc_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / dc_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )

dc <- decimals(dc)

#California
california_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "California"], na.rm = TRUE)
california_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "California"], na.rm = TRUE)
california_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "California"], na.rm = TRUE)

california <- density_final %>%
   filter(STATE_NAME == "California") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / california_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / california_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / california_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )

california <- decimals(california)

#Florida
florida_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "Florida"], na.rm = TRUE)
florida_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "Florida"], na.rm = TRUE)
florida_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "Florida"], na.rm = TRUE)

florida <- density_final %>%
   filter(STATE_NAME == "Florida") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / florida_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / florida_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / florida_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )

florida <- decimals(florida)

#New Jersey
newjersey_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "New Jersey"], na.rm = TRUE)
newjersey_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "New Jersey"], na.rm = TRUE)
newjersey_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "New Jersey"], na.rm = TRUE)

newjersey <- density_final %>%
   filter(STATE_NAME == "New Jersey") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / newjersey_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / newjersey_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / newjersey_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )

newjersey <- decimals(newjersey)

#Massachusetts
massachusetts_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "Massachusetts"], na.rm = TRUE)
massachusetts_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "Massachusetts"], na.rm = TRUE)
massachusetts_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "Massachusetts"], na.rm = TRUE)

massachusetts <- density_final %>%
   filter(STATE_NAME == "Massachusetts") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / massachusetts_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / massachusetts_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / massachusetts_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )

massachusetts <- decimals(massachusetts)

#Colorado
colorado_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "Colorado"], na.rm = TRUE)
colorado_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "Colorado"], na.rm = TRUE)
colorado_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "Colorado"], na.rm = TRUE)

colorado <- density_final %>%
   filter(STATE_NAME == "Colorado") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / colorado_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / colorado_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / colorado_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )

colorado <- decimals(colorado)


# New Housing per 1000 Residents (20ths) - BY STATE -----------------------

#Texas
newhousing_5pct_texas <- density_20ths %>%
   filter(STATE_NAME=="Texas") %>%
   mutate(density_rank_20_start = factor(density_rank_20_start, levels = 1:20)) %>%
   group_by(density_rank_20_start) %>%
   summarize(
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      total_pop14 = sum(pop14, na.rm = TRUE),
      new_housing_per_res = round((total_HU_14to24 / total_pop14) * 1000),
      avg_density14 = mean(density14, na.rm = TRUE),
      .groups = "drop")

#New York
newhousing_5pct_newyork <- density_20ths %>%
   filter(STATE_NAME=="New York") %>%
   mutate(density_rank_20_start = factor(density_rank_20_start, levels = 1:20)) %>%
   group_by(density_rank_20_start) %>%
   summarize(
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      total_pop14 = sum(pop14, na.rm = TRUE),
      new_housing_per_res = round((total_HU_14to24 / total_pop14) * 1000),
      avg_density14 = mean(density14, na.rm = TRUE),
      .groups = "drop")


#Florida
newhousing_5pct_florida <- density_20ths %>%
   filter(STATE_NAME=="Florida") %>%
   mutate(density_rank_20_start = factor(density_rank_20_start, levels = 1:20)) %>%
   group_by(density_rank_20_start) %>%
   summarize(
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      total_pop14 = sum(pop14, na.rm = TRUE),
      new_housing_per_res = round((total_HU_14to24 / total_pop14) * 1000),
      avg_density14 = mean(density14, na.rm = TRUE),
      .groups = "drop")


#California
newhousing_5pct_california <- density_20ths %>%
   filter(STATE_NAME=="California") %>%
   mutate(density_rank_20_start = factor(density_rank_20_start, levels = 1:20)) %>%
   group_by(density_rank_20_start) %>%
   summarize(
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      total_pop14 = sum(pop14, na.rm = TRUE),
      new_housing_per_res = round((total_HU_14to24 / total_pop14) * 1000),
      avg_density14 = mean(density14, na.rm = TRUE),
      .groups = "drop")

#Hawaii
newhousing_5pct_hawaii <- density_20ths %>%
   filter(STATE_NAME=="Hawaii") %>%
   mutate(density_rank_20_start = factor(density_rank_20_start, levels = 1:20)) %>%
   group_by(density_rank_20_start) %>%
   summarize(
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      total_pop14 = sum(pop14, na.rm = TRUE),
      new_housing_per_res = round((total_HU_14to24 / total_pop14) * 1000),
      avg_density14 = mean(density14, na.rm = TRUE),
      .groups = "drop")

   
#Colorado
   newhousing_5pct_colorado <- density_20ths %>%
      filter(STATE_NAME=="Colorado") %>%
      mutate(density_rank_20_start = factor(density_rank_20_start, levels = 1:20)) %>%
      group_by(density_rank_20_start) %>%
      summarize(
         total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
         total_pop14 = sum(pop14, na.rm = TRUE),
         new_housing_per_res = round((total_HU_14to24 / total_pop14) * 1000),
         avg_density14 = mean(density14, na.rm = TRUE),
         .groups = "drop")
   
   
#Arizona
   newhousing_5pct_arizona <- density_20ths %>%
      filter(STATE_NAME=="Arizona") %>%
      mutate(density_rank_20_start = factor(density_rank_20_start, levels = 1:20)) %>%
      group_by(density_rank_20_start) %>%
      summarize(
         total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
         total_pop14 = sum(pop14, na.rm = TRUE),
         new_housing_per_res = round((total_HU_14to24 / total_pop14) * 1000),
         avg_density14 = mean(density14, na.rm = TRUE),
         .groups = "drop" )
   
   
#Washington
newhousing_5pct_washington <- density_20ths %>%
   filter(STATE_NAME=="Washington") %>%
   mutate(density_rank_20_start = factor(density_rank_20_start, levels = 1:20)) %>%
   group_by(density_rank_20_start) %>%
   summarize(
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      total_pop14 = sum(pop14, na.rm = TRUE),
      new_housing_per_res = round((total_HU_14to24 / total_pop14) * 1000),
      avg_density14 = mean(density14, na.rm = TRUE),
      .groups = "drop")


#DC
newhousing_5pct_dc <- density_20ths %>%
   filter(STATE_NAME=="District of Columbia",
          density_rank_20_start > 10) %>%
   mutate(density_rank_20_start = factor(density_rank_20_start, levels = 1:20)) %>%
   group_by(density_rank_20_start) %>%
   summarize(
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      total_pop14 = sum(pop14, na.rm = TRUE),
      new_housing_per_res = round((total_HU_14to24 / total_pop14) * 1000),
      avg_density14 = mean(density14, na.rm = TRUE),
      .groups = "drop")

#Georgia
newhousing_5pct_georgia <- density_20ths %>%
   filter(STATE_NAME=="Georgia") %>%
   mutate(density_rank_20_start = factor(density_rank_20_start, levels = 1:20)) %>%
   group_by(density_rank_20_start) %>%
   summarize(
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      total_pop14 = sum(pop14, na.rm = TRUE),
      new_housing_per_res = round((total_HU_14to24 / total_pop14) * 1000),
      avg_density14 = mean(density14, na.rm = TRUE),
      .groups = "drop")

#Minnesota
newhousing_5pct_minnesota <- density_20ths %>%
   filter(STATE_NAME=="Minnesota") %>%
   mutate(density_rank_20_start = factor(density_rank_20_start, levels = 1:20)) %>%
   group_by(density_rank_20_start) %>%
   summarize(
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      total_pop14 = sum(pop14, na.rm = TRUE),
      new_housing_per_res = round((total_HU_14to24 / total_pop14) * 1000),
      avg_density14 = mean(density14, na.rm = TRUE),
      .groups = "drop")


#LINE GRAPH

newhousing_5pct_combined <- bind_rows(
   newhousing_5pct_texas  %>% mutate(STATE_NAME = "Texas"),
   newhousing_5pct_newyork %>% mutate(STATE_NAME = "New York"),
   newhousing_5pct_arizona %>% mutate(STATE_NAME = "Arizona"),
   newhousing_5pct_washington %>% mutate(STATE_NAME = "Washington"),
   newhousing_5pct_georgia %>% mutate(STATE_NAME = "Georgia"),
   newhousing_5pct_california %>% mutate(STATE_NAME = "California")
)

ggplot(newhousing_5pct_combined, aes(x = as.integer(as.character(density_rank_20_start)), 
                                     y = new_housing_per_res, 
                                     color = STATE_NAME)) +
   geom_line(size = 1.2) +
   geom_point(size = 2) +
   scale_x_continuous(
      breaks = c(1, 10, 20),
      labels = c("5%\n(Least Dense)", "50%", "95%\n(Densest)")
   ) +
   scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.1))) +
   scale_color_manual(
      values = c(
         "Texas" = "red",
         "Arizona" = "pink",
         "New York" = "steelblue",
         "California" = "darkgrey",
         "Washington" = "forestgreen",
         "Georgia" = "darkorange2"
      )
   ) +
   labs(
      title = "New Housing Built per 1,000 Residents by Pre-Existing Density",
      x = "National Density Rank",
      y = "Housing Units Built from 2014 to 2024",
      color = "State"
   ) +
   theme_minimal() +
   theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      legend.title = element_text(face = "bold"),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(size = 9)
   )
ggsave(filename = file.path(graphs, "Seattle_builds_alot_in_cities.png"),width = 8, height = 6, dpi = 300)

write.csv(newhousing_5pct_combined, file = file.path(outputs, "chart5data.csv"),row.names = FALSE)




# New Housing per 1000 Residents (20ths) - MULTIFAMILY SHARE --------------

#apartments
   # Step 1: Classify each tract as High or Low Apartment Share (based on median)
   threshold <- median(density_20ths$apartments, na.rm = TRUE)
   
   bar_data <- density_20ths %>%
      filter(!is.na(HU_14to24), !is.na(pop14), !is.na(apartments), pop14 > 0) %>%
      mutate(
         apartment_group = factor(
            if_else(apartments > median(apartments, na.rm = TRUE),
                    "High Apartment Share", "Low Apartment Share"),
            levels = c("Low Apartment Share", "High Apartment Share")  # ensure order
         ),
         density_rank_20_start = factor(density_rank_20_start, levels = 1:20)
      ) %>%
      group_by(density_rank_20_start, apartment_group) %>%
      summarize(
         total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
         total_pop14 = sum(pop14, na.rm = TRUE),
         new_housing_per_res = round((total_HU_14to24 / total_pop14) * 1000),
         .groups = "drop"
      )
   
   #Step 2: Graph
   ggplot(bar_data, aes(x = density_rank_20_start, y = new_housing_per_res, fill = apartment_group)) +
      geom_col(position = "dodge") +
      scale_fill_manual(
         values = c(
            "Low Apartment Share" = "steelblue",
            "High Apartment Share" = "coral")) +
      scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.1))) +
      scale_x_discrete(labels = function(x) {
         case_when(
            x == "1"  ~ "5%\n(Least Dense)",
            x == "10" ~ "50%",
            x == "20" ~ "95%\n(Densest)",
            TRUE      ~ "")}) +
      labs(
         title = "New Housing Built per 1,000 Residents by Pre-Existing Density and Apartment Share",
         x = "National Density Rank",
         y = "Housing Units Built per 1,000 Residents from 2014 to 2024",
         fill = "Apartment Share"
      ) +
      theme_minimal() +
      theme(
         plot.title = element_text(face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 10, hjust = 0.5),
         legend.position = "bottom",
         plot.background = element_rect(fill = "white", color = NA),
         panel.background = element_rect(fill = "white", color = NA),
         axis.text.x = element_text(size = 9)
      )
   
   ggsave(filename = file.path(graphs, "New Housing Units per 1,000 Residents by Apartment Share.png"),width = 10, height = 8, dpi = 300)

#SFH DETACHED
   
   # Step 1: Classify each tract as High or Low SFH-Detached Share (based on median)
   threshold <- median(density_20ths$SFH_detached, na.rm = TRUE)
   
   bar_data <- density_20ths %>%
      filter(!is.na(HU_14to24), !is.na(pop14), !is.na(SFH_detached), pop14 > 0) %>%
      mutate(
         sfhd_group = factor(
            if_else(SFH_detached > median(SFH_detached, na.rm = TRUE),
                    "High Share SFH Detached", "Low Share SFH Detached"),
            levels = c("Low Share SFH Detached", "High Share SFH Detached")  # ensure order
         ),
         density_rank_20_start = factor(density_rank_20_start, levels = 1:20)
      ) %>%
      group_by(density_rank_20_start, sfhd_group) %>%
      summarize(
         total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
         total_pop14 = sum(pop14, na.rm = TRUE),
         new_housing_per_res = round((total_HU_14to24 / total_pop14) * 1000),
         .groups = "drop"
      )
   
   #Step 2: Graph
   ggplot(bar_data, aes(x = density_rank_20_start, y = new_housing_per_res, fill = sfhd_group)) +
      geom_col(position = "dodge") +
      scale_fill_manual(
         values = c(
            "Low Share SFH Detached" = "steelblue",
            "High Share SFH Detached" = "coral")) +
      scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.1))) +
      scale_x_discrete(labels = function(x) {
         case_when(
            x == "1"  ~ "5%\n(Least Dense)",
            x == "10" ~ "50%",
            x == "20" ~ "95%\n(Densest)",
            TRUE      ~ "")}) +
      labs(
         title = "New Housing Built per 1,000 Residents by Density and Share of SFH Detached",
         x = "National Density Rank",
         y = "Housing Units Built per 1,000 Residents from 2014 to 2024",
         fill = "SFH Detached Share"
      ) +
      theme_minimal() +
      theme(
         plot.title = element_text(face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 10, hjust = 0.5),
         legend.position = "bottom",
         plot.background = element_rect(fill = "white", color = NA),
         panel.background = element_rect(fill = "white", color = NA),
         axis.text.x = element_text(size = 9)
      )

#COMBINED
   bar_data3 <- density_final %>%
      filter(!is.na(HU_14to24), !is.na(pop14), pop14 > 0) %>%
      group_by(density_rank_10_start) %>%
      mutate(
         high_SFH = SFH_detached > median(SFH_detached, na.rm = TRUE),
         high_middle = missing_middle > median(missing_middle, na.rm = TRUE),
         high_apts = apartments > median(apartments, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
         housing_type_group = factor(case_when(
            high_SFH ~ "SFH Detached",
            high_middle ~ "'Missing' Middle",
            high_apts ~ "Apartments",
            TRUE ~ NA_character_
         ), levels = c("SFH Detached", "'Missing' Middle", "Apartments")),
         density_rank_10_start = factor(density_rank_10_start, levels = 1:20)
      ) %>%
      filter(!is.na(housing_type_group)) %>%
      group_by(density_rank_10_start, housing_type_group) %>%
      summarize(
         total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
         total_pop14 = sum(pop14, na.rm = TRUE),
         new_housing_per_res = round((total_HU_14to24 / total_pop14) * 1000),
         .groups = "drop"
      )
   
   ggplot(bar_data3, aes(x = density_rank_10_start, y = new_housing_per_res, fill = housing_type_group)) +
      geom_col(position = "dodge") +
      scale_fill_manual(
         values = c(
            "SFH Detached" = "steelblue",
            "'Missing' Middle" = "seagreen",
            "Apartments" = "coral2"
         )
      ) +
      scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.1))) +
      scale_x_discrete(labels = function(x) {
         case_when(
            x == "1"  ~ "10%\n(Least Dense)",
            x == "5" ~ "50%",
            x == "10" ~ "90%\n(Densest)",
            TRUE      ~ ""
         )
      }) +
      labs(
         title = "New Housing Built per 1,000 Residents by Pre-Existing Density",
         x = "National Density Rank",
         y = "Housing Units Built per 1,000 Residents from 2014 to 2024",
         fill = "Neighborhoods with a high share of:"
      ) +
      theme_minimal() +
      theme(
         plot.title = element_text(face = "bold", hjust = 0.5),
         legend.position = "bottom",
         axis.text.x = element_text(size = 9),
         plot.background = element_rect(fill = "white", color = NA),
         panel.background = element_rect(fill = "white", color = NA)
      )
   
   ggsave(filename = file.path(graphs, "New Housing Units per 1,000 Residents by Housing Type.png"),width = 10, height = 8, dpi = 300)

# Notable counties  ---------------------------------------------------------

#Houston
houston_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "Texas" & density_final$COUNTY_NAME == "Harris County"], na.rm = TRUE)
houston_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "Texas" & density_final$COUNTY_NAME == "Harris County"], na.rm = TRUE)
houston_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "Texas" & density_final$COUNTY_NAME == "Harris County"], na.rm = TRUE)

houston <- density_final %>%
   filter(STATE_NAME == "Texas" & COUNTY_NAME == "Harris County") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      density_rank_10_end = median(density_rank_10_end, na.rm = TRUE),
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / houston_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / houston_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / houston_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )
houston <- decimals(houston)

#Austin
austin_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "Texas" & density_final$COUNTY_NAME == "Travis County"], na.rm = TRUE)
austin_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "Texas" & density_final$COUNTY_NAME == "Travis County"], na.rm = TRUE)
austin_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "Texas" & density_final$COUNTY_NAME == "Travis County"], na.rm = TRUE)

austin <- density_final %>%
   filter(STATE_NAME == "Texas" & COUNTY_NAME == "Travis County") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      density_rank_10_end = median(density_rank_10_end, na.rm = TRUE),
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / austin_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / austin_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / austin_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )
austin <- decimals(austin)

#Dallas
dallas_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "Texas" & density_final$COUNTY_NAME == "Dallas County"], na.rm = TRUE)
dallas_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "Texas" & density_final$COUNTY_NAME == "Dallas County"], na.rm = TRUE)
dallas_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "Texas" & density_final$COUNTY_NAME == "Dallas County"], na.rm = TRUE)

dallas <- density_final %>%
   filter(STATE_NAME == "Texas" & COUNTY_NAME == "Dallas County") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      density_rank_10_end = median(density_rank_10_end, na.rm = TRUE),
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / dallas_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / dallas_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / dallas_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )
dallas <- decimals(dallas)

#San Antonio
sanantonio_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "Texas" & density_final$COUNTY_NAME == "Bexar County"], na.rm = TRUE)
sanantonio_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "Texas" & density_final$COUNTY_NAME == "Bexar County"], na.rm = TRUE)
sanantonio_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "Texas" & density_final$COUNTY_NAME == "Bexar County"], na.rm = TRUE)

sanantonio <- density_final %>%
   filter(STATE_NAME == "Texas" & COUNTY_NAME == "Bexar County") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      density_rank_10_end = median(density_rank_10_end, na.rm = TRUE),
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / sanantonio_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / sanantonio_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / sanantonio_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )
sanantonio <- decimals(sanantonio)

#Ft Worth
ftworth_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "Texas" & density_final$COUNTY_NAME == "Tarrant County"], na.rm = TRUE)
ftworth_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "Texas" & density_final$COUNTY_NAME == "Tarrant County"], na.rm = TRUE)
ftworth_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "Texas" & density_final$COUNTY_NAME == "Tarrant County"], na.rm = TRUE)

ftworth <- density_final %>%
   filter(STATE_NAME == "Texas" & COUNTY_NAME == "Tarrant County") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      density_rank_10_end = median(density_rank_10_end, na.rm = TRUE),
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / ftworth_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / ftworth_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / ftworth_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )
ftworth <- decimals(ftworth)


#Denver
denver_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "Colorado" & density_final$COUNTY_NAME == "Denver County"], na.rm = TRUE)
denver_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "Colorado" & density_final$COUNTY_NAME == "Denver County"], na.rm = TRUE)
denver_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "Colorado" & density_final$COUNTY_NAME == "Denver County"], na.rm = TRUE)

denver <- density_final %>%
   filter(STATE_NAME == "Colorado" & COUNTY_NAME == "Denver County") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      density_rank_10_end = median(density_rank_10_end, na.rm = TRUE),
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / denver_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / denver_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / denver_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )
denver <- decimals(denver)

#Miami
miami_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "Florida" & density_final$COUNTY_NAME == "Miami-Dade County"], na.rm = TRUE)
miami_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "Florida" & density_final$COUNTY_NAME == "Miami-Dade County"], na.rm = TRUE)
miami_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "Florida" & density_final$COUNTY_NAME == "Miami-Dade County"], na.rm = TRUE)

miami <- density_final %>%
   filter(STATE_NAME == "Florida" & COUNTY_NAME == "Miami-Dade County") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      density_rank_10_end = median(density_rank_10_end, na.rm = TRUE),
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / miami_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / miami_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / miami_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )
miami <- decimals(miami)


#Brooklyn
brooklyn_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "New York" & density_final$COUNTY_NAME == "Kings County"], na.rm = TRUE)
brooklyn_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "New York" & density_final$COUNTY_NAME == "Kings County"], na.rm = TRUE)
brooklyn_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "New York" & density_final$COUNTY_NAME == "Kings County"], na.rm = TRUE)

brooklyn <- density_final %>%
   filter(STATE_NAME == "New York" & COUNTY_NAME == "Kings County") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      density_rank_10_end = median(density_rank_10_end, na.rm = TRUE),
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / brooklyn_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / brooklyn_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / brooklyn_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )
brooklyn <- decimals(brooklyn)

#Raleigh
raleigh_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "North Carolina" & density_final$COUNTY_NAME == "Wake County"], na.rm = TRUE)
raleigh_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "North Carolina" & density_final$COUNTY_NAME == "Wake County"], na.rm = TRUE)
raleigh_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "North Carolina" & density_final$COUNTY_NAME == "Wake County"], na.rm = TRUE)

raleigh <- density_final %>%
   filter(STATE_NAME == "North Carolina" & COUNTY_NAME == "Wake County") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      density_rank_10_end = median(density_rank_10_end, na.rm = TRUE),
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / raleigh_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / raleigh_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / raleigh_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )
raleigh <- decimals(raleigh)

#durham
durham_hu <- sum(density_final$HU_14to24[density_final$STATE_NAME == "North Carolina" & density_final$COUNTY_NAME == "Durham County"], na.rm = TRUE)
durham_pop14 <- sum(density_final$pop14[density_final$STATE_NAME == "North Carolina" & density_final$COUNTY_NAME == "Durham County"], na.rm = TRUE)
durham_pop23 <- sum(density_final$pop23[density_final$STATE_NAME == "North Carolina" & density_final$COUNTY_NAME == "Durham County"], na.rm = TRUE)

durham <- density_final %>%
   filter(STATE_NAME == "North Carolina" & COUNTY_NAME == "Durham County") %>%
   mutate(density_rank_10_start = factor(density_rank_10_start, levels = 1:10)) %>%
   group_by(density_rank_10_start) %>%
   summarize(
      density_rank_10_end = median(density_rank_10_end, na.rm = TRUE),
      median_hu_pctchg = median(HU_pctchg14to24, na.rm = TRUE),
      total_HU_14to24 = sum(HU_14to24, na.rm = TRUE),
      pct_decile = total_HU_14to24 / durham_hu * 100,
      pop_decile14 = sum(pop14, na.rm = TRUE) / durham_pop14 * 100,
      pop_decile23 = sum(pop23, na.rm = TRUE) / durham_pop23 * 100,
      median_density14 = median(density14, na.rm = TRUE),
      median_density23 = median(density23, na.rm = TRUE),
      density_chg = (median_density23 - median_density14)/median_density14 * 100,
      housing_per_res = total_HU_14to24/sum(pop14, na.rm = TRUE) * 1000,
      .groups = "drop"
   )
durham <- decimals(durham)



#New housing per 1,000 residents by county
density_counties <- density_counties %>%
   mutate(housing_per_res = HU_14to24/pop14 * 1000)

#New housing per 1,000 residents for MAJOR METROS ONLY
density_counties_bigcitiesonly <- density_counties %>%
   filter(density_rank_100_start > 94)

#
density_final_withcounty <- density_final_withcounty %>%
   mutate(housing_per_res = HU_14to24/pop14 * 1000)

#Density typologies - NEED TO EDIT
tracts_summary <- density_final_withcounty %>%
   filter(!is.na(STATE_NAME)) %>%  # remove missing STATE_NAME first
   group_by(STATE_NAME) %>%
   summarize(
      total_tracts = n(),
      vhighdensitycity = round(sum(density23 > 25000, na.rm = TRUE) / total_tracts * 100),
      highdensitycity = round(sum(density23 > 15000, na.rm = TRUE) / total_tracts * 100),
      middensitycity = round(sum(density23 > 9000 & density23 < 15000, na.rm = TRUE) / total_tracts * 100),
      highdensitysuburb = round(sum(density23 > 6000 & density23 < 9000, na.rm = TRUE) / total_tracts * 100),
      middensitysuburb = round(sum(density23 > 2000 & density23 < 6000, na.rm = TRUE) / total_tracts * 100),
      lowdensitysuburb = round(sum(density23 > 800 & density23 < 2000, na.rm = TRUE) / total_tracts * 100),
      exurb = round(sum(density23 > 300 & density23 < 800, na.rm = TRUE) / total_tracts * 100),
      rural = round(sum(density23 < 300, na.rm = TRUE) / total_tracts * 100)
   ) %>%
   arrange(desc(total_tracts))

#Housing growth by density typology - STATES
housing_growth_summary <- density_final_withcounty %>%
   filter(!is.na(STATE_NAME), !is.na(HU_14to24)) %>%
   group_by(STATE_NAME) %>%
   summarize(
      total_growth = sum(HU_14to24, na.rm = TRUE),
      total_pop14 = sum(pop14, na.rm = TRUE),
      new_housing_per_res = round(total_growth/total_pop14 * 1000),
      
      highdensitycity = round(sum(HU_14to24[density14 > 15000], na.rm = TRUE) / total_growth * 100, 1),
      middensitycity = round(sum(HU_14to24[density14 > 9000 & density14 <= 15000], na.rm = TRUE) / total_growth * 100, 1),
      highdensitysuburb = round(sum(HU_14to24[density14 > 6000 & density14 <= 9000], na.rm = TRUE) / total_growth * 100, 1),
      middensitysuburb = round(
         sum(ifelse(
            (density14 > 2000 & density14 <= 6000) |
               (density14 < 2000 & density_rank_10_startcounty == 10),
            HU_14to24, 0
         ), na.rm = TRUE) / total_growth * 100, 1),
      lowdensityexurb = round(
         sum(ifelse(
            (density14 >= 300 & density14 <= 2000 & density_rank_10_startcounty < 10)|
               (density14 < 300 & density_rank_10_startcounty %in% c(8, 9)),
            HU_14to24, 0
         ), na.rm = TRUE) / total_growth * 100, 1),
      rural = round(
         sum(ifelse(
            density14 < 300 & density_rank_10_startcounty < 7,
            HU_14to24, 0
         ), na.rm = TRUE) / total_growth * 100, 1)
   ) %>%
   arrange(desc(total_growth))

#Housing growth by density typology - BIG COUNTIES
housing_growth_summary_bigcounty <- density_final_withcounty %>%
   filter(!is.na(COUNTY_NAME), !is.na(HU_14to24)) %>%
   group_by(COUNTY_NAME, STATE_NAME) %>%
   summarize(
      total_growth = sum(HU_14to24, na.rm = TRUE),
      total_pop14 = sum(pop14, na.rm = TRUE),
      new_housing_per_res = round(total_growth/total_pop14 * 1000),
      
   ) %>%
   filter(total_pop14 > 400000) %>%
   arrange(desc(new_housing_per_res))

housing_growth_summary_bigcounty <- housing_growth_summary_bigcounty %>%
   mutate(CITY = case_when(
      COUNTY_NAME == "Harris County" ~ "Houston",
      COUNTY_NAME == "Wake County" ~ "Raleigh",
      COUNTY_NAME == "Durham County" ~ "Durham",
      COUNTY_NAME == "Travis County" ~ "Austin",
      COUNTY_NAME == "Dallas County" ~ "Dallas",
      COUNTY_NAME == "Tarrant County" ~ "Fort Worth",
      COUNTY_NAME == "Bexar County" ~ "San Antonio",
      COUNTY_NAME == "Maricopa County" ~ "Phoenix",
      COUNTY_NAME == "Los Angeles County" ~ "Los Angeles",
      COUNTY_NAME == "Mecklenburg County" ~ "Charlotte",
      COUNTY_NAME == "Williamson County" ~ "Austin suburb",
      COUNTY_NAME == "Collin County" ~ "Plano",
      COUNTY_NAME == "King County" ~ "Seattle",
      COUNTY_NAME == "Kings County" ~ "Brooklyn",
      COUNTY_NAME == "Clark County" ~ "Las Vegas",
      COUNTY_NAME == "Miami-Dade County" ~ "Miami",
      COUNTY_NAME == "Denton County" ~ "Dallas/Ft Worth suburb",
      COUNTY_NAME == "Orange County" ~ "Orlando",
      COUNTY_NAME == "Hillsborough County" ~ "Tampa",
      COUNTY_NAME == "Cook County" ~ "Chicago",
      COUNTY_NAME == "Polk County" ~ "Orlando/Tampa exurb",
      COUNTY_NAME == "Montgomery County" ~ "Houston suburb",
      COUNTY_NAME == "Fort Bend County" ~ "Houston suburb",
      COUNTY_NAME == "Ada County" ~ "Boise",
      COUNTY_NAME == "Montgomery County" ~ "Houston suburb",
      COUNTY_NAME == "Lee County" ~ "Fort Myers",
      COUNTY_NAME == "Pasco County" ~ "Tampa suburb",
      COUNTY_NAME == "Utah County" ~ "Provo",
      COUNTY_NAME == "Davidson County" ~ "Nashville",
      COUNTY_NAME == "District of Columbia" ~ "Washington, DC",
      COUNTY_NAME == "Greenville County" ~ "Greenville",
      COUNTY_NAME == "Denver County" ~ "Denver",
      COUNTY_NAME == "Davidson County" ~ "Nashville",
      COUNTY_NAME == "Dane County" ~ "Madison",
      TRUE ~ NA_character_
   )) %>%
   select(COUNTY_NAME, STATE_NAME, CITY, everything())

# Check your column and sort manually
housing_growth_summary_bigcounty <- housing_growth_summary_bigcounty %>% ungroup()

housing_growth_top20 <- housing_growth_summary_bigcounty %>%
   filter(!is.na(new_housing_per_res)) %>%
   arrange(desc(new_housing_per_res)) %>%
   slice(1:20)

#export data for datawrapper chart
write.csv(housing_growth_summary_bigcounty, file = file.path(outputs,"chart4data.csv"),row.names = FALSE)

   
   
   
   
   
      
   
   
   
#don't use, not useful (quintiles)
      #Housing growth by density rank quintiles - BIG COUNTIES
      #Rank density in quintiles
      
      density_quintiles <- density_final_withcounty %>%
         filter(!is.na(density14)) %>%
         mutate(
            density_rank_5_start = ntile(density14, 5),
            density_quintile_label = case_when(
               density_rank_5_start == 1 ~ "Very Low Density",
               density_rank_5_start == 2 ~ "Low Density",
               density_rank_5_start == 3 ~ "Moderate Density",
               density_rank_5_start == 4 ~ "High Density",
               density_rank_5_start == 5 ~ "Very High Density"
            )
         )
      
      housing_growth_summary_county2 <- density_quintiles %>%
         filter(!is.na(COUNTY_NAME), !is.na(HU_14to24)) %>%
         group_by(COUNTY_NAME, STATE_NAME) %>%
         summarize(
            total_growth = sum(HU_14to24, na.rm = TRUE),
            total_pop14 = sum(pop14, na.rm = TRUE),
            new_housing_per_res = round(total_growth/total_pop14 * 1000),
            quintile_5 = round(sum(HU_14to24[density_rank_5_start == 5], na.rm = TRUE) / total_growth * 100, 1),
            quintile_4 = round(sum(HU_14to24[density_rank_5_start == 4], na.rm = TRUE) / total_growth * 100, 1),
            quintile_3 = round(sum(HU_14to24[density_rank_5_start == 3], na.rm = TRUE) / total_growth * 100, 1),
            quintile_2 = round(sum(HU_14to24[density_rank_5_start == 2], na.rm = TRUE) / total_growth * 100, 1),
            quintile_1 = round(sum(HU_14to24[density_rank_5_start == 1], na.rm = TRUE) / total_growth * 100, 1),
         ) %>%
         filter(total_pop14 > 400000) %>%
         arrange(desc(total_growth))
      
      housing_growth_summary_county2 <- housing_growth_summary_county2 %>%
         rename(
            `Very Low Density` = quintile_1,
            `Low Density` = quintile_2,
            `Moderate Density` = quintile_3,
            `High Density` = quintile_4,
            `Very High Density` = quintile_5
         ) %>%
         mutate(CITY = case_when(
            COUNTY_NAME == "Harris County" ~ "Houston",
            COUNTY_NAME == "Wake County" ~ "Raleigh",
            COUNTY_NAME == "Durham County" ~ "Durham",
            COUNTY_NAME == "Travis County" ~ "Austin",
            COUNTY_NAME == "Dallas County" ~ "Dallas",
            COUNTY_NAME == "Tarrant County" ~ "Fort Worth",
            COUNTY_NAME == "Bexar County" ~ "San Antonio",
            COUNTY_NAME == "Maricopa County" ~ "Phoenix",
            COUNTY_NAME == "Los Angeles County" ~ "Los Angeles",
            COUNTY_NAME == "Mecklenburg County" ~ "Charlotte",
            COUNTY_NAME == "Williamson County" ~ "Austin suburb",
            COUNTY_NAME == "Collin County" ~ "Plano",
            COUNTY_NAME == "King County" ~ "Seattle",
            COUNTY_NAME == "Kings County" ~ "Brooklyn",
            COUNTY_NAME == "Clark County" ~ "Las Vegas",
            COUNTY_NAME == "Miami-Dade County" ~ "Miami",
            COUNTY_NAME == "Denton County" ~ "Dallas/Ft Worth suburb",
            COUNTY_NAME == "Orange County" ~ "Orlando",
            COUNTY_NAME == "Hillsborough County" ~ "Tampa",
            COUNTY_NAME == "Cook County" ~ "Chicago",
            COUNTY_NAME == "Polk County" ~ "Orlando/Tampa exurb",
            TRUE ~ NA_character_
         )) %>%
         select(COUNTY_NAME, STATE_NAME, CITY, everything())
      
      
         density_tracts_bigcitiesonly <- density_final_withcounty %>%
         select(GEOID, COUNTY_NAME, density_rank_10_start, density_rank_10_end, density14, density23, HU_14to24, housing_per_res, everything()) %>%
         filter(density14 > 1600) %>%
         arrange(desc(housing_per_res))
      
      housing_stats_by_state <- density_tracts_bigcitiesonly %>%
        group_by(STATE_NAME) %>%
        summarize(
          median_housing_per_res = median(housing_per_res, na.rm = TRUE),
          mean_housing_per_res = mean(housing_per_res, na.rm = TRUE),
          tracts_per_state = n()
          
  ) %>%
  arrange(desc(mean_housing_per_res))

      #highdensitycity = round(sum(HU_14to24[density14 > 15000], na.rm = TRUE) / total_growth * 100, 1),
      #middensitycity = round(sum(HU_14to24[density14 > 9000 & density14 <= 15000], na.rm = TRUE) / total_growth * 100, 1),
      #highdensitysuburb = round(sum(HU_14to24[density14 > 6000 & density14 <= 9000], na.rm = TRUE) / total_growth * 100, 1),
      #middensitysuburb = round(sum(HU_14to24[density14 > 2000 & density14 <= 6000], na.rm = TRUE) / total_growth * 100, 1),
      #lowdensityexurb = round(sum(HU_14to24[density14 > 300 & density14 <= 2000], na.rm = TRUE) / total_growth * 100, 1),
      #rural = round(sum(HU_14to24[density14 <= 300], na.rm = TRUE) / total_growth * 100, 1)
      
      # middensitysuburb = round(
      #    sum(ifelse(
      #       (density14 > 2000 & density14 <= 6000) |
      #          (density14 < 2000 & density_rank_10_startcounty == 10),
      #       HU_14to24, 0
      #    ), na.rm = TRUE) / total_growth * 100, 1),
      # lowdensityexurb = round(
      #    sum(ifelse(
      #       (density14 >= 300 & density14 <= 2000 & density_rank_10_startcounty < 10)|
      #          (density14 < 300 & density_rank_10_startcounty %in% c(8, 9)),
      #       HU_14to24, 0
      #    ), na.rm = TRUE) / total_growth * 100, 1),
      # vacant = round(
      #    sum(ifelse(
      #       density14 < 300 & density_rank_10_startcounty < 7,
      #       HU_14to24, 0
      #    ), na.rm = TRUE) / total_growth * 100, 1)
      
      #rename(
      #`City - High Density` = highdensitycity,
      #`City - Mid-Density` = middensitycity,
      #`Suburb - High Density` = highdensitysuburb,
      # `Suburb - Mid-Density` = middensitysuburb,
      #`Exurb - Low Density` = lowdensityexurb,
      # `Rural/Undeveloped` = rural
      # ) %>%
