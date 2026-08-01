library(dplyr)
library(lubridate)
library(readr)
library(httr)
library(MMWRweek)
library(curl)

# pull data from FluID API
who_raw <- {
  response <- GET("https://xmart-api-public.who.int/FLUMART/VIW_FID_EPI?$format=csv",
                  add_headers('Cache-Control' = 'no-cache'),                    # request the most recent data not cached data
                  config(http_version = 2))    
  stop_for_status(response)   # turn an HTTP error status into a clear R error                
  read_csv(content(response, as = "raw"),
                   col_types = cols(
                     GEOSPREAD_COMMENTS = col_character(),
                     PNEU_NB_SITES = col_character(),
                     COMMENTS = col_character()
                   )) %>%                         
    group_by(COUNTRY_AREA_TERRITORY) %>% 
    arrange(ISO_WEEKSTARTDATE, .by_group=TRUE) %>% 
    ungroup()
}
rm(response)

# countries to work with
who <- who_raw %>% 
  filter(COUNTRY_AREA_TERRITORY == "Chile") %>% 
  filter(MMWR_YEAR >= 2022) %>% 
  mutate(date = MMWRweek::MMWRweek2Date(MMWR_YEAR, MMWR_WEEK, 7)) %>%
  select(COUNTRY = COUNTRY_AREA_TERRITORY, 
         date,
         target_group = AGEGROUP_CODE, value = SARI_CASE) 

# Recode age groups
who_recode <- who %>% 
  arrange(COUNTRY, date, target_group) %>% 
  mutate(new_target_group = case_when(
    COUNTRY == "Chile" & target_group %in% c("0TO1", "2TO4", "5TO19") ~ "Pediatrics",
    COUNTRY == "Chile" & target_group %in% c("20TO39", "40TO59", "60TO") ~ "Adults",
    TRUE ~ NA_character_
  )) %>% 
  filter(!is.na(new_target_group)) %>%
  group_by(COUNTRY, date, new_target_group) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  rename(target_group = new_target_group)

who_overall <- who_recode %>% 
  group_by(COUNTRY, date) %>% 
  summarise(value = sum(value)) %>% 
  mutate(target_group = "Overall")

who_final <- who_recode %>% 
  rbind(who_overall) %>% 
  select(-COUNTRY) %>% 
  arrange(date, target_group)

# output data
write_csv(who_final, file = "../target-data/chile-target.csv")

sat_date <- Sys.Date() + (7 - wday(Sys.Date()))
write_csv(who_final, file = paste0("../auxiliary-data/target-data-archive/chile-target_",
                                   year(sat_date), 
                                   "_",
                                   sprintf("%02d", month(sat_date)),
                                   "_",
                                   sprintf("%02d", day(sat_date)),
                                   ".csv"))
