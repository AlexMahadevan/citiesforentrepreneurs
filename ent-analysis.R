library(readr)
library(dplyr)
library(tigris)
library(censusapi)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(scales)

df <- read_csv("data/ent.csv")

# We have a ton of metro economic data.
# But first, let's just try to match the metros in the two datasets so we have a working GEOID.

metro_data <- read_csv("data/ACS_17_1YR_DP03/ACS_17_1YR_DP03.csv") %>% 
  select(`GEO.id2`, `GEO.display-label`) %>% 
  rename(geoid=`GEO.id2`,
         metro_old=`GEO.display-label`)

# Let's split up the column with the dumb metro names.
metro <- as.tibble(str_split(metro_data$metro_old, ",", simplify=TRUE)[,1])

metro <- data.frame(metro)

metro <- metro %>% 
  rename(metro=value)

new_df <- data.frame(geoid = metro_data$geoid[2:nrow(metro_data)], 
                     metro = metro$metro[2:nrow(metro)])

new_df <- new_df %>% 
  mutate(metro= as.character(metro))

# Test to make sure they match.
test <- left_join(new_df, metro_data)
rm(test)

test_2 <- left_join(df, new_df) 

test_3 <- test_2 %>% 
  filter(geoid != 18020 | is.na(geoid)) %>% 
  filter(geoid != 17980 | is.na(geoid)) %>% 
  filter(geoid != 27340 | is.na(geoid)) %>% 
  arrange(desc(geoid))

write_csv(test_3, "test.csv")

### OK, now I have manually added in the geo ids of all the other misnamed metros.

fixed_metro <- read_csv("/Users/alexmahadevan/Desktop/ent-kaitlyn/fixed_metros.csv")

# Now I want to test, just to make sure the GEOIDs are correct. So we read the original Census data back in.
metro_data <- read_csv("data/ACS_17_1YR_DP03/ACS_17_1YR_DP03.csv") %>% 
  select(`GEO.id2`, `GEO.display-label`) %>% 
  rename(geoid=`GEO.id2`,
         metro_old=`GEO.display-label`)

test_3 <- left_join(fixed_metro, metro_data) %>% 
  select(metro, metro_old)
rm(test_3)

# Alright, we are good to go. I also went and fixed the % stuff manually. It's Friday, I don't feel like spending a bunch of time coding that.

fixed_metro <- read_csv("/Users/alexmahadevan/Desktop/ent-kaitlyn/fixed_metros.csv")

# Let's add in some regional price data so we can look at affordability later

prices <- read_csv("data/download.csv", skip=4) %>% 
  rename(geoid=GeoFips,
         prices=`2016`) %>% 
  mutate(geoid=as.numeric(geoid)) %>% 
  select(geoid, prices)

new_metro <- left_join(fixed_metro, prices)

new_metro$geoid <- as.character(new_metro$geoid)
  

# Now let's figure out what we want from the Census data.

Sys.setenv(CENSUS_KEY="4dadb724c39915eb375f70336f7c801f518833cb")

readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

apis <- listCensusApis() 
View(apis)

acs_vars <- listCensusMetadata(name="acs/acs1", type="variables", vintage=2017)

View(acs_vars)bak

# Getting unemployment rate data.
jobs <- c(labor_force = "B23025_005E", 
          unemployed = "B23025_002E")

# B24091_004E: Median earnings for self-employed folks
# C24060_013E: Number of self employed workers.
# B01003_001E: total population
# B24080_015E: Female entrepreneurs

census_variables <- getCensus(name = "acs/acs1", vintage = 2017, 
                          vars = c("NAME", "B23025_005E", "B23025_002E", 
                                   "B24091_004E", "C24060_013E", "B01003_001E",
                                   "B24080_015E"), 
                          region = "metropolitan statistical area/micropolitan statistical area:*") %>% 
  mutate(unemployment_rate=(B23025_005E/B23025_002E)*100) %>% 
  mutate(geoid=as.numeric(metropolitan_statistical_area_micropolitan_statistical_area)) %>% 
  rename(ent_earnings=B24091_004E,
         number_ents=C24060_013E,
         number_female_ents=B24080_015E) %>% 
  mutate(ent_ratio=number_ents/B01003_001E) %>% 
  mutate(female_ent_ratio=number_female_ents/number_ents)

# Merge the metro data with our local prices and entrepreneur data with our census data.

merged <- left_join(new_metro, census_variables, by="geoid")

merged <- merged %>% 
  mutate(pricesZ=-1*((prices - mean(prices, na.rm = TRUE))/sd(prices, na.rm = TRUE))) %>% 
  mutate(densityZ=(density - mean(density, na.rm = TRUE))/sd(density, na.rm = TRUE)) %>% 
  mutate(ent_ratioZ=(ent_ratio - mean(ent_ratio, na.rm = TRUE))/sd(ent_ratio, na.rm = TRUE)) %>%
  mutate(unemployment_rateZ=(unemployment_rate - mean(unemployment_rate, na.rm = TRUE))/sd(unemployment_rate, na.rm = TRUE)) %>%
  mutate(female_ent_ratioZ=(female_ent_ratio - mean(female_ent_ratio, na.rm = TRUE))/sd(female_ent_ratio, na.rm = TRUE)) %>%
  mutate(ent_earningsZ=(ent_earnings - mean(ent_earnings, na.rm = TRUE))/sd(ent_earnings, na.rm = TRUE)) %>%
  mutate(finalZ = pricesZ*0.4 +  densityZ*0.2 + ent_ratioZ*0.2 + female_ent_ratioZ*0.1 + unemployment_rateZ*0.1 + ent_earningsZ*0.1)

df <- read_csv("/Users/alexmahadevan/Downloads/2018.q1-q1.singlefile.csv") %>% 
  filter(area_fips == "10140")

age <- read_csv("/Users/alexmahadevan/Downloads/bds_f_age_release-2.csv") %>% 
  filter(fage4 == "a) 0") %>% 
  select(year2, Estabs_Entry, Estabs_Entry_Rate, Job_Creation, Net_Job_Creation) %>% 
  mutate(Year=mdy(str_c("01-01-", year2)))


ggplot(age, aes(x=Year, y=Estabs_Entry)) +
  geom_line() +
  labs(aes(x="Year", y="Individuals",
           title="Business Creation Still Lags Recession High",
           subtitle="Why aren't entrepreneurs founding businesses the way they did before the Great Recession?",
           caption="Source: U.S. Census Bureau Business Dynamics Statistics")) +
  scale_y_continuous(label=comma, limits = c(0, 600000)) +
  theme_fivethirtyeight()
  

### Look at small businesses ###

age_small_biz <- read_csv("/Users/alexmahadevan/Downloads/bds_f_agesz_release.csv") %>% 
  filter(fage4 == "a) 0" & fsize == "a) 1 to 4") %>% 
  select(year2, estabs_entry, job_creation, net_job_creation) %>% 
  mutate(Year=mdy(str_c("01-01-", year2)))


ggplot(age_small_biz, aes(x=Year, y=net_job_creation)) +
  geom_line() +
  labs(aes(x="Year", y="Individuals",
           title="Business Creation Still Lags Recession High",
           subtitle="Why aren't entrepreneurs founding businesses the way they did before the Great Recession?",
           caption="Source: U.S. Census Bureau Business Dynamics Statistics")) +
  scale_y_continuous(label=comma, limits = c(0, 2000000)) +
  theme_fivethirtyeight()

#### Now look at metros ####

age_metro <- read_csv("/Users/alexmahadevan/Downloads/bds_f_agemsa_release.csv") %>% 
  filter(fage4 == "a) 0" & year2 == 2014) %>% 
  select(year2, msa, Estabs_Entry, Estabs_Entry_Rate, Job_Creation, Net_Job_Creation) %>% 
  mutate(Year=mdy(str_c("01-01-", year2))) %>% 
  mutate(geoid=as.numeric(msa))

age_metro_2009 <- read_csv("/Users/alexmahadevan/Downloads/bds_f_agemsa_release.csv") %>% 
  filter(fage4 == "a) 0" & year2 == 2009) %>% 
  select(year2, msa, Estabs_Entry, Net_Job_Creation) %>% 
  mutate(Year=mdy(str_c("01-01-", year2))) %>% 
  mutate(geoid=as.numeric(msa)) %>% 
  rename(Estabs_Entry_2009=Estabs_Entry, 
         Net_Job_Creation_2009=Net_Job_Creation)

age_metro_change <- left_join(age_metro_2009, age_metro, by="geoid") %>% 
  select(geoid, Estabs_Entry_2009, Net_Job_Creation_2009, Estabs_Entry, Net_Job_Creation) %>% 
  mutate(estab_change = (Estabs_Entry-Estabs_Entry_2009) / Estabs_Entry_2009) %>% 
  mutate(job_change = (Net_Job_Creation-Net_Job_Creation_2009) / Net_Job_Creation_2009)

age_metro_change <- age_metro_change %>% 
  select(geoid, estab_change, job_change)

### Now we merge in the establishment births and job creation by metro area ###

new_merged <- left_join(merged, age_metro_change)


#### Now let's look at all data w/o Kauffman index ####

all_merged <- left_join(census_variables, age_metro_change)

all_merged_with_prices <- left_join(all_merged, prices)

write_csv(all_merged_with_prices, "Output/all_merged_with_price_levels.csv")

### Now our final Z-scores ####

final <- all_merged_with_prices %>% 
  mutate(ent_earningsZ = (ent_earnings -  mean(ent_earnings, na.rm = TRUE)) / sd(ent_earnings, na.rm = TRUE)) %>% 
  mutate(pricesZ = -1*((prices -  mean(prices, na.rm = TRUE)) / sd(prices, na.rm = TRUE))) %>% 
  mutate(ent_ratioZ = (ent_ratio -  mean(ent_ratio, na.rm = TRUE)) / sd(ent_ratio, na.rm = TRUE)) %>% 
  mutate(female_ent_ratioZ = (female_ent_ratio -  mean(female_ent_ratio, na.rm = TRUE)) / sd(female_ent_ratio, na.rm = TRUE)) %>%
  mutate(estab_changeZ = (estab_change -  mean(estab_change, na.rm = TRUE)) / sd(estab_change, na.rm = TRUE)) %>% 
  mutate(finalZ = ent_earningsZ*0.2 + pricesZ*0.3 + ent_ratioZ*0.2 + female_ent_ratioZ*0.1 + estab_changeZ*0.2)

final_no_nas <- final %>% 
  filter(!is.na(finalZ)) %>% 
  arrange(desc(B23025_002E)) 

top_100 <- head(final_no_nas, 100) %>% 
  arrange(desc(finalZ))

write_csv(top_100, "Output/final_dataset_top_100_metros.csv")