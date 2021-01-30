# ---- code: Import Emissions Trajectories Data ----

# ---- sections ----
# 1.  Import data
# 2.  Examine Top20 current emitters
# 3.  Extract Future Emissions trajectories for Top 10 current emitters.
# 4.  Export data


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: IMPORT DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 1.1 Import libraries ----

pacman::p_load(rio, tidyr, stringr, grid, gridtext, ggplot2, dplyr)


# ---- 1.2 Import data ----

data <- import('data/inputs/PMSSPBIE_05Feb20.csv', header = T)


# ---- 1.3 Identify filters for data wrangling ----

scenario.choice <- "SSP2BL" # baseline scenarios to filter to
marker.scenario <- "SSP2BLMESGB" # marker model for scenario.choice


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: FILTER TO CURRENT EMISSIONS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# 2.1 Extract harmonized SSP2 baseline data
# 2.2 Major current emitters (with EU 27 as single entity)
# 2.3 Major current emitters (with EU27 as independent nation states)


# ---- 2.1 Select emissions trajectories harmonized with historical data & the chosen scenario ----

datafiltered <- data %>%
  filter(grepl("PMSSPBIE", source) & grepl(scenario.choice, scenario))  # This subsets to all the chosen baseline scenarios for each of the different IAMs (listed under scenario)


# ---- 2.2 Major current emitters (with EU including as single entity) ----

CurrentEmitters.EU <- datafiltered %>%
  select(source, scenario, country, entity, unit, `2020`) %>%
  filter(grepl("KYOTOGHGAR4", entity)) %>%
  filter(!grepl("ANNEXI|NONANNEXI|AOSIS|BASIC|LDC|UMBRELLA|EU28", country))

CurrentEmitters.EU <- CurrentEmitters.EU %>%
  mutate(country = recode(country, AUT = "EU27", BEL = "EU27", BGR = "EU27", HRV = "EU27", CYP = "EU27", CZE = "EU27", DNK = "EU27", 
                          EST = "EU27", FIN = "EU27", FRA = "EU27", DEU = "EU27", GRC = "EU27", HUN = "EU27", IRL = "EU27", ITA = "EU27",
                          LTU =  "EU27", LUX = "EU27", LVA = "EU27", MLT = "EU27", NLD = "EU27", POL = "EU27", PRT = "EU27", ROU = "EU27",
                          SWE = "EU27", SVK = "EU27", ESP = "EU27", SVN = "EU27"))%>%
  group_by(source, unit, entity, scenario, country) %>%
  summarise(`2020` = sum(`2020`)) %>%
  ungroup %>%
  mutate(marker = ifelse(scenario==marker.scenario, 1, 0)) %>%
  filter(marker==1) %>%
  slice_max(`2020`, n = 21)  #N=21 because data includes global total "EARTH" in country column.  Subsetting to Top20. 

# Create list of top 10 and top 20 emitters (2020 as reference year; EU considered a single entity)
List.Top10.EU <- CurrentEmitters.EU %>%
  filter(country!="EARTH") %>%
  slice_max(`2020`, n = 10) %>%
  select(country)

List.Top20.EU <- CurrentEmitters.EU %>%
  filter(country!="EARTH") %>%
  slice_max(`2020`, n = 20) %>%
  select(country)


# ---- 2.2 Major current emitters (with EU as separate nation states) ----

CurrentEmitters.NoGrp <- datafiltered %>%
  select(source, scenario, country, entity, unit, `2020`) %>%
  filter(grepl("KYOTOGHGAR4", entity)) %>%
  filter(!grepl("ANNEXI|NONANNEXI|AOSIS|BASIC|LDC|UMBRELLA|EU28", country)) %>%
  mutate(marker = ifelse(scenario==marker.scenario, 1, 0))%>%
  filter(marker==1) %>%
  slice_max(`2020`, n = 21) #N=21 because data includes global total "EARTH" in country column.  Subsetting to Top20. 


# Create list of top 10 and top 20 emitters (2020 as reference year; EU countries as separate nation states)
List.Top10.NoGrp <- CurrentEmitters.NoGrp %>%
  filter(country!="EARTH") %>%
  slice_max(`2020`, n = 10) %>%
  select(country)

List.Top20.NoGrp <- CurrentEmitters.NoGrp %>%
  filter(country!="EARTH") %>%
  slice_max(`2020`, n = 20) %>%
  select(country)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: FILTER FOR FUTURE TRAJECTORIES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Reshape emissions data ----

FutureGHG <- datafiltered %>%
  tidyr::pivot_longer(c(`1850`:`2100`), names_to = "year", values_to = "value") %>%
  filter(grepl("KYOTOGHGAR4", entity)) %>%
  filter(!grepl("ANNEXI|NONANNEXI|AOSIS|BASIC|LDC|UMBRELLA|EU28", country)) %>%
  mutate(marker = ifelse(scenario==marker.scenario, 1, 0),
         year = as.numeric(year))  

#KC:  SSPM2BLMESGB is the marker scenario for SSP2 (meaning this is the trend line we will use as the average)
#FutureGHG should contain all of the baseline scenarios for SSP2.  We can talk about whether we create a shaded error "wedge" behind the marker scenario,  or whether we show all SSP2 baseline scenarios with the non-marker scenarios as paler lines.


# ---- 3.2 Extract emissions trajectories for all countries (with EU as single entity) ----

# --- EU as a single entity
FutureGHG.EU <- FutureGHG %>%
  mutate(country = recode(country, AUT = "EU27", BEL = "EU27", BGR = "EU27", HRV = "EU27", CYP = "EU27", CZE = "EU27", DNK = "EU27", 
                         EST = "EU27", FIN = "EU27", FRA = "EU27", DEU = "EU27", GRC = "EU27", HUN = "EU27", IRL = "EU27", ITA = "EU27", 
                         LTU =  "EU27", LUX = "EU27", LVA = "EU27", MLT = "EU27", NLD = "EU27", POL = "EU27", PRT = "EU27", ROU = "EU27",
                         SWE = "EU27", SVK = "EU27", ESP = "EU27", SVN = "EU27")) %>%
  group_by(source, unit, entity, scenario, country, year) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  mutate(marker = ifelse(scenario==marker.scenario, 1 , 0)) %>%
  filter(marker==1)

# --- EU as independent nation states
FutureGHG.NoGrp <- FutureGHG %>%
  group_by(source, unit, entity, scenario, country, year) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  mutate(marker = ifelse(scenario==marker.scenario, 1 , 0)) %>%
  filter(marker==1)


# ---- 3.3 Create data frames of historic and future trajectories for Top 10 and Top 20 emitters ----

# --- EU as single entity
GHGTop10.EU <- FutureGHG.EU %>%
  filter(country %in% as.matrix(List.Top10.EU)) %>%
  filter(marker==1 & year>=1850 & year<=2050) %>%
  mutate(country = factor(country, levels = List.Top10.EU$country, ordered = T))

GHGTop20.EU <- FutureGHG.EU %>%
  filter(country %in% as.matrix(List.Top20.EU)) %>%
  filter(marker==1 & year>=1850 & year<=2050) %>%
  mutate(country = factor(country, levels = List.Top20.EU$country, ordered = T))

# --- EU as independent nation states
GHGTop10.NoGrp <- FutureGHG.NoGrp %>%
  filter(country %in% as.matrix(List.Top10.NoGrp)) %>%
  filter(marker==1 & year>=1850 & year<=2050) %>%
  mutate(country = factor(country, levels = List.Top10.NoGrp$country, ordered = T))

GHGTop20.NoGrp <- FutureGHG.NoGrp %>%
  filter(country %in% as.matrix(List.Top20.NoGrp)) %>%
  filter(marker==1 & year>=1850 & year<=2050) %>%
  mutate(country = factor(country, levels = List.Top20.NoGrp$country, ordered = T))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: COUNTRY-SPECIFIC EMISSIONS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 4.1 Create country lists for filtering ----

country.labels <- data.frame(list = c("CHN", "USA", "EU27"), # list of countries that data will be filtered to, for country-specific figures
                             name = c("China", "United States", "European Union")) # the name of the country, to be used in the figures


# ---- 4.2 Filter historic and future trajectories data for specific countries ----

for(i in List.Top10.EU$country) {
  filename <- paste("GHG.", i, sep = "")
  
  assign(filename,
         FutureGHG %>%
           mutate(country = recode(country, AUT = "EU27", BEL = "EU27", BGR = "EU27", HRV = "EU27", CYP = "EU27", CZE = "EU27", DNK = "EU27", 
                                   EST = "EU27", FIN = "EU27", FRA = "EU27", DEU = "EU27", GRC = "EU27", HUN = "EU27", IRL = "EU27", ITA = "EU27", 
                                   LTU =  "EU27", LUX = "EU27", LVA = "EU27", MLT = "EU27", NLD = "EU27", POL = "EU27", PRT = "EU27", ROU = "EU27",
                                   SWE = "EU27", SVK = "EU27", ESP = "EU27", SVN = "EU27")) %>%
           group_by(source, unit, entity, scenario, country, year) %>%
           summarise(value = sum(value)) %>%
           ungroup %>%
           mutate(marker = ifelse(scenario==marker.scenario, "1" , "0")) %>%
           filter(country==i  & year>=1850 & year<=2050))
}

