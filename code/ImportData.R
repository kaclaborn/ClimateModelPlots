# 
# code: Import Emissions Trajectories Data
# 
# ---- sections ----
# 1.  Import data
# 2.  Examine Top20 current emitters
# 3.  Extract Future Emissions trajectories for Top 10 current emitters
# 4.  Export data


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: IMPORT DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 1.1 Import libraries ----

pacman::p_load(rio, grid, gridtext, gridExtra, extrafont, ggthemes, ggplot2, 
               treemapify, tidyr, stringr, dplyr)


# ---- 1.2 Import data ----

gutschow.data <- 
  import('data/inputs/PMSSPBIE_05Feb20.csv', header = T) # note, all values are in Gg (gigagrams)
# more info about Gutschow data can be found at https://doi.org/10.5281/zenodo.3638137

GCAM.future.sector.data <- 
  import('data/inputs/GCAM_emissions_data_20210224x.xlsx') %>%
  rename("EmissionsSectorLabel" = "EmisisonsSectorLabel")

CAIT.current.sector.data <- 
  import('data/inputs/CAIT_historical_emissions_20210225.csv', header = T) %>%
  mutate(country.name = ifelse(Country=="European Union (27)", "EU-27", Country))



# ---- 1.3 Identify filters for data wrangling ----


# -- Baseline SSP2 scenario from Gutschow data (could be edited in with another SSP of interest)
scenario.choice <- "SSP2BL" # baseline scenarios to filter to
marker.scenario <- "SSP2BLMESGB" # marker model for scenario.choice


# -- Include all SSP baseline scenarios from Gutschow data
scenario.allssp <- c("SSP1BL", "SSP2BL", "SSP3BL", "SSP4BL", "SSP5BL") # baseline scenarios to filter to (all SSPs)
marker.allssp <- c("SSP1BLIMAGE", "SSP2BLMESGB", "SSP3BLAIMCGE", "SSP4BLGCAM4", "SSP5BLREMMP") # marker model for each baseline SSP scenario (SSP 1-5)
# more info about each marker model for each baseline SCP scenario can be found at https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=10


# -- Include all SSP scenarios from marker models from Gutschow data
marker.allssp.allscenario <- c("^SSP1.*IMAGE$", "^SSP2.*MESGB$", "^SSP3.*AIMCGE$", "^SSP4.*GCAM4$", "^SSP5.*REMMP$")


# -- Filter Gutschow data to remove Annex, EU28, etc. rows (not of interest for our purposes)
clean.rows <- c("ANNEXI", "NONANNEXI", "AOSIS", "BASIC", "LDC", "UMBRELLA", "EU28")


# -- Country labels
country.labels <- data.frame(country = c("CHN", "USA", "EU27", "IND", "RUS", "JPN", "BRA", "IDN", "IRN", "SAU",
                                         "CAN", "MEX", "KOR", "AUS", "TUR", "ZAF", "GBR", "THA", "PAK", "NGA",
                                         "DEU", "FRA", "MYS", "KAZ", "VNM", "ARG", "EGY", "UKR", "TWN", "VEN", 
                                         "IRQ", "ARE"), # list of countries
                             country.name = c("China", "United States", "EU-27", "India", "Russia", "Japan", 
                                              "Brazil", "Indonesia", "Iran", "Saudi Arabia", "Canada",
                                              "Mexico", "South Korea", "Australia", "Turkey", "South Africa", 
                                              "United Kingdom", "Thailand", "Pakistan", "Nigeria", "Germany", "France",
                                              "Malaysia", "Kazakhstan", "Vietnam", "Argentina", "Egypt", "Ukraine",
                                              "Taiwan", "Venezuela", "Iraq", "UAE")) # the name of the country, to be used in the figures

EU27.list <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", 
               "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA",
               "LTU", "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU",
               "SWE", "SVK", "ESP", "SVN")

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: FILTER GUTSCHOW DATA TO CURRENT EMISSIONS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# 2.1 Extract harmonized SSP2 baseline data
# 2.2 Major current emitters (with EU 27 as single entity)
# 2.3 Major current emitters (with EU27 as independent nation states)


# ---- 2.1 Select emissions trajectories harmonized with historical data & the chosen scenario ----

gutschow.filtered <- gutschow.data %>%
  filter(grepl("PMSSPBIE", source) & 
           grepl(scenario.choice, scenario) & # This subsets to all the chosen baseline scenarios for each of the different IAMs (listed under scenario)
                   !grepl(paste(clean.rows, collapse = "|"), country))


# ---- 2.2 Major current emitters (with EU included as single entity) ----

CurrentEmitters.EU <- gutschow.filtered %>%
  select(source, scenario, country, entity, unit, `2018`, `2020`) %>%
  filter(entity=="KYOTOGHGAR4") %>%
  mutate(country = recode(country, AUT = "EU27", BEL = "EU27", BGR = "EU27", HRV = "EU27", CYP = "EU27", CZE = "EU27", DNK = "EU27", 
                          EST = "EU27", FIN = "EU27", FRA = "EU27", DEU = "EU27", GRC = "EU27", HUN = "EU27", IRL = "EU27", ITA = "EU27",
                          LTU =  "EU27", LUX = "EU27", LVA = "EU27", MLT = "EU27", NLD = "EU27", POL = "EU27", PRT = "EU27", ROU = "EU27",
                          SWE = "EU27", SVK = "EU27", ESP = "EU27", SVN = "EU27"))%>%
  group_by(source, unit, entity, scenario, country) %>%
  summarise(`2020` = sum(`2020`),
            `2018` = sum(`2018`)) %>%
  ungroup %>%
  mutate(marker = ifelse(scenario==marker.scenario, 1, 0)) %>%
  filter(marker==1) 


# Create list of top 10 and top 20 emitters (2020 as reference year; EU considered a single entity)
List.Top10.EU <- CurrentEmitters.EU %>%
  filter(country!="EARTH") %>%
  slice_max(`2018`, n = 10) %>%
  select(country) %>%
  left_join(country.labels, by = "country")

List.Top20.EU <- CurrentEmitters.EU %>%
  filter(country!="EARTH") %>%
  slice_max(`2018`, n = 20) %>%
  select(country) %>%
  left_join(country.labels, by = "country")

List.Top30.EU <- CurrentEmitters.EU %>%
  filter(country!="EARTH") %>%
  slice_max(`2018`, n = 30) %>%
  select(country) %>%
  left_join(country.labels, by = "country")


# ---- 2.2 Major current emitters (with EU as separate nation states) ----

CurrentEmitters.NoGrp <- gutschow.filtered %>%
  select(source, scenario, country, entity, unit, `2018`, `2020`) %>%
  filter(entity=="KYOTOGHGAR4") %>%
  mutate(marker = ifelse(scenario==marker.scenario, 1, 0))%>%
  filter(marker==1)

# Create list of top 10 and top 20 emitters (2020 as reference year; EU countries as separate nation states)
List.Top10.NoGrp <- CurrentEmitters.NoGrp %>%
  filter(country!="EARTH") %>%
  slice_max(`2018`, n = 10) %>%
  select(country) %>%
  left_join(country.labels, by = "country")

List.Top20.NoGrp <- CurrentEmitters.NoGrp %>%
  filter(country!="EARTH") %>%
  slice_max(`2018`, n = 20) %>%
  select(country) %>%
  left_join(country.labels, by = "country")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: FILTER GUTSCHOW DATA FOR FUTURE TRAJECTORIES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Reshape emissions data ----

FutureGHG <- gutschow.filtered %>%
  tidyr::pivot_longer(c(`1850`:`2100`), names_to = "year", values_to = "value") %>%
  filter(entity=="KYOTOGHGAR4") %>%
  mutate(marker = ifelse(scenario==marker.scenario, 1, 0),
         year = as.numeric(year))  
  

# --- Create filtered data frame with only global emissions 

GlobalGHG <- FutureGHG %>%
  filter(country=="EARTH") %>%
  mutate(marker = ifelse(scenario==marker.scenario, 1 , 0)) %>%
  rename("totalGHG" = "value") %>%
  filter(marker==1)



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
  mutate(marker = ifelse(scenario==marker.scenario, 1 , 0)) %>%
  filter(marker==1)


# ---- 3.3 Create data frames of historic and future trajectories for Top 10 and Top 20 emitters ----

# --- EU as single entity
GHGTop10.EU <- FutureGHG.EU %>%
  filter(country %in% as.matrix(List.Top10.EU)) %>%
  left_join(country.labels, by = "country") %>%
  mutate(country = factor(country, levels = List.Top10.EU$country, ordered = T),
         country.name = factor(country.name, levels = List.Top10.EU$country.name, ordered = T))

GHGTop20.EU <- FutureGHG.EU %>%
  filter(country %in% as.matrix(List.Top20.EU)) %>%
  left_join(country.labels, by = "country") %>%
  mutate(country = factor(country, levels = List.Top20.EU$country, ordered = T),
         country.name = factor(country.name, levels = List.Top20.EU$country.name, ordered = T))

GHGTop30.EU <- FutureGHG.EU %>%
  filter(country %in% as.matrix(List.Top30.EU)) %>%
  left_join(country.labels, by = "country") %>%
  left_join(GlobalGHG[,c("year","totalGHG")], by = "year") %>%
  mutate(propGHG = value / totalGHG,
         country = factor(country, levels = List.Top30.EU$country, ordered = T),
         country.name = factor(country.name, levels = List.Top30.EU$country.name, ordered = T))


# --- EU as independent nation states
GHGTop10.NoGrp <- FutureGHG.NoGrp %>%
  filter(country %in% as.matrix(List.Top10.NoGrp)) %>%
  left_join(country.labels, by = "country") %>%
  mutate(country = factor(country, levels = List.Top10.NoGrp$country, ordered = T),
         country.name = factor(country.name, levels = List.Top10.NoGrp$country.name, ordered = T))

GHGTop20.NoGrp <- FutureGHG.NoGrp %>%
  filter(country %in% as.matrix(List.Top20.NoGrp)) %>%
  left_join(country.labels, by = "country") %>%
  mutate(country = factor(country, levels = List.Top20.NoGrp$country, ordered = T),
         country.name = factor(country.name, levels = List.Top20.NoGrp$country.name, ordered = T))


# --- EU as single entity, all baseline scenarios included (not only marker scenario -- to display uncertainty)
GHGTop30.MultiScenarios.EU <- 
  FutureGHG %>%
  mutate(country = recode(country, AUT = "EU27", BEL = "EU27", BGR = "EU27", HRV = "EU27", CYP = "EU27", CZE = "EU27", DNK = "EU27", 
                          EST = "EU27", FIN = "EU27", FRA = "EU27", DEU = "EU27", GRC = "EU27", HUN = "EU27", IRL = "EU27", ITA = "EU27", 
                          LTU =  "EU27", LUX = "EU27", LVA = "EU27", MLT = "EU27", NLD = "EU27", POL = "EU27", PRT = "EU27", ROU = "EU27",
                          SWE = "EU27", SVK = "EU27", ESP = "EU27", SVN = "EU27")) %>%
  group_by(source, unit, entity, scenario, country, year) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  mutate(marker = ifelse(scenario==marker.scenario, "1" , "0")) %>%
  filter(country %in% as.matrix(List.Top30.EU)) %>%
  left_join(country.labels, by = "country") %>%
  left_join(GlobalGHG[,c("year","totalGHG")], by = "year") %>%
  mutate(propGHG = value / totalGHG,
         country = factor(country, levels = List.Top30.EU$country, ordered = T),
         country.name = factor(country.name, levels = List.Top30.EU$country.name, ordered = T))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: FILTER GUTSCHOW DATA FOR ALL SSP SCENARIOS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 4.1 Select all 5 SSP emissions trajectories (baseline marker models & then all marker models) harmonized with historical data----

gutschow.allssp <- gutschow.data %>%
  filter(grepl("PMSSPBIE", source) & 
           grepl(paste(scenario.allssp, collapse="|"), scenario) &
           !grepl(paste(clean.rows, collapse = "|"), country))

gutschow.radiativeforcing <- gutschow.data %>%
  filter(grepl("PMSSPBIE", source) &
           grepl(paste(marker.allssp.allscenario, collapse="|"), scenario) &
           !grepl(paste(clean.rows, collapse = "|"), country))


# ---- 4.2 Extract emissions trajectories (all SSPs) for Top 10 emitters (with EU including as single entity) ----

# -- EU as single entity
FutureGHG.AllSSPs.Top10.EU <- gutschow.allssp %>%
  tidyr::pivot_longer(c(`1850`:`2100`), names_to = "year", values_to = "value") %>%
  filter(entity %in% c("GDPPPP", "POP", "KYOTOGHGAR4")) %>% # NOTE: this filtering also includes GDP and population estimates
  mutate(marker = ifelse(scenario==marker.scenario, 1, 0),
         year = as.numeric(year),
         country = recode(country, AUT = "EU27", BEL = "EU27", BGR = "EU27", HRV = "EU27", CYP = "EU27", CZE = "EU27", DNK = "EU27", 
                          EST = "EU27", FIN = "EU27", FRA = "EU27", DEU = "EU27", GRC = "EU27", HUN = "EU27", IRL = "EU27", ITA = "EU27", 
                          LTU =  "EU27", LUX = "EU27", LVA = "EU27", MLT = "EU27", NLD = "EU27", POL = "EU27", PRT = "EU27", ROU = "EU27",
                          SWE = "EU27", SVK = "EU27", ESP = "EU27", SVN = "EU27")) %>%
  group_by(source, unit, entity, scenario, country, year) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  left_join(List.Top10.EU, by = "country") %>%
  filter(country %in% as.matrix(List.Top10.EU)) %>%
  mutate(marker.allssp = ifelse(scenario%in%marker.allssp, 1 , 0),
         marker = ifelse(scenario%in%marker.scenario, 1 , 0),
         country = factor(country, levels = List.Top10.EU$country, ordered = T),
         country.name = factor(country.name, levels = List.Top10.EU$country.name, ordered = T))

# -- Include different radiative forcing levels (all marker models)
FutureGHG.AllScenarios.Top10.EU <- gutschow.radiativeforcing %>%
  tidyr::pivot_longer(c(`1850`:`2100`), names_to = "year", values_to = "value") %>%
  filter(entity %in% c("GDPPPP", "POP", "KYOTOGHGAR4")) %>% # NOTE: this filtering also includes GDP and population estimates
  mutate(marker = ifelse(scenario==marker.scenario, 1, 0),
         year = as.numeric(year),
         country = recode(country, AUT = "EU27", BEL = "EU27", BGR = "EU27", HRV = "EU27", CYP = "EU27", CZE = "EU27", DNK = "EU27", 
                          EST = "EU27", FIN = "EU27", FRA = "EU27", DEU = "EU27", GRC = "EU27", HUN = "EU27", IRL = "EU27", ITA = "EU27", 
                          LTU =  "EU27", LUX = "EU27", LVA = "EU27", MLT = "EU27", NLD = "EU27", POL = "EU27", PRT = "EU27", ROU = "EU27",
                          SWE = "EU27", SVK = "EU27", ESP = "EU27", SVN = "EU27")) %>%
  group_by(source, unit, entity, scenario, country, year) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  left_join(List.Top10.EU, by = "country") %>%
  filter(country %in% as.matrix(List.Top10.EU)) %>%
  mutate(marker.allssp = ifelse(scenario%in%marker.allssp, 1 , 0),
         country = factor(country, levels = List.Top10.EU$country, ordered = T),
         country.name = factor(country.name, levels = List.Top10.EU$country.name, ordered = T))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: WRANGLE CAIT & GCAM DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 5.1 CAIT data: create data frame for historic emissions for Top 10 emitters across 5 sector categories ----

GHGTop10.CAIT.5sectors <- CAIT.current.sector.data %>%
  left_join(country.labels, by = "country.name") %>%
  filter(country %in% as.matrix(List.Top10.EU)) %>%
  mutate(Sector = recode(Sector, `Electricity/Heat` = "Power & Heat"),
         Sector = recode(Sector, Transportation = "Transport"),
         Sector = recode(Sector, `Industrial Processes` = "Industry"),
         Sector = recode(Sector, `Manufacturing/Construction` = "Industry"),
         Sector = recode(Sector, Agriculture = "AFOLU"),
         Sector = recode(Sector, `Land-Use Change and Forestry` = "AFOLU"),
         Sector = recode(Sector, Building = "Other"),
         Sector = recode(Sector, Waste = "Other"),
         Sector = recode(Sector, `Bunker Fuels` = "Other"),
         Sector = recode(Sector, `Other Fuel Combustion` = "Other"),
         Sector = recode(Sector, `Fugitive Emissions` = "Other")) %>%
  rename("sector" = "Sector") %>%
  mutate(across(starts_with("20") | starts_with("19"), as.numeric)) %>%
  pivot_longer(cols = starts_with("20") | starts_with("19"), names_to = "Year") %>%
  group_by(country, country.name, Year, sector) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  mutate(Year = as.numeric(Year),
         country = factor(country, levels = List.Top10.EU$country, ordered = T),
         country.name = factor(country.name, levels = List.Top10.EU$country.name, ordered = T),
         sector = factor(sector, levels = c("Power & Heat", "Industry", "Transport", "AFOLU", "Other"), ordered = T))


# ---- 5.2 GCAM data: create data frame for modelled projected emissions for Top 10 emitters across 5 sector categories ----

# currently has 5 sector categories (Power & Heat, Industry, AFOLU, Transport, Other (buildings, waste, other fugitive gases))
# --- see excel spreadsheet that identifies the what observations from the raw GCAM data comprise each of the 5 sector categories
GHGTop10.GCAM <-
  GCAM.future.sector.data %>% 
  mutate(IncSector = ifelse(SuperSector=="Industrial Processes", 1, 
                            ifelse(grepl("Freight", Variable) | grepl("Passenger", Variable) | grepl("Cement", Variable), 
                                   0, IncSector))) %>%
  filter(IncSector==1 & KyotoGas==1) %>%
  pivot_longer(cols = starts_with("X2"), names_to = "Year") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)),
         Region = ifelse(Region=="EU", "EU-28", 
                         ifelse(Region=="USA", "United States", 
                                                       Region)),
         EmissionsCO2e = ((value * MtFactor)* as.numeric(GWP100)),
         UnitAdjusted = "Mt CO2e/yr",
         EmissionsSectorLabel = ifelse(EmissionsSectorLabel=="Buildings", "Other", EmissionsSectorLabel),
         EmissionsSectorLabel = ifelse(Gas%in%c("HFC", "F-Gases") | SuperSector=="Industrial Processes", "Industry", 
                                                     EmissionsSectorLabel)) %>%
  group_by(Model, Scenario, Region, Year, EmissionsSectorLabel, UnitAdjusted) %>%
  summarise(EmissionsCO2e = sum(EmissionsCO2e)) %>%
  rename("country.name" = "Region",
         "sector" = "EmissionsSectorLabel",
         "value" = "EmissionsCO2e") %>%
  mutate(sector = recode(sector, `Agriculture, Forestry & Other Land Use` = "AFOLU"),
         sector = recode(sector, Energy = "Power & Heat"),
         sector = factor(sector, levels = c("Power & Heat", "Industry", "Transport", "AFOLU", "Other"), ordered = T))


# Find the total magnitude and relative percent value for each sub-section of industry category (energy consumption, process emissions, fugitive emissions)
GCAM.Top10.IndustrySubSections <-
  GCAM.future.sector.data %>% 
  mutate(IncSector = ifelse(SuperSector=="Industrial Processes", 1, 
                            ifelse(grepl("Freight", Variable) | grepl("Passenger", Variable) | grepl("Cement", Variable), 
                                   0, IncSector))) %>%
  filter(IncSector==1 & KyotoGas==1) %>%
  pivot_longer(cols = starts_with("X2"), names_to = "Year") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)),
         Region = ifelse(Region=="EU", "EU-28", 
                         ifelse(Region=="USA", "United States", 
                                Region)),
         EmissionsCO2e = ((value * MtFactor)* as.numeric(GWP100)),
         UnitAdjusted = "Mt CO2e/yr",
         EmissionsSectorLabel = ifelse(EmissionsSectorLabel=="Buildings", "Other", EmissionsSectorLabel),
         EmissionsSectorLabel = ifelse(Gas%in%c("HFC", "F-Gases") | SuperSector=="Industrial Processes", "Industry", 
                                       EmissionsSectorLabel),
         Industry.Subcategory = ifelse(Sector=="Demand", "Energy Consumption",
                                       ifelse(Sector%in%c("Energy", "Total", "Industrial Processes") |
                                                (Sector=="Supply" & Gas!="CH4"), "Process Emissions", "Fugitive Emissions"))) %>%
  filter(EmissionsSectorLabel=="Industry") %>%
  group_by(Model, Scenario, Region, Year, Industry.Subcategory, UnitAdjusted) %>%
  summarise(EmissionsCO2e = sum(EmissionsCO2e)) %>%
  rename("CountryName" = "Region") %>%
  ungroup() %>%
  group_by(CountryName, Year) %>%
  mutate(Percent.IndustryTotal.PerCountryYear = (EmissionsCO2e / sum(EmissionsCO2e, na.rm = T)) * 100)

GCAM.Top10.AllIndustryVars <-
  GCAM.future.sector.data %>% 
  mutate(IncSector = ifelse(SuperSector=="Industrial Processes", 1, 
                            ifelse(grepl("Freight", Variable) | grepl("Passenger", Variable) | grepl("Cement", Variable), 
                                   0, IncSector))) %>%
  filter(IncSector==1 & KyotoGas==1) %>%
  pivot_longer(cols = starts_with("X2"), names_to = "Year") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)),
         Region = ifelse(Region=="EU", "EU-28", 
                         ifelse(Region=="USA", "United States", 
                                Region)),
         EmissionsCO2e = ((value * MtFactor)* as.numeric(GWP100)),
         UnitAdjusted = "Mt CO2e/yr",
         EmissionsSectorLabel = ifelse(EmissionsSectorLabel=="Buildings", "Other", EmissionsSectorLabel),
         EmissionsSectorLabel = ifelse(Gas%in%c("HFC", "F-Gases") | SuperSector=="Industrial Processes", "Industry", 
                                       EmissionsSectorLabel)) %>%
  filter(EmissionsSectorLabel=="Industry") %>%
  group_by(Model, Scenario, Region, Year, Variable, UnitAdjusted) %>%
  summarise(EmissionsCO2e = sum(EmissionsCO2e)) %>%
  rename("CountryName" = "Region") %>%
  ungroup() %>%
  group_by(CountryName, Year) %>%
  mutate(Percent.IndustryTotal.PerCountryYear = (EmissionsCO2e / sum(EmissionsCO2e, na.rm = T)) * 100)


GCAM.Top10.AllVars <-
  GCAM.future.sector.data %>% 
  mutate(IncSector = ifelse(SuperSector=="Industrial Processes", 1, 
                            ifelse(grepl("Freight", Variable) | grepl("Passenger", Variable) | grepl("Cement", Variable), 
                                   0, IncSector))) %>%
  filter(IncSector==1 & KyotoGas==1) %>%
  pivot_longer(cols = starts_with("X2"), names_to = "Year") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)),
         Region = ifelse(Region=="EU", "EU-28", 
                         ifelse(Region=="USA", "United States", 
                                Region)),
         EmissionsCO2e = ((value * MtFactor)* as.numeric(GWP100)),
         UnitAdjusted = "Mt CO2e/yr",
         EmissionsSectorLabel = ifelse(EmissionsSectorLabel=="Buildings", "Other", EmissionsSectorLabel),
         EmissionsSectorLabel = ifelse(Gas%in%c("HFC", "F-Gases") | SuperSector=="Industrial Processes", "Industry", 
                                       EmissionsSectorLabel)) %>%
  group_by(Model, Scenario, Region, Year, EmissionsSectorLabel, Variable, UnitAdjusted) %>%
  summarise(EmissionsCO2e = sum(EmissionsCO2e)) %>%
  rename("CountryName" = "Region") %>%
  ungroup() %>%
  group_by(CountryName, Year) %>%
  mutate(Percent.IndustryTotal.PerCountryYear = (EmissionsCO2e / sum(EmissionsCO2e, na.rm = T)) * 100)

# export(GCAM.Top10.AllVars, 'data/outputs/GCAM.Top10.AllVars.csv')
# export(GCAM.Top10.AllIndustryVars, 'data/outputs/GCAM.Top10.AllIndustryVars.csv')


