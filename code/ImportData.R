# 
# code: Import Emissions Trajectories Data
# 
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

pacman::p_load(rio, grid, gridtext, gridExtra, extrafont, ggthemes, ggplot2, 
               tidyr, stringr, dplyr)


# ---- 1.2 Import data ----

gutschow.data <- 
  import('data/inputs/PMSSPBIE_05Feb20.csv', header = T) # note, all values are in Gg (gigagrams)

GCAM.future.sector.data <- 
  import('data/inputs/GCAMSectorProjections.xlsx')

CAIT.current.sector.data <- 
  import('data/inputs/CAIT_GHG_sector.csv') %>% # note, all values are in Mt (megatons)
  mutate(Code = ifelse(Entity=="European Union (27)", "EU27", Code))




# ---- 1.3 Identify filters for data wrangling ----

scenario.choice <- "SSP2BL" # baseline scenarios to filter to
marker.scenario <- "SSP2BLMESGB" # marker model for scenario.choice

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
  filter(grepl("PMSSPBIE", source) & grepl(scenario.choice, scenario))  # This subsets to all the chosen baseline scenarios for each of the different IAMs (listed under scenario)


# ---- 2.2 Major current emitters (with EU including as single entity) ----

CurrentEmitters.EU <- gutschow.filtered %>%
  select(source, scenario, country, entity, unit, `2018`, `2020`) %>%
  filter(grepl("KYOTOGHGAR4", entity)) %>%
  filter(!grepl("ANNEXI|NONANNEXI|AOSIS|BASIC|LDC|UMBRELLA|EU28", country)) %>%
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
  filter(grepl("KYOTOGHGAR4", entity)) %>%
  filter(!grepl("ANNEXI|NONANNEXI|AOSIS|BASIC|LDC|UMBRELLA|EU28", country)) %>%
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
  filter(grepl("KYOTOGHGAR4", entity)) %>%
  filter(!grepl("ANNEXI|NONANNEXI|AOSIS|BASIC|LDC|UMBRELLA|EU28", country)) %>%
  mutate(marker = ifelse(scenario==marker.scenario, 1, 0),
         year = as.numeric(year))  
  
#KC:  SSPM2BLMESGB is the marker scenario for SSP2 (meaning this is the trend line we will use as the average)
#FutureGHG should contain all of the baseline scenarios for SSP2.  We can talk about whether we create a shaded error "wedge" behind the marker scenario,  or whether we show all SSP2 baseline scenarios with the non-marker scenarios as paler lines.

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
  group_by(source, unit, entity, scenario, country, year) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  mutate(marker = ifelse(scenario==marker.scenario, 1 , 0)) %>%
  filter(marker==1)


# ---- 3.3 Create data frames of historic and future trajectories for Top 10 and Top 20 emitters ----

# --- EU as single entity
GHGTop10.EU <- FutureGHG.EU %>%
  filter(country %in% as.matrix(List.Top10.EU) & marker==1) %>%
  left_join(country.labels, by = "country") %>%
  mutate(country = factor(country, levels = List.Top10.EU$country, ordered = T),
         country.name = factor(country.name, levels = List.Top10.EU$country.name, ordered = T))

GHGTop20.EU <- FutureGHG.EU %>%
  filter(country %in% as.matrix(List.Top20.EU) & marker==1) %>%
  left_join(country.labels, by = "country") %>%
  mutate(country = factor(country, levels = List.Top20.EU$country, ordered = T),
         country.name = factor(country.name, levels = List.Top20.EU$country.name, ordered = T))

GHGTop30.EU <- FutureGHG.EU %>%
  filter(country %in% as.matrix(List.Top30.EU) & marker==1) %>%
  left_join(country.labels, by = "country") %>%
  left_join(GlobalGHG[,c("year","totalGHG")], by = "year") %>%
  mutate(propGHG = value / totalGHG,
         country = factor(country, levels = List.Top30.EU$country, ordered = T),
         country.name = factor(country.name, levels = List.Top30.EU$country.name, ordered = T))


# --- EU as independent nation states
GHGTop10.NoGrp <- FutureGHG.NoGrp %>%
  filter(country %in% as.matrix(List.Top10.NoGrp) & marker==1) %>%
  left_join(country.labels, by = "country") %>%
  mutate(country = factor(country, levels = List.Top10.NoGrp$country, ordered = T),
         country.name = factor(country.name, levels = List.Top10.NoGrp$country.name, ordered = T))

GHGTop20.NoGrp <- FutureGHG.NoGrp %>%
  filter(country %in% as.matrix(List.Top20.NoGrp) & marker==1) %>%
  left_join(country.labels, by = "country") %>%
  mutate(country = factor(country, levels = List.Top20.NoGrp$country, ordered = T),
         country.name = factor(country.name, levels = List.Top20.NoGrp$country.name, ordered = T))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: WRANGLE CAIT & GCAM DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 4.1 Create data frames for historic emissions for Top 10 and Top 20 emitters (CAIT data) ----

GHGTop10.CAIT <- CAIT.current.sector.data %>%
  filter(Code %in% as.matrix(List.Top10.EU)) %>%
  rename("country" = "Code",
         "Agriculture" = "Agriculture (GHG Emissions, CAIT)",
         "International Bunkers" = "Bunker Fuels (GHG Emissions, CAIT)",
         "Industrial Processes" = "Industry (GHG Emissions, CAIT)", 
         "Waste" = "Waste (GHG Emissions, CAIT)",
         "Buildings" = "Buildings (GHG Emissions, CAIT)",
         "Land-Use Change and Forestry" = "Land-Use Change and Forestry (GHG Emissions, CAIT)") %>%
  mutate(Energy = rowSums(.[,c("Electricity & Heat (GHG Emissions, CAIT)",
                               "Manufacturing/Construction energy (GHG Emissions, CAIT)",
                               "Transport (GHG Emissions, CAIT)",
                               "Other Fuel Combustion (GHG Emissions, CAIT)",
                               "Fugitive from energy production (GHG Emissions, CAIT)")],
                          na.rm = T)) %>%
  mutate(net.total = rowSums(.[,c("International Bunkers", "Buildings", "Waste", "Agriculture", 
                                  "Land-Use Change and Forestry", "Industrial Processes", "Energy")], na.rm = T),
         gross.total.minusLUCF = rowSums(.[,c("International Bunkers", "Buildings", "Waste", "Agriculture", 
                                              "Land-Use Change and Forestry", "Industrial Processes", "Energy")], na.rm = T),
         gross.total = ifelse(`Land-Use Change and Forestry`<0, 
                                        gross.total.minusLUCF + abs(`Land-Use Change and Forestry`),
                                        gross.total.minusLUCF)) %>%
  pivot_longer(c("International Bunkers", "Buildings", "Waste", "Agriculture", "Land-Use Change and Forestry", "Industrial Processes", "Energy"),
               names_to = "sector") %>%
  left_join(country.labels, by = "country") %>%
  mutate(country = factor(country, levels = List.Top10.EU$country, ordered = T),
         country.name = factor(country.name, levels = List.Top10.EU$country.name, ordered = T),
         sector = factor(sector, 
                         levels = c("Energy", "Industrial Processes", "Agriculture", "Waste", "Buildings", "International Bunkers", "Land-Use Change and Forestry"),
                         ordered = T),
         negativeLUCF = ifelse(sector=="Land-Use Change and Forestry" & value<0, "1", "0"))


# ---- 4.2 Bind GCAM totals data frame (with calculated gross/net totals) with full GCAM data frame ----

GHGTop10.GCAM <-
  GCAM.future.sector.data %>% 
  mutate(Region = ifelse(Region=="EU", "EU-28", ifelse(Region=="USA", "United States", Region))) %>%
  filter(Region!="World") %>%
  rename("country.name" = "Region",
         "value" = "EmissionsMtCO2e",
         "sector" = "EmisisonsSectorLabel") %>%
  mutate(sector = factor(sector, 
                         levels = c("Energy", "Industry", "Agriculture, Forestry & Other Land Use", "Transport", 
                                    "Buildings", "Other"),
                         ordered = T))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: COUNTRY-SPECIFIC EMISSIONS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# # ---- 5.1 Filter historic and future trajectories data (Gütschow) for specific countries ----
# 
# for(i in List.Top10.EU$country) {
#   filename <- paste("GHG.", i, sep = "")
#   
#   assign(filename,
#          FutureGHG %>%
#            mutate(country = recode(country, AUT = "EU27", BEL = "EU27", BGR = "EU27", HRV = "EU27", CYP = "EU27", CZE = "EU27", DNK = "EU27", 
#                                    EST = "EU27", FIN = "EU27", FRA = "EU27", DEU = "EU27", GRC = "EU27", HUN = "EU27", IRL = "EU27", ITA = "EU27", 
#                                    LTU =  "EU27", LUX = "EU27", LVA = "EU27", MLT = "EU27", NLD = "EU27", POL = "EU27", PRT = "EU27", ROU = "EU27",
#                                    SWE = "EU27", SVK = "EU27", ESP = "EU27", SVN = "EU27")) %>%
#            group_by(source, unit, entity, scenario, country, year) %>%
#            summarise(value = sum(value)) %>%
#            ungroup %>%
#            mutate(marker = ifelse(scenario==marker.scenario, "1" , "0")) %>%
#            filter(country==i  & year>=1850 & year<=2050))
# }


# # ---- 4.2 Filter historical sector-specific data (CAIT) for specific countries ----
# 
# for(i in List.Top10.EU$country) {
#   filename <- paste("CAIT.", i, sep = "")
#   
#   assign(filename, 
#          GHGTop10.CAIT %>%
#            filter(country==i) %>%
#            rename("Agriculture" = "Agriculture (GHG Emissions, CAIT)",
#                   "International Bunkers" = "Bunker Fuels (GHG Emissions, CAIT)",
#                   "Industrial Processes" = "Industry (GHG Emissions, CAIT)", 
#                   "Waste" = "Waste (GHG Emissions, CAIT)",
#                   "Buildings" = "Buildings (GHG Emissions, CAIT)",
#                   "Land-Use Change and Forestry" = "Land-Use Change and Forestry (GHG Emissions, CAIT)") %>%
#            mutate(Energy = rowSums(.[,c("Electricity & Heat (GHG Emissions, CAIT)",
#                                         "Manufacturing/Construction energy (GHG Emissions, CAIT)",
#                                         "Transport (GHG Emissions, CAIT)",
#                                         "Other Fuel Combustion (GHG Emissions, CAIT)",
#                                         "Fugitive from energy production (GHG Emissions, CAIT)")],
#                                    na.rm = T)) %>%
#            pivot_longer(c("International Bunkers", "Buildings", "Waste", "Agriculture", "Land-Use Change and Forestry", "Industrial Processes", "Energy"),
#                         names_to = "sector") %>%
#            mutate(negativeLUCF = ifelse(sector=="Land-Use Change and Forestry" & value<0, 1, 0),
#                   sector = factor(sector, 
#                                   levels = c("Energy", "Industrial Processes", "Land-Use Change and Forestry", "Agriculture", "Waste", "Buildings", "International Bunkers"),
#                                   ordered = T)))
# }


