### SCF Emissions Trajectories Analysis ###

#Code Sections
# 1.  Import data
# 2.  Examine Top20 current emitters
# 3.  Extract Future Emissions trajectories for Top 10 current emitters.
# 4.  Export data

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: IMPORT DATA & SET UP DATA EXPORT FILE ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ----1.1 Libraries ----

library(tidyr)
library(stringr)
library(openxlsx)

#---- 1.2 Import data ----
PMSSPBIE<- data.frame(read.csv("PMSSPBIE_05Feb20.csv"))

#--- 1.3 Set up xlsx output file

EmissionsTrajectories <- createWorkbook()



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: CURRENT EMISSIONS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2.1 Extract harmonized SSP2 baseline data
# 2.2 Major current emitters (with EU 27 as single entity)
# 2.3 Major current emitters (with EU27 as independent nation states)


# ---- 2.1 Select emissions trajectories harmonized with historical datA & the SSP2BL scenario ----


SSP2BL <- PMSSPBIE %>%
  filter (grepl("PMSSPBIE", source) & grepl("SSP2BL", scenario))  #This subsets to all the SSP2 baseline scenarios for each of the different IAMs (listed under scenario).



# ----2.2 Major current emitters (with EU including as single entity)----

CurrentEmitters.EU <- SSP2BL %>%
  select(source,scenario, country,entity,unit,X2020) %>%
  filter (grepl("KYOTOGHGAR4", entity)) %>%
  filter(!grepl("ANNEXI|NONANNEXI|AOSIS|BASIC|LDC|UMBRELLA|EU28", country))

CurrentEmitters.EU <- CurrentEmitters.EU %>%
  mutate(country =recode(country, AUT= "EU27", BEL ="EU27", BGR="EU27", HRV="EU27", CYP ="EU27",CZE="EU27", DNK = "EU27", 
                         EST="EU27", FIN ="EU27", FRA="EU27", DEU="EU27", GRC="EU27", HUN="EU27", IRL="EU27", ITA="EU27",
                         LTU= "EU27", LUX="EU27", LVA="EU27", MLT= "EU27", NLD = "EU27", POL="EU27", PRT ="EU27", ROU= "EU27",
                         SWE="EU27", SVK="EU27", ESP = "EU27", SVN="EU27"))%>%
  group_by(source,unit,entity,scenario, country)%>%
  summarise(X2020 = sum(X2020))%>%
  ungroup %>%
  mutate (marker= ifelse(scenario=="SSP2BLMESGB",1,0))%>%
  filter (marker==1) %>%
  slice_max(X2020, n=21)  #N=21 because data includes global total "EARTH" in country column.  Subsetting to Top20. 

addWorksheet(EmissionsTrajectories, "CurrentEmitters_EU")
writeData(EmissionsTrajectories, sheet = "CurrentEmitters_EU", x = CurrentEmitters.EU)

# ----2.2 Major current emitters (with EU as separate nation states) ----

CurrentEmitters.NoGrp <- SSP2BL %>%
  select(source,scenario, country,entity,unit,X2020) %>%
  filter (grepl("KYOTOGHGAR4", entity)) %>%
  filter(!grepl("ANNEXI|NONANNEXI|AOSIS|BASIC|LDC|UMBRELLA|EU28", country)) %>%
  mutate (marker= ifelse(scenario=="SSP2BLMESGB",1,0))%>%
  filter (marker==1) %>%
  slice_max(X2020, n=21) #N=21 because data includes global total "EARTH" in country column.  Subsetting to Top20. 


addWorksheet(EmissionsTrajectories, "CurrentEmitters_NoGrp")
writeData(EmissionsTrajectories, sheet = "CurrentEmitters_NoGrp", x =CurrentEmitters.NoGrp)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: FUTURE TRAJECTORIES (of major emitters) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 3.1 Reshape emissions data ----

FutureGHG <- SSP2BL %>%
  gather(Year,Value, X1850:X2100) %>%
  filter (grepl("KYOTOGHGAR4", entity)) %>%
  filter(!grepl("ANNEXI|NONANNEXI|AOSIS|BASIC|LDC|UMBRELLA|EU28", country)) %>%
  mutate (marker= ifelse(scenario=="SSP2BLMESGB",1,0))  

#KC:  SSPM2BLMESGB is the marker scenario for SSP2 (meaning this is the trend line we will use as the average)
#FutureGHG should contain all of the baseline scenarios for SSP2.  We can talk about whether we create a shaded error "wedge" behind the marker scenario,  or whether we show all SSP2 baseline scenarios with the non-marker scenarios as paler lines.

FutureGHG$Year <- as.numeric (sub('.', '',FutureGHG$Year))


# ----3.2 Extract emissions trajectories for Top 10 current emitters (with EU as single bloc)

FutureGHG.EU <- FutureGHG %>%
  mutate(country =recode(country, AUT= "EU27", BEL ="EU27", BGR="EU27", HRV="EU27", CYP ="EU27",CZE="EU27", DNK = "EU27", 
                         EST="EU27", FIN ="EU27", FRA="EU27", DEU="EU27", GRC="EU27", HUN="EU27", IRL="EU27", ITA="EU27", 
                         LTU= "EU27", LUX="EU27", LVA="EU27", MLT= "EU27", NLD = "EU27", POL="EU27", PRT ="EU27", ROU= "EU27",
                         SWE="EU27", SVK="EU27", ESP ="EU27", SVN="EU27")) %>%
  group_by(source,unit,entity,scenario, country, Year)%>%
  summarise(Value= sum(Value))%>%
  ungroup %>%
  mutate (marker= ifelse(scenario=="SSP2BLMESGB",1,0))%>%
  filter (marker==1)

FutureGHGTop10.EU <- FutureGHG.EU %>%
  filter (grepl("CHN|USA|EU27|IND|RUS|JPN|BRA|IDN|IRN|SAU",country)) %>%
  filter (marker==1 & Year>=2000 & Year <=2050)


FutureGHGTop10.EU <- FutureGHG.EU %>%
  filter (grepl("CHN|USA|EU27|IND|RUS|JPN|BRA|IDN|IRN|SAU",country)) %>%
  filter (marker==1 & Year>=2000 & Year<=2050)

addWorksheet(EmissionsTrajectories, "FutureTrajectory_Top10_EU")
writeData(EmissionsTrajectories, sheet = "FutureTrajectory_Top10_EU", x = FutureGHGTop10.EU)

#---- 3.3 Extract emissions trajectories for Top 20 current emitters (with EU as independent nation states)
TopEmitterNames <- SSP2BL %>%
  select(source,scenario, country,entity,unit,X2020) %>%
  filter (grepl("KYOTOGHGAR4", entity)) %>%
  filter(!grepl("ANNEXI|NONANNEXI|AOSIS|BASIC|LDC|UMBRELLA|EU28", country)) %>%
  mutate (marker= ifelse(scenario=="SSP2BLMESGB",1,0))%>%
  filter (marker==1) %>%
  slice_max(X2020, n=11) %>%  #Top 11, because includes total listed as "EARTH".  
  select(country)

FutureGHGTop10 <- FutureGHG %>%
  filter (grepl("CHN|USA|IND|RUS|JPN|BRA|IDN|DEU|IRN|SAU",country)) %>%
  filter (marker==1 & Year>=2000 & Year<=2050)


addWorksheet(EmissionsTrajectories, "FutureTrajectory_Top10")
writeData(EmissionsTrajectories, sheet = "FutureTrajectory_Top10", x =FutureGHGTop10)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: EXPORT DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

saveWorkbook(EmissionsTrajectories, "EmissionsTrajectories.xlsx")
