### SCF Emissions By Sector ###

#Code Sections
# 1.  Import data
# 2.  Emissions By Sector Major Emitters

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

# ---- 1.2 Import data ----

GHGSector<- data.frame(read.csv("CAIT_GHG_sector.csv"))

#--- 1.3 Set up xlsx output file

EmissionsTrajectories <- createWorkbook()

MajorEmittersSector  <- GHGSector %>%
  filter(Entity=="China" |Entity =="India" |Entity=="European Union (27)" |Entity == "United States"|
          Entity=="Russia"|Entity=="Indonesia"|Entity=="Brazil"|Entity=="Japan"|Entity=="Mexico"|Entity=="Iran"
         | Entity=="Saudi Arabia") %>%
  filter(Year==1990 |Year==1995 |Year ==2000 |Year==2005 |Year==2010 | Year==2015 |Year ==2016)

MajorEmitterSector2016 <- GHGSector %>%
  filter(Entity=="China" |Entity =="India" |Entity=="European Union (27)" |Entity == "United States"|
           Entity=="Russia"|Entity=="Indonesia"|Entity=="Brazil"|Entity=="Japan"|Entity=="Mexico"|Entity=="Iran"
         | Entity=="Saudi Arabia") %>%
  filter(Year ==2016)
