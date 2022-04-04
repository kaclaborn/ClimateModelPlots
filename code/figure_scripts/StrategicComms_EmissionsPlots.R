# 
# code: Create emissions trajectories plots for Strategic Comms team
# 
# ---- sections ----
# 1.  Source Plot Themes, Pre-Process Data
# 2.  Annex II Country Emissions Plots - STACKED
# 3.  Single-Country Emissions Plots - WITH UNCERTAINTY
# 4.  Export
# 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE PLOT THEMES, PRE-PROCESS DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 1.1 Source PlotThemes.R ----

eval(parse('code/PlotThemes.R', encoding = 'UTF-8'))


# ---- 1.2 Name specific scenarios, markers, and countries ----

scenario.ssp2 <- "SSP2BL" 
marker.ssp2 <- "SSP2BLMESGB" # marker model for scenario

scenario.ssp1 <- "SSP1BL"
marker.ssp1 <- "SSP1BLIMAGE" # marker model for scenario

annexII.countries <- # all Annex II countries, except for Iceland and New Zealand, given their very small emissions
  data.frame(country = c("USA", "CAN", "EU27", "GBR", "JPN", "AUS"),
             country.name = c("United States", "Canada", "EU-27", "United Kingdom",
                              "Japan", "Australia")) %>%
  mutate(country.name = factor(country.name, 
                               levels = c("United States", "Canada", "EU-27", "United Kingdom",
                                          "Japan", "Australia"),
                               ordered = TRUE),
         country.label = c("United\nStates", "Canada", "EU-27", "United\nKingdom", "Japan", "Australia"))


source.label.gutschow.SSP1SSP2.pareddown <- "Source:         Gütschow et al (2020). <https://doi.org/10.5281/zenodo.3638137>\n\nModel Note:  The highlighted line represents the marker model for each SSP; uncertainty bands in the Annual Emissions plot (left)\n                      represent the minimum and maximum values across all five integrated assessment models.\n\n                      The SSP1 reference scenario illustrates modelled emissions trajectories in a 'sustainable development' paradigm,,\n                      with less resource intensive lifestyles, global cooperation, and high economic growth. It does not include any climate policies beyond\n                       those in place today. More details on the SSP1 marker model can be found at <https://doi.org/10.1016/j.gloenvcha.2016.05.008>\n\n                      The SSP2 reference scenario illustrates modelled emissions trajectories if historic energy supply/demand and land use patterns\n                      persist into the future. It does not include any climate policies beyond those in place today.\n                      More details on the SSP2 marker model can be found at <https://doi.org/10.1016/j.gloenvcha.2016.06.004>\n\nUnits:            Gigatonnes (Gt) CO2e"




# ---- 1.3 Process Gutschow data for SSP2 ----

gutschow.filtered.ssp2 <- gutschow.data %>%
  filter(grepl("PMSSPBIE", source) & 
           grepl(scenario.ssp2, scenario) & # This subsets to all baseline scenarios for each of the different IAMs (listed under SSP2)
           !grepl(paste(clean.rows, collapse = "|"), country))


# -- Historic and future GHG trajectories for Annex II countries (with EU included as single entity) ----

FutureGHG.ssp2 <- gutschow.filtered.ssp2 %>%
  tidyr::pivot_longer(c(`1850`:`2100`), names_to = "year", values_to = "value") %>%
  filter(entity=="KYOTOGHGAR4") %>%
  mutate(year = as.numeric(year),
         country = recode(country, AUT = "EU27", BEL = "EU27", BGR = "EU27", HRV = "EU27", CYP = "EU27", CZE = "EU27", DNK = "EU27", 
                          EST = "EU27", FIN = "EU27", FRA = "EU27", DEU = "EU27", GRC = "EU27", HUN = "EU27", IRL = "EU27", ITA = "EU27", 
                          LTU =  "EU27", LUX = "EU27", LVA = "EU27", MLT = "EU27", NLD = "EU27", POL = "EU27", PRT = "EU27", ROU = "EU27",
                          SWE = "EU27", SVK = "EU27", ESP = "EU27", SVN = "EU27")) %>%
  group_by(source, unit, entity, scenario, country, year) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  mutate(marker = ifelse(scenario==marker.ssp2, 1, 0)) %>%
  filter(marker==1 & country%in%annexII.countries$country) %>%
  left_join(annexII.countries, by = "country")


# ---- 1.4 Process Gutschow data for SSP5 ----

gutschow.filtered.ssp1 <- gutschow.data %>%
  filter(grepl("PMSSPBIE", source) & 
           grepl(scenario.ssp1, scenario) & # This subsets to all baseline scenarios for each of the different IAMs (listed under SSP1)
           !grepl(paste(clean.rows, collapse = "|"), country))


# -- Historic and future GHG trajectories for Annex II countries (with EU included as single entity) ----

FutureGHG.ssp1 <- gutschow.filtered.ssp1 %>%
  tidyr::pivot_longer(c(`1850`:`2100`), names_to = "year", values_to = "value") %>%
  filter(entity=="KYOTOGHGAR4") %>%
  mutate(year = as.numeric(year),
         country = recode(country, AUT = "EU27", BEL = "EU27", BGR = "EU27", HRV = "EU27", CYP = "EU27", CZE = "EU27", DNK = "EU27", 
                          EST = "EU27", FIN = "EU27", FRA = "EU27", DEU = "EU27", GRC = "EU27", HUN = "EU27", IRL = "EU27", ITA = "EU27", 
                          LTU =  "EU27", LUX = "EU27", LVA = "EU27", MLT = "EU27", NLD = "EU27", POL = "EU27", PRT = "EU27", ROU = "EU27",
                          SWE = "EU27", SVK = "EU27", ESP = "EU27", SVN = "EU27")) %>%
  group_by(source, unit, entity, scenario, country, year) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  mutate(marker = ifelse(scenario==marker.ssp1, 1, 0)) %>%
  filter(marker==1 & country%in%annexII.countries$country) %>%
  left_join(annexII.countries, by = "country")


# ---- 1.5 Process Gutschow data for all SSPs with uncertainty ----

gutschow.allssp <- gutschow.data %>%
  filter(grepl("PMSSPBIE", source) & 
           grepl(paste(scenario.allssp, collapse="|"), scenario) &
           !grepl(paste(clean.rows, collapse = "|"), country))

FutureGHG.AnnexII.allssp <- gutschow.allssp %>%
  tidyr::pivot_longer(c(`1950`:`2100`), names_to = "year", values_to = "value") %>%
  filter(entity=="KYOTOGHGAR4") %>%
  mutate(year = as.numeric(year),
         country = recode(country, AUT = "EU27", BEL = "EU27", BGR = "EU27", HRV = "EU27", CYP = "EU27", CZE = "EU27", DNK = "EU27", 
                          EST = "EU27", FIN = "EU27", FRA = "EU27", DEU = "EU27", GRC = "EU27", HUN = "EU27", IRL = "EU27", ITA = "EU27", 
                          LTU =  "EU27", LUX = "EU27", LVA = "EU27", MLT = "EU27", NLD = "EU27", POL = "EU27", PRT = "EU27", ROU = "EU27",
                          SWE = "EU27", SVK = "EU27", ESP = "EU27", SVN = "EU27")) %>%
  group_by(source, unit, entity, scenario, country, year) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  mutate(marker.allssp = ifelse(scenario%in%marker.allssp, 1, 0),
         SSP = ifelse(grepl("SSP1", scenario), "SSP 1",
                      ifelse(grepl("SSP2", scenario), "SSP 2",
                             ifelse(grepl("SSP3", scenario), "SSP 3",
                                    ifelse(grepl("SSP4", scenario), "SSP 4",
                                           ifelse(grepl("SSP5", scenario), "SSP 5", NA)))))) %>%
  group_by(SSP, country, year) %>%
  mutate(lower = min(value, na.rm = T),
         upper = max(value, na.rm = T)) %>%
  ungroup() %>%
  filter(marker.allssp==1 & country%in%annexII.countries$country) %>%
  left_join(annexII.countries, by = "country")



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: ANNEX II COUNTRY EMISSIONS PLOTS - STACKED ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Historical and future trajectories (1950 - 2050) - Top 10 emitters (2018 as reference to determine top emitters) ----

Stacked.AnnexIIEmissions.SSP2.Plot <-
  ggplot(FutureGHG.ssp2 %>% filter(year>=1950 & year<=2050), aes(x = year, y = value/1000000)) +
  geom_area(aes(group = country.name, fill = country.name)) +
  geom_vline(aes(xintercept = 2018),
             size = 0.5,
             colour = "#909090",
             alpha = 0.5) +
  annotate("text", x = 2015, y = 19, label = "2018", size = 2.5, colour = "#909090") +
  scale_fill_manual(values = colours.6categories) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(1950, 2050, by = 10),
                     labels = c("1950", "", "1970", "", "1990", "", "2010", "", "2030", "", "2050")) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0, 20),
                     breaks = seq(5, 20, by = 5),
                     labels = c("5 Gt", "10 Gt", "15 Gt", "20 Gt")) +
  plot.theme.top10 + legend.guide.top10 +
  labs(title = "Annual GHG Emissions Trajectories: UNFCCC Annex II Countries", 
       subtitle = "Historic and future projected emissions (Gt CO2e) of Kyoto greenhouse gases (AR4) for \nIPCC Shared Socioeconomic Pathway 2 “Middle of the Road” reference case")

Stacked.AnnexIIEmissions.SSP2.Arranged <- 
  grid.arrange(Stacked.AnnexIIEmissions.SSP2.Plot, 
               bottom = grid.text(label = source.label.gutschow.AnnexII, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- 2.2 SSP1 Stacked Plot ----

Stacked.AnnexIIEmissions.SSP1.Plot <-
  ggplot(FutureGHG.ssp1 %>% filter(year>=1950 & year<=2050), aes(x = year, y = value/1000000)) +
  geom_area(aes(group = country.name, fill = country.name)) +
  geom_vline(aes(xintercept = 2018),
             size = 0.5,
             colour = "#909090",
             alpha = 0.5) +
  annotate("text", x = 2015, y = 19, label = "2018", size = 2.5, colour = "#909090") +
  scale_fill_manual(values = colours.6categories) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(1950, 2050, by = 10),
                     labels = c("1950", "", "1970", "", "1990", "", "2010", "", "2030", "", "2050")) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0, 20),
                     breaks = seq(5, 20, by = 5),
                     labels = c("5 Gt", "10 Gt", "15 Gt", "20 Gt")) +
  plot.theme.top10 + legend.guide.top10 +
  labs(title = "Annual GHG Emissions Trajectories: UNFCCC Annex II Countries", 
       subtitle = "Historic and future projected emissions (Gt CO2e) of Kyoto greenhouse gases (AR4) for \nIPCC Shared Socioeconomic Pathway 1 “Sustainable Development” reference case")

Stacked.AnnexIIEmissions.SSP1.Arranged <- 
  grid.arrange(Stacked.AnnexIIEmissions.SSP1.Plot, 
               bottom = grid.text(label = source.label.gutschow.AnnexII, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: SINGLE-COUNTRY EMISSIONS PLOTS - WITH UNCERTAINTY ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Projected annual emissions & growth for each country (SSP1 and SSP2, with model uncertainty) ----

# -- Projected annual emissions, across SSP1 and SSP2

for(i in 1:length(annexII.countries$country)){
  a <- annexII.countries$country[i]
  b <- annexII.countries$country.name[i]
  
  label.Gt <- function(x) format(paste0((x), " Gt", sep = ""))
  
  assign(paste("SSP1SSP2", a, sep = "."), 
         ggplot(FutureGHG.AnnexII.allssp %>% filter(SSP%in%c("SSP 1", "SSP 2") & year>2017 & country==a), 
                aes(x = year)) +
           geom_ribbon(aes(ymin = lower/1000000, ymax = upper/1000000, group = SSP, fill = SSP),
                       alpha = 0.3,
                       show.legend = F) +
           geom_line(aes(y = value/1000000, group = SSP, colour = SSP),
                     size = 1.5) +
           scale_colour_manual(name = "",
                               values = c("#332288", "#CC6677"),
                               labels = c("SSP 1: Sustainability", "SSP 2: Middle of the Road")) +
           scale_fill_manual(name = "",
                             values = c("#332288", "#CC6677")) +
           scale_y_continuous(expand = c(0, 0),
                              labels = label.Gt) +
           scale_x_continuous(name = "",
                              expand = c(0, 0),
                              breaks = seq(2020, 2100, by = 20)) +
           guides(colour = guide_legend(reverse = T)) +
           plot.theme.top10 + labs(y = "", title = paste("Annual GHG Emissions Trajectories: ", b, sep = ""),
                                   subtitle = "Future projected emissions (Gt CO2e) of Kyoto greenhouse gases (AR4)\nacross two IPCC Shared Socioeconomic Pathway reference cases"))
  
  assign(paste("SSP1SSP2", a, "Arranged", sep = "."),
         grid.arrange(get(paste("SSP1SSP2", a, sep = ".")),
                      bottom = grid.text(label = source.label.gutschow.SSP1SSP2.pareddown, 
                                         x = unit(50, "pt"),
                                         just = "left",
                                         gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
                      padding = unit(5, "pt"), 
                      vp = viewport(width = 1, height = 0.95)))
  
  } 



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: CARBON MAJORS, TOP 20 ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


CarbonMajors <- import('data/inputs/CarbonMajors-Top20-1965-2018.csv') %>%
  filter(Entity!="Global") %>%
  mutate(EntityCountry = paste0(Entity, "\n", "(", Country, ")", sep = ""),
         EntityCountry_ordered = factor(EntityCountry, levels = rev(EntityCountry), ordered = T))


CarbonMajorsIOC <- import('data/inputs/CarbonMajors_IOC_cumulative_1850-2018.csv')

CarbonMajorsIOC.byCountry <- # NOTE: Norway is not an EU member state, so is removed from all analyses for these figures
  CarbonMajorsIOC %>%
  mutate(Country = ifelse(Country%in%c("Austria", "France", "Germany", "Netherlands", "Switzerland", "Spain", "Italy"), "EU-27", 
                          ifelse(Country=="USA", "United States",
                                 ifelse(Country=="UK", "United Kingdom",
                                        Country)))) %>%
  filter(Country%in%annexII.countries$country.name) %>%
  group_by(Country) %>%
  summarise(MtCO2e = sum(MtCO2e),
            nIOC = length(EntityIOC)) %>%
  ungroup() %>%
  mutate(GlobalMtCO2e = CarbonMajorsIOC$MtCO2e[CarbonMajorsIOC$EntityIOC=="CDIAC"],
         PercentGlobal = MtCO2e / GlobalMtCO2e) %>%
  rename("country.name" = "Country")

Global.CumulativeEmissions <- 
  GlobalGHG %>%
  filter(year%in%c(1850:2017)) %>%
  summarise(totalCO2e = sum(totalGHG)/1000)

AnnexII.CumulativeEmissions <-
  FutureGHG.EU %>% 
  left_join(annexII.countries, by = "country") %>%
  filter(!is.na(country.name) & year%in%c(1850:2018)) %>%
  group_by(country.name) %>%
  summarise(MtCO2e = sum(value)/1000) %>%
  ungroup() %>%
  mutate(GlobalMtCO2e = Global_CumulativeEmissions$totalCO2e,
         PercentGlobal = MtCO2e / GlobalMtCO2e)


# -- CHECK global emissions values from Gutschow with those from Climate Accountability
# NOTE: Climate Accountability data only uses CO2 and CH4, while we use all Kyoto AR4 gases in Gutschow.
# 
# GlobalEmissions.check <- gutschow.filtered %>%
#   filter((entity=="CO2" | entity=="CH4") & country=="EARTH") %>%
#   tidyr::pivot_longer(c(`1850`:`2100`), names_to = "year", values_to = "value") %>%
#   mutate(marker = ifelse(scenario==marker.scenario, 1, 0),
#          year = as.numeric(year),
#          CO2e = ifelse(entity=="CH4", value * 25, value)) %>%
#   filter(year%in%c(1850:2017) & marker==1) %>%
#   summarise(totalCO2e = sum(CO2e)/1000,
#             CH4 = sum(value[entity=="CH4"])/1000,
#             CO2 = sum(CO2e[entity=="CO2"]/1000),
#             unit = "million Mt")


# ---- 4.1 Top 20 Carbon Majors Plot ----

CarbonMajorsPlot <-
  ggplot(CarbonMajors %>% arrange(MtCO2e), aes(x = EntityCountry_ordered, y = MtCO2e)) +
  geom_bar(fill = "#23117D", alpha = 0.9, stat = "identity") +
  geom_text(aes(y = 3000, label = paste(PercentGlobal, "%", sep = "")),
            colour = "white", size = 3) +
  annotate("text", x = 20, y = 9500, label = "global share", size = 3, colour = "white") +
  scale_x_discrete(name = "") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, max(CarbonMajors$MtCO2e)+0.2*max(CarbonMajors$MtCO2e))) +
  coord_flip() + plot.theme.top30 + labs(title = "Top 20 Carbon Majors by Emissions", 
                                         subtitle = "Cumulative emissions from 1965-2018")


CarbonMajorsArranged <-
  grid.arrange(CarbonMajorsPlot,
               bottom = grid.text(label = "Source: Climate Accountability Initiative <https://climateaccountability.org/carbonmajors_dataset2020.html>", 
                                  x = unit(105, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- 4.2 Only Investor-owned from the Top 20 ----

CarbonMajorsPlot.InvestorOwned <-
  ggplot(CarbonMajors %>% filter(InvestorOwned==1) %>% arrange(MtCO2e), aes(x = EntityCountry_ordered, y = MtCO2e)) +
  geom_bar(fill = "#23117D", alpha = 0.9, stat = "identity") +
  geom_text(aes(y = 3000, label = paste(PercentGlobal, "%", sep = "")),
            colour = "white", size = 3) +
  annotate("text", x = 8, y = 9000, label = "global share", size = 3, colour = "white") +
  scale_x_discrete(name = "") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, max(CarbonMajors$MtCO2e)+0.2*max(CarbonMajors$MtCO2e))) +
  coord_flip() + plot.theme.top30 + labs(title = "Top Investor-Owned Carbon Majors by Emissions", 
                                         subtitle = "Cumulative emissions from 1965-2018")


CarbonMajors.InvestorOwned.Arranged <-
  grid.arrange(CarbonMajorsPlot.InvestorOwned,
               bottom = grid.text(label = "Source: Climate Accountability Initiative <https://climateaccountability.org/carbonmajors_dataset2020.html>", 
                                  x = unit(85, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- 4.3 Include full Top 20 with investor-owned separate color ----

CarbonMajorsPlot.Highlight.InvestorOwned <-
  ggplot(CarbonMajors %>% arrange(MtCO2e), aes(x = EntityCountry_ordered, y = MtCO2e)) +
  geom_bar(aes(fill = as.character(InvestorOwned)), alpha = 0.9, stat = "identity") +
  geom_text(aes(y = 3000, label = paste(PercentGlobal, "%", sep = "")),
            colour = "white", size = 3) +
  annotate("text", x = 20, y = 9000, label = "global share", size = 3, colour = "white") +
  scale_fill_manual(name = "",
                    labels = c("Not Investor Owned",
                               "Investor Owned"),
                    values = c("#23117D", "#44AA99")) +
  scale_x_discrete(name = "") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, max(CarbonMajors$MtCO2e)+0.2*max(CarbonMajors$MtCO2e))) +
  coord_flip() + plot.theme.top30 + labs(title = "Top 20 Carbon Majors by Emissions",
                                         subtitle = "Cumulative emissions from 1965-2018")


CarbonMajors.InvestorOwned.Highlight.Arranged <-
  grid.arrange(CarbonMajorsPlot.Highlight.InvestorOwned,
               bottom = grid.text(label = "Source: Climate Accountability Initiative <https://climateaccountability.org/carbonmajors_dataset2020.html>", 
                                  x = unit(105, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- 4.4 Pie chart of carbon majors global percent share (compared to global emissions) ----




for(i in 1:length(annexII.countries$country)) {
  a <- annexII.countries$country.name[i]
  b <- annexII.countries$country[i]
  c <- annexII.countries$country.label[i]
  # colour <- colours.6categories[i]
  
  assign(paste("CountryEmissionsPie", b, sep = "."),
         ggplot(AnnexII.CumulativeEmissions %>% 
                  filter(country.name==a) %>% 
                  pivot_longer(c(`MtCO2e`, `GlobalMtCO2e`)), aes(x = "", y = value, fill = name)) +
           geom_bar(stat = "identity", width = 1, 
                    colour = "white", 
                    show.legend = F) +
           coord_polar("y", start = 0) + 
           geom_text(aes(x = 2, y = ifelse(unique(PercentGlobal*100>=12), 200000,
                                             ifelse(unique(PercentGlobal)*100>=10 & unique(PercentGlobal)*100<12, 170000, 
                                                    170000)), 
                         label = paste(round(unique(PercentGlobal)*100, 2), "%", sep = "")),
                     colour = "#909090", size = 3.5, fontface = "bold", lineheight = 0.9, show.legend = F) +
           scale_fill_manual(values = c("#C0C0C0", "#54ADAD")) +
           theme(plot.title = element_text(size = rel(1),
                                           colour = "#303030",
                                           face = "bold"),
                 plot.subtitle = element_text(size = rel(0.75),
                                              colour = "#303030"),
                 axis.ticks = element_blank(),
                 panel.background = element_rect(fill = "white",
                                                 colour = "white"),
                 panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
                 axis.title = element_blank(),
                 axis.text = element_blank()))
  
  assign(paste("IOCEmissionsPie", b, sep = "."),
         ggplot(CarbonMajorsIOC.byCountry %>% 
                  filter(country.name==a) %>% 
                  pivot_longer(c(`MtCO2e`, `GlobalMtCO2e`)), aes(x = "", y = value, fill = name)) +
           geom_bar(stat = "identity", width = 1, 
                    colour = "white", 
                    show.legend = F) +
           coord_polar("y", start = 0) + 
           geom_text(aes(x = 2, y = ifelse(unique(PercentGlobal*100>=12), 200000,
                                             ifelse(unique(PercentGlobal)*100>=10 & unique(PercentGlobal)*100<12, 170000, 
                                                    170000)), 
                         label = paste(round(unique(PercentGlobal)*100, 2), "%", sep = "")),
                     colour = "#909090", size = 3.5, fontface = "bold", lineheight = 0.9, show.legend = F) +
           scale_fill_manual(values = c("#C0C0C0", "#54ADAD")) +
           theme(plot.title = element_text(size = rel(1),
                                           colour = "#303030",
                                           face = "bold"),
                 plot.subtitle = element_text(size = rel(0.75),
                                              colour = "#303030"),
                 axis.ticks = element_blank(),
                 panel.background = element_rect(fill = "white",
                                                 colour = "white"),
                 panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
                 axis.title = element_blank(),
                 axis.text = element_blank()))
  

  
  assign(paste("PiesArranged", b, sep = "."),
         grid.arrange(grid.text(label = c, 
                                gp = gpar(fontsize = 11, fontface = "bold", lineheight = 1, col = "#303030")),
                      arrangeGrob(get(paste("CountryEmissionsPie", b, sep = ".")), 
                                   get(paste("IOCEmissionsPie", b, sep = ".")),
                                   nrow = 1),
                      nrow = 1,
                      padding = unit(5, "pt"), 
                      widths = unit(c(0.9, 2), "null"),
                      vp = viewport(width = 1, height = 0.95)))
  
}


PiesArranged <- grid.arrange(arrangeGrob(textGrob(label = "Global Share of Emissions, Annex II Countries (1850-2018)\n",
                                                  gp = gpar(fontsize = 13, fontface = "bold", col = "#303030"),
                                                  just = "left", x = 0),
                                         textGrob(label = "In this visual, we compare a country’s share of total global emissions\nfrom 1850-2018 and the share of global emissions of fossil fuel corporations\nthat are based in that same country. The emissions reflected in the\nlatter were produced by a corporation globally.\n",
                                                  gp = gpar(fontsize = 10, col = "#303030", lineheight = 0.9),
                                                  just = "left", x = 0),
                                         nrow = 2,
                                         padding = unit(10, "pt")),
                             arrangeGrob(textGrob(label = "\n"),
                                         textGrob(label = "\nCountry Total\n",
                                                  gp = gpar(fontsize = 11, fontface = "bold", col = "#303030")), 
                                         textGrob(label = "\nInvestor-Owned\nFossil Fuel\nCorporations\n",
                                                  gp = gpar(fontsize = 11, fontface = "bold", col = "#303030", lineheight = 0.9)),
                                         nrow = 1,
                                         widths = unit(c(1.3, 2, 1.5), "null")) ,
                             arrangeGrob(PiesArranged.USA,
                                         PiesArranged.CAN,
                                         PiesArranged.EU27,
                                         PiesArranged.GBR,
                                         PiesArranged.JPN,
                                         PiesArranged.AUS,
                                         ncol = 1),
                             # arrangeGrob(textGrob(label = ""),
                             #             textGrob(label = "Global Share by Country\n",
                             #                      gp = gpar(fontsize = 9, col = "#303030", lineheight = 0.9)),
                             #             textGrob(label = "Global Share of by Fossil Fuel\nCorporations Based in Country",
                             #                      gp = gpar(fontsize = 9, col = "#303030", lineheight = 0.9)),
                             #             nrow = 1),
                             arrangeGrob(textGrob(label = "\nSource: Country Total (global share of emissions) derived from Gütschow et al. (2020).\n             <https://doi.org/10.5281/zenodo.3638137>\n",
                                                  gp = gpar(fontsize = 9, col = "#303030", lineheight = 0.9),
                                                  just = "left", x = 0),
                                         textGrob(label = "             Investor-Owned Fossil Fuel Corporations (global share of emissions) derived\n             from Climate Accountability Initiative.\n             <https://climateaccountability.org/carbonmajors_dataset2020.html>\n",
                                                  gp = gpar(fontsize = 9, col = "#303030", lineheight = 0.9),
                                                  just = "left", x = 0),
                                         textGrob(label = "Note:    Country Total includes all Kyoto AR4 gases (in CO2e).\n             Investor-Owned Fossil Fuel Corporations includes CO2 and CH4 (in CO2e).",
                                                  gp = gpar(fontsize = 9, col = "#303030", lineheight = 0.9),
                                                  just = "left", x = 0),
                                         ncol = 1),
                             ncol = 1,
                             heights = unit(c(1.3, 1, 10, 2.5), "null"),
                             padding = unit(0, "pt"))


png(paste(FigureFileName, "/PiesArranged.png", sep = ""),
    units = "in", height = 10, width = 5, res = 400)
grid.newpage()
grid.draw(PiesArranged)
dev.off()



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: EXPORT PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 5.1 Stacked plots ----

png(paste(FigureFileName, "/AnnexII.EmissionsTrajectories.SSP2.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(Stacked.AnnexIIEmissions.SSP2.Arranged)
dev.off()

png(paste(FigureFileName, "/AnnexII.EmissionsTrajectories.SSP1.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(Stacked.AnnexIIEmissions.SSP1.Arranged)
dev.off()


# ---- 5.2 Individual country plots ----

png(paste(FigureFileName, "/EmissionsTrajectories.AUS.SSP1SSP2.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(SSP1SSP2.AUS.Arranged)
dev.off()

png(paste(FigureFileName, "/EmissionsTrajectories.CAN.SSP1SSP2.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(SSP1SSP2.CAN.Arranged)
dev.off()

png(paste(FigureFileName, "/EmissionsTrajectories.EU27.SSP1SSP2.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(SSP1SSP2.EU27.Arranged)
dev.off()

png(paste(FigureFileName, "/EmissionsTrajectories.GBR.SSP1SSP2.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(SSP1SSP2.GBR.Arranged)
dev.off()

png(paste(FigureFileName, "/EmissionsTrajectories.ISL.SSP1SSP2.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(SSP1SSP2.ISL.Arranged)
dev.off()

png(paste(FigureFileName, "/EmissionsTrajectories.JPN.SSP1SSP2.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(SSP1SSP2.JPN.Arranged)
dev.off()

png(paste(FigureFileName, "/EmissionsTrajectories.NZL.SSP1SSP2.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(SSP1SSP2.NZL.Arranged)
dev.off()

png(paste(FigureFileName, "/EmissionsTrajectories.USA.SSP1SSP2.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(SSP1SSP2.USA.Arranged)
dev.off()


# ---- 5.3 Carbon majors plot ----

png(paste(FigureFileName, "/CarbonMajors.Top20.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(CarbonMajorsArranged)
dev.off()

png(paste(FigureFileName, "/CarbonMajors.InvestorOwned.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(CarbonMajors.InvestorOwned.Arranged)
dev.off()

png(paste(FigureFileName, "/CarbonMajors.InvestorOwned.Highlighted.png", sep = ""),
    units = "in", height = 6, width = 10, res = 400)
grid.newpage()
grid.draw(CarbonMajors.InvestorOwned.Highlight.Arranged)
dev.off()
