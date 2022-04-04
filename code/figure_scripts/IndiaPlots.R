#
# code: India-specific emissions trajectories & state-level emissions
# 
# ---- sections ----
# 1.  SOURCE PLOT THEMES, IMPORT & WRANGLE DATA
# 2.  INDIA-SPECIFIC EMISSIONS TRAJECTORIES
# 3.  INDIA EMISSIONS IN GLOBAL CONTEXT
# 4.  INDIA EMISSIONS BY SECTOR
# 5.  INDIA EMISSIONS BY CURRENT & FUTURE INFRASTRUCTURE
# 6.  INDIA COLD CHAIN EMISSIONS
# 7.  INDIA EMISSIONS & ENERGY BY STATE
# 8.  EXPORT
# 9.  ADDITIONAL STATISTICS FOR INDIA
# 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE PLOT THEMES, IMPORT & WRANGLE DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 1.1 Source plot themes ----

eval(parse('code/PlotThemes.R', encoding = 'UTF-8'))


# ---- 1.2 Import flat files ----

Emissions.bySector.India <-
  import('data/inputs/India_Emissions_bySector_2016.csv')

Emissions.Infrastructure.India <-
  import('data/inputs/India_NewExisting_Infrastructure.csv')

CoolingDemand.bySector.India <-
  import('data/inputs/AEEE_2018_Demand_Analysis_Cooling_bySector_India.xlsx')

EnergyCapacity.byState.India <-
  import('data/inputs/Vasudha_India_energy_bystate.xlsx', which = 2) %>%
  rename(state = "State")


# ---- 1.3 Filter & wrangle emissions trajectories data for plots ----

India.FutureGHGs.AllSSPs <- FutureGHG.AllSSPs.Top10.EU %>%
  filter(country=="IND" & entity=="KYOTOGHGAR4" & year>1949) %>%
  mutate(SSP = ifelse(grepl("SSP1", scenario), "SSP 1",
                      ifelse(grepl("SSP2", scenario), "SSP 2",
                             ifelse(grepl("SSP3", scenario), "SSP 3",
                                    ifelse(grepl("SSP4", scenario), "SSP 4",
                                           ifelse(grepl("SSP5", scenario), "SSP 5", NA)))))) %>%
  group_by(SSP, country, year) %>%
  mutate(lower = min(value, na.rm = T),
         upper = max(value, na.rm = T)) %>%
  ungroup() %>%
  filter(marker.allssp==1)


India.FutureGHGs.AllScenarios <- FutureGHG.AllScenarios.Top10.EU %>%
  filter(country=="IND" & entity=="KYOTOGHGAR4" & year>1949) %>%
  mutate(SSP = ifelse(grepl("SSP1", scenario), "SSP 1",
                      ifelse(grepl("SSP2", scenario), "SSP 2",
                             ifelse(grepl("SSP3", scenario), "SSP 3",
                                    ifelse(grepl("SSP4", scenario), "SSP 4",
                                           ifelse(grepl("SSP5", scenario), "SSP 5", NA))))))


FutureGHGs.AllSSPs.Wrangled <- FutureGHG.AllSSPs.Top10.EU %>%
  select(-c(unit, source)) %>%
  tidyr::pivot_wider(names_from = entity, values_from = value) %>%
  mutate(Percent.GDP = KYOTOGHGAR4 / GDPPPP, # emissions (GgCO2e) per GDP (Million2011GKD)
         Percent.POP = (KYOTOGHGAR4/1000000) / (POP/1000)) %>% # emissions (GtCO2e) per million people
  group_by(country, scenario) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(Percent.change = (KYOTOGHGAR4 / lag(KYOTOGHGAR4) - 1)) %>% # percent change of emissions from one year to the next, per country/scenario
  ungroup()


Emissions.bySector.India <-
  Emissions.bySector.India %>%
  filter(category!="") %>%
  select(category, value, percent.label) %>%
  arrange(desc(value)) %>%
  mutate(ypos = cumsum(value)- 0.5*value,
         category = ifelse(grepl("Industrial", category), "Industrial\nProcesses &\nProduct Use", 
                           ifelse(grepl("Waste", category), "Waste\nProcesses", category)),
         lab = paste0(category, "\n", "(", percent.label, ")"),
         order = c("d", "c", "b", "a"))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: INDIA-SPECIFIC EMISSIONS TRAJECTORIES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Create SSP plot legend ----

SSP.legendplot <-   
  ggplot(India.FutureGHGs.AllSSPs, aes(x = year)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, group = SSP, fill = SSP)) +
  scale_fill_manual(name = "",
                    values = colours.5categories,
                    labels = c("SSP 1: Sustainability", "SSP 2: Middle of the Road",
                               "SSP 3: Regional Rivalry", "SSP 4: Inequality", "SSP 5: Fossil-Fueled Development")) +
  theme(legend.justification = "right",
        legend.margin = margin(0, 20, 30, 0),
        legend.key.width = unit(0.75, "cm"),
        legend.key.height = unit(0.5, "cm"))

SSP.legend <- get_legend(SSP.legendplot)


# ---- 2.2 Plot emissions trajectories for India (all SSPs, baseline marker models) ----

India.AllSSPs.plot <- 
  ggplot(India.FutureGHGs.AllSSPs, aes(x = year)) +
  geom_line(aes(y = value/1000000, group = SSP, colour = SSP),
            size = 1.5) +
  geom_vline(aes(xintercept = 2018),
             size = 0.5, 
             colour = "#909090",
             alpha = 0.5) +
  annotate("text", x = 2015, y = 1, label = "2018", size = 2.5, colour = "#909090") +
  geom_line(data = India.AllGHGs.AllSSPs %>% filter(marker==1), aes(x = year, y = value/1000000),
            colour = colours.5categories[2],
            size = 1.5) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(1950, 2100, by = 10),
                     labels = c("1950", "", "1970", "", "1990", "", "2010", "", "2030", "", "2050", "", "2070", "", "2090", "")) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0,30),
                     breaks = seq(5, 25, by = 5),
                     labels = c("5 Gt", "10 Gt", "15 Gt", "20 Gt", "25 Gt")) +
  scale_colour_manual(name = "",
                      values = colours.5categories,
                      labels = c("SSP 1: Sustainability", "SSP 2: Middle of the Road",
                                 "SSP 3: Regional Rivalry", "SSP 4: Inequality", "SSP 5: Fossil-Fueled Development")) +
  theme(plot.title = element_text(size = rel(1),
                                  colour = "#303030",
                                  face = "bold"),
        plot.subtitle = element_text(size = rel(0.75),
                                     colour = "#303030"),
        axis.ticks.x = element_line(colour = "#C0C0C0"),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "#909090"),
        panel.border = element_rect(fill = NA,
                                    size = 0.25,
                                    colour = "#C0C0C0"),
        panel.grid.major.y = element_line(colour = "#C0C0C0",
                                          size = 0.35,
                                          linetype = 3),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "pt"),
        axis.title = element_text(size = rel(0.9),
                                  angle = 0,
                                  face = "bold",
                                  colour = "#303030"),
        axis.text = element_text(size = rel(0.9),
                                 angle = 0,
                                 colour = "#303030",
                                 lineheight = 0.7),
        legend.key = element_rect(fill = "white"),
        legend.position = c(0.4, 0.95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background = element_rect(fill = "white",
                                         colour = "#909090")) + 
  legend.guide.top10 + labs(title = "Annual GHG Emissions Trajectories: India",
                            subtitle = "Future projected emissions of Kyoto greenhouse gases (AR4) in CO2e, across all Shared Socioeconomic Pathways baseline scenarios")


India.AllSSPs.Arranged <- 
  grid.arrange(India.AllSSPs.plot, 
               bottom = grid.text(label = source.label.gutschow, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- 2.3 Emissions trajectories for each SSP on own plot (with model uncertainty), creating 5x1 image of all SSPs ----

India.SSP1.plot <- 
  ggplot(India.FutureGHGs.AllSSPs %>% filter(SSP=="SSP 1" & year>2017), aes(x = year)) +
  geom_ribbon(aes(ymin = lower/1000000, ymax = upper/1000000, group = SSP, fill = SSP),
              alpha = 0.3,
              show.legend = F) +
  geom_line(aes(y = value/1000000, group = SSP, colour = SSP),
            size = 1.5,
            show.legend = F) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(2020, 2100, by = 10),
                     labels = c("", "2030", "", "2050", "", "2070", "", "2090", "")) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0,30),
                     breaks = seq(5, 25, by = 5),
                     labels = c("5 Gt", "10 Gt", "15 Gt", "20 Gt", "25 Gt")) +
  scale_colour_manual(name = "",
                      values = colours.5categories[1]) +
  scale_fill_manual(values = colours.5categories[1]) +
  plot.theme.comparisons


India.SSP2.plot <- 
  ggplot(India.FutureGHGs.AllSSPs %>% filter(SSP=="SSP 2" & year>2017), aes(x = year)) +
  geom_ribbon(aes(ymin = lower/1000000, ymax = upper/1000000, group = SSP, fill = SSP),
              alpha = 0.3,
              show.legend = F) +
  geom_line(aes(y = value/1000000, group = SSP, colour = SSP),
            size = 1.5,
            show.legend = F) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(2020, 2100, by = 10),
                     labels = c("", "2030", "", "2050", "", "2070", "", "2090", "")) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0,30),
                     breaks = seq(5, 25, by = 5),
                     labels = c("5 Gt", "10 Gt", "15 Gt", "20 Gt", "25 Gt")) +
  scale_colour_manual(name = "",
                      values = colours.5categories[2]) +
  scale_fill_manual(values = colours.5categories[2]) +
  plot.theme.comparisons


India.SSP3.plot <- 
  ggplot(India.FutureGHGs.AllSSPs %>% filter(SSP=="SSP 3" & year>2017), aes(x = year)) +
  geom_ribbon(aes(ymin = lower/1000000, ymax = upper/1000000, group = SSP, fill = SSP),
              alpha = 0.3,
              show.legend = F) +
  geom_line(aes(y = value/1000000, group = SSP, colour = SSP),
            size = 1.5,
            show.legend = F) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(2020, 2100, by = 10),
                     labels = c("", "2030", "", "2050", "", "2070", "", "2090", "")) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0,30),
                     breaks = seq(5, 25, by = 5),
                     labels = c("5 Gt", "10 Gt", "15 Gt", "20 Gt", "25 Gt")) +
  scale_colour_manual(name = "",
                      values = colours.5categories[3]) +
  scale_fill_manual(values = colours.5categories[3]) +
  plot.theme.comparisons


India.SSP4.plot <- 
  ggplot(India.FutureGHGs.AllSSPs %>% filter(SSP=="SSP 4" & year>2017), aes(x = year)) +
  geom_ribbon(aes(ymin = lower/1000000, ymax = upper/1000000, group = SSP, fill = SSP),
              alpha = 0.3,
              show.legend = F) +
  geom_line(aes(y = value/1000000, group = SSP, colour = SSP),
            size = 1.5,
            show.legend = F) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(2020, 2100, by = 10),
                     labels = c("", "2030", "", "2050", "", "2070", "", "2090", "")) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0,30),
                     breaks = seq(5, 25, by = 5),
                     labels = c("5 Gt", "10 Gt", "15 Gt", "20 Gt", "25 Gt")) +
  scale_colour_manual(name = "",
                      values = colours.5categories[4]) +
  scale_fill_manual(values = colours.5categories[4]) +
  plot.theme.comparisons


India.SSP5.plot <- 
  ggplot(India.FutureGHGs.AllSSPs %>% filter(SSP=="SSP 5" & year>2017), aes(x = year)) +
  geom_ribbon(aes(ymin = lower/1000000, ymax = upper/1000000, group = SSP, fill = SSP),
              alpha = 0.3,
              show.legend = F) +
  geom_line(aes(y = value/1000000, group = SSP, colour = SSP),
            size = 1.5,
            show.legend = F) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(2020, 2100, by = 10),
                     labels = c("", "2030", "", "2050", "", "2070", "", "2090", "")) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0,30),
                     breaks = seq(5, 25, by = 5),
                     labels = c("5 Gt", "10 Gt", "15 Gt", "20 Gt", "25 Gt")) +
  scale_colour_manual(name = "",
                      values = colours.5categories[5]) +
  scale_fill_manual(values = colours.5categories[5]) +
  plot.theme.comparisons


# ---- 2.4 Arrange the 5 SSPs into single plot (with legend) ----


SSP.title <- textGrob("Annual GHG Emissions Trajectories - India", 
                      gp = gpar(fontsize = 13, fontface = "bold"), 
                      x = unit(45, "pt"),
                      just = "left")
SSP.subtitle <- textGrob("Future projected emissions of Kyoto greenhouse gases (AR4) in CO2e\nAll Shared Socioeconomic Pathways baseline scenarios (with model uncertainty)\n", 
                         gp = gpar(fontsize = 10, lineheight = 0.9),
                         x = unit(45, "pt"),
                         just = "left")

margin <- unit(0.5, "line")

India.AllSSPs.WithUncertainty <- 
  grid.arrange(India.SSP1.plot, India.SSP2.plot, India.SSP3.plot, 
               India.SSP4.plot, India.SSP5.plot, SSP.legend,
               ncol = 3, nrow = 2) %>%
  grid.arrange(SSP.title, SSP.subtitle, ., 
               heights = unit.c(grobHeight(SSP.title) + 1.2*margin,
                                grobHeight(SSP.subtitle) + margin,
                                unit(1, "null")),
               bottom = grid.text(label = source.label.gutschowallssp, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- 2.5 Include radiative forcing scenarios into SSP 2 (India-specific) plot ----

India.AllScenario.SSP2 <-
  ggplot(India.FutureGHGs.AllScenarios %>% filter(SSP=="SSP 2"), aes(x = year)) +
  geom_line(aes(y = value/1000000, group = scenario, colour = scenario)) +
  geom_vline(aes(xintercept = 2018),
             size = 0.5, 
             colour = "#909090",
             alpha = 0.5) +
  annotate("text", x = 2015, y = 1, label = "2018", size = 2.5, colour = "#909090") +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(1950, 2100, by = 10),
                     labels = c("1950", "", "1970", "", "1990", "", "2010", "", "2030", "", "2050", "", "2070", "", "2090", "")) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0)) +
  scale_colour_ptol() + plot.theme.top10
  

India.AllScenario.SSP5 <-
  ggplot(India.FutureGHGs.AllScenarios %>% filter(SSP=="SSP 5"), aes(x = year)) +
  geom_line(aes(y = value/1000000, group = scenario, colour = scenario)) +
  geom_vline(aes(xintercept = 2018),
             size = 0.5, 
             colour = "#909090",
             alpha = 0.5) +
  annotate("text", x = 2015, y = 1, label = "2018", size = 2.5, colour = "#909090") +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(1950, 2100, by = 10),
                     labels = c("1950", "", "1970", "", "1990", "", "2010", "", "2030", "", "2050", "", "2070", "", "2090", "")) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0)) +
  scale_colour_ptol() + plot.theme.top10


# ---- 2.6 Projected annual emissions & growth for India (SSP1 and SSP2, with model uncertainty) ----

# -- Projected annual emissions for India, across SSP1 and SSP2
FutureGHGs.India.SSP1SSP2 <- 
  ggplot(India.FutureGHGs.AllSSPs %>% filter(SSP%in%c("SSP 1", "SSP 2") & year>2017), 
         aes(x = year)) +
  geom_ribbon(aes(ymin = lower/1000000, ymax = upper/1000000, group = SSP, fill = SSP),
              alpha = 0.3,
              show.legend = F) +
  geom_line(aes(y = value/1000000, group = SSP, colour = SSP),
            size = 1.5,
            show.legend = F) +
  scale_colour_manual(name = "",
                      values = c("#332288", "#CC6677")) +
  scale_fill_manual(name = "",
                    values = c("#332288", "#CC6677")) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 15),
                     breaks = c(1, 3, 5, 7, 9, 11, 13),
                     labels = c("1 Gt", "3 Gt", "5 Gt", "7 Gt", "9 Gt", "11 Gt", "13 Gt")) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(2030, 2100, by = 20)) +
  plot.theme.top10 + labs(y = "", title = "Annual Emissions")
  # plot.theme.top10 + labs(y = "", title = "Annual GHG Emissions Trajectories: India",
  #                         subtitle = "Future projected emissions (Gt CO2e) of Kyoto greenhouse gases (AR4) across two IPCC Shared Socioeconomic Pathway reference cases")

# -- Projected interannual growth for India, across SSP1 and SSP2
PercentChange.India.SSP1SSP2 <- 
  ggplot(FutureGHGs.AllSSPs.Wrangled %>% filter(year>2019 & country=="IND" & marker.allssp==1 & grepl("SSP1|SSP2", scenario)), 
         aes(x = year, y = Percent.change)) +
  geom_smooth(aes(group = scenario, colour = scenario, fill = scenario),
              show.legend = F) +
  geom_hline(aes(yintercept = 0),
             size = 0.5, 
             colour = "#505050",
             linetype = 1) +
  scale_colour_manual(name = "",
                      values = c("#332288", "#CC6677")) +
  scale_fill_manual(name = "",
                    values = c("#332288", "#CC6677")) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.022, 0.046),
                     breaks = c(-0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04),
                     labels = scales::percent_format(accuracy = 0.1)) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(2030, 2100, by = 20)) +
  plot.theme.top10 + labs(y = "", title = "Interannual Growth")

# -- Get legend for India-specific SSP1 and SSP2 plot
PercentChange.India.SSP1SSP2.legend.plot <- 
  ggplot(FutureGHGs.AllSSPs.Wrangled %>% filter(year>2019 & country=="IND" & marker.allssp==1 & grepl("SSP1|SSP2", scenario)), 
         aes(x = year, y = Percent.change)) +
  geom_ribbon(aes(ymin = Percent.change, ymax = Percent.change, group = scenario, fill = scenario)) +
  scale_fill_manual(name = "",
                    values = c("#332288", "#CC6677"),
                    labels = c("SSP 1: Sustainability", "SSP 2: Middle of the Road")) +
  theme(legend.key.width = unit(0.75, "cm"),
        legend.key.height = unit(0.5, "cm"))

PercentChange.India.SSP1SSP2.legend <- get_legend(PercentChange.India.SSP1SSP2.legend.plot)

# -- Arrange India-specific emissions projections and interannual growth (for SSP1 and SSP2) into single plot
India.SSP1SSP2.title <- textGrob("Annual GHG Emissions Trajectories: India", 
                                 gp = gpar(fontsize = 13, fontface = "bold"), 
                                 x = unit(45, "pt"),
                                 just = "left")
India.SSP1SSP2.subtitle <- textGrob("Future projected emissions of Kyoto greenhouse gases (AR4), comparison of two Shared Socioeconomic Pathways (SSP)\nreference scenarios (with model uncertainty)\n", 
                                    gp = gpar(fontsize = 10, lineheight = 0.9),
                                    x = unit(45, "pt"),
                                    just = "left")

margin <- unit(0.5, "line")

India.SSP1SSP2.Arranged <- 
  grid.arrange(FutureGHGs.India.SSP1SSP2, PercentChange.India.SSP1SSP2,
               ncol = 2, nrow = 1) %>%
  grid.arrange(., PercentChange.India.SSP1SSP2.legend,
               widths = unit(c(10, 3), "null"),
               ncol = 2) %>%
  grid.arrange(India.SSP1SSP2.title, India.SSP1SSP2.subtitle, .,
               heights = unit.c(grobHeight(India.SSP1SSP2.title) + 1.2*margin,
                                grobHeight(India.SSP1SSP2.subtitle) + margin,
                                unit(1, "null")),
               bottom = grid.text(label = source.label.gutschow.SSP1SSP2.details, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: INDIA EMISSIONS IN GLOBAL CONTEXT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Historical and future trajectories (1950 - 2100) - Top 10 emitters (2018 as reference to determine top emitters) ----

Stacked.Top10Emissions.to2100.Plot <-
  ggplot(GHGTop10.EU %>% filter(year>=1950), aes(x = year, y = value/1000000)) +
  geom_area(aes(group = country, fill = country.name)) +
  geom_vline(aes(xintercept = 2018),
             size = 0.5,
             colour = "#909090",
             alpha = 0.5) +
  annotate("text", x = 2014, y = 41, label = "2018", size = 2.8, colour = "#909090") +
  scale_fill_ptol() +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(1950, 2100, by = 10),
                     labels = c("1950", "", "1970", "", "1990", "", "2010", "", "2030", "", "2050", "", "2070", "", "2090", "")) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0, 55),
                     breaks = seq(10, 50, by = 10),
                     labels = c("10 Gt", "20 Gt", "30 Gt", "40 Gt", "50 Gt")) +
  plot.theme.top10 + legend.guide.top10 +
  labs(title = "Annual GHG Emissions Trajectories: Top 10 Current Emitters", 
       subtitle = "Historic and future projected emissions (Gt CO2e) of Kyoto greenhouse gases (AR4) for ten countries with highest present-day emissions\nIPCC Shared Socioeconomic Pathway (SSP) 2 “Middle of the Road” reference case")

Stacked.Top10Emissions.to2100.Arranged <- 
  grid.arrange(Stacked.Top10Emissions.to2100.Plot, 
               bottom = grid.text(label = source.label.gutschow.extradetails, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- 3.2 Plot projected annual growth for Top 4 emitters (SSP2 baseline marker model) ----

PercentChange.FutureGHGs.SSP2 <- 
  ggplot(FutureGHGs.AllSSPs.Wrangled %>% filter(marker==1 & year>2019 & country%in%c("CHN", "USA", "EU27", "IND")) %>%
           mutate(country.name = factor(country.name, levels = c("China", "India", "EU-27", "United States", ordered = T))), 
         aes(x = year, y = Percent.change)) +
  geom_smooth(aes(group = country.name, colour = country.name, fill = country.name),
              show.legend = F) +
  geom_hline(aes(yintercept = 0),
             size = 0.5, 
             colour = "#505050",
             linetype = 1) +
  scale_colour_manual(name = "",
                    values = c("#332288", "#CC6677", "#117733", "#88CCEE")) +
  scale_fill_manual(name = "",
                    values = c("#332288", "#CC6677", "#117733", "#88CCEE")) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.022, 0.046),
                     breaks = c(-0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04),
                     labels = scales::percent_format(accuracy = 0.1)) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(2030, 2100, by = 20)) +
  plot.theme.top10 + labs(y = "", title = "Annual Percent Change in Emissions: Top 4 Current Emitters",
                          subtitle = "Projected growth in emissions of Kyoto greenhouse gas (AR4) for top four countries with highest present-day emissions\nIPCC Shared Socioeconomic Pathway (SSP) 2 “Middle of the Road” reference case")

PercentChange.legend.plot <- 
  ggplot(FutureGHGs.AllSSPs.Wrangled %>% filter(year>2019 & country%in%c("CHN", "USA", "EU27", "IND")) %>%
           mutate(country.name = factor(country.name, levels = c("China", "India", "EU-27", "United States", ordered = T))), 
         aes(x = year)) +
  geom_ribbon(aes(ymin = Percent.change, ymax = Percent.change, group = country.name, fill = country.name)) +
  scale_fill_manual(name = "",
                    values = c("#332288", "#CC6677", "#117733", "#88CCEE")) +
  theme(legend.key.width = unit(0.75, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.box.margin = margin(0, 20, 0, 5, unit = "pt"))

PercentChange.legend <- get_legend(PercentChange.legend.plot)

PercentChange.FutureGHGs.SSP2.Arranged <- 
  grid.arrange(PercentChange.FutureGHGs.SSP2, PercentChange.legend, ncol = 2, nrow = 1,
               bottom = grid.text(label = source.label.gutschow.percent.extradetails, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               widths = unit(c(10, 1.8), "null"),
               # padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# -- Projected annual growth for India, across all SSPs
PercentChange.India.AllSSPs <- 
  ggplot(FutureGHGs.AllSSPs.Wrangled %>% filter(year>2019 & country=="IND" & marker.allssp==1) %>%
           mutate(scenario = factor(scenario, c("SSP5BLREMMP", "SSP4BLGCAM4", "SSP3BLAIMCGE", "SSP2BLMESGB", "SSP1BLIMAGE"),
                                    ordered = T)), 
         aes(x = year, y = Percent.change)) +
  geom_smooth(aes(group = scenario, colour = scenario, fill = scenario),
              show.legend = F) +
  geom_hline(aes(yintercept = 0),
             size = 0.5, 
             colour = "#505050",
             linetype = 1) +
  scale_colour_manual(name = "",
                      values = c("#DDCC77", "#88CCEE", "#117733", "#CC6677", "#332288")) +
  scale_fill_manual(name = "",
                    values = c("#DDCC77", "#88CCEE", "#117733", "#CC6677", "#332288")) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.022, 0.046),
                     breaks = c(-0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04),
                     labels = scales::percent_format(accuracy = 0.1)) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(2020, 2100, by = 10)) +
  plot.theme.top10 + labs(y = "", title = "Projected Interannual Growth in Emissions: India",
                          subtitle = "Percent change in emissions of Kyoto greenhouse gas (AR4) across IPCC Shared Socioeconomic Pathway reference cases")

PercentChange.India.AllSSPs.legend.plot <- 
  ggplot(FutureGHGs.AllSSPs.Wrangled %>% filter(year>2019 & country=="IND" & marker.allssp==1) %>%
           mutate(scenario = factor(scenario, c("SSP5BLREMMP", "SSP4BLGCAM4", "SSP3BLAIMCGE", "SSP2BLMESGB", "SSP1BLIMAGE"),
                                    ordered = T)), 
         aes(x = year, y = Percent.change)) +
  geom_ribbon(aes(ymin = Percent.change, ymax = Percent.change, group = scenario, fill = scenario)) +
  scale_fill_manual(name = "",
                    values = c("#DDCC77", "#88CCEE", "#117733", "#CC6677", "#332288"),
                    labels = c("SSP 5: Fossil-Fueled Development", "SSP 4: Inequality", 
                               "SSP 3: Regional Rivalry", "SSP 2: Middle of the Road", "SSP 1: Sustainability")) +
  theme(legend.key.width = unit(0.75, "cm"),
        legend.key.height = unit(0.5, "cm")) + guides(fill = guide_legend(reverse = T))

PercentChange.India.AllSSPs.legend <- get_legend(PercentChange.India.AllSSPs.legend.plot)

PercentChange.India.AllSSPs.Arranged <- 
  grid.arrange(PercentChange.India.AllSSPs, PercentChange.India.AllSSPs.legend, ncol = 2, nrow = 1,
               bottom = grid.text(label = source.label.gutschowpercentchange, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               widths = unit(c(10, 2), "null"),
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))



# ---- 3.3 Plot Top 10 per capita emissions trajectories (SSP2 baseline marker model) ----

EmissionsPerCapita.FutureGHGs <- 
  ggplot(FutureGHGs.AllSSPs.Wrangled %>% filter(year>2019), aes(x = year, y = Percent.POP)) +
  geom_smooth(aes(group = country.name, colour = country.name, fill = country.name)) +
  annotate("text", x = 2065, y = 0.0045, label = "India", size = 2.8, colour = "#909090") +
  annotate("text", x = 2082, y = 0.0155, label = "EU-27", size = 2.8, colour = "#909090") +
  annotate("text", x = 2065, y = 0.0137, label = "China", size = 2.8, colour = "#909090") +
  scale_colour_ptol(name = "") +
  scale_fill_ptol(name = "") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 0.02, by = 0.005)) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(2020, 2100, by = 10)) +
  plot.theme.top10 + labs(y = "", title = "Annual Per Capita Emissions Trajectories: Top 10 Current Emitters",
                          subtitle = "Projected emissions (Gt CO2e) of Kyoto greenhouse gas (AR4) per projected population (in millions), for ten countries with highest present-day emissions\nIPCC Shared Socioeconomic Pathway 2 “Middle of the Road” reference case")

EmissionsPerCapita.FutureGHGs.Arranged <- 
  grid.arrange(EmissionsPerCapita.FutureGHGs, 
               bottom = grid.text(label = source.label.gutschowpercapita, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- 3.4 Plot Top 10 per GDP emissions trajectories (SSP2 baseline marker model) ----

EmissionsPerGDP.FutureGHGs <- 
  ggplot(FutureGHGs.AllSSPs.Wrangled %>% filter(year>2019), aes(x = year, y = Percent.GDP)) +
  geom_smooth(aes(group = country.name, colour = country.name, fill = country.name)) +
  # annotate("text", x = 2065, y = 0.0045, label = "India", size = 2.8, colour = "#909090") +
  # annotate("text", x = 2082, y = 0.0155, label = "EU-27", size = 2.8, colour = "#909090") +
  # annotate("text", x = 2065, y = 0.0137, label = "China", size = 2.8, colour = "#909090") +
  scale_colour_ptol(name = "") +
  scale_fill_ptol(name = "") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0.07, 0.66),
                     breaks = seq(0.1, 0.6, by = 0.1)) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(2020, 2100, by = 10)) +
  plot.theme.top10 + labs(y = "", title = "Annual Per GDP Emissions Trajectories: Top 10 Current Emitters",
                          subtitle = "Projected emissions (Gt CO2e) of Kyoto greenhouse gas (AR4) per projected GDP (million 2011 international dollars), for ten countries with highest present-day emissions\nIPCC Shared Socioeconomic Pathway 2 “Middle of the Road” reference case")

EmissionsPerGDP.FutureGHGs.Arranged <- 
  grid.arrange(EmissionsPerGDP.FutureGHGs, 
               bottom = grid.text(label = source.label.gutschow, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: INDIA EMISSIONS BY SECTOR ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 4.1 Create pie chart of India's emissions by sector for 2016 national GHG inventory ----

India.Emissions.bySector.PieChart <-
  ggplot(Emissions.bySector.India, aes(x = "", y = value, fill = order)) +
  geom_bar(stat = "identity", width = 1, 
           colour = "white", 
           show.legend = F) +
  coord_polar("y", start = 0) +
  # geom_text(aes(x = 1.68, y = ypos, label = lab, colour = order),
  #           size = 3.5, fontface = "bold", lineheight = 0.9, show.legend = F) +
  geom_text(aes(x = c(1, 1.1, 1.28, 1.62), y = ypos, label = lab, colour = order),
            size = 3.5, fontface = "bold", lineheight = 0.9, show.legend = F) +
  scale_fill_manual(values = c("#88CCEE", "#117733", "#CC6677", "#332288")) +
  # scale_colour_manual(values = c("#3C93C0", "#117733", "#C35668", "#332288")) +
  scale_colour_manual(values = c("#3C93C0", "white", "white", "white")) +
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
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "pt"),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  labs(title = "Sectoral GHG Emissions: India",
       subtitle = "Share of 2016 national emissions (excluding land use, land-use change, and forestry)")

India.Emissions.bySector.PieChart.Arranged <- 
  grid.arrange(India.Emissions.bySector.PieChart, 
               bottom = grid.text(label = "Source: MoEFCC (2021). India: Third Biennial Update Report to the United Nations Framework Convention\n               on Climate Change. Ministry of Environment, Forest and Climate Change, Government of India.\n              <https://unfccc.int/sites/default/files/resource/INDIA_%20BUR-3_20.02.2021_High.pdf>", 
                                  x = unit(45, "pt"),
                                  y = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               vp = viewport(width = 1, height = 0.95))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: INDIA EMISSIONS BY CURRENT & FUTURE INFRASTRUCTURE ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 5.1 Recreate IEA image from India Energy Outlook 2021 - emissions by current and future infrastructure ----

India.PowerInfra.Emissions <- 
  ggplot(Emissions.Infrastructure.India %>% filter(source=="Power"), aes(x = year, y = emissions)) + 
  geom_line(aes(group = status, colour = status),
            size = 1.5, show.legend = F) +
  scale_x_continuous(name = "",
                     expand = c(0,0),
                     breaks = c(2020, 2030, 2040)) +
  scale_y_continuous(name = "MtCO2e",
                     expand = c(0.005,0),
                     limits = c(0, 1400),
                     breaks = seq(200, 1400, by = 200)) +
  scale_colour_manual(values = c("#332288", "#CC6677")) + 
  plot.theme.India.infra +
  labs(title = "Power")

India.IndustryInfra.Emissions <- 
  ggplot(Emissions.Infrastructure.India %>% filter(source=="Industry"), aes(x = year, y = emissions)) + 
  geom_line(aes(group = status, colour = status),
            size = 1.5, show.legend = F) +
  scale_x_continuous(name = "",
                     expand = c(0,0),
                     breaks = c(2020, 2030, 2040)) +
  scale_y_continuous(name = "",
                     expand = c(0.005,0),
                     limits = c(0, 1400),
                     breaks = seq(200, 1400, by = 200)) +
  scale_colour_manual(values = c("#332288", "#CC6677")) +
  plot.theme.India.infra + theme(axis.text.y = element_blank()) +
  labs(title = "Industry")

India.TransportInfra.Emissions <- 
  ggplot(Emissions.Infrastructure.India %>% filter(source=="Transport"), aes(x = year, y = emissions)) + 
  geom_line(aes(group = status, colour = status),
            size = 1.5, show.legend = F) +
  scale_x_continuous(name = "",
                     expand = c(0,0),
                     breaks = c(2020, 2030, 2040)) +
  scale_y_continuous(name = "",
                     expand = c(0.005,0),
                     limits = c(0, 1400),
                     breaks = seq(200, 1400, by = 200)) +
  scale_colour_manual(values = c("#332288", "#CC6677")) +
  plot.theme.India.infra + theme(axis.text.y = element_blank()) +
  labs(title = "Transport")

India.BuildingsInfra.Emissions <- 
  ggplot(Emissions.Infrastructure.India %>% filter(source=="Buildings"), aes(x = year, y = emissions)) + 
  geom_line(aes(group = status, colour = status),
            size = 1.5, show.legend = F) +
  scale_x_continuous(name = "",
                     expand = c(0,0),
                     breaks = c(2020, 2030, 2040)) +
  scale_y_continuous(name = "",
                     expand = c(0.005,0),
                     limits = c(0, 1400),
                     breaks = seq(200, 1400, by = 200)) +
  scale_colour_manual(values = c("#332288", "#CC6677")) +
  plot.theme.India.infra + theme(axis.text.y = element_blank()) +
  labs(title = "Buildings")


# ---- 5.2 Create legend plot for infrastructure plots ----

India.Infrastructure.legend.plot <- 
  ggplot(Emissions.Infrastructure.India %>% filter(source=="Buildings"), aes(x = year, y = emissions)) + 
  geom_line(aes(group = status, colour = status),
            size = 1.5) +
  scale_x_continuous(name = "",
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0.005,0),
                     limits = c(0, 1400),
                     breaks = seq(200, 1400, by = 200)) +
  scale_colour_manual(name = "",
                      labels = c("Existing and under construction     ", "New"),
                      values = c("#332288", "#CC6677")) +
  theme(legend.key.width = unit(0.75, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.box.margin = margin(0, 0, 20, 0, unit = "pt")) + guides(colour = guide_legend(ncol = 2))

India.Infrastructure.legend <- get_legend(India.Infrastructure.legend.plot)


# ---- 5.3 Arrange infrastructure plot, labels, legend ----

India.Infra.title <- textGrob("Emissions from Existing and New Infrastructure in India", 
                                 gp = gpar(fontsize = 13, fontface = "bold"), 
                                 x = unit(45, "pt"),
                                 just = "left")
India.Infra.subtitle <- textGrob("Projected emissions (MtCO2e) from the Stated Policies Scenario, 2019-2040\n", 
                                    gp = gpar(fontsize = 10, lineheight = 0.9),
                                    x = unit(45, "pt"),
                                    just = "left")

margin <- unit(0.5, "line")


India.Infrastructure.Arranged <- 
  grid.arrange(India.IndustryInfra.Emissions, India.TransportInfra.Emissions, India.BuildingsInfra.Emissions,
               ncol = 3) %>%
  grid.arrange(India.PowerInfra.Emissions, ., 
               widths = unit(c(3.5, 9), "null"),
               ncol = 2, nrow = 1) %>%
  grid.arrange(., India.Infrastructure.legend,
               heights = unit(c(10, 0.7), "null"),
               nrow = 2) %>%
  grid.arrange(India.Infra.title, India.Infra.subtitle, .,
               heights = unit.c(grobHeight(India.Infra.title) + 1.2*margin,
                                grobHeight(India.Infra.subtitle) + margin,
                                unit(1, "null")),
               bottom = grid.text(label = "Source:  India Energy Outlook 2021, IEA.", 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 6: INDIA COLD CHAIN EMISSIONS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 6.1 Filter cold chain data ----

ColdChainEmissions.India <- 
  CoolingDemand.bySector.India %>%
  filter(sector=="Cold Chain" & classification!="Aggregated") %>% # include only cold chain, disaggregated data
  filter(scenario=="BAU" & metric=="Total emissions") # include only the BAU scenario for 2027 (not the "Improved" scenario) and total emissions as metric
  # data does include Annual Carbon Emissions by sector (aggregated): industrial process cooling, cold chain, refrigeration, mobile air-conditioning, and space cooling in buildings
  # -- this would allow for comparison of scale of emissions via cold chain vs. other cooling demand sectors
  # data includes historical 2017 data, and then 2027 data for both a BAU scenario and an improved scenario
  # data includes, for the cold storage disaggregated classifications, annual energy consumption and indirect/direct emissions as well as total emissions


# ---- 6.2 Recreate Figure 7 from Net Zero Cold Chains for Food report ----
# --       LINK to report: https://prod-drupal-files.storage.googleapis.com/documents/resource/public/Net_zero_cold_chains_for_food.pdf

ColdChainEmissions.India.plot <- ggplot(ColdChainEmissions.India, aes(x = year, y = value)) +
  geom_bar(aes(group = classification, fill = classification), 
           position = "stack", stat = "identity", width = 7) +
  geom_segment(data = data.frame(x = c(2017, 2017, 2023.5, 2027),
                                 xend = c(2017, 2020.5, 2027, 2027),
                                 y = c(4.4, 11, 11, 10),
                                 yend = c(11, 11, 11, 11)),
               aes(x = x, xend = xend, y = y, yend = yend),
               size = 0.5, colour = "#303030") +
  geom_label(data = data.frame(x = c(2017, 2027), 
                        y = c(4.5, 10.1),
                        label = c("4.1 Mt", "9.7 Mt")),
             aes(x = x, y = y, label = label),
             size = 3.5, colour = "white", fill = "#303030") +
  geom_text(aes(x = 2022, y = 11.05, label = "+136%"),
            size = 3.5, colour = "#303030") +
  scale_fill_manual(name = "",
                    values = colours.4categories) +
  scale_x_continuous(name = "",
                     breaks = c(2017, 2027)) +
  scale_y_continuous(name = "",
                     expand = c(0,0),
                     limits = c(0, 11.25),
                     breaks = seq(0, 10, by = 1),
                     labels = c("0 Mt", "1 Mt", "2 Mt", "3 Mt", "4 Mt", "5 Mt", "6 Mt", "7 Mt", "8 Mt", "9 Mt", "10 Mt")) +
  plot.theme.India.coldchain + labs(title = "Growth in Indian Cold Chain Emissions by Component (2017-2027)\n")


ColdChainEmissions.India.arranged <- 
  grid.arrange(ColdChainEmissions.India.plot, 
               bottom = grid.text(label = "Source: Kumar, S., Sachar, S., Kachhawa, S., Goenka, A., Kasamsetty, S., George, G. (2018). Demand Analysis of\n              Cooling by Sector in India in 2027. New Delhi: Alliance for an Energy Efficient Economy.\nNote:     Units in megatonnes (Mt) CO2e", 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               vp = viewport(width = 1, height = 0.95))
  

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 7: INDIA EMISSIONS & ENERGY BY STATE ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# Below is preliminary code for a state-level map of India energy capacity by state

theme_map <- function (base_size = 12, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.ticks.length = unit(0.3, "lines"),
      axis.title = element_blank(),
      legend.background = element_rect(fill = "white", 
                                       colour = NA),
      legend.key = element_rect(colour = "white"),
      legend.key.width = unit(0.75, "cm"),
      legend.key.height = unit(0.5, "cm"),
      legend.position = "right",
      legend.text = element_text(size = rel(0.9)),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
      plot.title = element_text(size = rel(1.8), 
                                face = "bold", 
                                hjust = 0.5),
      strip.background = element_rect(fill = "grey90", 
                                      colour = "grey50"),
      strip.text.x = element_text(size = rel(0.8)),
      strip.text.y = element_text(size = rel(0.8), 
                                  angle=-90) 
    )   
}

india <- getData('GADM', country="IND", level=1) 
india.map <- fortify(india) %>%
  mutate(id = as.integer(id))

map.dat <- data.frame(id = 1:(length(india@data$NAME_1)), 
                      state = india@data$NAME_1)

india.map.forplot <- inner_join(india.map, map.dat, by = "id")

india.centers <- 
  data.frame(gCentroid(india, byid = TRUE),
             state = map.dat$state) %>%
  mutate(state = ifelse(state=="NCT of Delhi", "Delhi", state)) %>%
  left_join(EnergyCapacity.byState.India, by = "state")


India.statelevel.map <- ggplot() + 
  geom_polygon(data = india.map.forplot,
           aes(x = long, y = lat, group = group),
           color = "#ffffff", fill = "#bbbbbb", size = 0.25) +
  geom_text(data = india.centers, aes(label = state, x = x, y = y), size = 2) +
  geom_scatterpie(data = india.centers, aes(x = x, y = y - 0.5), cols = c("CoalCapacity", "OilGasCapacity",
                                                                    "NuclearCapacity", "HydroCapacity",
                                                                    "WindCapacity", "SolarCapacity",
                                                                    "BioPowerCapacity", "SmallHydroCapacity")) +
  scale_fill_ptol(name = "",
                  labels = c("Coal", "Oil & Gas", "Nuclear", "Hydro", "Wind", "Solar", "Bio-Power", "Small Hydro")) +
  coord_map() +
  labs(x = "", y = "", title = "India State") +
  theme_map()
  


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 8: EXPORT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# png(paste(FigureFileName, "/India.AllSSPs.png", sep = ""),
#     units = "in", height = 6, width = 8, res = 400)
# grid.newpage()
# grid.draw(India.AllSSPs.Arranged)
# dev.off()


png(paste(FigureFileName, "/India.AllSSPs.WithUncertainty.png", sep = ""),
    units = "in", height = 8, width = 10, res = 400)
grid.newpage()
grid.draw(India.AllSSPs.WithUncertainty)
dev.off()


png(paste(FigureFileName, "/Top10.StackedEmissions.to2100.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(Stacked.Top10Emissions.to2100.Arranged)
dev.off()


png(paste(FigureFileName, "/Top4.EmissionsPercentChange.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(PercentChange.FutureGHGs.SSP2.Arranged)
dev.off()


# png(paste(FigureFileName, "/India.EmissionsPercentChange.SSP1SSP2.png", sep = ""),
#     units = "in", height = 6, width = 9, res = 400)
# grid.newpage()
# grid.draw(PercentChange.India.SSP1SSP2.Arranged)
# dev.off()
# 
# 
# png(paste(FigureFileName, "/India.EmissionsPercentChange.AllSSPs.png", sep = ""),
#     units = "in", height = 6, width = 9, res = 400)
# grid.newpage()
# grid.draw(PercentChange.India.AllSSPs.Arranged)
# dev.off()


png(paste(FigureFileName, "/India.SSP1SSP2.FutureGHG.png", sep = ""),
    units = "in", height = 7, width = 10, res = 400)
grid.newpage()
grid.draw(India.SSP1SSP2.Arranged)
dev.off()


# png(paste(FigureFileName, "/Top10.EmissionsPerCapita.png", sep = ""),
#     units = "in", height = 6, width = 8, res = 400)
# grid.newpage()
# grid.draw(EmissionsPerCapita.FutureGHGs.Arranged)
# dev.off()


png(paste(FigureFileName, "/India.bySector.PieChart.png", sep = ""),
    units = "in", height = 8, width = 8, res = 400)
grid.newpage()
grid.draw(India.Emissions.bySector.PieChart.Arranged)
dev.off()


png(paste(FigureFileName, "/India.bySector.PieChart.v2.png", sep = ""),
    units = "in", height = 8, width = 8, res = 400)
grid.newpage()
grid.draw(India.Emissions.bySector.PieChart.Arranged)
dev.off()


png(paste(FigureFileName, "/India.NewExisting.Infrastructure.png", sep = ""),
    units = "in", height = 6, width = 10, res = 400)
grid.newpage()
grid.draw(India.Infrastructure.Arranged)
dev.off()


png(paste(FigureFileName, "/India.ColdChainEmissions.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(ColdChainEmissions.India.arranged)
dev.off()




# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 9: ADDITIONAL STATISTICS FOR INDIA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 9.1 India percent share of emissions in 2030 ----
#          Using SSP2 reference scenario

FutureGHGs.India.v.world <- gutschow.data %>%
  filter(grepl("PMSSPBIE", source) & 
           grepl(paste(scenario.allssp, collapse="|"), scenario) &
           country%in%c("IND", "EARTH")) %>%
  tidyr::pivot_longer(c(`1850`:`2100`), names_to = "year", values_to = "value") %>%
  filter(entity=="KYOTOGHGAR4") %>% 
  mutate(marker = ifelse(scenario==marker.scenario, 1, 0),
         year = as.numeric(year)) %>%
  group_by(source, unit, entity, scenario, country, year, marker) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  filter(marker==1)

AnnualEmissions.India.v.world <-
  FutureGHGs.India.v.world %>%
  mutate(units = "GtCO2e",
         percentShare = value / value[country=="EARTH" & year==year],
         value = value / 1000000) %>%
  filter(country=="IND")


# ---- 9.2 Cumulative emissions from 2018 - 2050, and India percent share of cumulative emissions ----
#          Using SSP2 reference scenario

CumulativeEmissions.India.v.world <- 
  FutureGHGs.India.v.world %>%
  filter(year>2017) %>%
  group_by(country) %>%
  mutate(cumulativeVal = cumsum(value)) %>%
  ungroup() %>%
  mutate(units = "GtCO2e",
         percentShare = cumulativeVal / cumulativeVal[country=="EARTH" & year==year],
         value = value / 1000000,
         cumulativeVal = cumulativeVal / 1000000) %>%
  filter(country=="IND")

export(AnnualEmissions.India.v.world, 'data/outputs/AnnualEmissions.India.v.world.csv')
export(CumulativeEmissions.India.v.world, 'data/outputs/CumulativeEmissions.India.v.world.csv')
