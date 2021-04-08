# 
# code: Country-specific future emissions trajectories, by sector
# 
# ---- sections ----
# 1.  Source Plot Themes, Wrangle Data
# 2.  Historical Sector-Specific Emissions Plots
# 3.  Projections of Sector-Specific Emissions Plots
# 4.  Export


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE PLOT THEMES, WRANGLE DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 1.1 Source PlotThemes.R ----

eval(parse('code/PlotThemes.R', encoding = 'UTF-8'))


# ---- 1.2 Identify which countries (of Top 10) to produce sector-specific plots for ----

# this is only necessary if we automate the plotting process by country (see experiment at bottom of script)
sector.plot.list <- c("China", "United States", "EU-27", "India") 



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: HISTORICAL SECTOR-SPECIFIC EMISSIONS PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 2.1 China ----
# -- same sector categories as treemap (Power & Heat, Industry, Transport, AFOLU, Other)
# -- different data source between historical sector-specific emissions (CAIT) and treemap & future projected emissions (GCAM)

China.Emissions.BySector.Historical.Plot <-
  ggplot(GHGTop10.CAIT.5sectors %>% filter(country.name=="China"),
                  aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  scale_fill_manual(values = colours.5categories) +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "",
                     expand = c(0, 0),
                     limits = c(0, 18),
                     breaks = seq(3, 15, by = 3),
                     labels = c("3 Gt", "6 Gt", "9 Gt", "12 Gt", "15 Gt")) +
  legend.guide.top10 + plot.theme.sector + 
  labs(title = "Annual Sectoral Emissions for China (1990 - 2017)",
       subtitle = "Historic emissions (Gt CO2e) from fossil fuel combustion, industrial processes, and land-use change\n")


China.Emissions.BySector.Historical.Arranged <- 
  grid.arrange(China.Emissions.BySector.Historical.Plot, 
               bottom = grid.text(label = source.label.cait, 
                                  x = unit(47, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 9, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))



# ---- 2.2 United States ----
# -- same sector categories as treemap (Power & Heat, Industry, Transport, AFOLU, Other)
# -- different data source between historical sector-specific emissions (CAIT) and treemap & future projected emissions (GCAM)

USA.Emissions.BySector.Historical.Plot <-
  ggplot(GHGTop10.CAIT.5sectors %>% filter(country.name=="United States"),
         aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  scale_fill_manual(values = colours.5categories) +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "",
                     expand = c(0, 0),
                     limits = c(0, 18),
                     breaks = seq(3, 15, by = 3),
                     labels = c("3 Gt", "6 Gt", "9 Gt", "12 Gt", "15 Gt")) +
  legend.guide.top10 + plot.theme.sector + 
  labs(title = "Annual Sectoral Emissions for United States (1990 - 2017)",
       subtitle = "Historic emissions (Gt CO2e) from fossil fuel combustion, industrial processes, and land-use change\n")


USA.Emissions.BySector.Historical.Arranged <- 
  grid.arrange(USA.Emissions.BySector.Historical.Plot, 
               bottom = grid.text(label = source.label.cait, 
                                  x = unit(47, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 9, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))



# ---- 2.3 EU-27 ----
# -- same sector categories as treemap (Power & Heat, Industry, Transport, AFOLU, Other)
# -- different data source between historical sector-specific emissions (CAIT) and treemap & future projected emissions (GCAM)

EU27.Emissions.BySector.Historical.Plot <-
  ggplot(GHGTop10.CAIT.5sectors %>% filter(country.name=="EU-27"),
         aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  scale_fill_manual(values = colours.5categories) +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "",
                     expand = c(0, 0),
                     limits = c(0, 18),
                     breaks = seq(3, 15, by = 3),
                     labels = c("3 Gt", "6 Gt", "9 Gt", "12 Gt", "15 Gt")) +
  legend.guide.top10 + plot.theme.sector + 
  labs(title = "Annual Sectoral Emissions for EU-27 (1990 - 2017)",
       subtitle = "Historic emissions (Gt CO2e) from fossil fuel combustion, industrial processes, and land-use change\n")


EU27.Emissions.BySector.Historical.Arranged <- 
  grid.arrange(EU27.Emissions.BySector.Historical.Plot, 
               bottom = grid.text(label = source.label.cait, 
                                  x = unit(47, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 9, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- 2.4 India ----
# -- same sector categories as treemap (Power & Heat, Industry, Transport, AFOLU, Other)
# -- different data source between historical sector-specific emissions (CAIT) and treemap & future projected emissions (GCAM)

India.Emissions.BySector.Historical.Plot <-
  ggplot(GHGTop10.CAIT.5sectors %>% filter(country.name=="India"),
         aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  scale_fill_manual(values = colours.5categories) +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "",
                     expand = c(0, 0),
                     limits = c(0, 18),
                     breaks = seq(3, 15, by = 3),
                     labels = c("3 Gt", "6 Gt", "9 Gt", "12 Gt", "15 Gt")) +
  legend.guide.top10 + plot.theme.sector + 
  labs(title = "Annual Sectoral Emissions for India (1990 - 2017)",
       subtitle = "Historic emissions (Gt CO2e) from fossil fuel combustion, industrial processes, and land-use change\n")


India.Emissions.BySector.Historical.Arranged <- 
  grid.arrange(India.Emissions.BySector.Historical.Plot, 
               bottom = grid.text(label = source.label.cait, 
                                  x = unit(47, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 9, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: PROJECTIONS OF SECTOR-SPECIFIC EMISSIONS PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 China ----

China.Emissions.BySector.Future.Plot <-
  ggplot(GHGTop10.GCAM %>% filter(country.name=="China"), 
         aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  scale_fill_manual(values = colours.5categories) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(2020, 2100, by = 10),
                     labels = c("2020", "", "2040", "", "2060", "", "2080", "", "2100")) +
  scale_y_continuous(name = "",
                     expand = c(0, 0),
                     limits = c(0, 18),
                     breaks = seq(3, 15, by = 3),
                     labels = c("3 Gt", "6 Gt", "9 Gt", "12 Gt", "15 Gt")) +
  plot.theme.top10 + legend.guide.top10 +
  labs(title = "Projected Sectoral Emissions for China (2015 - 2100)",
       subtitle = 'Emissions modelled under the assumptions of IPCC Shared Socioeconomic Pathway 2 "Middle of the Road"\nscenario, with no new climate policies after 2015 (GCAM calibration year)')

China.Emissions.BySector.Future.Arranged <- 
  grid.arrange(China.Emissions.BySector.Future.Plot, 
               bottom = grid.text(label = source.label.gcam, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 9, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- 3.2 United States ----

USA.Emissions.BySector.Future.Plot <-
  ggplot(GHGTop10.GCAM %>% filter(country.name=="United States"), 
         aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  scale_fill_manual(values = colours.5categories) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(2020, 2100, by = 10),
                     labels = c("2020", "", "2040", "", "2060", "", "2080", "", "2100")) +
  scale_y_continuous(name = "",
                     expand = c(0, 0),
                     limits = c(0, 18),
                     breaks = seq(3, 15, by = 3),
                     labels = c("3 Gt", "6 Gt", "9 Gt", "12 Gt", "15 Gt")) +
  plot.theme.top10 + legend.guide.top10 +
  labs(title = "Projected Sectoral Emissions for United States (2015 - 2100)",
       subtitle = 'Emissions modelled under the assumptions of IPCC Shared Socioeconomic Pathway 2 "Middle of the Road"\nscenario, with no new climate policies after 2015 (GCAM calibration year)')

USA.Emissions.BySector.Future.Arranged <- 
  grid.arrange(USA.Emissions.BySector.Future.Plot, 
               bottom = grid.text(label = source.label.gcam, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 9, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- 3.3 EU-28 ----

EU28.Emissions.BySector.Future.Plot <-
  ggplot(GHGTop10.GCAM %>% filter(country.name=="EU-28"), 
         aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  scale_fill_manual(values = colours.5categories) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(2020, 2100, by = 10),
                     labels = c("2020", "", "2040", "", "2060", "", "2080", "", "2100")) +
  scale_y_continuous(name = "",
                     expand = c(0, 0),
                     limits = c(0, 18),
                     breaks = seq(3, 15, by = 3),
                     labels = c("3 Gt", "6 Gt", "9 Gt", "12 Gt", "15 Gt")) +
  plot.theme.top10 + legend.guide.top10 +
  labs(title = "Projected Sectoral Emissions for EU-28 (2015 - 2100)",
       subtitle = 'Emissions modelled under the assumptions of IPCC Shared Socioeconomic Pathway 2 "Middle of the Road"\nscenario, with no new climate policies after 2015 (GCAM calibration year)')

EU28.Emissions.BySector.Future.Arranged <- 
  grid.arrange(EU28.Emissions.BySector.Future.Plot, 
               bottom = grid.text(label = source.label.gcam, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 9, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- 3.4 INDIA ----

India.Emissions.BySector.Future.Plot <-
  ggplot(GHGTop10.GCAM %>% filter(country.name=="India"), 
         aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  scale_fill_manual(values = colours.5categories) +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(2020, 2100, by = 10),
                     labels = c("2020", "", "2040", "", "2060", "", "2080", "", "2100")) +
  scale_y_continuous(name = "",
                     expand = c(0, 0),
                     limits = c(0, 18),
                     breaks = seq(3, 15, by = 3),
                     labels = c("3 Gt", "6 Gt", "9 Gt", "12 Gt", "15 Gt")) +
  plot.theme.top10 + legend.guide.top10 +
  labs(title = "Projected Sectoral Emissions for India (2015 - 2100)",
       subtitle = 'Emissions modelled under the assumptions of IPCC Shared Socioeconomic Pathway 2 "Middle of the Road"\nscenario, with no new climate policies after 2015 (GCAM calibration year)')

India.Emissions.BySector.Future.Arranged <- 
  grid.arrange(India.Emissions.BySector.Future.Plot, 
               bottom = grid.text(label = source.label.gcam, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 9, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Export ---
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 4.1 CHINA - historical emissions by sector ----
png(paste(FigureFileName, "/China.historical.bysector.png", sep = ""),
    units = "in", height = 6, width = 9, res = 400)
grid.newpage()
grid.draw(China.Emissions.BySector.Historical.Arranged)
dev.off()

# ---- 4.2 CHINA - future projected emissions by sector ----
png(paste(FigureFileName, "/China.future.bysector.png", sep = ""),
    units = "in", height = 6, width = 9.2, res = 400)
grid.newpage()
grid.draw(China.Emissions.BySector.Future.Arranged)
dev.off()

# ---- 4.3 USA - historical emissions by sector ----
png(paste(FigureFileName, "/USA.historical.bysector.png", sep = ""),
    units = "in", height = 6, width = 9, res = 400)
grid.newpage()
grid.draw(USA.Emissions.BySector.Historical.Arranged)
dev.off()

# ---- 4.4 USA - future projected emissions by sector ----
png(paste(FigureFileName, "/USA.future.bysector.png", sep = ""),
    units = "in", height = 6, width = 9.2, res = 400)
grid.newpage()
grid.draw(USA.Emissions.BySector.Future.Arranged)
dev.off()


# ---- 4.5 EU27 - historical emissions by sector ----
png(paste(FigureFileName, "/EU27.historical.bysector.png", sep = ""),
    units = "in", height = 6, width = 9, res = 400)
grid.newpage()
grid.draw(EU27.Emissions.BySector.Historical.Arranged)
dev.off()

# ---- 4.6 EU28 - future projected emissions by sector ----
png(paste(FigureFileName, "/EU28.future.bysector.png", sep = ""),
    units = "in", height = 6, width = 9.2, res = 400)
grid.newpage()
grid.draw(EU28.Emissions.BySector.Future.Arranged)
dev.off()


# ---- 4.7 INDIA - historical emissions by sector ----
png(paste(FigureFileName, "/India.historical.bysector.png", sep = ""),
    units = "in", height = 6, width = 9, res = 400)
grid.newpage()
grid.draw(India.Emissions.BySector.Historical.Arranged)
dev.off()

# ---- 4.8 INDIA - future projected emissions by sector ----
png(paste(FigureFileName, "/India.future.bysector.png", sep = ""),
    units = "in", height = 6, width = 9.2, res = 400)
grid.newpage()
grid.draw(India.Emissions.BySector.Future.Arranged)
dev.off()




# EXPERIMENT WITH AUTOMATING PLOTTING PROCESS (WOULD REQUIRE SOME WORK IN CREATING ADAPTIVE Y AXIS SIZE AND LABELS)
# for(i in sector.plot.list) {
#   
#   filename <- paste(List.Top10.EU$country[List.Top10.EU$country.name==i], "Emissions.BySector.Historical.Plot", sep = ".")
#   
#   max.val <- max(GHGTop10.CAIT %>% filter(GHGTop10.CAIT$country.name==i & sector=="Energy") %>% # this is an arbitrary selection of sector -- just filtering to only have 1 row per year
#                    select(gross.total))
#   
#   # Would need to figure out how to automate the y axis labels and breaks to fully automate these plots
#   
#   assign(filename,
#          ggplot(GHGTop10.CAIT %>% filter(country.name==i & negativeLUCF==0), aes(x = Year, y = value/1000)) +
#            geom_area(aes(group = sector, fill = sector),
#                      position = "stack") +
#            geom_line(aes(group = sector),
#                      colour = "#C0C0C0",
#                      size = 0.5,
#                      position = "stack",
#                      show.legend = F) +
#            geom_line(data = GHGTop10.CAIT %>% filter(country.name==i & sector=="Energy"), # this is an arbitrary selection of sector -- just filtering to only have 1 row per year
#                      aes(x = Year, y = net.total/1000),
#                      linetype = 2,
#                      size = 1,
#                      colour = "#C0C0C0") +
#            scale_fill_ptol(drop = F) +
#            scale_x_continuous(name = "",
#                               expand = c(0, 0)) +
#            scale_y_continuous(name = "", 
#                               expand = c(0, 0),
#                               limits = c(0, max.val/1000 + 3),
#                               labels = c("3 Gt", "6 Gt", "9 Gt", "12 Gt")) +
#            plot.theme.top10 + legend.guide.top10 +
#            labs(title = paste("Annual GHG Emissions:", i, sep = " "), 
#                 subtitle = "Historic emissions (Gt CO2e) from fossil fuel combustion, industrial processes, and land-use change (1990 - 2016)"))
#   
# }