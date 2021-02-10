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
# ---- SECTION 1: Source Plot Themes, Wrangle Data ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


source('code/PlotThemes.R')


# ---- 1.1 Identify which countries (of Top 10) to produce sector-specific plots for ----

# this is only necessary if we automate the plotting process by country (see experiment at bottom of script)
sector.plot.list <- c("China", "United States", "EU-27","India") 


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Historical Sector-Specific Emissions Plots ---
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 2.1 CHINA ----

China.Emissions.BySector.Historical.Plot <-
  ggplot(GHGTop10.CAIT %>% filter(country.name=="China" & negativeLUCF==0), aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  geom_line(aes(group = sector),
            colour = "#C0C0C0",
            size = 0.5,
            position = "stack",
            show.legend = F) +
  geom_line(data = GHGTop10.CAIT %>% filter(country.name=="China" & sector=="Energy"), # this is an arbitrary selection of sector -- just filtering to only have 1 row per year
            aes(x = Year, y = net.total/1000),
            linetype = 2,
            size = 1,
            colour = "#C0C0C0") +
  scale_fill_ptol(drop = F) +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "",
                     expand = c(0, 0),
                     limits = c(0, 15),
                     breaks = seq(3, 12, by = 3),
                     labels = c("3 Gt", "6 Gt", "9 Gt", "12 Gt")) +
  plot.theme.top10 + legend.guide.top10 +
  labs(title = "Annual GHG Emissions: China",
       subtitle = "Historic emissions (Gt CO2e) from fossil fuel combustion, industrial processes, and land-use change (1990 - 2016)")

China.Emissions.BySector.Historical.Arranged <- 
  grid.arrange(China.Emissions.BySector.Historical.Plot, 
               bottom = grid.text(label = source.label.cait, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- 2.2 UNITED STATES ----

USA.Emissions.BySector.Historical.Plot <-
  ggplot(GHGTop10.CAIT %>% filter(country.name=="United States" & negativeLUCF==0), aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  geom_line(aes(group = sector),
            colour = "#C0C0C0",
            size = 0.5,
            position = "stack",
            show.legend = F) +
  geom_line(data = GHGTop10.CAIT %>% filter(country.name=="United States" & sector=="Energy"), # this is an arbitrary selection of sector -- just filtering to only have 1 row per year
            aes(x = Year, y = net.total/1000),
            linetype = 2,
            size = 1,
            colour = "#C0C0C0") +
  scale_fill_ptol(drop = F) +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "",
                     expand = c(0, 0),
                     limits = c(0, 9),
                     breaks = seq(2, 8, by = 2),
                     labels = c("2 Gt", "4 Gt", "6 Gt", "8 Gt")) +
  plot.theme.top10 + legend.guide.top10 +
  labs(title = "Annual GHG Emissions: United States",
       subtitle = "Historic emissions (Gt CO2e) from fossil fuel combustion, industrial processes, and land-use change (1990 - 2016)")

USA.Emissions.BySector.Historical.Arranged <- 
  grid.arrange(USA.Emissions.BySector.Historical.Plot, 
               bottom = grid.text(label = source.label.cait, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Projections of Sector-Specific Emissions Plots ---
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 CHINA ----

China.Emissions.BySector.Future.Plot <-
  ggplot(GHGTop10.GCAM %>% filter(country.name=="China" & scenario=="Low policy (2015)"), aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  geom_line(aes(group = sector),
            colour = "#C0C0C0",
            size = 0.5,
            position = "stack",
            show.legend = F) +
  geom_line(data = GHGTop10.GCAM %>% filter(country.name=="China" & sector=="Electricity" & scenario=="Low policy (2015)"), # this is an arbitrary selection of sector -- just filtering to only have 1 row per year
            aes(x = Year, y = net.total/1000),
            linetype = 2,
            size = 1,
            colour = "#303030") +
  scale_fill_ptol(drop = F) +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "",
                     expand = c(0, 0),
                     limits = c(0, 15),
                     breaks = seq(3, 12, by = 3),
                     labels = c("3 Gt", "6 Gt", "9 Gt", "12 Gt")) +
  plot.theme.top10 + legend.guide.top10 +
  labs(title = "Projected Annual GHG Emissions: China",
       subtitle = "Total carbon dioxide emissions from fossil fuel combustion, industrial processes, and land-use change (2020 - 2050)\nLow policy scenario (2015)")

China.Emissions.BySector.Future.Arranged <- 
  grid.arrange(China.Emissions.BySector.Future.Plot, 
               bottom = grid.text(label = source.label.gcam, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- 3.2 INDIA ----

India.Emissions.BySector.Future.Plot <-
  ggplot(GHGTop10.GCAM %>% filter(country.name=="India" & scenario=="Low policy (2015)"), aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  geom_line(aes(group = sector),
            colour = "#C0C0C0",
            size = 0.5,
            position = "stack",
            show.legend = F) +
  geom_line(data = GHGTop10.GCAM %>% filter(country.name=="India" & sector=="Electricity" & scenario=="Low policy (2015)"), # this is an arbitrary selection of sector -- just filtering to only have 1 row per year
            aes(x = Year, y = net.total/1000),
            linetype = 2,
            size = 1,
            colour = "#303030") +
  scale_fill_ptol(drop = F) +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "",
                     expand = c(0, 0),
                     limits = c(0, 9),
                     breaks = seq(2, 8, by = 2),
                     labels = c("2 Gt", "4 Gt", "6 Gt", "8 Gt")) +
  plot.theme.top10 + legend.guide.top10 +
  labs(title = "Projected Annual GHG Emissions: India",
       subtitle = "Total carbon dioxide emissions from fossil fuel combustion, industrial processes, and land-use change (2020 - 2050)\nLow policy scenario (2015)")

India.Emissions.BySector.Future.Arranged <- 
  grid.arrange(India.Emissions.BySector.Future.Plot, 
               bottom = grid.text(label = source.label.gcam, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
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


# ---- CHINA - historical emissions by sector ----
png(paste(FigureFileName, "/China.historical.bysector.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(China.Emissions.BySector.Historical.Arranged)
dev.off()

# ---- CHINA - future projected emissions by sector ----
png(paste(FigureFileName, "/China.future.bysector.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(China.Emissions.BySector.Future.Arranged)
dev.off()


# ---- USA - historical emissions by sector ----
png(paste(FigureFileName, "/USA.historical.bysector.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(USA.Emissions.BySector.Historical.Arranged)
dev.off()

# ---- INDIA - future projected emissions by sector ----
png(paste(FigureFileName, "/India.future.bysector.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
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