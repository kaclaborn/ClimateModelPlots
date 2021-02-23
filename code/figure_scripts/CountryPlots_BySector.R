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


# ---- 1.1 Source PlotThemes.R ----

eval(parse('code/PlotThemes.R', encoding = 'UTF-8'))


# ---- 1.2 Identify which countries (of Top 10) to produce sector-specific plots for ----

# this is only necessary if we automate the plotting process by country (see experiment at bottom of script)
sector.plot.list <- c("China", "United States", "EU-27","India") 


# ---- 1.3 Define dummy plot with legend with proper categories ---
sector.7categories.legend <-
  ggplot(GHGTop10.CAIT %>% filter(country.name=="China"), 
         aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector)) +
  scale_fill_ptol(drop = F) +
  legend.guide.top10

sector.7categories.legend <- get_legend(sector.7categories.legend)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Historical Sector-Specific Emissions Plots ---
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 2.1 CHINA ----

China.Emissions.BySector.Historical.Plot <-
  ggplot(GHGTop10.CAIT %>% filter(country.name=="China"), aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack",
            show.legend = F) +
  # geom_line(aes(group = sector),
  #           colour = "#C0C0C0",
  #           size = 0.5,
  #           position = "stack",
  #           show.legend = F) +
  geom_hline(aes(yintercept = 0),
             linetype = 3,
             size = 0.35,
             colour = "#C0C0C0") +
  # annotate("text", x = 2016.5, y = 2, label = toupper("gross emissions"), 
  #          size = 2.5, colour = "#909090", angle = 270) +
  annotate("segment", x = 2016, xend = 2016.5, y = 0, yend = 0,
           linetype = 1,
           size = 0.35,
           colour = "#C0C0C0") +
  coord_cartesian(xlim = c(1990, 2016), clip = "off") +
  scale_fill_ptol(drop = F) +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "",
                     expand = c(0, 0),
                     limits = c(-2.5, 13),
                     breaks = seq(-2, 12, by = 2),
                     labels = c("-2 Gt", "0 Gt","2 Gt", "4 Gt", "6 Gt", "8 Gt", "10 Gt", "12 Gt")) +
  plot.theme.sector + legend.guide.top10 +
  labs(title = "Annual Sectoral Emissions for China (1990 - 2016)",
       subtitle = "Historic emissions (Gt CO2e) from fossil fuel combustion, industrial processes, and land-use change\n")

China.Emissions.BySector.Historical.Arranged <- 
  grid.arrange(China.Emissions.BySector.Historical.Plot, 
               sector.7categories.legend,
               bottom = grid.text(label = source.label.cait, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 2,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95),
               widths = c(10, 4))


# CHINA emissions plot with same sector categories as treemap (Power & Heat, Industry, Transport, AFOLU, Other)

China.Emissions.5Sector.Historical.Plot <-
  ggplot(GHGTop10.CAIT.5sectors %>% filter(country.name=="China"), aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  # geom_line(aes(group = sector),
  #           colour = "#C0C0C0",
  #           size = 0.5,
  #           position = "stack",
  #           show.legend = F) +
  scale_fill_manual(values = colours.5categories) +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "",
                     expand = c(0, 0),
                     limits = c(0, 13),
                     breaks = seq(2, 12, by = 2),
                     labels = c("2 Gt", "4 Gt", "6 Gt", "8 Gt", "10 Gt", "12 Gt")) +
  plot.theme.sector + legend.guide.top10 +
  labs(title = "Annual Sectoral Emissions for China (1990 - 2016)",
       subtitle = "Historic emissions (Gt CO2e) from fossil fuel combustion, industrial processes, and land-use change\n")

China.Emissions.5Sector.Historical.Arranged <- 
  grid.arrange(China.Emissions.5Sector.Historical.Plot, 
               bottom = grid.text(label = source.label.cait, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))



# ---- 2.2 UNITED STATES ----

USA.Emissions.BySector.Historical.Plot <-
  ggplot(GHGTop10.CAIT %>% filter(country.name=="United States"), 
         aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack",
            show.legend = F) +
  # geom_line(aes(group = sector),
  #           colour = "#C0C0C0",
  #           size = 0.5,
  #           position = "stack",
  #           show.legend = F) +
  geom_hline(aes(yintercept = 0),
             linetype = 3,
             size = 0.35,
             colour = "#C0C0C0") +
  annotate("segment", x = 2016, xend = 2016.5, y = 0, yend = 0,
           linetype = 1,
           size = 0.35,
           colour = "#C0C0C0") +
  coord_cartesian(xlim = c(1990, 2016), clip = "off") +
  scale_fill_ptol(drop = F) +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "",
                     expand = c(0, 0),
                     limits = c(-2.5, 13),
                     breaks = seq(-2, 12, by = 2),
                     labels = c("-2 Gt", "0 Gt","2 Gt", "4 Gt", "6 Gt", "8 Gt", "10 Gt", "12 Gt")) +
  plot.theme.sector + legend.guide.top10 +
  labs(title = "Annual Sectoral Emissions for United States (1990 - 2016)",
       subtitle = "Historic emissions (Gt CO2e) from fossil fuel combustion, industrial processes, and land-use change\n")

USA.Emissions.BySector.Historical.Arranged <- 
  grid.arrange(USA.Emissions.BySector.Historical.Plot, 
               sector.7categories.legend,
               bottom = grid.text(label = source.label.cait, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 2,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95),
               widths = c(10, 4))


# ---- 2.3 EU-27 ----

EU27.Emissions.BySector.Historical.Plot <-
  ggplot(GHGTop10.CAIT %>% filter(country.name=="EU-27"), 
         aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack",
            show.legend = F) +
  # geom_line(aes(group = sector),
  #           colour = "#C0C0C0",
  #           size = 0.5,
  #           position = "stack",
  #           show.legend = F) +
  geom_hline(aes(yintercept = 0),
             linetype = 3,
             size = 0.35,
             colour = "#C0C0C0") +
  annotate("segment", x = 2016, xend = 2016.5, y = 0, yend = 0,
           linetype = 1,
           size = 0.35,
           colour = "#C0C0C0") +
  coord_cartesian(xlim = c(1990, 2016), clip = "off") +
  scale_fill_ptol(drop = F) +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "",
                     expand = c(0, 0),
                     limits = c(-2.5, 13),
                     breaks = seq(-2, 12, by = 2),
                     labels = c("-2 Gt", "0 Gt","2 Gt", "4 Gt", "6 Gt", "8 Gt", "10 Gt", "12 Gt")) +
  plot.theme.sector + legend.guide.top10 +
  labs(title = "Annual Sectoral Emissions for EU-27 (1990 - 2016)",
       subtitle = "Historic emissions (Gt CO2e) from fossil fuel combustion, industrial processes, and land-use change\n")

EU27.Emissions.BySector.Historical.Arranged <- 
  grid.arrange(EU27.Emissions.BySector.Historical.Plot, 
               sector.7categories.legend,
               bottom = grid.text(label = source.label.cait, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 2,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95),
               widths = c(10, 4))


# ---- 2.4 INDIA ----

India.Emissions.BySector.Historical.Plot <-
  ggplot(GHGTop10.CAIT %>% filter(country.name=="India"), 
         aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack",
            show.legend = F) +
  # geom_line(aes(group = sector),
  #           colour = "#C0C0C0",
  #           size = 0.5,
  #           position = "stack",
  #           show.legend = F) +
  geom_hline(aes(yintercept = 0),
             linetype = 3,
             size = 0.35,
             colour = "#C0C0C0") +
  annotate("segment", x = 2016, xend = 2016.5, y = 0, yend = 0,
           linetype = 1,
           size = 0.35,
           colour = "#C0C0C0") +
  coord_cartesian(xlim = c(1990, 2016), clip = "off") +
  scale_fill_ptol(drop = F) +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "",
                     expand = c(0, 0),
                     limits = c(-2.5, 13),
                     breaks = seq(-2, 12, by = 2),
                     labels = c("-2 Gt", "0 Gt","2 Gt", "4 Gt", "6 Gt", "8 Gt", "10 Gt", "12 Gt")) +
  plot.theme.sector + legend.guide.top10 +
  labs(title = "Annual Sectoral Emissions for India (1990 - 2016)",
       subtitle = "Historic emissions (Gt CO2e) from fossil fuel combustion, industrial processes, and land-use change\n")

India.Emissions.BySector.Historical.Arranged <- 
  grid.arrange(India.Emissions.BySector.Historical.Plot, 
               sector.7categories.legend,
               bottom = grid.text(label = source.label.cait, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 2,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95),
               widths = c(10, 4))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Projections of Sector-Specific Emissions Plots ---
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 CHINA ----

China.Emissions.BySector.Future.Plot <-
  ggplot(GHGTop10.GCAM %>% filter(country.name=="China"), aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  # geom_line(aes(group = sector),
  #           colour = "#C0C0C0",
  #           size = 0.5,
  #           position = "stack",
  #           show.legend = F) +
  scale_fill_manual(values = colours.6categories) +
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
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- 3.2 UNITED STATES ----

USA.Emissions.BySector.Future.Plot <-
  ggplot(GHGTop10.GCAM %>% filter(country.name=="United States"), 
         aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  # geom_line(aes(group = sector),
  #           colour = "#C0C0C0",
  #           size = 0.5,
  #           position = "stack",
  #           show.legend = F) +
  scale_fill_manual(values = colours.6categories) +
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
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- 3.3 EU-28 ----

EU28.Emissions.BySector.Future.Plot <-
  ggplot(GHGTop10.GCAM %>% filter(country.name=="EU-28"), aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  # geom_line(aes(group = sector),
  #           colour = "#C0C0C0",
  #           size = 0.5,
  #           position = "stack",
  #           show.legend = F) +
  scale_fill_manual(values = colours.6categories) +
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
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- 3.4 INDIA ----

India.Emissions.BySector.Future.Plot <-
  ggplot(GHGTop10.GCAM %>% filter(country.name=="India"), aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  # geom_line(aes(group = sector),
  #           colour = "#C0C0C0",
  #           size = 0.5,
  #           position = "stack",
  #           show.legend = F) +
  scale_fill_manual(values = colours.6categories) +
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
    units = "in", height = 6, width = 9, res = 400)
grid.newpage()
grid.draw(China.Emissions.BySector.Historical.Arranged)
dev.off()

# ---- CHINA - future projected emissions by sector ----
png(paste(FigureFileName, "/China.future.bysector.png", sep = ""),
    units = "in", height = 6, width = 9.2, res = 400)
grid.newpage()
grid.draw(China.Emissions.BySector.Future.Arranged)
dev.off()

# ---- CHINA - hisorical emissions, 5 sectors (harmonized with treemap) ----
png(paste(FigureFileName, "/China.historical.5sectors.png", sep = ""),
    units = "in", height = 6, width = 9.2, res = 400)
grid.newpage()
grid.draw(China.Emissions.5Sector.Historical.Arranged)
dev.off()

# ---- USA - historical emissions by sector ----
png(paste(FigureFileName, "/USA.historical.bysector.png", sep = ""),
    units = "in", height = 6, width = 9, res = 400)
grid.newpage()
grid.draw(USA.Emissions.BySector.Historical.Arranged)
dev.off()

# ---- USA - future projected emissions by sector ----
png(paste(FigureFileName, "/USA.future.bysector.png", sep = ""),
    units = "in", height = 6, width = 9.2, res = 400)
grid.newpage()
grid.draw(USA.Emissions.BySector.Future.Arranged)
dev.off()


# ---- EU27 - historical emissions by sector ----
png(paste(FigureFileName, "/EU27.historical.bysector.png", sep = ""),
    units = "in", height = 6, width = 9, res = 400)
grid.newpage()
grid.draw(EU27.Emissions.BySector.Historical.Arranged)
dev.off()

# ---- EU28 - future projected emissions by sector ----
png(paste(FigureFileName, "/EU28.future.bysector.png", sep = ""),
    units = "in", height = 6, width = 9.2, res = 400)
grid.newpage()
grid.draw(EU28.Emissions.BySector.Future.Arranged)
dev.off()


# ---- INDIA - historical emissions by sector ----
png(paste(FigureFileName, "/India.historical.bysector.png", sep = ""),
    units = "in", height = 6, width = 9, res = 400)
grid.newpage()
grid.draw(India.Emissions.BySector.Historical.Arranged)
dev.off()

# ---- INDIA - future projected emissions by sector ----
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