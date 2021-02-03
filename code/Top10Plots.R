
# Top 10 Plots

source('code/PlotThemes.R')

# TOP 10 EMISSIONS, NOT STACKED
# Future trajectories only - Top 10 emitters (2020 as reference)
NonStacked.Top10Emissions.Future.Plot <-
  ggplot(GHGTop10.EU %>% filter(year>=2020 & year<=2050), aes(x = year, y = value/1000000)) +
  geom_line(aes(group = country, colour = country),
            size = 1.25,
            position = "identity") +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0, 21),
                     breaks = seq(5, 20, by = 5),
                     labels = c("5 Gt", "10 Gt", "15 Gt", "20 Gt")) +
  plot.theme.top10 + legend.guide.top10 +
  labs(title = "Annual GHG emissions trajectories, top 10 current emitters", 
       subtitle = "Future projected emissions of Kyoto greenhouse gases (AR4) in CO2e,\nSSP2 baseline scenario")

NonStacked.Top10Emissions.Future.Arranged <- 
  grid.arrange(NonStacked.Top10Emissions.Future.Plot, 
               bottom = grid.text(label = source.label, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1.2, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# Historical and future trajectories (1850 - 2050) - Top 10 emitters (2020 as reference)
NonStacked.Top10Emissions.Time.Plot <-
  ggplot(GHGTop10.EU %>% filter(year>=1950 & year<=2050), aes(x = year, y = value/1000000)) +
  geom_line(aes(group = country, colour = country.name),
            size = 1.25,
            position = "identity") +
  geom_vline(aes(xintercept = 2018),
             size = 0.5,
             colour = "#909090",
             alpha = 0.5) +
  annotate("text", x = 2015, y = 19, label = "2018", size = 2.5, colour = "#909090") +
  scale_fill_ptol() +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(1950, 2050, by = 25)) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0, 21),
                     breaks = seq(5, 20, by = 5),
                     labels = c("5 Gt", "10 Gt", "15 Gt", "20 Gt")) +
  plot.theme.top10 + legend.guide.top10 +
  labs(title = "Annual GHG emissions trajectories, top 10 current emitters", 
       subtitle = "Historic and future projected emissions of Kyoto greenhouse gases (AR4) in CO2e,\nSSP2 baseline scenario")

NonStacked.Top10Emissions.Time.Arranged <- 
  grid.arrange(NonStacked.Top10Emissions.Time.Plot, 
               bottom = grid.text(label = source.label, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1.2, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))



# TOP 10 EMISSIONS, STACKED
# Future trajectories only - Top 10 emitters (2020 as reference)
Stacked.Top10Emissions.Future.Plot <-
  ggplot(GHGTop10.EU %>% filter(year>=2020 & year<=2050), aes(x = year, y = value/1000000)) +
  geom_area(aes(group = country, fill = country.name)) +
  geom_line(aes(group = country),
            colour = "#C0C0C0",
            size = 0.5,
            position = "stack",
            show.legend = F) +
  scale_fill_ptol() +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0,50),
                     breaks = seq(10, 40, by = 10),
                     labels = c("10 Gt", "20 Gt", "30 Gt", "40 Gt")) +
  plot.theme.top10 + legend.guide.top10 +
  labs(title = "Annual GHG emissions trajectories, top 10 current emitters", 
       subtitle = "Future projected emissions of Kyoto greenhouse gases (AR4) in CO2e,\nSSP2 baseline scenario")

Stacked.Top10Emissions.Future.Arranged <- 
  grid.arrange(Stacked.Top10Emissions.Future.Plot, 
               bottom = grid.text(label = source.label, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1.2, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# Historical and future trajectories (1850 - 2050) - Top 10 emitters (2020 as reference)
Stacked.Top10Emissions.Time.Plot <-
  ggplot(GHGTop10.EU %>% filter(year>=1950 & year<=2050), aes(x = year, y = value/1000000)) +
  geom_area(aes(group = country, fill = country.name)) +
  geom_line(aes(group = country),
            colour = "#C0C0C0",
            size = 0.5,
            position = "stack",
            show.legend = F) +
  geom_vline(aes(xintercept = 2018),
             size = 0.5,
             colour = "#909090",
             alpha = 0.5) +
  annotate("text", x = 2015, y = 41, label = "2018", size = 2.5, colour = "#909090") +
  scale_fill_ptol() +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(1950, 2050, by = 25)) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0,50),
                     breaks = seq(10, 40, by = 10),
                     labels = c("10 Gt", "20 Gt", "30 Gt", "40 Gt")) +
  plot.theme.top10 + legend.guide.top10 +
  labs(title = "Annual GHG emissions trajectories, top 10 current emitters", 
       subtitle = "Historic and future projected emissions of Kyoto greenhouse gases (AR4) in CO2e,\nSSP2 baseline scenario")

Stacked.Top10Emissions.Time.Arranged <- 
  grid.arrange(Stacked.Top10Emissions.Time.Plot, 
               bottom = grid.text(label = source.label, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1.2, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- EXPORT ----

# OPTION 1 PLOT - historical and future trajectories; stacked
png('figures/Top10.Option1.png',
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(Stacked.Top10Emissions.Time.Arranged)
dev.off()

# OPTION 2 PLOT - only future trajectories; stacked
png('figures/Top10.Option2.png',
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(Stacked.Top10Emissions.Future.Arranged)
dev.off()

# OPTION 3 PLOT - historical and future trajectories; non-stacked
png('figures/Top10.Option3.png',
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(NonStacked.Top10Emissions.Time.Arranged)
dev.off()

# OPTION 4 PLOT - only future trajectories; non-stacked
png('figures/Top10.Option4.png',
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(NonStacked.Top10Emissions.Future.Arranged)
dev.off()
