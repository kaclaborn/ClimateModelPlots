
# Country-specific future trajectories (all GHGs)


source('code/PlotThemes.R')


# ---- CHINA ----

# All IAMs as separate lines
CHN.GHGEmissions.Future.Plot <- 
  ggplot(GHG.CHN %>% filter(year>=2018 & year<=2050), aes(x = year, y = value/1000000)) +
  geom_line(aes(group = scenario, size = marker, alpha = marker)) +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0,25),
                     breaks = seq(5, 20, by = 5),
                     labels = c("5 Gt", "10 Gt", "15 Gt", "20 Gt")) +
  scale_size_manual(values = c("0" = 0.3, "1" = 1.2),
                    guide = F) +
  scale_alpha_manual(values = c("0" = 0.3, "1" = 1),
                     guide = F) +
  plot.theme.top10 +
  labs(title = "Annual GHG emissions trajectories - China", 
       subtitle = "Future projected emissions of Kyoto greenhouse gases (AR4) in CO2e,\nSSP2 baseline scenario with associated integrated assessment models")

CHN.GHGEmissions.Future.Arranged <- 
  grid.arrange(CHN.GHGEmissions.Future.Plot, 
               bottom = grid.text(label = source.label,
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1.2, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


CHN.GHGEmissions.Time.Plot <- 
  ggplot(GHG.CHN %>% filter(year>=1950 & year<=2050), aes(x = year, y = value/1000000)) +
  geom_line(aes(group = scenario, size = marker, alpha = marker)) +
  geom_vline(aes(xintercept = 2018),
             size = 0.5, 
             colour = "#909090",
             alpha = 0.5) +
  annotate("text", x = 2015, y = 1, label = "2018", size = 2.5, colour = "#909090") +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0, 25),
                     breaks = seq(5, 20, by = 5),
                     labels = c("5 Gt", "10 Gt", "15 Gt", "20 Gt")) +
  scale_size_manual(values = c("0" = 0.3, "1" = 1.2),
                    guide = F) +
  scale_alpha_manual(values = c("0" = 0.3, "1" = 1),
                     guide = F) +
  plot.theme.top10 +
  labs(title = "Annual GHG emissions trajectories - China", 
       subtitle = "Historic and future projected emissions of Kyoto greenhouse gases (AR4) in CO2e,\nSSP2 baseline scenario with associated integrated assessment models")

CHN.GHGEmissions.Time.Arranged <- 
  grid.arrange(CHN.GHGEmissions.Time.Plot, 
               bottom = grid.text(label = source.label,
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1.2, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))

# 'Wedges' plots (only minimum and maximum IAM shown for each year)
CHN.GHGEmissions.Wedges.Future.Plot <- 
  ggplot(GHG.CHN %>% filter(year>=2018 & year<=2050) %>%
           group_by(year) %>% summarise(min = min(value),
                                        max = max(value),
                                        value = value[scenario==marker.scenario]), 
         aes(x = year)) +
  geom_ribbon(aes(ymin = min/1000000, ymax = max/1000000),
              stat = "identity", outline.type = "full", 
              fill = "#303030", alpha = 0.3) +
  geom_line(aes(y = value/1000000),
            size = 1.2) +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0, 25),
                     breaks = seq(5, 20, by = 5),
                     labels = c("5 Gt", "10 Gt", "15 Gt", "20 Gt")) +
  scale_size_manual(values = c("0" = 0.3, "1" = 1.2),
                    guide = F) +
  scale_alpha_manual(values = c("0" = 0.3, "1" = 1),
                     guide = F) +
  plot.theme.top10 +
  labs(title = "Annual GHG emissions trajectories - China", 
       subtitle = "Future projected emissions of Kyoto greenhouse gases (AR4) in CO2e,\nSSP2 baseline scenario with integrated assessment model uncertainty")

CHN.GHGEmissions.Wedges.Future.Arranged <- 
  grid.arrange(CHN.GHGEmissions.Wedges.Future.Plot, 
               bottom = grid.text(label = source.label,
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1.2, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


CHN.GHGEmissions.Wedges.Time.Plot <- 
  ggplot(GHG.CHN %>% filter(year>=1950 & year<=2050) %>% 
           group_by(year) %>% summarise(min = min(value),
                                        max = max(value),
                                        value = value[scenario==marker.scenario]), 
         aes(x = year)) +
  geom_ribbon(aes(ymin = min/1000000, ymax = max/1000000),
              stat = "identity", outline.type = "full", 
              fill = "#303030", alpha = 0.3) +
  geom_line(aes(y = value/1000000),
            size = 1.2) +
  geom_vline(aes(xintercept = 2018),
             size = 0.5, 
             colour = "#909090",
             alpha = 0.5) +
  annotate("text", x = 2015, y = 1, label = "2018", size = 2.5, colour = "#909090") +
  scale_x_continuous(name = "",
                     expand = c(0, 0),
                     breaks = seq(1950, 2050, by = 25)) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0, 25),
                     breaks = seq(5, 20, by = 5),
                     labels = c("5 Gt", "10 Gt", "15 Gt", "20 Gt")) +
  scale_size_manual(values = c("0" = 0.3, "1" = 1.2),
                    guide = F) +
  scale_alpha_manual(values = c("0" = 0.3, "1" = 1),
                     guide = F) +
  plot.theme.top10 +
  labs(title = "Annual GHG emissions trajectories - China", 
       subtitle = "Historic and future projected emissions of Kyoto greenhouse gases (AR4) in CO2e,\nSSP2 baseline scenario with integrated assessment model uncertainty")

CHN.GHGEmissions.Wedges.Time.Arranged <- 
  grid.arrange(CHN.GHGEmissions.Wedges.Time.Plot, 
               bottom = grid.text(label = source.label,
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1.2, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))

# grid.text("Source: Gütschow, J.; Jeffery, L.; Günther, A.; Meinshausen, M. (2020):\n              Country resolved combined emission and socio-economic pathways based on the RCP and SSP scenarios - dataset.\n              Zenodo. <https://doi.org/10.5281/zenodo.3638137>.\nNote: Units in gigagrams (Gg) CO2eq", 
#           x = unit(25, "pt"), 
#           just = "left", 
#           gp = gpar(fontsize = 8, lineheight = 1.2, col = "#303030"))


# ---- EXPORT ----

# OPTION 1 PLOT - historical and future trajectories; all IAMs as separate lines
png('figures/CountryEmissions.Option1.png',
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(CHN.GHGEmissions.Time.Arranged)
dev.off()

# OPTION 2 PLOT - only future trajectories; all IAMs as separate lines
png('figures/CountryEmissions.Option2.png',
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(CHN.GHGEmissions.Future.Arranged)
dev.off()

# OPTION 3 PLOT - historical and future trajectories; min and max IAM shown for each year
png('figures/CountryEmissions.Option3.png',
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(CHN.GHGEmissions.Wedges.Time.Arranged)
dev.off()

# OPTION 4 PLOT - historical and future trajectories; min and max IAM shown for each year
png('figures/CountryEmissions.Option4.png',
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(CHN.GHGEmissions.Wedges.Future.Arranged)
dev.off()
