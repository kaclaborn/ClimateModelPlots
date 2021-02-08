
# code: Top 30 plots

# Current Emissions - Top 30 emitters (2018 as reference)
Top30Emissions.Bar.Plot <-
  ggplot(GHGTop30.EU %>% filter(year==2018), aes(x = country.name, y = value/1000000)) +
  geom_bar(fill = "#56ACAD",
           stat = "identity") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0, 16),
                     breaks = seq(2.5, 15, by = 2.5),
                     labels = c("2.5 Gt", "5 Gt", "7.5 Gt", "10 Gt", "12.5 Gt", "15 Gt")) +
  plot.theme.top30 + legend.guide.top10 +
  labs(title = "Current GHG Emissions: Top 30 Current Emitters", 
       subtitle = "Emissions (Gt CO2e) of Kyoto greenhouse gases (AR4) for thirty countries with highest present-day emissions (2018)")

Top30Emissions.Bar.Arranged <- 
  grid.arrange(Top30Emissions.Bar.Plot, 
               bottom = grid.text(label = source.label, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# EXPORT
png('figures/Top30.png',
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(Top30Emissions.Bar.Arranged)
dev.off()
