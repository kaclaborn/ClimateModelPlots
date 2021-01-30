
# Country-specific future trajectories (all GHGs)


source('code/PlotThemes.R')


# CHINA

x <- ggplot(TimeGHG.CHN, aes(x = year, y = value/1000000)) +
  geom_line(aes(group = scenario, size = marker, alpha = marker)) +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0,25),
                     breaks = seq(5, 20, by = 5),
                     labels = c("5 million Gg", "10 million Gg", "15 million Gg", "20 million Gg")) +
  scale_size_manual(values = c("0" = 0.3, "1" = 1.2),
                    guide = F) +
  scale_alpha_manual(values = c("0" = 0.3, "1" = 1),
                     guide = F) +
  plot.theme.top10 +
  labs(title = "Annual GHG emissions trajectories - China", 
       subtitle = "Historic and projected future emissions of all greenhouse gases (CO2eq),\nSSP2 scenario with associated model uncertainty")

y <- grid.arrange(x, bottom = grid.text("Source: IPCC KYOTOGHGAR4\nNote: units in gigagrams", x = unit(25, "pt"), 
                          just = "left", gp = gpar(fontsize = 8, lineheight = 1.2)), ncol = 1,
             padding = unit(5, "pt"), vp = viewport(width = 1, height = 0.95))


png('figures/test.png',
    units="in",height=6,width=8,res=400)
grid.newpage()
grid.draw(y)
dev.off()
