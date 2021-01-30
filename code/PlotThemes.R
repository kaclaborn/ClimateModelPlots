
# Plot themes

plot.theme.top10 <-
  theme(plot.title = element_text(size = rel(1.2),
                                  colour = "#303030",
                                  face = "bold"),
        plot.subtitle = element_text(size = rel(1),
                                  colour = "#303030",
                                  face = "italic"),
        axis.ticks = element_blank(),
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
        legend.position = "right",
        legend.box.spacing = unit(0.1, "cm"))


# Legend guides

legend.guide.top10 <-
  guides(fill = guide_legend(title.hjust = 1,
                             title.theme = element_blank(),
                             label.vjust = 0.5,
                             label.theme = element_text(size = rel(9),
                                                        angle = 0,
                                                        colour = "#505050",
                                                        lineheight = 0.75),
                             direction = "horizontal",
                             ncol = 1,
                             title.position = "left",
                             label.position = "right",
                             keywidth = unit(0.75, "cm"),
                             keyheight = unit(0.5, "cm")),
         colour = guide_legend(title.hjust = 1,
                               title.theme = element_blank(),
                               label.vjust = 0.5,
                               label.theme = element_text(size = rel(9),
                                                          angle = 0,
                                                          colour = "#505050",
                                                          lineheight = 0.75),
                               direction = "horizontal",
                               ncol = 1,
                               title.position = "left",
                               label.position = "right",
                               keywidth = unit(0.75, "cm"),
                               keyheight = unit(0.5, "cm")))

# Colour schemes
x <- as.matrix(List.Top10.EU)
top10.colours <- c("red", "orange", "yellow", "green", "blue", "purple", "pink", "brown", "gray", "black")
