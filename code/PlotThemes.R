
# Labels
source.label <- "Source: Gütschow et al (2020). <https://doi.org/10.5281/zenodo.3638137>\nNote: Units in gigatonnes (Gt) CO2e"


# Plot themes

# # NOTE only need to do this one time -- once you have "Poppins" font on your local device (I'm sure there's a better way to do this!)
# font_import()

plot.theme.top10 <-
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
        legend.position = "right",
        legend.box.spacing = unit(0.1, "cm"))


plot.theme.top30 <-
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
        plot.margin = margin(t = 5, r = 50, b = 5, l = 5, unit = "pt"),
        axis.title = element_text(size = rel(0.9),
                                  angle = 0,
                                  face = "bold",
                                  colour = "#303030"),
        axis.text.x = element_text(size = rel(0.9),
                                 angle = 300,
                                 colour = "#303030",
                                 lineheight = 0.7,
                                 hjust = 0),
        axis.text.y = element_text(size = rel(0.9),
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

