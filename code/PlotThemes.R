# 
# code: Define plot themes, labels, legend guides, export folder
# 
# ---- sections ----
# 1.  Manual Labels & Other Configurations
# 2.  Plot Themes
# 3.  Legend Guides

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Manual Labels & Other Configurations ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 1.1 Labels to be used for bottom of figures (source, notes) -----

source.label.gutschow <- "Source: Gütschow et al (2020). <https://doi.org/10.5281/zenodo.3638137>\nNote: Units in gigatonnes (Gt) CO2e"
source.label.cait <- "Source: Climate Watch Historical GHG Emissions. 2020. <https://www.climatewatchdata.org/ghg-emissions>\nNote: Units in gigatonnes (Gt) CO2e"
source.label.gcam <- "Source: Scenario from the Global Change Analysis Model (GCAM 5.3) by the Center for Global Sustainability, University of Maryland\nNote: Units in gigatonnes (Gt); scenario includes impacts of COVID-19 on GDP and emissions"

source.label.caittreemap <- "Source: Climate Watch Historical GHG Emissions. 2020. <https://www.climatewatchdata.org/ghg-emissions>\nNote: Sector-specific percentages reflect global share of all GHG emissions"


# ---- 1.2 Create a new output figure folder with today's date ----

# --- Define figure output directory
dir.create(paste("figures/outputs/", format(Sys.Date(), format = "%Y%m%d"), sep = ""))

FigureFileName <- paste("figures/outputs/", format(Sys.Date(), format = "%Y%m%d"), sep = "")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Plot Themes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

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
                                 angle = 330,
                                 colour = "#303030",
                                 lineheight = 0.7,
                                 hjust = 0),
        axis.text.y = element_text(size = rel(0.9),
                                   angle = 0,
                                   colour = "#303030",
                                   lineheight = 0.7),
        legend.position = "right",
        legend.box.spacing = unit(0.1, "cm"))


plot.theme.sector <-
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


plot.theme.donut <-
  theme(plot.title = element_text(size = rel(1),
                                  colour = "#303030",
                                  face = "bold"),
        plot.subtitle = element_text(size = rel(0.75),
                                     colour = "#303030"),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "#909090"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "pt"),
        axis.title = element_text(size = rel(0.9),
                                  angle = 0,
                                  face = "bold",
                                  colour = "#303030"),
        axis.text = element_blank(),
        legend.position = "right",
        legend.box.spacing = unit(0.1, "cm"))

colours.6categories <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677")
colours.5categories <- c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Legend Guides ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Legend guide for Top 10 (and currently, all) plots ----

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


# ---- 3.2 Define function to retrieve legend from plot ----

get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
