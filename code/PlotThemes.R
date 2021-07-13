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

source.label.gutschow <- "Source: Gütschow et al (2020). <https://doi.org/10.5281/zenodo.3638137>\nNote:     Units in gigatonnes (Gt) CO2e"
source.label.cait <- "Source: Climate Watch Historical GHG Emissions. 2020. <https://www.climatewatchdata.org/ghg-emissions>\nNote:     Units in gigatonnes (Gt) CO2e;\n              'Industry' category comprised of industrial energy consumption, process emissions, and fugitive emissions"
source.label.gcam <- "Source: Scenario from the Global Change Analysis Model (GCAM 5.3) by the Center for Global Sustainability,\n              University of Maryland\nNote:     Units in gigatonnes (Gt);\n              Scenario includes impacts of COVID-19 on GDP and emissions;\n              'Industry' category comprised of industrial energy consumption, process emissions, and fugitive emissions"
source.label.iea <- "Source: IEA World Energy Outlook 2019\nNote:     Oil production units in million barrels per day (mb/d);\n              Gas production units in billions cubic meters (bcm)"

source.label.gcamtreemap <- "Source: Scenario from the Global Change Analysis Model (GCAM 5.3) by the Center for Global Sustainability,\n              University of Maryland\nNote:     Sector-specific percentages reflect global share of all GHG emissions;\n              Scenario includes impacts of COVID-19 on GDP and emissions;\n              'Industry' category comprised of industrial energy consumption, process emissions, and fugitive emissions"
source.label.gutschowpercapita <- "Source: Gütschow et al (2020). <https://doi.org/10.5281/zenodo.3638137>\nNote:     Units in gigatonnes (Gt) CO2e per million persons"
source.label.gutschowallssp <- "Source: Gütschow et al (2020). <https://doi.org/10.5281/zenodo.3638137>\nNote:     Highlighted models represent the model marker for each SSP; uncertainty bands represent the minimum and maximum values across all five IAMs;\n              Units in gigatonnes (Gt) CO2e"

source.label.gutschow.extradetails <- "Source:         Gütschow et al (2020). <https://doi.org/10.5281/zenodo.3638137>\n\nModel Note:  The SSP2 reference scenario illustrates modelled emissions trajectories if historic energy supply/demand and\n                      land use patterns persist into the future. It does not include any climate policies beyond those in place today.\n                      More details on the SSP2 marker model can be found at <https://doi.org/10.1016/j.gloenvcha.2016.06.004>\n\nUnits:            Gigatonnes (Gt) CO2e"
source.label.gutschow.percent.extradetails <- "Source:         Gütschow et al (2020). <https://doi.org/10.5281/zenodo.3638137>\n\nModel Note:  The SSP2 reference scenario illustrates modelled emissions trajectories if historic energy supply/demand and\n                      land use patterns persist into the future. It does not include any climate policies beyond those in place today.\n                      More details on the SSP2 marker model can be found at <https://doi.org/10.1016/j.gloenvcha.2016.06.004>\n\nUnits:            Interannual percent change"

source.label.gutschow.SSP1SSP2.details <- "Source:         Gütschow et al (2020). <https://doi.org/10.5281/zenodo.3638137>\n\nModel Note:  The highlighted line represents the marker model for each SSP; uncertainty bands in the Annual Emissions plot (left)\n                      represent the minimum and maximum values across all five integrated assessment models.\n\n                      The SSP1 reference scenario illustrates modelled emissions trajectories in a 'sustainable development' paradigm, with less resource intensive lifestyles,\n                      global cooperation, and high economic growth. It does not include any climate policies beyond those in place today.\n                      More details on the SSP1 marker model can be found at <https://doi.org/10.1016/j.gloenvcha.2016.05.008>\n\n                      The SSP2 reference scenario illustrates modelled emissions trajectories if historic energy supply/demand and land use patterns persist into the future.\n                      It does not include any climate policies beyond those in place today. More details on the SSP2 marker model can be found at\n                      <https://doi.org/10.1016/j.gloenvcha.2016.06.004>\n\nUnits:            Gigatonnes (Gt) CO2e (left) and interannual percent change (right)"


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
  theme(plot.title = element_text(size = 12,
                                  colour = "#303030",
                                  face = "bold"),
        plot.subtitle = element_text(size = 10,
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
        axis.text = element_text(size = 9,
                                 angle = 0,
                                 colour = "#303030",
                                 lineheight = 0.7),
        legend.position = "right",
        legend.box.spacing = unit(0.1, "cm"))


plot.theme.treemap <-
  theme(plot.title = element_text(size = 14,
                                  colour = "#303030",
                                  face = "bold"),
        plot.subtitle = element_text(size = 12,
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


plot.theme.comparisons <- 
  theme(plot.title = element_text(size = 10,
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
                                 lineheight = 0.7))

plot.theme.India.infra <- 
  theme(plot.title = element_text(size = rel(1),
                                  colour = "#303030",
                                  face = "bold", 
                                  hjust = 0.5),
        axis.ticks = element_line(colour = "#C0C0C0"),
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
                                 lineheight = 0.7))

  
colours.6categories <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677")
colours.5categories <- c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
colours.4categories <- c("#332288", "#88CCEE", "#117733", "#CC6677")


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


# ---- 3.2 Legend guide for all SSP emissions trajectories  ----

legend.guide.allssp <-
  guides(colour = guide_legend(title.theme = element_text(size = rel(11),
                                                          colour = "#505050",
                                                          face = "bold"),
                               label.vjust = 0.5,
                               label.theme = element_text(size = rel(9),
                                                          angle = 0,
                                                          colour = "#505050",
                                                          lineheight = 0.75),
                               direction = "horizontal",
                               ncol = 1,
                               title.position = "top",
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
