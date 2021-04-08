# 
# code: Oil & Gas Specific Plots - Demand, Production, & Developed Reserves
# 
# ---- sections ----
# 1. Load libraries, import data
# 2. Production & demand maps
# 3. Demand by sector
# 4. Developed reserves
# 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: IMPORT DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 1.1 Load libraries ----

pacman::p_load(rio, grid, gridtext, gridExtra, extrafont, ggthemes, ggpubr, ggplot2, 
               maps, tidyr, stringr, dplyr)


# ---- 1.2 Import data

World <- map_data("world")

IEA.OilGas.ProductionDemand <- 
  import('data/inputs/OilGas_ProductionDemand_IEA2019.csv', header = T)

IEA.OilGas.Demand.bySector <-
  import('data/inputs/OilGas_Demand_Sector_IEA2019.csv', header = T) 

Rystad.DevelopedReserves <-
  import('data/inputs/DevelopedReserves_Rystad.csv', header = T)


# ---- 1.3 Source PlotThemes.R ----

eval(parse('code/PlotThemes.R', encoding = 'UTF-8'))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: PRODUCTION & DEMAND MAPS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Oil & Gas Production legends ----

oilgas.production.legend <-
  ggplot() +
  geom_point(data = IEA.OilGas.ProductionDemand %>% filter(type%in%c("Oil Production", "Gas Production")),
             aes(x = long2, y = lat, 
                 size = ifelse(type=="Oil Production", `2018` * 15, `2018`),
                 colour = type)) +
  scale_size_continuous(guide = F) +
  scale_colour_manual(name = "Production Type",
                      values = c("Oil Production" = colours.5categories[1], 
                                 "Gas Production" = colours.5categories[2]),
                      labels = c("Oil Production" = "Oil (mb/d)", 
                                 "Gas Production" = "Gas (bcm)"),
                      guide = guide_legend(reverse = T)) +
  theme(legend.title = element_text(size = 9,
                                    face = "bold",
                                    colour = "#303030"),
        legend.text = element_text(size = 8,
                                   colour = "#303030"))


oilgas.production.legend <- get_legend(oilgas.production.legend)



# Plot each individual year 
OilGas.Production.2018.plot <-
  ggplot() +
  geom_polygon(data = World, aes(x = long, y = lat, group = group), fill="grey", colour = "white", alpha=0.6) +
  geom_text(data = IEA.OilGas.ProductionDemand %>% filter(type=="Oil Production"),
             aes(x = (long + long2) / 2, y = lat + 7, label = region),
             size = 3) +
  geom_point(data = IEA.OilGas.ProductionDemand %>% filter(type%in%c("Oil Production", "Gas Production")),
             aes(x = long2, y = lat, 
                 size = ifelse(type=="Oil Production", `2018` * 15, `2018`),
                 colour = type),
             show.legend = F) +
  geom_label(data = IEA.OilGas.ProductionDemand %>% filter(type%in%c("Oil Production", "Gas Production")),
             aes(x = ifelse(type=="Oil Production", ((long + long2) / 2) - 5, ((long + long2) / 2) + 5),
                 y = lat - 10, 
                 label = `2018`),
             size = 2) +
  annotate("text", x = 172, y = -78, label = "2018", fontface = "bold") +
  scale_colour_manual(name = "Production Type",
                      values = c("Oil Production" = colours.5categories[1], 
                                 "Gas Production" = colours.5categories[2]),
                      labels = c("Oil Production" = "Oil (mb/d)", 
                                 "Gas Production" = "Gas (bcm)"),
                      guide = guide_legend(reverse = T)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(size = rel(1),
                                  colour = "#303030",
                                  face = "bold"),
        plot.subtitle = element_text(size = rel(0.75),
                                     colour = "#303030"),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "#909090"),
        panel.border = element_rect(fill = NA,
                                    size = 0.25,
                                    colour = "#C0C0C0"),
        panel.grid = element_blank(),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "pt"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 9,
                                    face = "bold",
                                    colour = "#303030"),
        legend.text = element_text(size = 8,
                                   colour = "#303030")) +
  labs(title = "Global Oil and Gas Production, by Region",
       subtitle = "Historic and future projections (2018, 2030, 2040)")

OilGas.Production.2030.plot <-
  ggplot() +
  geom_polygon(data = World, aes(x = long, y = lat, group = group), fill="grey", colour = "white", alpha=0.6) +
  geom_text(data = IEA.OilGas.ProductionDemand %>% filter(type=="Oil Production"),
            aes(x = (long + long2) / 2, y = lat + 7, label = region),
            size = 3) +
  geom_point(data = IEA.OilGas.ProductionDemand %>% filter(type%in%c("Oil Production", "Gas Production")),
             aes(x = long2, y = lat, 
                 size = ifelse(type=="Oil Production", `2030` * 15, `2030`),
                 colour = type),
             show.legend = F) +
  geom_label(data = IEA.OilGas.ProductionDemand %>% filter(type%in%c("Oil Production", "Gas Production")),
             aes(x = ifelse(type=="Oil Production", ((long + long2) / 2) - 5, ((long + long2) / 2) + 5),
                 y = lat - 10, 
                 label = `2030`),
             size = 2) +
  annotate("text", x = 172, y = -78, label = "2030", fontface = "bold") +
  scale_colour_manual(values = c("Oil Production" = colours.5categories[1], 
                                 "Gas Production" = colours.5categories[2])) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "#909090"),
        panel.border = element_rect(fill = NA,
                                    size = 0.25,
                                    colour = "#C0C0C0"),
        panel.grid = element_blank(),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "pt"),
        axis.title = element_blank(),
        axis.text = element_blank())


OilGas.Production.2040.plot <-
  ggplot() +
  geom_polygon(data = World, aes(x = long, y = lat, group = group), fill="grey", colour = "white", alpha=0.6) +
  geom_text(data = IEA.OilGas.ProductionDemand %>% filter(type=="Oil Production"),
            aes(x = (long + long2) / 2, y = lat + 7, label = region),
            size = 3) +
  geom_point(data = IEA.OilGas.ProductionDemand %>% filter(type%in%c("Oil Production", "Gas Production")),
             aes(x = long2, y = lat, 
                 size = ifelse(type=="Oil Production", `2040` * 15, `2040`),
                 colour = type),
             show.legend = F) +
  geom_label(data = IEA.OilGas.ProductionDemand %>% filter(type%in%c("Oil Production", "Gas Production")),
             aes(x = ifelse(type=="Oil Production", ((long + long2) / 2) - 5, ((long + long2) / 2) + 5),
                 y = lat - 10, 
                 label = `2040`),
             size = 2) +
  annotate("text", x = 172, y = -78, label = "2040", fontface = "bold") +
  scale_colour_manual(values = c("Oil Production" = colours.5categories[1], 
                                 "Gas Production" = colours.5categories[2])) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "#909090"),
        panel.border = element_rect(fill = NA,
                                    size = 0.25,
                                    colour = "#C0C0C0"),
        panel.grid = element_blank(),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "pt"),
        axis.title = element_blank(),
        axis.text = element_blank())


OilGas.Production.Arranged <- 
  grid.arrange(arrangeGrob(OilGas.Production.2018.plot, 
                           OilGas.Production.2030.plot,
                           OilGas.Production.2040.plot,
                           ncol = 1),
               oilgas.production.legend,
               bottom = grid.text(label = source.label.iea, 
                                  x = unit(8, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 2,
               padding = unit(5, "pt"), 
               widths = c(10, 1.5),
               vp = viewport(width = 0.95, height = 0.95))



# Export oil and gas production world bubble maps
png(paste(FigureFileName, "/OilGas.production.map.test.png", sep = ""),
    units = "in", height = 12, width = 8, res = 400)
grid.newpage()
grid.draw(OilGas.Production.Arranged)
dev.off()



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: DEMAND BY SECTOR ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 OIL DEMAND BY SECTOR ----

# Stacked bar chart
OilDemand.bySector.BarPlot <-
  ggplot(data = 
         IEA.OilGas.Demand.bySector %>% 
         filter(scenario=="STEPS" & type=="Oil demand" & sector!="Total") %>%
         mutate(year = as.character(year),
                sector = factor(sector, levels = c("Other Sectors", "Buildings and Power",
                                                   "Industry and Petrochemicals", "Aviation and Shipping", 
                                                   "Road Transport"),
                                ordered = T)),
       aes(x = year, y = value)) +
  geom_bar(aes(group = sector, fill = sector),
           width = 0.75,
           alpha = 0.9,
           stat = "identity",
           position = "stack",
           show.legend = T) +
  scale_fill_manual(name = "",
                    values = rev(colours.5categories),
                    guide = guide_legend(label.theme = element_text(size = 9),
                                         reverse = T)) +
  scale_x_discrete(name = "",
                   breaks = c(2018, 2030, 2040),
                   expand = c(0.3, 0)) +
  scale_y_continuous(name = "",
                     limits = c(0, 110),
                     breaks = c(20, 40, 60, 80, 100),
                     labels = c("20 mb/d", "40 mb/d", "60 mb/d", "80 mb/d", "100 mb/d"),
                     expand = c(0, 0)) +
  coord_flip() +
  theme(plot.title = element_text(size = 12,
                                  colour = "#303030",
                                  face = "bold"),
        plot.subtitle = element_text(size = 10,
                                     colour = "#303030"),
        axis.ticks.x = element_line(colour = "#303030"),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "#909090"),
        panel.border = element_rect(fill = NA,
                                    size = 0.25,
                                    colour = "#C0C0C0"),
        panel.grid.major.x = element_line(colour = "#303030",
                                          size = 0.35,
                                          linetype = 3),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "pt"),
        axis.title.x = element_text(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 9,
                                 angle = 0,
                                 colour = "#303030",
                                 lineheight = 0.7)) +
  labs(title = "Global Oil Demand, by Sector",
       subtitle = "Current & Projected Expansion for Stated Policies Scenario")


OilDemand.bySector.Arranged <-
  grid.arrange(OilDemand.bySector.BarPlot, 
             bottom = grid.text(label = "Source: IEA World Energy Outlook 2019", 
                                x = unit(32, "pt"),
                                just = "left",
                                gp = gpar(fontsize = 9, lineheight = 1, col = "#303030")),
             ncol = 1,
             padding = unit(5, "pt"), 
             vp = viewport(width = 1, height = 0.95))

# Export oil demand by sector bar plot
png(paste(FigureFileName, "/OilDemand.bySector.png", sep = ""),
    units = "in", height = 6, width = 9, res = 400)
grid.newpage()
grid.draw(OilDemand.bySector.Arranged)
dev.off()

## Bubble plot

# colours.5categories.alpha <- c("#33228880", "#88CCEE80", "#11773380", "#DDCC7780", "#CC667780")
# 
# 
# Oil.Demand.bySector.BubblePlot <-
#   ggplot(data = 
#            IEA.OilGas.Demand.bySector %>% 
#            filter(scenario=="STEPS" & type=="Oil demand" & sector!="Total") %>%
#            mutate(year = as.character(year),
#                   sector = factor(sector, levels = c("Road Transport", "Aviation and Shipping", 
#                                                      "Industry and Petrochemicals", "Buildings and Power",
#                                                      "Other Sectors"),
#                                   ordered = T)),
#          aes(x = sector, y = year)) +
#   geom_point(aes(size = value, fill = sector, colour = sector)) +
#   scale_fill_manual(name = "",
#                     values = colours.5categories.alpha,
#                     guide = guide_legend()) +
#   scale_colour_manual(values = colours.5categories,
#                       guide = F) +
#   scale_size_area(max_size = 20,
#                   guide = F) +
#   scale_x_discrete(name = "",
#                    expand = c(0.3, 0)) +
#   scale_y_discrete(name = "",
#                    breaks = c(2018, 2030, 2040),
#                    expand = c(0, 0.3)) +
#   theme(plot.title = element_text(size = rel(1),
#                                   colour = "#303030",
#                                   face = "bold"),
#         plot.subtitle = element_text(size = rel(0.75),
#                                      colour = "#303030"),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_line(colour = "#C0C0C0"),
#         panel.background = element_rect(fill = "white",
#                                         colour = "#909090"),
#         panel.border = element_rect(fill = NA,
#                                     size = 0.25,
#                                     colour = "#C0C0C0"),
#         panel.grid.major.y = element_line(colour = "#C0C0C0",
#                                           size = 0.35,
#                                           linetype = 3),
#         panel.grid.major.x = element_blank(),
#         plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "pt"),
#         axis.text.y = element_text(size = rel(0.9),
#                                  angle = 0,
#                                  colour = "#303030",
#                                  lineheight = 0.7),
#         axis.text.x = element_blank(), 
#         legend.position = "right",
#         legend.box.spacing = unit(0.1, "cm"))


# ---- 3.2 GAS DEMAND BY SECTOR ----

# Stacked bar chart
GasDemand.bySector.BarPlot <-
  ggplot(data = 
           IEA.OilGas.Demand.bySector %>% 
           filter(scenario=="STEPS" & type=="Gas demand" & sector!="Total") %>%
           mutate(year = as.character(year),
                  sector = factor(sector, levels = c("Other Sectors", "Transport",
                                                     "Buildings", "Industrial Use", 
                                                     "Power"),
                                  ordered = T)),
         aes(x = year, y = value)) +
  geom_bar(aes(group = sector, fill = sector),
           width = 0.75,
           alpha = 0.9,
           stat = "identity",
           position = "stack",
           show.legend = T) +
  scale_fill_manual(name = "",
                    values = rev(colours.5categories),
                    guide = guide_legend(label.theme = element_text(size = 9),
                                         reverse = T)) +
  scale_x_discrete(name = "",
                   breaks = c(2018, 2030, 2040),
                   expand = c(0.3, 0)) +
  scale_y_continuous(name = "",
                     limits = c(0, 5500),
                     breaks = c(1000, 2000, 3000, 4000, 5000),
                     labels = c("1000 bcm", "2000 bcm", "3000 bcm", "4000 bcm", "5000 bcm"),
                     expand = c(0, 0)) +
  coord_flip() +
  theme(plot.title = element_text(size = 12,
                                  colour = "#303030",
                                  face = "bold"),
        plot.subtitle = element_text(size = 10,
                                     colour = "#303030"),
        axis.ticks.x = element_line(colour = "#303030"),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "#909090"),
        panel.border = element_rect(fill = NA,
                                    size = 0.25,
                                    colour = "#C0C0C0"),
        panel.grid.major.x = element_line(colour = "#303030",
                                          size = 0.35,
                                          linetype = 3),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "pt"),
        axis.title.x = element_text(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 9,
                                 angle = 0,
                                 colour = "#303030",
                                 lineheight = 0.7)) +
  labs(title = "Global Gas Demand, by Sector",
       subtitle = "Current & Projected Expansion for Stated Policies Scenario")


GasDemand.bySector.Arranged <-
  grid.arrange(GasDemand.bySector.BarPlot, 
               bottom = grid.text(label = "Source: IEA World Energy Outlook 2019", 
                                  x = unit(32, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 9, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))

# Export gas demand by sector bar plot
png(paste(FigureFileName, "/GasDemand.bySector.png", sep = ""),
    units = "in", height = 6, width = 9, res = 400)
grid.newpage()
grid.draw(GasDemand.bySector.Arranged)
dev.off()



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: DEVELOPED RESERVES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 4.1 WRANGLE DATA ----

Reserves <-
  Rystad.DevelopedReserves %>%
  mutate(group = 1,
         degreesC = ifelse(is.na(degreesC), (value/1000) * 1.75, degreesC),
         category = factor(category, 
                           levels = c("Oil","Gas", "Coal", "postSR15", "SR15"),
                           ordered = T))


# ---- 3.2 RESERVES TEMPERATURE RISE ----

# Stacked bar chart
Reserves.TempRise.BarPlot <-
  ggplot(data = Reserves,
         aes(x = group, y = degreesC)) +
  geom_bar(aes(fill = category),
           width = 0.5,
           alpha = 0.9,
           stat = "identity",
           position = "stack",
           show.legend = T) +
  geom_segment(aes(x = 1.3, xend = 1.3,
                   y = sum(degreesC[type=="Emitted"])+0.01,
                   yend = sum(degreesC)),
               colour = "#303030") +
  geom_segment(aes(x = 1.27, xend = 1.3,
                   y = sum(degreesC[type=="Emitted"])+0.01,
                   yend = sum(degreesC[type=="Emitted"])+0.01),
               colour = "#303030") +
  geom_segment(aes(x = 1.27, xend = 1.3,
                   y = sum(degreesC),
                   yend = sum(degreesC)),
               colour = "#303030") +
  annotate("text", x = 1.41, 
           y = ((sum(Reserves$degreesC) - sum(Reserves$degreesC[Reserves$type=="Emitted"]))/2) + 
             sum(Reserves$degreesC[Reserves$type=="Emitted"]), 
           label = "Potential Warming:\nDeveloped Reserves",
           size = 3.5,
           colour = "#303030") +
  geom_segment(aes(x = 1.3, xend = 1.3,
                   y = sum(degreesC[type=="Emitted"])-0.01,
                   yend = 0.01),
               colour = "#303030") +
  geom_segment(aes(x = 1.27, xend = 1.3,
                   y = sum(degreesC[type=="Emitted"])-0.01,
                   yend = sum(degreesC[type=="Emitted"])-0.01),
               colour = "#303030") +
  geom_segment(aes(x = 1.27, xend = 1.3,
                   y = 0.01,
                   yend = 0.01),
               colour = "#303030") +
  annotate("text", x = 1.37, 
           y = sum(Reserves$degreesC[Reserves$type=="Emitted"])/2, 
           label = "Pre-Industrial\nWarming",
           size = 3.5,
           colour = "#303030") +
  scale_fill_manual(name = "",
                    values = rev(colours.5categories),
                    labels = c("Oil", "Gas", "Coal", 
                               "2018-2019", "1880-2015"),
                    guide = guide_legend(label.theme = element_text(size = 9))) +
  scale_x_discrete(name = "",
                   expand = c(0.3, 0)) +
  scale_y_continuous(name = "",
                     limits = c(0, 3),
                     breaks = c(0.5, 1, 1.5, 2, 2.5, 3),
                     labels = expression(0.5*" "*degree*C, 1*" "*degree*C, 1.5*" "*degree*C, 2*" "*degree*C, 2.5*" "*degree*C, 3*" "*degree*C),
                     expand = c(0, 0)) +
  theme(plot.title = element_text(size = 12,
                                  colour = "#303030",
                                  face = "bold"),
        plot.subtitle = element_text(size = 10,
                                     colour = "#303030"),
        axis.ticks.x = element_line(colour = "#303030"),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "#909090"),
        panel.border = element_rect(fill = NA,
                                    size = 0.25,
                                    colour = "#C0C0C0"),
        panel.grid.major.y = element_line(colour = "#909090",
                                          size = 0.35,
                                          linetype = 3),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "pt"),
        axis.title.x = element_text(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 9,
                                 angle = 0,
                                 colour = "#303030",
                                 lineheight = 0.7)) +
  labs(title = "Estimated Global Temperature Rise: Developed Reserves",
       subtitle = "Historic & Projected Pre-Industrial Temperature Rise Attributable to Developed Reserves (2020)")


Reserves.TempRise.Arranged <-
  grid.arrange(Reserves.TempRise.BarPlot, 
               bottom = grid.text(label = "Source: Rystad Energy for developed reserves(oil and gas GtC estimates updated in 2020);\n             2018-2019 GtC emissions from Friedlingstein et al. (2010) Global Carbon Budget 2019. <https://doi.org/10.5194/essd-11-1783-2019>;\n             1880-2015 temperature rise from IPCC (2017) Special Report: Global Warming of 1.5C. <https://www.ipcc.ch/sr15/chapter/chapter-1/>\nNote:     GtC were converted for all reserves data and 2018-2019 GtC emissions using the conversion: 1.75 degrees C = 1000 GtC;\n              Pre-industrial temperature rise is the average modelled trend in temperature between 1880-2015", 
                                  x = unit(33, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 9, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))

# Export gas demand by sector bar plot
png(paste(FigureFileName, "/Reserves.TempRise.png", sep = ""),
    units = "in", height = 9, width = 9, res = 400)
grid.newpage()
grid.draw(Reserves.TempRise.Arranged)
dev.off()

