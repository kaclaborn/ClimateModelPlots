
# Treemap & donut chart of current emissions by sector (globally)

# ---- CAIT data for treemap ----

CAIT.ForTreemap <- CAIT.current.sector.data %>%
  filter(Entity!="World" & Year==2016 & !Code%in%EU27.list) %>%
  rename("country" = "Code",
         "country.name" = "Entity",
         "Agriculture" = "Agriculture (GHG Emissions, CAIT)",
         "International Bunkers" = "Bunker Fuels (GHG Emissions, CAIT)",
         "Industry" = "Industry (GHG Emissions, CAIT)", 
         "Waste" = "Waste (GHG Emissions, CAIT)",
         "Buildings" = "Buildings (GHG Emissions, CAIT)",
         "Land-Use Change and Forestry" = "Land-Use Change and Forestry (GHG Emissions, CAIT)") %>%
  mutate(Energy = rowSums(.[,c("Electricity & Heat (GHG Emissions, CAIT)",
                               "Manufacturing/Construction energy (GHG Emissions, CAIT)",
                               "Transport (GHG Emissions, CAIT)",
                               "Other Fuel Combustion (GHG Emissions, CAIT)",
                               "Fugitive from energy production (GHG Emissions, CAIT)")],
                          na.rm = T),
         AFOLU = rowSums(.[,c("Agriculture",
                              "Land-Use Change and Forestry")],
                         na.rm = T),
         Other = rowSums(.[,c("Buildings",
                              "Waste",
                              "International Bunkers")],
                         na.rm = T),
         country.group = ifelse(country.name=="China", 1, 
                                ifelse(country.name=="United States", 2,
                                       ifelse(country.name=="European Union (27)", 3,
                                              ifelse(country.name=="India", 4, 5))))) %>%
  pivot_longer(c("International Bunkers", "Buildings", "Waste", "Agriculture", 
                 "Land-Use Change and Forestry", "Industry", "Energy",
                 "AFOLU", "Other"), # Energy, Industry, AFOLU, and Other are currently mutually exclusive and exhaustive.
               names_to = "sector") %>%
  group_by(country.group, Year, sector) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  filter(sector%in%c("Energy", "Industry", "AFOLU", "Other")) %>%
  mutate(country = ifelse(country.group==1, "China", 
                          ifelse(country.group==2, "United States", 
                                 ifelse(country.group==3, "EU-27",
                                        ifelse(country.group==4, "India", "Rest of World")))),
         country = factor(country, levels = c("China", "United States", "EU-27", "India", "Rest of World"),
                          ordered = T),
         value = ifelse(value<0 & !is.na(value), 0, 
                        ifelse(value>=0 & !is.na(value), value, 
                               NA)),
         country.sector = paste(country, sector, sep = "."),
         sector = factor(sector, levels = c("Energy", "Industry", "AFOLU", "Other"), ordered = T)) %>%
  .[order(.$country, .$sector),] %>%
  mutate(country.sector = factor(country.sector, levels = c("China.Energy", "China.Industry", "China.AFOLU", "China.Other",
                                                            "United States.Energy", "United States.Industry", "United States.AFOLU", "United States.Other",
                                                            "EU-27.Energy", "EU-27.Industry", "EU-27.AFOLU", "EU-27.Other",
                                                            "India.Energy", "India.Industry", "India.AFOLU", "India.Other",
                                                            "Rest of World.Energy", "Rest of World.Industry", "Rest of World.AFOLU", "Rest of World.Other"),
                                 ordered = T),
         fill.cols = c("#332288", "#5B4E9F", "#847AB7", "#ADA6CF",
                       "#88CCEE", "#9FD6F1", "#B7E0F4", "#DBEFF9",
                       "#117733", "#40925B", "#70AD84", "#9FC8AD",
                       "#DDCC77", "#E3D692", "#EEE5BB", "#F4EFD6",
                       "#CC6677", "#D68492", "#E0A3AD", "#EAC1C8"),
         text.cols = c("white", "white", "white", "#707070",
                       "white", "#909090", "#909090", "#909090",
                       "white", "white", "white", "#909090",
                       "white", "#909090", "#909090", "#909090",
                       "white", "white", "white", "#909090"),
         percent.val = value / sum(value),
         cumulative.val = cumsum(percent.val),
         sector.value = paste(sector, paste(round(percent.val * 100, 1), "%", sep = ""), sep = ": "),
         ymin = c(0, head(cumulative.val, n=-1)))

# treemap using treemapify package (which piggybacks on ggplot)
Treemap.CurrentEmissions.BySector <-
  CAIT.ForTreemap %>% 
  ggplot(aes(area = value, fill = country.sector, label = sector.value,
             subgroup = country, subgroup2 = sector)) +
  geom_treemap(layout = "srow",
               show.legend = F) +
  geom_treemap_subgroup_border(colour = "white",
                               size = 6, 
                               layout = "srow",
                               show.legend = F) +
  geom_treemap_text(aes(colour = country.sector), 
                    size = 8, layout = "srow",
                    place = "topleft", reflow = T, grow = F, min.size = 2,
                    padding.x = unit(2, "mm"), padding.y = unit(2, "mm"),
                    show.legend = F) +
  geom_treemap_subgroup_text(size = 18, layout = "srow", colour = "white", fontface = "bold",
                             place = "center", grow = F, show.legend = F) +
  scale_fill_manual(values = CAIT.ForTreemap$fill.cols) +
  scale_colour_manual(values = CAIT.ForTreemap$text.cols) +
  labs(title = " Global Share of GHG Emissions",
       subtitle = " Sectoral emissions (Gt CO2e) from fossil fuel combustion, industrial processes, and land-use change (2016)") + plot.theme.donut
  
Treemap.CurrentEmissions.BySector.Arranged <- 
  grid.arrange(Treemap.CurrentEmissions.BySector, 
               bottom = grid.text(label = source.label.caittreemap, 
                                  x = unit(8, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))

# EXPORT TREEMAP
png("figures/outputs/test.treemap.png",
    units = "in", height = 9, width = 9, res = 400)
grid.newpage()
grid.draw(Treemap.CurrentEmissions.BySector.Arranged)
dev.off()



# test donut chart using ggplot

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
        panel.grid = element_blank()
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "pt"),
        axis.title = element_text(size = rel(0.9),
                                  angle = 0,
                                  face = "bold",
                                  colour = "#303030"),
        axis.text = element_blank(),
        legend.position = "right",
        legend.box.spacing = unit(0.1, "cm"))

Donut.CurrentEmissions.BySector <-
  ggplot() +
  geom_rect(CAIT.ForTreemap, 
            mapping = aes(ymax = cumulative.val, ymin = ymin, xmax = 4, xmin = 3,
                fill = country, alpha = sector),
            colour = "white",
            show.legend = F) +
  geom_segment(CAIT.ForTreemap %>%
                 group_by(country) %>%
                 summarise(y = ymin[sector=="Energy"],
                           yend = cumulative.val[sector=="Other"]),
               mapping = aes(x = 4.2, xend = 4.2, y = y + 0.01, yend = yend - 0.01,
                             colour = country),
               size = 1.5,
               show.legend = F) +
  geom_text(CAIT.ForTreemap %>%
              mutate(total.val = sum(value)) %>%
              group_by(country) %>%
              summarise(y = ymin[sector=="Energy"],
                        yend = cumulative.val[sector=="Other"],
                        total.share = round((sum(value)/total.val)*100, 2)) %>%
              ungroup() %>%
              mutate(ymid = (y+yend)/2,
                     total.share = paste(total.share, "%", sep = ""),
                     label.x = c(5, 5.5, 5, 4.7, 5.5)), 
            mapping = aes(x = label.x, y = ymid, 
                          label = paste(country, total.share, sep = "\n"),
                          colour = country),
            show.legend = F) +
  scale_colour_manual(values = colours.5categories) +
  scale_fill_manual(values = colours.5categories) +
  scale_alpha_manual(values = c("Energy" = 1, "Industry" = 0.8, "AFOLU" = 0.6, "Other" = 0.4),
                     guide = F) +
  coord_polar(theta="y") +
  xlim(c(2, 6)) +
  theme_void()


# ---- DONUT PLOT EXPORT ----
png("test.png",
    units = "in", height = 6, width = 9.2, res = 400)
grid.newpage()
grid.draw(Donut.CurrentEmissions.BySector)
dev.off()
