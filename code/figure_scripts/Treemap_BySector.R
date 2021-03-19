# 
# code: Treemap & donut chart of current emissions by sector (globally)
# 
# 
# OPTION 1: CAIT DATA
# 
# ---- Wrangle CAIT data for treemap ----

CAIT.ForTreemap <- CAIT.current.sector.data %>%
  left_join(country.labels, by = "country.name") %>%
  filter(country.name!="World" & !country%in%EU27.list) %>%
  mutate(Sector = recode(Sector, `Electricity/Heat` = "Power & Heat"),
         Sector = recode(Sector, Transportation = "Transport"),
         Sector = recode(Sector, `Industrial Processes` = "Industry"),
         Sector = recode(Sector, `Manufacturing/Construction` = "Industry"),
         Sector = recode(Sector, Agriculture = "AFOLU"),
         Sector = recode(Sector, `Land-Use Change and Forestry` = "AFOLU"),
         Sector = recode(Sector, Building = "Other"),
         Sector = recode(Sector, Waste = "Other"),
         Sector = recode(Sector, `Bunker Fuels` = "Other"),
         Sector = recode(Sector, `Other Fuel Combustion` = "Other"),
         Sector = recode(Sector, `Fugitive Emissions` = "Other")) %>%
  rename("sector" = "Sector") %>%
  mutate(across(starts_with("20") | starts_with("19"), as.numeric)) %>%
  pivot_longer(cols = starts_with("20") | starts_with("19"), names_to = "Year") %>%
  mutate(country.group = ifelse(country.name=="China", 1, 
                                ifelse(country.name=="United States", 2,
                                       ifelse(country.name=="EU-27", 3,
                                              ifelse(country.name=="India", 4, 5))))) %>%
  group_by(country.group, Year, sector) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  group_by(Year, country.group) %>%
  mutate(country.perc = round(sum(value)/total * 100, 1)) %>%
  ungroup() %>%
  filter(Year=="2017") %>%
  mutate(country.name = ifelse(country.group==1, "China", 
                          ifelse(country.group==2, "United States", 
                                 ifelse(country.group==3, "EU-27",
                                        ifelse(country.group==4, "India", "Rest of World")))),
         country = paste0(country.name, " (", country.perc, "%)", sep = ""),
         value = ifelse(value<0 & !is.na(value), 0, 
                        ifelse(value>=0 & !is.na(value), value, 
                               NA)),
         country.sector = paste(country.name, sector, sep = "."),
         sector = factor(sector, levels = c("Power & Heat", "Industry", "Transport", "AFOLU", "Other"), ordered = T)) %>%
  .[order(.$country.name, .$sector),] %>%
  mutate(country.sector = factor(country.sector, levels = c("China.Power & Heat", "China.Industry", "China.Transport", "China.AFOLU", "China.Other",
                                                            "United States.Power & Heat", "United States.Industry", "United States.Transport", "United States.AFOLU", "United States.Other",
                                                            "EU-27.Power & Heat", "EU-27.Industry", "EU-27.Transport", "EU-27.AFOLU", "EU-27.Other",
                                                            "India.Power & Heat", "India.Industry", "India.Transport", "India.AFOLU", "India.Other",
                                                            "Rest of World.Power & Heat", "Rest of World.Industry", "Rest of World.Transport", "Rest of World.AFOLU", "Rest of World.Other"),
                                 ordered = T),
         fill.cols = c("#332288", "#6053a2", "#8d84bc", "#a49cca", "#d1cde4",
                       "#5bb9e8", "#88ccee", "#a2d7f1", "#bce2f5", "#d7eef9", 
                       "#117733", "#459560", "#7ab38d", "#95c2a4", "#cae0d1",
                       "#d3bd4e", "#ddcc77", "#e4d795", "#ece2b3", "#f3eed1",
                       "#cc6677", "#d78895", "#e2aab3", "#e8bbc2", "#f3dde0"),
         text.cols = c("white", "white", "white", "white", "#909090",
                       "white", "white", "#909090", "#909090", "#909090",
                       "white", "white", "white", "white", "#909090",
                       "white", "white", "#909090", "#909090", "#909090",
                       "white", "white", "white", "white", "#909090"),
         percent.val = value / sum(value),
         cumulative.val = cumsum(percent.val),
         sector.value = paste(sector, paste(round(percent.val * 100, 1), "%", sep = ""), sep = ": "),
         ymin = c(0, head(cumulative.val, n=-1)))


# # *** This version of dataset has "Industry (Energy & Process Emissions)" all in label
# CAIT.ForTreemap <- CAIT.current.sector.data %>%
#   left_join(country.labels, by = "country.name") %>%
#   filter(country.name!="World" & !country%in%EU27.list) %>%
#   mutate(Sector = recode(Sector, `Electricity/Heat` = "Power & Heat"),
#          Sector = recode(Sector, Transportation = "Transport"),
#          Sector = recode(Sector, `Industrial Processes` = "Industry"),
#          Sector = recode(Sector, `Manufacturing/Construction` = "Industry"),
#          Sector = recode(Sector, Agriculture = "AFOLU"),
#          Sector = recode(Sector, `Land-Use Change and Forestry` = "AFOLU"),
#          Sector = recode(Sector, Building = "Other"),
#          Sector = recode(Sector, Waste = "Other"),
#          Sector = recode(Sector, `Bunker Fuels` = "Other"),
#          Sector = recode(Sector, `Other Fuel Combustion` = "Other"),
#          Sector = recode(Sector, `Fugitive Emissions` = "Other")) %>%
#   rename("sector" = "Sector") %>%
#   mutate(across(starts_with("20") | starts_with("19"), as.numeric)) %>%
#   pivot_longer(cols = starts_with("20") | starts_with("19"), names_to = "Year") %>%
#   mutate(country.group = ifelse(country.name=="China", 1, 
#                                 ifelse(country.name=="United States", 2,
#                                        ifelse(country.name=="EU-27", 3,
#                                               ifelse(country.name=="India", 4, 5))))) %>%
#   group_by(country.group, Year, sector) %>%
#   summarise(value = sum(value, na.rm = T)) %>%
#   ungroup() %>%
#   group_by(Year) %>%
#   mutate(total = sum(value)) %>%
#   ungroup() %>%
#   group_by(Year, country.group) %>%
#   mutate(country.perc = round(sum(value)/total * 100, 1)) %>%
#   ungroup() %>%
#   filter(Year=="2017") %>%
#   mutate(country.name = ifelse(country.group==1, "China", 
#                                ifelse(country.group==2, "United States", 
#                                       ifelse(country.group==3, "EU-27",
#                                              ifelse(country.group==4, "India", "Rest of World")))),sector = recode(sector, Industry = "Industry (Energy & Process Emissions)"),
#          country = paste0(country.name, " (", country.perc, "%)", sep = ""),
#          value = ifelse(value<0 & !is.na(value), 0,
#                         ifelse(value>=0 & !is.na(value), value,
#                                NA)),
#          country.sector = paste(country.name, sector, sep = "."),
#          sector = factor(sector, levels = c("Power & Heat", "Industry (Energy & Process Emissions)", "Transport", "AFOLU", "Other"), ordered = T)) %>%
#   .[order(.$country.name, .$sector),] %>%
#   mutate(country.sector = factor(country.sector, levels = c("China.Power & Heat", "China.Industry (Energy & Process Emissions)", "China.Transport", "China.AFOLU", "China.Other",
#                                                             "United States.Power & Heat", "United States.Industry (Energy & Process Emissions)", "United States.Transport", "United States.AFOLU", "United States.Other",
#                                                             "EU-27.Power & Heat", "EU-27.Industry (Energy & Process Emissions)", "EU-27.Transport", "EU-27.AFOLU", "EU-27.Other",
#                                                             "India.Power & Heat", "India.Industry (Energy & Process Emissions)", "India.Transport", "India.AFOLU", "India.Other",
#                                                             "Rest of World.Power & Heat", "Rest of World.Industry (Energy & Process Emissions)", "Rest of World.Transport", "Rest of World.AFOLU", "Rest of World.Other"),
#                                  ordered = T),
#          fill.cols = c("#332288", "#6053a2", "#8d84bc", "#a49cca", "#d1cde4",
#                        "#5bb9e8", "#88ccee", "#a2d7f1", "#bce2f5", "#d7eef9",
#                        "#117733", "#459560", "#7ab38d", "#95c2a4", "#cae0d1",
#                        "#d3bd4e", "#ddcc77", "#e4d795", "#ece2b3", "#f3eed1",
#                        "#cc6677", "#d78895", "#e2aab3", "#e8bbc2", "#f3dde0"),
#          text.cols = c("white", "white", "white", "white", "#909090",
#                        "white", "white", "#909090", "#909090", "#909090",
#                        "white", "white", "white", "white", "#909090",
#                        "white", "white", "#909090", "#909090", "#909090",
#                        "white", "white", "white", "white", "#909090"),
#          percent.val = value / sum(value),
#          cumulative.val = cumsum(percent.val),
#          sector.value = paste(sector, paste(round(percent.val * 100, 1), "%", sep = ""), sep = ": "),
#          ymin = c(0, head(cumulative.val, n=-1)))



# OPTION 2: GCAM DATA

# ---- Wrangle GCAM data for treemap ----

# # *** This version of dataset has "Industry (Energy & Process Emissions)" all in label
# GCAM.ForTreemap <- GHGTop10.GCAM %>%
#   filter(Year==2020 & country.name%in%c("China", "United States",
#                                   "EU-28", "India", "World")) %>%
#   group_by(Year, sector) %>%
#   mutate(value = ifelse(country.name=="World",
#                                 value - sum(value[country.name%in%c("China", "United States",
#                                                                                     "EU-28", "India")]),
#                                 value)) %>%
#   ungroup() %>%
#   group_by(Year) %>%
#   mutate(total = sum(value)) %>%
#   ungroup() %>%
#   group_by(Year, country.name) %>%
#   mutate(country.perc = round(sum(value)/total * 100, 1)) %>%
#   ungroup() %>%
#   mutate(country.name = recode(country.name, World = "Rest of World"),
#          sector = recode(sector, Industry = "Industry (Energy & Process Emissions)"),
#          country = paste0(country.name, " (", country.perc, "%)", sep = ""),
#          value = ifelse(value<0 & !is.na(value), 0,
#                         ifelse(value>=0 & !is.na(value), value,
#                                NA)),
#          country.sector = paste(country.name, sector, sep = "."),
#          sector = factor(sector, levels = c("Power & Heat", "Industry (Energy & Process Emissions)", "Transport", "AFOLU", "Other"), ordered = T)) %>%
#   .[order(.$country.name, .$sector),] %>%
#   mutate(country.sector = factor(country.sector, levels = c("China.Power & Heat", "China.Industry (Energy & Process Emissions)", "China.Transport", "China.AFOLU", "China.Other",
#                                                             "United States.Power & Heat", "United States.Industry (Energy & Process Emissions)", "United States.Transport", "United States.AFOLU", "United States.Other",
#                                                             "EU-28.Power & Heat", "EU-28.Industry (Energy & Process Emissions)", "EU-28.Transport", "EU-28.AFOLU", "EU-28.Other",
#                                                             "India.Power & Heat", "India.Industry (Energy & Process Emissions)", "India.Transport", "India.AFOLU", "India.Other",
#                                                             "Rest of World.Power & Heat", "Rest of World.Industry (Energy & Process Emissions)", "Rest of World.Transport", "Rest of World.AFOLU", "Rest of World.Other"),
#                                  ordered = T),
#          fill.cols = c("#332288", "#6053a2", "#8d84bc", "#a49cca", "#d1cde4",
#                        "#5bb9e8", "#88ccee", "#a2d7f1", "#bce2f5", "#d7eef9",
#                        "#117733", "#459560", "#7ab38d", "#95c2a4", "#cae0d1",
#                        "#d3bd4e", "#ddcc77", "#e4d795", "#ece2b3", "#f3eed1",
#                        "#cc6677", "#d78895", "#e2aab3", "#e8bbc2", "#f3dde0"),
#          text.cols = c("white", "white", "white", "white", "#909090",
#                        "white", "white", "#909090", "#909090", "#909090",
#                        "white", "white", "white", "white", "#909090",
#                        "white", "white", "#909090", "#909090", "#909090",
#                        "white", "white", "white", "white", "#909090"),
#          percent.val = value / sum(value),
#          cumulative.val = cumsum(percent.val),
#          sector.value = paste(sector, paste(round(percent.val * 100, 1), "%", sep = ""), sep = ": "),
#          ymin = c(0, head(cumulative.val, n=-1)))
# 
# Treemap.CurrentEmissions.BySector <-
#   GCAM.ForTreemap %>%
#   ggplot(aes(area = value, fill = country.sector, label = sector.value,
#              subgroup = country, subgroup2 = sector)) +
#   geom_treemap(show.legend = F) +
#   geom_treemap_subgroup_border(colour = "white",
#                                size = 6,
#                                show.legend = F) +
#   geom_treemap_text(aes(colour = country.sector),
#                     size = 8,
#                     place = "topleft", reflow = T, grow = F, min.size = 2,
#                     padding.x = unit(2, "mm"), padding.y = unit(2, "mm"),
#                     show.legend = F) +
#   geom_treemap_subgroup_text(size = 20, colour = "white", fontface = "bold",
#                              place = "bottomleft", grow = F,
#                              padding.x = unit(3, "mm"), padding.y = unit(4, "mm"),
#                              show.legend = F) +
#   scale_fill_manual(values = GCAM.ForTreemap$fill.cols) +
#   scale_colour_manual(values = GCAM.ForTreemap$text.cols) +
#   labs(title = " Global Share of GHG Emissions",
#        subtitle = " Sectoral emissions (CO2e) from fossil fuel combustion, industrial processes, and land-use change (2020)") + plot.theme.treemap
# 
# Treemap.CurrentEmissions.BySector.Arranged <-
#   grid.arrange(Treemap.CurrentEmissions.BySector,
#                bottom = grid.text(label = source.label.gcamtreemap,
#                                   x = unit(8, "pt"),
#                                   just = "left",
#                                   gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
#                ncol = 1,
#                padding = unit(5, "pt"),
#                vp = viewport(width = 1, height = 0.95))
# 
# png(paste(FigureFileName, "/treemap.industrylabels.inplot.png", sep = ""),
#     units = "in", height = 9, width = 9, res = 400)
# grid.newpage()
# grid.draw(Treemap.CurrentEmissions.BySector.Arranged)
# dev.off()


# Wrangle 
GCAM.ForTreemap <- GHGTop10.GCAM %>%
  filter(Year==2020 & country.name%in%c("China", "United States", 
                                        "EU-28", "India", "World")) %>%
  group_by(Year, sector) %>%
  mutate(value = ifelse(country.name=="World", 
                                value - sum(value[country.name%in%c("China", "United States", 
                                                                                    "EU-28", "India")]),
                                value)) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  group_by(Year, country.name) %>%
  mutate(country.perc = round(sum(value)/total * 100, 1)) %>%
  ungroup() %>%
  mutate(country.name = recode(country.name, World = "Rest of World"),
         country = paste0(country.name, " (", country.perc, "%)", sep = ""),
         value = ifelse(value<0 & !is.na(value), 0, 
                        ifelse(value>=0 & !is.na(value), value, 
                               NA)),
         country.sector = paste(country.name, sector, sep = "."),
         sector = factor(sector, levels = c("Power & Heat", "Industry", "Transport", "AFOLU", "Other"), ordered = T)) %>%
  .[order(.$country.name, .$sector),] %>%
  mutate(country.sector = factor(country.sector, levels = c("China.Power & Heat", "China.Industry", "China.Transport", "China.AFOLU", "China.Other",
                                                            "United States.Power & Heat", "United States.Industry", "United States.Transport", "United States.AFOLU", "United States.Other",
                                                            "EU-28.Power & Heat", "EU-28.Industry", "EU-28.Transport", "EU-28.AFOLU", "EU-28.Other",
                                                            "India.Power & Heat", "India.Industry", "India.Transport", "India.AFOLU", "India.Other",
                                                            "Rest of World.Power & Heat", "Rest of World.Industry", "Rest of World.Transport", "Rest of World.AFOLU", "Rest of World.Other"),
                                 ordered = T),
         fill.cols = c("#332288", "#6053a2", "#8d84bc", "#a49cca", "#d1cde4",
                       "#5bb9e8", "#88ccee", "#a2d7f1", "#bce2f5", "#d7eef9", 
                       "#117733", "#459560", "#7ab38d", "#95c2a4", "#cae0d1",
                       "#d3bd4e", "#ddcc77", "#e4d795", "#ece2b3", "#f3eed1",
                       "#cc6677", "#d78895", "#e2aab3", "#e8bbc2", "#f3dde0"),
         text.cols = c("white", "white", "white", "white", "#909090",
                       "white", "white", "#909090", "#909090", "#909090",
                       "white", "white", "white", "white", "#909090",
                       "white", "white", "#909090", "#909090", "#909090",
                       "white", "white", "white", "white", "#909090"),
         percent.val = value / sum(value),
         cumulative.val = cumsum(percent.val),
         sector.value = paste(sector, paste(round(percent.val * 100, 1), "%", sep = ""), sep = ": "),
         ymin = c(0, head(cumulative.val, n=-1)))


# ---- Define treemap using treemapify package (which piggybacks on ggplot) ----

Treemap.CurrentEmissions.BySector <-
  GCAM.ForTreemap %>% 
  ggplot(aes(area = value, fill = country.sector, label = sector.value,
             subgroup = country, subgroup2 = sector)) +
  geom_treemap(layout = "srow",
               show.legend = F) +
  geom_treemap_subgroup_border(layout = "srow",
                               colour = "white",
                               size = 6, 
                               show.legend = F) +
  geom_treemap_text(layout = "srow",
                    aes(colour = country.sector), 
                    size = 8, 
                    place = "topleft", reflow = T, grow = F, min.size = 2,
                    padding.x = unit(2, "mm"), padding.y = unit(2, "mm"),
                    show.legend = F) +
  geom_treemap_subgroup_text(layout = "srow",
                             size = 20, colour = "white", fontface = "bold",
                             place = "bottomleft", grow = F, 
                             padding.x = unit(3, "mm"), padding.y = unit(4, "mm"),
                             show.legend = F) +
  scale_fill_manual(values = GCAM.ForTreemap$fill.cols) +
  scale_colour_manual(values = GCAM.ForTreemap$text.cols) +
  labs(title = " Global Share of GHG Emissions",
       subtitle = " Modelled sectoral emissions (CO2e) from fossil fuel combustion, industrial processes, and land-use change (2020)") + plot.theme.treemap
  
Treemap.CurrentEmissions.BySector.Arranged <- 
  grid.arrange(Treemap.CurrentEmissions.BySector, 
               bottom = grid.text(label = source.label.gcamtreemap1, 
                                  x = unit(8, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))


# ---- EXPORT TREEMAP ---- 

png(paste(FigureFileName, "/treemap.industrylabels.atbottom.png", sep = ""),
    units = "in", height = 9, width = 9, res = 400)
grid.newpage()
grid.draw(Treemap.CurrentEmissions.BySector.Arranged)
dev.off()



# ---- Test donut chart using ggplot ----

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

png("figures/output/test.donut.png",
    units = "in", height = 6, width = 9.2, res = 400)
grid.newpage()
grid.draw(Donut.CurrentEmissions.BySector)
dev.off()
