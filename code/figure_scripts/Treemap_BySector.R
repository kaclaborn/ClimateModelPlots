#
# code: Treemap by sector, using GCAM data
# 
# ---- sections ----
# 1.  Source plot themes, wrangle data
# 2.  Create treemap
# 3.  Export
# 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE PLOT THEMES, WRANGLE DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 1.1 Source plot themes ----

eval(parse('code/PlotThemes.R', encoding = 'UTF-8'))


# ---- 1.2 Wrangle GCAM data for global treemap of emissions by sector ----

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


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: CREATE TREEMAP ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Define treemap using treemapify package (which piggybacks on ggplot) ----

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
                    size = 11, 
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
  

# ---- 2.2 Create fully arranged treemap with associated text and formatting ----

Treemap.CurrentEmissions.BySector.Arranged <- 
  grid.arrange(Treemap.CurrentEmissions.BySector, 
               bottom = grid.text(label = source.label.gcamtreemap, 
                                  x = unit(8, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 11, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: EXPORT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


png(paste(FigureFileName, "/treemap.bysector.png", sep = ""),
    units = "in", height = 9, width = 9, res = 400)
grid.newpage()
grid.draw(Treemap.CurrentEmissions.BySector.Arranged)
dev.off()


