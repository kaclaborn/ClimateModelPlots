# 
# code: Create Top 30 plots for GHG emissions
# 
# ---- sections ----
# 1.  Source Plot Themes, Wrangle Data
# 2.  Top 30 Current Emissions Bar Plot
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


# ---- 1.1 Source PlotThemes.R ----

eval(parse('code/PlotThemes.R', encoding = 'UTF-8'))


# ---- 1.2 Create data frame that identifies proportion of global GHGs for top 10 countries, grouped ----

Grouped.GHG.Proportions <- 
  GHGTop30.EU %>%
  filter(year==2018) %>%
  mutate(group = ifelse(country %in% c("CHN", "USA"), 1, 
                        ifelse(country %in% c("EU27", "IND", "RUS"), 2,
                               ifelse(country %in% c("JPN", "BRA", "IDN"), 3, 4)))) %>%
  filter(group<4) %>%
  group_by(group) %>%
  summarise(grouped.GHGprop = paste(round(sum(propGHG), 4)*100, "%", sep = "")) %>% 
  mutate(xmin = c(0.5,2.5,5.5), # add xmin and xmax variables to identify where to draw the geom_rect in section 2 below
         xmax = c(2.45, 5.45, 8.45))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: TOP 30 CURRENT EMISSIONS BAR PLOT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Current Emissions - Top 30 emitters (2018 as reference to determine top emitters) ----

Top30Emissions.Bar.Plot <-
  ggplot() +
  geom_rect(data = Grouped.GHG.Proportions, 
            aes(ymin = 0, 
                ymax = c(15, (15*0.43), (15*0.17)), 
                xmin = xmin, 
                xmax = xmax),
            alpha = 0.25,
            fill = "#56ACAD") +
  geom_text(data = Grouped.GHG.Proportions, 
            aes(x = c(1.5, 4, 7), 
                y = c(14.7, (15*0.43)-0.3, (15*0.17)-0.3), 
                label = grouped.GHGprop),
            size = 2.25,
            colour = "#606060",
            fontface = "bold") +
  annotate("text", x = 4.5, y = 15.4, label = toupper("Share of global emissions"), 
           size = 2.4,
           colour = "#909090") +
  geom_bar(data = GHGTop30.EU %>% filter(year==2018),
           aes(x = country.name, y = value/1000000),
           fill = "#56ACAD",
           stat = "identity",
           width = 0.7) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0, 17),
                     breaks = seq(2.5, 15, by = 2.5),
                     labels = c("2.5 Gt", "5 Gt", "7.5 Gt", "10 Gt", "12.5 Gt", "15 Gt")) +
  plot.theme.top30 + legend.guide.top10 +
  labs(title = "Current GHG Emissions: Top 30 Current Emitters", 
       subtitle = "Emissions (Gt CO2e) of Kyoto greenhouse gases (AR4) for thirty countries with highest present-day emissions (2018)")

Top30Emissions.Bar.Arranged <- 
  grid.arrange(Top30Emissions.Bar.Plot, 
               bottom = grid.text(label = source.label.gutschow, 
                                  x = unit(45, "pt"),
                                  just = "left",
                                  gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
               ncol = 1,
               padding = unit(5, "pt"), 
               vp = viewport(width = 1, height = 0.95))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3 : EXPORT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- Top 30 bar plot ----

png(paste(FigureFileName, "/Top30.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(Top30Emissions.Bar.Arranged)
dev.off()
