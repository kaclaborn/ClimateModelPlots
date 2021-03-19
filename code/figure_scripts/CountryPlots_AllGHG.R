
# code: Country-specific historical and future trajectories (all GHGs)
# 
# ---- sections ----
# 1.  Source Plot Themes, Wrangle Data
# 2.  Historical & Projected Country-Specific Emissions Plots
# 3.  Export


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE PLOT THEMES, WRANGLE DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 1.1 Source PlotThemes.R ----

eval(parse('code/PlotThemes.R', encoding = 'UTF-8'))



# ---- 1.2 Identify which countries (of Top 30) to produce sector-specific plots for ----

# this is only necessary if we automate the plotting process by country (see experiment at bottom of script)
country.allghg.plot.list <- c("China", "United States", "EU-27","India") 



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: HISTORICAL & PROJECTED COUNTRY-SPECIFIC EMISSIONS PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 2.1 Automate production of figures for any given list of countries (from Top 30) ----

# --- All IAMs as separate lines & 'wedge' associated with uncertainty
# --- Timeline between 1950 - 2050, Gutschow data

for(i in country.allghg.plot.list) {
  
  filename <- paste(List.Top30.EU$country[List.Top10.EU$country.name==i], "FutureProjected.AllGHGs.Plot", sep = ".")
  
  # Would need to figure out how to automate the y axis labels and breaks to fully automate these plots (if we want on diff scales)
  
  x <- ggplot(GHGTop30.MultiScenarios.EU %>% filter(year>=1950 & year<=2050 & country.name==i) %>% 
                group_by(year) %>% mutate(min = min(value),
                                          max = max(value)), 
              aes(x = year, y = value/1000000)) +
    geom_line(aes(group = scenario, size = marker, alpha = marker, linetype = marker)) +
    geom_ribbon(aes(ymin = min/1000000, ymax = max/1000000),
                stat = "identity", outline.type = "full", 
                fill = "#303030", alpha = 0.3) +
    geom_vline(aes(xintercept = 2018),
               size = 0.5, 
               colour = "#909090",
               alpha = 0.5) +
    annotate("text", x = 2015, y = 1, label = "2018", size = 2.5, colour = "#909090") +
    scale_x_continuous(name = "",
                       expand = c(0, 0),
                       breaks = seq(1950, 2050, by = 10)) +
    scale_y_continuous(name = "", 
                       expand = c(0, 0),
                       limits = c(0,25),
                       breaks = seq(5, 20, by = 5),
                       labels = c("5 Gt", "10 Gt", "15 Gt", "20 Gt")) +
    scale_linetype_manual(values = c("0" = 5, "1" = 1), 
                          guide = F) +
    scale_size_manual(values = c("0" = 0.3, "1" = 1.2),
                      guide = F) +
    scale_alpha_manual(values = c("0" = 0.4, "1" = 1),
                       guide = F) +
    plot.theme.top10 +
    labs(title = paste("Annual GHG emissions trajectories - ", i, sep = ""), 
         subtitle = "Future projected emissions of Kyoto greenhouse gases (AR4) in CO2e,\nSSP2 baseline scenario with associated integrated assessment models & uncertainty")
  
  assign(filename,
         grid.arrange(x, 
                      bottom = grid.text(label = source.label.gutschow,
                                         x = unit(45, "pt"),
                                         just = "left",
                                         gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
                      ncol = 1,
                      padding = unit(5, "pt"), 
                      vp = viewport(width = 1, height = 0.95)))

  
  # export figure
  png(paste0(FigureFileName, "/", filename, ".png", sep = ""),
      units = "in", height = 6, width = 8, res = 400)
  grid.newpage()
  grid.draw(get(filename))
  dev.off()
  
}


# ***NON AUTOMATED VERSION (FOR CHINA)

# # ---- CHINA ----
# 
# # All IAMs as separate lines & 'wedge
# CHN.GHGEmissions.Future.Plot <- 
#   ggplot(GHGTop30.MultiScenarios.EU %>% filter(year>=1950 & year<=2050 & country.name=="China") %>% 
#            group_by(year) %>% mutate(min = min(value),
#                                      max = max(value)), 
#          aes(x = year, y = value/1000000)) +
#   geom_line(aes(group = scenario, size = marker, alpha = marker, linetype = marker)) +
#   geom_ribbon(aes(ymin = min/1000000, ymax = max/1000000),
#               stat = "identity", outline.type = "full", 
#               fill = "#303030", alpha = 0.3) +
#   geom_vline(aes(xintercept = 2018),
#              size = 0.5, 
#              colour = "#909090",
#              alpha = 0.5) +
#   annotate("text", x = 2015, y = 1, label = "2018", size = 2.5, colour = "#909090") +
#   scale_x_continuous(name = "",
#                      expand = c(0, 0)) +
#   scale_y_continuous(name = "", 
#                      expand = c(0, 0),
#                      limits = c(0,25),
#                      breaks = seq(5, 20, by = 5),
#                      labels = c("5 Gt", "10 Gt", "15 Gt", "20 Gt")) +
#   scale_linetype_manual(values = c("0" = 5, "1" = 1), 
#                         guide = F) +
#   scale_size_manual(values = c("0" = 0.3, "1" = 1.2),
#                     guide = F) +
#   scale_alpha_manual(values = c("0" = 0.4, "1" = 1),
#                      guide = F) +
#   plot.theme.top10 +
#   labs(title = "Annual GHG emissions trajectories - China", 
#        subtitle = "Future projected emissions of Kyoto greenhouse gases (AR4) in CO2e,\nSSP2 baseline scenario with associated integrated assessment models & uncertainty")
# 
# CHN.GHGEmissions.Future.Arranged <- 
#   grid.arrange(CHN.GHGEmissions.Future.Plot, 
#                bottom = grid.text(label = source.label.gutschow,
#                                   x = unit(45, "pt"),
#                                   just = "left",
#                                   gp = gpar(fontsize = 8, lineheight = 1, col = "#303030")),
#                ncol = 1,
#                padding = unit(5, "pt"), 
#                vp = viewport(width = 1, height = 0.95))


