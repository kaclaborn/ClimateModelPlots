
# code: Country-specific future emissions trajectories, by sector


# ---- Identify which countries (of Top 10) to produce sector-specific plots for ----

sector.plot.list <- c("China", "United States", "EU-27","India")


# ---- Produce historical sector specific emissions plots, by country ----

# KC: Automate assignment of plot name, etc.
# KC: Automate export proess (separately)
China.Emissions.BySector.Historical.Plot <-
  ggplot(GHGTop10.CAIT %>% filter(country.name=="China" & negativeLUCF==0), aes(x = Year, y = value/1000)) +
  geom_area(aes(group = sector, fill = sector),
            position = "stack") +
  geom_line(aes(group = sector),
            colour = "#C0C0C0",
            size = 0.5,
            position = "stack",
            show.legend = F) +
  geom_line(data = GHGTop10.CAIT %>% filter(country.name=="China" & sector=="Energy"), # this is an arbitrary selection of sector -- just filtering to only have 1 row per year
            aes(x = Year, y = net.total/1000),
            linetype = 2,
            size = 1,
            colour = "#C0C0C0") +
  scale_fill_ptol(drop = F) +
  scale_x_continuous(name = "",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "", 
                     expand = c(0, 0),
                     limits = c(0, 15),
                     breaks = seq(3, 12, by = 3),
                     labels = c("3 Gt", "6 Gt", "9 Gt", "12 Gt")) +
  plot.theme.top10 + legend.guide.top10 +
  labs(title = "Historic Annual GHG Emissions: China", 
       subtitle = "Total carbon dioxide emissions, including emissions from fossil fuel combustion, industrial processes, and land-use change (1990 - 2016)")
