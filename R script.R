library(rnaturalearth)
library(sf)
library(mapview)
library(tmap)
library(ggplot2)
library(viridis)
library(coronavirus)
library(dplyr)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)


data("covid19_vaccine")

head(covid19_vaccine)

plot_covid19_vaccine <- function(data, country) {
  data %>%
    filter(country_region == country) %>%
    select(date, doses_admin, total = people_at_least_one_dose) %>%
    arrange(date) %>%
    plot_ly() %>%
    add_lines(x = ~ date, y = ~ doses_admin, name = "Total Doses") %>%
    add_lines(x = ~ date, y = ~ total, name = "Received Vaccine with One/Two Doses") %>%
    layout(title = paste0(country, " Cumulative Number of Doses by Type"),
           legend = list(x = 0.05, y = 0.95),
           margin = list(l = 50, r = 50, b = 50, t = 90),
           yaxis = list(title = "Number of Doses"),
           xaxis = list(title = ""))
}

plot_covid19_vaccine(covid19_vaccine, "Nigeria")
plot_covid19_vaccine(covid19_vaccine, "United Kingdom")
plot_covid19_vaccine(covid19_vaccine, "US")


#case summary 
total_cases <- coronavirus %>%
  filter(type != "recovery") %>%
  group_by(type) %>%
  summarise(cases = sum(cases)) %>%
  mutate(type = factor(type, levels = c("confirmed", "death"))) 

total_cases

coronavirus %>%
  filter(type != "recovery") %>%
  group_by(type, continent_name) %>%
  summarise(cases = sum(cases), .groups = "drop") %>%
  mutate(type = factor(type, levels = c("confirmed", "death"))) %>%
  pivot_wider(names_from = type, values_from = cases) %>%
  mutate(death_rate = death / confirmed) %>%
  filter(!is.na(continent_name)) %>%
  arrange(-death_rate) %>%
  datatable(rownames = FALSE,
            colnames = c("Continent", "Confrimed Cases", "Death Cases","Death Rate %")) %>%
  formatPercentage("death_rate", 2)

round(100 * total_cases$cases[2] / total_cases$cases[1], 2)


#world wide aggregrate
df <- coronavirus %>%
  filter(type != "recovery") %>%
  group_by(date,type) %>%
  summarise(total = sum(cases), .groups = "drop")

p_1 <- plot_ly(data = df %>% filter(type == "confirmed"),
               x = ~ date,
               y = ~ total,
               name = "Confirmed",
               type = "scatter",
               mode = "line") %>%
  layout(yaxis = list(title = "Cases"),
         xaxis = list(title = ""))

p_2 <- plot_ly(data = df %>% filter(type == "death"),
               x = ~ date,
               y = ~ total,
               name = "Death",
               line = list(color = "red"),
               type = "scatter",
               mode = "line") %>%
  layout(yaxis = list(title = "Cases"),
         xaxis = list(title = "Source: Johns Hopkins University Center for Systems Science and Engineering"))

subplot(p_1, p_2, nrows = 2, 
        titleX = TRUE,
        titleY = TRUE) %>%
  layout(title = "Worldwide - Daily Confirmed and Death Cases",
         margin = list(t = 60, b = 60, l = 40, r = 40),
         legend = list(x = 0.05, y = 1)
  )
#top affected countries
confirmed_country <- coronavirus %>% 
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  mutate(perc = total_cases / sum(total_cases)) %>%
  arrange(-total_cases)

confirmed_country %>%
  head(10) %>%
  datatable(rownames = FALSE,
            colnames = c("Country", "Cases", "Perc of Total")) %>%
  formatPercentage("perc", 2)


conf_df <- coronavirus %>% 
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  mutate(parents = "Confirmed") %>%
  ungroup() 

plot_ly(data = conf_df,
        type= "treemap",
        values = ~total_cases,
        labels= ~ country,
        parents=  ~parents,
        domain = list(column=0),
        name = "Confirmed",
        textinfo="label+value+percent parent")
#death rates
coronavirus %>% 
  filter(country != "Others") %>%
  group_by(country, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(- confirmed) %>%
  mutate(death_rate = death / confirmed)  %>%
  datatable(rownames = FALSE,
            colnames = c("Country", "Confirmed","Death", "Death Rate")) %>%
  formatPercentage("death_rate", 2) 

#covid-19 vaccine data
data("covid19_vaccine")

head(covid19_vaccine)

# country level summary
max(covid19_vaccine$date)


df_summary <- covid19_vaccine |>
  filter(date == max(date)) |>
  select(date, country_region, doses_admin, total = people_at_least_one_dose, population, continent_name) |>
  mutate(doses_pop_ratio = doses_admin / population,
         total_pop_ratio = total / population) |>
  arrange(- total)

head(df_summary, 10)


df_summary |> 
  filter(country_region != "World", 
         !is.na(population),
         !is.na(total)) |>
  plot_ly(x = ~ population,
          y = ~ total,
          text = ~ paste("Country: ", country_region, "<br>",
                         "Population: ", population, "<br>",
                         "Total Doses: ", total, "<br>",
                         "Ratio: ", round(total_pop_ratio, 2), 
                         sep = ""),
          type = "scatter",
          mode = "markers") |>
  layout(title = "Total Doses vs. Population",
         margin = list(l = 50, r = 50, b = 60, t = 70),
         yaxis = list(title = "Number of Doses"),
         xaxis = list(title = "Population Size"))


# Setting the diagonal lines range
line_start <- 10000
line_end <- 1500 * 10 ^ 6

# Filter the data
d <- df_summary |> 
  filter(country_region != "World", 
         !is.na(population),
         !is.na(total)) 


# Replot it
plot_ly() |>
  add_markers(x = d$population,
              y = d$total,
              text = ~ paste("Country: ", d$country_region, "<br>",
                             "Population: ", d$population, "<br>",
                             "Total Doses: ", d$total, "<br>",
                             "Ratio: ", round(d$total_pop_ratio, 2), 
                             sep = ""),
              color = d$continent_name,
              type = "scatter",
              mode = "markers") |>
  add_lines(x = c(line_start, line_end),
            y = c(line_start, line_end),
            showlegend = FALSE,
            line = list(color = "gray", width = 0.5)) |>
  add_lines(x = c(line_start, line_end),
            y = c(0.5 * line_start, 0.5 * line_end),
            showlegend = FALSE,
            line = list(color = "gray", width = 0.5)) |>
  
  add_lines(x = c(line_start, line_end),
            y = c(0.25 * line_start, 0.25 * line_end),
            showlegend = FALSE,
            line = list(color = "gray", width = 0.5)) |>
  add_annotations(text = "1:1",
                  x = log10(line_end * 1.25),
                  y = log10(line_end * 1.25),
                  showarrow = FALSE,
                  textangle = -25,
                  font = list(size = 8),
                  xref = "x",
                  yref = "y") |>
  add_annotations(text = "1:2",
                  x = log10(line_end * 1.25),
                  y = log10(0.5 * line_end * 1.25),
                  showarrow = FALSE,
                  textangle = -25,
                  font = list(size = 8),
                  xref = "x",
                  yref = "y") |>
  add_annotations(text = "1:4",
                  x = log10(line_end * 1.25),
                  y = log10(0.25 * line_end * 1.25),
                  showarrow = FALSE,
                  textangle = -25,
                  font = list(size = 8),
                  xref = "x",
                  yref = "y") |>
  add_annotations(text = "Source: Johns Hopkins University - Centers for Civic Impact",
                  showarrow = FALSE,
                  xref = "paper",
                  yref = "paper",
                  x = -0.05, y = - 0.33) |>
  layout(title = "Covid19 Vaccine - Total Doses vs. Population Ratio (Log Scale)",
         margin = list(l = 50, r = 50, b = 90, t = 70),
         yaxis = list(title = "Number of Doses",
                      type = "log"),
         xaxis = list(title = "Population Size",
                      type = "log"),
         legend = list(x = 0.75, y = 0.05))


plot_ly(d,
        y = ~ total_pop_ratio,
        color = ~ continent_name,
        type = "box", boxpoints = "all", jitter = 0.3,
        pointpos = -1.8
) |>
  layout(title = "Distribution of Total Doses Admited vs Population Size Ratio",
         margin = list(l = 50, r = 50, b = 60, t = 60),
         yaxis = list(title = "Total Doses/Population Ratio"),
         xaxis = list(title = "Continent")
         
  )


us_vaccine <- covid19_vaccine |> 
  filter(country_region == "US") |>
  select(date, total_doses = people_at_least_one_dose) |>
  mutate(total_doses_lag1 = lag(total_doses, 1),
         daily_doses = total_doses - total_doses_lag1)|>
  select(date, daily_doses) |>
  arrange(date) 


head(us_vaccine)


plot_ly(us_vaccine) |>
  add_lines(x = ~ date,
            y = ~ daily_doses)  |>
  layout(title = "US Daily Vaccine Doses Received",
         margin = list(l = 50, r = 50, b = 60, t = 60),
         yaxis = list(title = "Total Doses"),
         xaxis = list(title = ""))


covid19_cases <- refresh_coronavirus_jhu()

head(covid19_cases)


us_cases <- covid19_cases |>
  filter(location == "US",
         data_type == "cases_new") |>
  select(date, cases = value)


head(us_cases)


df <- us_vaccine |> 
  left_join(us_cases, by = "date") |>
  select(date, daily_doses, daily_cases = cases) |>
  arrange(date)

head(df)

plot_ly(df) |>
  add_lines(x = ~ date,
            y = ~ daily_cases,
            name = "Daily Cases") |>
  add_lines(x = ~ date,
            y = ~ daily_doses,
            name = "Daily Doses") |>
  layout(title = "US Daily New Cases vs. Doses of Vaccine",
         yaxis = list(title = "New Cases/Doses"),
         xaxis = list(title = ""),
         legend = list(x = 0.75, y = 0.95),
         margin = list(l = 50, r = 50, b = 50, t = 90))


sf_use_s2(FALSE)

map <- ne_countries(returnclass = "sf") %>%
  dplyr::select(name, iso2 = iso_a2, iso3 = iso_a3, geometry)

head(map)

df <- map %>% left_join(
  covid19_vaccine %>%
    filter(date == max(date)) %>%
    mutate(perc = round(100 * people_at_least_one_dose / population, 2)) %>%
    select(country_region, iso2, iso3, people_at_least_one_dose, perc, continent_name),
  by = c("iso2", "iso3")
)

class(df)

df1 <- df %>% 
  filter(!name %in% c("Greenland", "Antarctica"))

df1  %>%
  mapview::mapview(zcol = "perc", 
                   at = seq(0, max(df1$perc, na.rm = TRUE), 10), 
                   legend = TRUE,
                   layer.name = "Fully Vaccinated %")

df2 <- df1 %>%
  filter(continent_name == "Africa")


tm_shape(df2) + 
  tm_polygons(col = "perc",  
              n = 5,
              title = "Perc. Group",
              palette = "Blues") 


tmap_mode("plot")


tm_shape(df2) + 
  tm_polygons(col = "perc",  
              n = 5,
              title = "Perc. Group",
              palette = "Greens") + 
  tmap_options(limits = c(facets.view = 13)) + 
  tm_facets(by = "name") 


ggplot(data = df2, aes(fill = `perc`)) + 
  geom_sf() + 
  scale_fill_viridis_b()



# check the time structure used for reporting covid cases

