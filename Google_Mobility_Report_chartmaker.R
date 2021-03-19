# this code downloads the latest Google Google Mobility Report data,
# matches Google regions to Eurostat nuts regions and
# produces charts that show activity levels weighted by nominal EUR gdp

library(regions)
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(fst)


# download the latest Google Mobility Report to the working directory 
# reading csv file
download.file( 'https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv', 
               destfile = file.path('data-raw', 'Global_Mobility_Report.csv') )
gmr_csv <- readr::read_csv( 
  file.path('data-raw', 'Global_Mobility_Report.csv'),
  col_types = readr::cols(
    country_region_code = readr::col_character(),
    country_region = readr::col_character(),
    sub_region_1 = readr::col_character(),
    sub_region_2 = readr::col_character(),
    metro_area = readr::col_character(),
    iso_3166_2_code = readr::col_character(),
    census_fips_code = readr::col_character(),
    date = readr::col_date(format = ""),
    retail_and_recreation_percent_change_from_baseline = readr::col_double(),
    grocery_and_pharmacy_percent_change_from_baseline = readr::col_double(),
    parks_percent_change_from_baseline = readr::col_double(),
    transit_stations_percent_change_from_baseline = readr::col_double(),
    workplaces_percent_change_from_baseline = readr::col_double(),
    residential_percent_change_from_baseline = readr::col_double()
  )
  )

## Do not synch on github the large data file.
require(usethis)
usethis::use_git_ignore("data-raw/Global_Mobility_Report.csv")
usethis::use_git_ignore("plots/")

# simplify names in Google Mobility Report file
gmr <- gmr_csv %>%
  set_names ( c("country_code",
                "google_country_name", 
                "google_region_name_1", 
                "google_region_name_2",
                "metro_area",
                "iso_3166_2_code",
                "census_fips_code",
                "place_id",
                "date",
                "retail",
                "grocery", 
                "parks",
                "transit",
                "workplaces",
                "residential") ) 


# obtain the match-table that maps google regions to nuts from the region package
# data("google_nuts_matchtable", package = "regions")
# google_nuts_matchtable <- google_nuts_matchtable %>% distinct()


google_nuts_matchtable <- read.fst ( "google_nuts_matchtable.fst" )

# create google matchtable without invalid topology lines
google_nuts_matchtable_filtered <- google_nuts_matchtable %>%
  filter ( typology != 'invalid typology') %>%
  select (-all_of("google_region_level")) %>%
  filter(!duplicated(google_region_name))

# create a list of all country codes in matchtable
google_nuts_matchtable_countries <- google_nuts_matchtable %>%
  distinct(country_code) %>% pull ()

# create list of countries where available nuts codes do not cover full country in the matchtable
countries_missing_full_nuts <- google_nuts_matchtable %>%
  filter ( typology == 'invalid typology') %>% select(country_code) %>% unique() %>% unlist() %>% unname()

# create list of countries where aggregation is not possible or needed
# we will include Italy where we will fix the missing regions and the UK where coverage is reasonably complete
countries_no_aggregation <- c(countries_missing_full_nuts[countries_missing_full_nuts != "IT" & countries_missing_full_nuts != "GB"], "LI", "LU",  "MT", "MK", "RS", "TR", "CH")

# gmr data for whole countries that are in the matchtable
gmr_eurostat_countries <- gmr %>%
  filter ( is.na(google_region_name_1) & is.na(google_region_name_2) ) %>%
  select ( -all_of(c("google_region_name_1", "google_region_name_2"))) %>%
  rename ( country_name = google_country_name ) %>%
  filter ( country_code %in% google_nuts_matchtable_countries )

# gmr data for regions where countries are in the matchtable
gmr_eurostat_regions <- gmr %>%
  rename ( country_name = google_country_name ) %>%
  filter ( country_code %in% google_nuts_matchtable_countries )

# gdp data from Eurostat for whole countries
gdp_countries <- eurostat::get_eurostat ( 
    'tec00001', 
    time_format = "num" ) %>%
  filter ( unit == "CP_MEUR", 
           time  >= 2015 ) %>%
  select ( -all_of(c("unit", "na_item"))) %>% 
  pivot_wider ( names_from = 'geo', 
                values_from = 'values') %>%
  fill ( names(.)) %>%
  pivot_longer ( cols = -all_of(c( "time")), 
                 names_to = 'geo', 
                 values_to = 'values') %>%
  mutate ( country_code = regions::get_country_code(geo)) %>%
  filter ( time == 2018 ) %>%
  rename ( gdp = values )

# gdp data from Eurostat, which contains  nuts3, nuts2 and nuts1 aggregation as well
gdp_nuts3 <- eurostat::get_eurostat ( "nama_10r_3gdp", 
                                      time_format = "num" ) %>%
  filter ( unit == "MIO_EUR", 
           time  >= 2015 ) %>%
  select ( -all_of("unit")) %>% 
  pivot_wider ( names_from = 'geo', 
                values_from = 'values') %>%
  fill ( names(.)) %>%
  pivot_longer ( cols = -all_of(c( "time")), 
                 names_to = 'geo', 
                 values_to = 'values') %>%
  mutate ( country_code = regions::get_country_code(geo)) %>%
  filter ( time == 2017 ) %>%
  rename ( gdp = values ) %>%
  rbind(c(2017, "ITDX", NA, "IT")) %>% # create "South Tyrol" region in Italy (sum of ITH1 and ITH2)
  mutate( gdp = ifelse (geo == "ITDX", (as.numeric(gdp[geo == "ITH1"]) + as.numeric(gdp[geo == "ITH2"])), gdp)) %>%
  mutate (gdp = as.numeric ( gdp))

gdp_regional_gmr <- gmr_eurostat_regions %>%
  rename(google_region_name = google_region_name_1) %>%
  filter (is.na(google_region_name_2)) %>% #at this stage we only use google_region_name_1
  left_join ( google_nuts_matchtable, 
              by = c("country_code", "google_region_name")
              )

# weight Gooogle Mobility Data by regional gdp where possible, country by country
gdp_weighted_gmr <- gmr %>%
  anti_join( gmr_eurostat_countries, 
             by = c("country_code", "date", 
                    "retail", "grocery", "parks", 
                    "transit", "workplaces", 
                    "residential")
             ) %>%
  rename(google_region_name = google_region_name_1) %>%
  left_join ( google_nuts_matchtable_filtered,
              by = c("country_code", "google_region_name") ) %>%
  filter ( !is.na(code_2016), 
           !country_code %in% countries_no_aggregation) %>%
  group_by ( country_code, code_2016, date) %>%
  summarize_at ( vars(all_of(c("retail", "grocery", "parks", "transit", "workplaces", "residential"))), mean, na.rm=TRUE) %>%
  left_join ( gdp_nuts3 %>% rename ( code_2016 = geo ), 
              by = c("country_code", "code_2016")) %>%
  filter (!is.na( code_2016 )) %>%
  filter (!is.na( gdp )) %>%
  group_by ( date,  country_code ) %>%
  summarize_at ( vars(all_of(c("retail", "grocery", "parks", "transit", "workplaces", "residential"))), 
                 funs(weighted.mean(., w=gdp, na.rm=TRUE))) %>%
  arrange (country_code, date) %>%
  full_join(gmr %>%
              filter (country_code %in% countries_no_aggregation) %>%
              filter ( is.na(google_region_name_1) & is.na(google_region_name_2)) %>%
              select(
                c("date", "country_code", "retail", "grocery",
                  "parks", "transit", "workplaces", "residential" )))


# weight Google Mobility Data further with gdp (creating European aggregates)
gdp_weighted_gmr_Europe <- gdp_weighted_gmr %>%
  left_join ( gdp_countries %>%
                select ( gdp, country_code ), 
              by = c("country_code") 
              ) %>%
  group_by ( date) %>%
  summarize_at ( vars(all_of(c("retail", "grocery", "parks", "transit", "workplaces", "residential"))),
                 funs( weighted.mean ( ., w = gdp, na.rm = T)))


# convert to data table
setDT(gdp_weighted_gmr)
setDT(gdp_weighted_gmr_Europe)

# calculate 7 day moving averages by country
gdp_weighted_gmr[, `:=` (retail_MA = frollmean(retail, n = 7,  na.rm = TRUE),
                    workplaces_MA = frollmean(workplaces, n = 7,  na.rm = TRUE),
                    transit_MA = frollmean(transit, n = 7,  na.rm = TRUE),
                    grocery_MA = frollmean(grocery, n = 7,  na.rm = TRUE)), by = "country_code"]



# calculate 7 day moving averages of aggregates
gdp_weighted_gmr_Europe[, `:=` (retail_MA = frollmean(retail, n = 7,  na.rm = TRUE),
                         workplaces_MA = frollmean(workplaces, n = 7,  na.rm = TRUE),
                         transit_MA = frollmean(transit, n = 7,  na.rm = TRUE),
                         grocery_MA = frollmean(grocery, n = 7,  na.rm = TRUE))]


# melting/pivoting longer
gdp_weighted_gmr_Europe <-  melt(gdp_weighted_gmr_Europe, id.vars = c("date"),
                                        measure.vars = c("retail", "grocery", "parks", "transit",
                                                         "workplaces","residential", "retail_MA",
                                                         "workplaces_MA", "transit_MA", "grocery_MA" ),
                                 variable.name = "activity",
                                 value.name = "pct_change")

#charts

# Define palette:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
large_eu_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
# satellitereport::sr_palette()

mobility_palette <- c(
  "residential" = "#4EC0E4", 
  "grocery" = "#BAC615", 
  "transit" = "grey60", 
  "workplaces" = "#00348A",
  "retail" = "#E88500")

# European aggragate charts
ggplot(data = gdp_weighted_gmr_Europe %>%
         filter ( 
           activity %in% c("retail", "grocery", "workplaces", 
                           "transit", "residential") 
           ),
       aes(x = as.Date(date), 
           y = pct_change/100, 
           group = activity)
       ) +
  geom_line(aes(col = activity), size=1 ) +
  scale_colour_manual( name = "activity type", 
                       values = mobility_palette ) +
  scale_y_continuous( labels = scales::percent ) +
  scale_x_date () +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.text   = element_text(size = 12) 
  ) + 
  labs ( title = "European Community Mobility Levels", 
         subtitle = "Compared to baseline and weighted with regional GDP",
         x = "", 
         y = "% change from baseline", 
         caption = "\ua9 Daniel Antal - István Zsoldos, regions.satellitereport.eu, 2020.")

ggsave("plots/gdp_weighted_gmr_Europe.png", unit = "cm", 
       width = 16, height = 9)

mobility_MA_palette <- c(
  "residential_MA" = "#4EC0E4", 
  "grocery_MA" = "#BAC615", 
  "transit_MA" = "grey60", 
  "workplaces_MA" = "#00348A",
  "retail_MA" = "#E88500")

# European aggregate chart, 7 day moving average
ggplot(data = gdp_weighted_gmr_Europe[activity %in% c("retail_MA", "grocery_MA", "workplaces_MA", "transit_MA")],
       aes(x = as.Date(date), 
           y = pct_change/100, 
           group = activity)
       )+
  geom_line(aes(col = activity), size=1 ) +
  scale_y_continuous( labels = scales::percent ) +
  scale_x_date () +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.text   = element_text(size = 12) 
  ) +
  scale_colour_manual( values = mobility_MA_palette ) +
  labs ( 
    title = "European Community Mobility Levels", 
    subtitle = "Compared to baseline and weighted by regional GDP",
    x = "", 
    y = "% change from baseline" 
  )

ggsave("plots/gdp_weighted_gmr_MA_Europe.png", unit = "cm", 
       width = 16, height = 9)

#retail charts

# large_eu_palette <- satellitereport::palette_eu_countries()
# large_eu_palette[which(names(large_eu_palette)=="ES")] <- "#DB001C"


#large EU countries, retail and recreation activity time series
ggplot(data = gdp_weighted_gmr[country_code %in% c("SE", "FR", "ES", "IT","DE", "GB") ],
       aes(x = as.Date(date), 
           y = retail/100, 
           group = country_code))+
  geom_line(aes(col = country_code),size=1)+
  scale_y_continuous( labels = scales::percent ) +
  scale_x_date () +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.text   = element_text(size = 12) 
  ) +
  scale_colour_manual(values = large_eu_palette, 
                      name = "") +
  labs ( 
    title = "Retail & Recreation Community Mobility Levels", 
    subtitle = "Weighted by regional GDP, 7-day moving average values", 
    x = "", y = "% change from baseline")

ggsave("plots/gdp_weighted_rr_MA_large_EU.png", unit = "cm", 
       width = 16, height = 9)

#large EU countries, retail and recreation activity time series, 7 day moving average
ggplot(data = gdp_weighted_gmr[country_code %in% c("SE", "FR", "ES", "IT","DE", "GB") ],
       aes(
         x = as.Date(date), 
         y = retail_MA/100, 
         group = country_code)
       )+
  geom_line(aes(col = country_code),size=1)+
  scale_y_continuous( labels = scales::percent ) +
  scale_x_date () +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.text   = element_text(size = 12) 
  ) +
  scale_colour_manual(values = large_eu_palette, 
                      name = "") +
  labs ( 
    title = "Retail & Recreation Community Mobility Levels", 
    subtitle = "Weighted by regional GDP, 7-day moving average values", 
    x = "", y = "% change from baseline")

ggsave("plots/gdp_weighted_gmr_rr_MA_large.png", unit = "cm", 
       width = 16, height = 9)

# select_eu_palette <- satellitereport::palette_eu_countries()
# select_eu_palette[which(names(large_eu_palette)=="PL")] <- "grey70"

select_eu_palette <- large_eu_palette

#selected EU countries, retail and recreation activity time series, 7 day moving average
ggplot(data = gdp_weighted_gmr[country_code %in% c("DE", "HU", "SK", "PL","AT", "CZ", "RO") ],
       aes(
         x=as.Date(date), 
         y = retail_MA/100,
         group = country_code))+
  geom_line(aes(col = country_code),size=1) +
  scale_y_continuous( labels = scales::percent ) +
  scale_x_date () +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.text   = element_text(size = 12) 
  ) +
   scale_colour_manual(values = select_eu_palette, 
                       name = "") +
  labs ( 
    title = "Retail & Recreation Community Mobility Levels", 
    subtitle = "Weighted by regional GDP, 7-day moving average values", 
    x = "", y = "% change from baseline")

ggsave("plots/gdp_weighted_gmr_rr_MA_select.png", unit = "cm", 
       width = 16, height = 9)
#workplaces charts

#large EU countries, workplace activity time series
ggplot(data = gdp_weighted_gmr[country_code %in% c("SE", "FR", "ES", "IT","DE", "GB") ],
       aes(
         x = as.Date(date), 
         y = workplaces/100, 
         group = country_code)
       )+
  geom_line(aes(col = country_code),size=1)+
  scale_y_continuous( labels = scales::percent ) +
  scale_x_date () +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.text   = element_text(size = 12) 
  ) +
  scale_colour_manual(values = large_eu_palette, 
                      name = "") +
  labs ( 
    title = "Workplace Activity Levels", 
    subtitle = "Weighted by regional GDP", 
    x = "", y = "% change from baseline")

ggsave("plots/gdp_weighted_gmr_workplace_large.png", unit = "cm", 
       width = 16, height = 9)

#large EU countries, workplace activity time series, 7 day moving average
ggplot(data = gdp_weighted_gmr[country_code %in% c("SE", "FR", "ES", "IT","DE", "GB") ],
       aes(
         x =as.Date(date), 
         y = workplaces_MA/100, 
         group = country_code)
       ) +
  geom_line(aes(col = country_code),size=1)+
  scale_y_continuous( labels = scales::percent ) +
  scale_x_date () +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.text   = element_text(size = 12) 
  ) +
  scale_colour_manual(values = large_eu_palette, 
                      name = "") +
  labs ( 
    title = "Workplace Activity Levels", 
    subtitle = "Weighted by regional GDP, 7-day moving average values", 
    x = "", y = "% change from baseline")


ggsave("plots/gdp_weighted_gmr_workplace_MA_large.png", unit = "cm", 
       width = 16, height = 9)

##selected EU countries, workplace activity time series, 7 day moving average
ggplot(data = gdp_weighted_gmr[country_code %in% c("DE", "HU", "SK", "PL","AT", "CZ", "RO") ],
       aes(
         x = as.Date(date), 
         y = workplaces_MA/100, 
         group = country_code)
       )+
  geom_line(aes(col = country_code),size=1)+
  scale_y_continuous( labels = scales::percent ) +
  scale_x_date () +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.text   = element_text(size = 12) ) +
  scale_colour_manual(values = large_eu_palette, 
                      name = "") +
  labs ( 
    title = "Workplace Activity Levels", 
    subtitle = "Weighted by regional GDP, 7-day moving average values", 
    x = "", y = "% change from baseline")

ggsave("plots/gdp_weighted_gmr_rr_MA_select.png", unit = "cm", 
       width = 16, height = 9)

#transit charts

#large EU countries, transit activity time series
ggplot(data = gdp_weighted_gmr[country_code %in% c("SE", "FR", "ES", "IT","DE", "GB") ],
       aes(
         x= as.Date(date), 
         y = transit/100, 
         group = country_code)
       )+
  geom_line(aes(col = country_code),size=1) +
  scale_y_continuous( labels = scales::percent ) +
  scale_x_date () +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.text   = element_text(size = 12) 
  ) +
  scale_colour_manual(values = large_eu_palette, 
                      name = "") +
  labs ( 
    title = "Public Transport Mobility Levels", 
    subtitle = "Weighted by regional GDP", 
    x = "", y = "% change from baseline")

ggsave("plots/gdp_weighted_gmr_transit_large.png", unit = "cm", 
       width = 16, height = 9)


#large EU countries, transit activity time series, 7 day moving average
ggplot(data = gdp_weighted_gmr[country_code %in% c("SE", "FR", "ES", "IT","DE", "GB") ],
       aes(
         x = as.Date(date), 
         y = transit_MA/100, 
         group = country_code)
       )+
  geom_line(aes(col = country_code),size=1)+
  scale_y_continuous( labels = scales::percent ) +
  scale_x_date () +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.text   = element_text(size = 12) 
  ) +
  scale_colour_manual(values = large_eu_palette, 
                      name = "") +
  labs ( 
    title = "Public Transport Mobility Levels", 
    subtitle = "Weighted by regional GDP, 7-day moving average values", 
    x = "", y = "% change from baseline")

ggsave("plots/gdp_weighted_gmr_transit_MA_large.png", unit = "cm", 
       width = 16, height = 9)

##selected EU countries, transit activity time series, 7 day moving average
ggplot(data = gdp_weighted_gmr[country_code %in% c("DE", "HU", "SK", "PL","AT", "CZ", "RO")  ],
       aes(x= as.Date(date), 
           y = transit_MA/100, 
           group = country_code))+
  geom_line(aes(col = country_code),size=1)+
  scale_y_continuous( labels = scales::percent ) +
  scale_x_date () +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.text   = element_text(size = 12) 
  ) +
  scale_colour_manual(values = select_eu_palette, 
                      name = "") +
  labs ( 
    title = "Public Transport Mobility Levels", 
    subtitle = "Weighted by regional GDP, 7-day moving average values", 
    x = "", y = "% change from baseline")

ggsave("plots/gdp_weighted_gmr_transit_MA_select.png", unit = "cm", 
       width = 16, height = 9)

#grocery charts

#large EU countries, grocery activity time series
ggplot(data = gdp_weighted_gmr[country_code %in% c("SE", "FR", "ES", "IT","DE", "GB") ],
       aes(
         x = as.Date(date), 
         y = grocery/100, 
         group = country_code) 
       )+
  geom_line(aes(col = country_code),size=1)+
  scale_y_continuous( labels = scales::percent ) +
  scale_x_date () +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.text   = element_text(size = 12) 
  ) +
  scale_colour_manual(values = large_eu_palette, 
                      name = "") +
  labs ( 
    title = "Grocery Store Mobility Levels", 
    subtitle = "Weighted by regional GDP", 
    x = "", y = "% change from baseline"
    )

ggsave("plots/gdp_weighted_gmr_grocery_large.png", unit = "cm", 
       width = 16, height = 9)

#large EU countries, grocery activity time series, 7 day moving average
ggplot(data = gdp_weighted_gmr[country_code %in% c("SE", "FR", "ES", "IT","DE", "GB") ],
       aes(
         x = as.Date(date), 
         y = grocery_MA/100, 
         group = country_code)
       )+
  geom_line(aes(col = country_code),size=1)+
  scale_y_continuous( labels = scales::percent ) +
  scale_x_date () +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.text   = element_text(size = 12) 
  ) +
  scale_colour_manual(values = large_eu_palette, 
                      name = "") +
  labs ( 
    title = "Grocery Store Mobility Levels", 
    subtitle = "Weighted by regional GDP, 7-day moving average values", 
    x = "", y = "% change from baseline")

ggsave("plots/gdp_weighted_gmr_grocery_MA_large.png", unit = "cm", 
       width = 16, height = 9)

#selected EU countries, grocery activity time series, 7 day moving average
ggplot(data = gdp_weighted_gmr[country_code %in% c("DE", "HU", "SK", "PL","AT", "CZ", "RO") ],
       aes(
         x= as.Date(date), 
         y = grocery_MA/100, 
         group = country_code) 
       )+
  geom_line(aes(col = country_code),size=1)+
  scale_y_continuous( labels = scales::percent ) +
  scale_x_date () +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.text   = element_text(size = 12) 
  ) +
  scale_colour_manual(values = select_eu_palette, 
                      name = "") +
  labs ( 
    title = "Grocery Store Mobility Levels", 
    subtitle = "Weighted by regional GDP, 7-day moving average values", 
    x = "", y = "% change from baseline")


ggsave("plots/gdp_weighted_gmr_grocery_MA_select.png", unit = "cm", 
       width = 16, height = 9)
