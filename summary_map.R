library(geojsonio)
library(tidyverse)
library(spdplyr)
library(viridis)
library(sf)
library(ggthemes)
library(fuzzyjoin)

mx_map <- rgdal::readOGR('maps/mx_states.geojson')

ids <- data.frame(CVE_ENT=mx_map@data$CVE_ENT, NOM_ENT=mx_map@data$NOM_ENT)

data <- readRDS('data/summary_data-2020-05-27.rds') %>%
  rename(Cambio=`Expected change in daily cases`) %>%
  mutate(
    Cambio=recode(Cambio,
                  'Unsure'='Incierto',
                  'Increasing'='Aumentando',
                  'Likely increasing'='Probablemente aumentando',
                  'Likely decreasing'='Probablemente disminuyendo',
                  'Decreasing'='Disminuyendo')
  ) %>%
  filter(!grepl('NACIONAL', region)) %>%
  mutate(region = str_replace(region, 'COAHUILA', 'COAHUILA DE ZARAGOZA')) %>%
  mutate(region = str_replace(region, 'MICHOACAN', 'MICHOACAN DE OCAMPO')) %>%
  mutate(region = str_replace(region, 'VERACRUZ', 'VERACRUZ DE IGNACIO DE LA LLAVE')) %>%
  as.data.frame()

# Contains status a n
status_df <- data %>% filter(!grepl('Effective reproduction no.', metric))
rt_df <- data %>% filter(!grepl('New confirmed cases by infection date', metric))

summary_tbl <- left_join(status_df, rt_df, by='region') %>%
  rename(cases_status = Cambio.y) %>%
  rename(cases_min = lower.x) %>%
  rename(cases_max = upper.x) %>%
  mutate(rt_avg=(round(lower.y+upper.y)/2), 2) %>%
  select(region, cases_status, cases_min, cases_max, rt_avg) %>%
  as.data.frame()
  

# Fuzzy join to map names
map_data <- stringdist_left_join(ids, summary_tbl, by=c(NOM_ENT='region'), max_dist=1, ignore_case=TRUE) %>%
  as.data.frame()

# To fix 'row.names of data and Polygons IDs do not match' error
row.names(map_data) <- sapply(slot(mx_map, 'polygons'), function(x) slot(x, 'ID'))

# Merge nowcasts data withmap
nowcasts_map <- SpatialPolygonsDataFrame(mx_map,  map_data)

# Write data
geojson_write(nowcasts_map, file='output_maps/nowcasts-2020-05-27')
write_csv(summary_tbl, 'output_data/summary_table-2020-05-27.csv')

# Test map
test_map <- sf::st_as_sf(nowcasts_map)
p1 <- ggplot(test_map) +
  ggtitle(expression(atop('Nowcasts', atop('Casos nuevos')))) +
  geom_sf(aes(fill = lower), colour = NA) +
  scale_fill_viridis() + theme_bw()