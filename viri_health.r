

library(xml2)
library(forcats)
library(viridis)
library(tmaptools)
library(sf)
library(tidyverse)
library(geom_raster)
library(sf)
library(ggthemes)
G_PROJ4DEF <- '+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
G_CRS_CODE <- 'EPSG:3978'


#crs_canada <- "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
G_CRS_WGS <- "+proj=longlat +datum=WGS84"

#getwd()


##################################333
# Geocode with Open Streeat maps
# but only if not in spreadsheet
geocode_OSM_with_cache <- function(vec, file_loc = file.path("OSM_geocoded_strings.tsv")){
  cache <- read_tsv(file_loc)
  
  to_look_up <- vec[!vec %in% cache$query]
  
  new_loc <- tryCatch({
    geocode_OSM(to_look_up, as.data.frame = T)
  }, error = function(e) {
    cache %>% filter( 1 == 0 )
  }, finally = {
    print("Done")
  })
  
  new_loc <- 
    new_loc %>% 
    mutate(human_check = "none")
  
  if (!is.null(new_loc))
    cache <- bind_rows(cache, new_loc) %>% distinct()
  
  cache %>% write_tsv(file_loc)
  cache %>% filter(query %in% vec)
}


#########################################
#
# Read data from VIRI Health websit
#
read_viri_health_data <- function(webpage_url = "https://virihealth.com/", crs_out = G_CRS_WGS){
  #########################
  # Dont do this all the time
  webpage <- xml2::read_html(webpage_url)
  
  
  ret_val <- list()
  ret_val[["totals"]] <- rvest::html_table(webpage)[[1]] %>% 
    tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
  
  
  ret_val[["prov"]] <- rvest::html_table(webpage)[[2]] %>% 
    tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
  
  ret_val[["city"]] <- rvest::html_table(webpage)[[3]] %>% 
    tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
  
  
  ret_val[["sex"]] <- rvest::html_table(webpage)[[4]] %>% 
    tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
  
  
  ret_val[["hospital"]] <- rvest::html_table(webpage)[[5]] %>% 
    tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
  
  
  ret_val[["tests"]] <- rvest::html_table(webpage)[[6]] %>% 
    tibble::as_tibble(.name_repair = "unique")# repair the repeated columns
  
  
  ret_val[["prov_recovered"]] <- rvest::html_table(webpage)[[7]] %>% 
    tibble::as_tibble(.name_repair = "unique") 
  
  
  ret_val[["deaths"]] <- rvest::html_table(webpage)[[8]] %>% 
    tibble::as_tibble(.name_repair = "unique") 
  
  ret_val[["cases"]] <- rvest::html_table(webpage)[[9]] %>% 
    tibble::as_tibble(.name_repair = "unique") 
  
  
  ret_val[["cases"]] <- ret_val[["cases"]] %>% 
    mutate(to_geo = tolower(paste(City, Prov, "Canada",sep = ", ")) )
  
  for_geo <- ret_val[["cases"]] %>% pull(to_geo) %>% unique()
  cases_cols <- ret_val[["cases"]] %>% colnames()
  
  
  
  geo_loc <- geocode_OSM_with_cache(for_geo)
  
  ret_val[["cases"]] <- left_join(ret_val[["cases"]], geo_loc, by = c("to_geo" = "query"))
  
  
  
  ret_val[["cases"]] <- 
    ret_val[["cases"]] %>% 
    mutate(to_geo = if_else(is.na(lat),    tolower(paste( Prov, "Canada",sep = ", ")), to_geo ))
  
  ctry2 <- ret_val[["cases"]] %>% filter(is.na(lat)) 
  ctry1 <- ret_val[["cases"]] %>% filter(!is.na(lat)) 
  geo_loc <- geocode_OSM_with_cache(ctry2$to_geo %>% unique())
  
  
  ctry2 <- ctry2 %>% select(cases_cols)
  ctry2 <- left_join(ctry2, geo_loc, by = c("to_geo" = "query"))
  
  ret_val[["cases"]] <- bind_rows(ctry2, ctry1)
  
  ret_val[["cases"]] <- 
    ret_val[["cases"]] %>% 
    replace_na(list(lat = 75, lon = -40)) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = G_CRS_WGS) %>%  
    st_transform(crs = crs_out)
  
  
  return(ret_val)
}



############################33
# TODO ::
Plot_provincial_choropleth(data){

  
}

 ############3 Some bugs below ##################
data <- NULL
data <- read_viri_health_data()



canada_HR_shp <- read_sf(file.path("HR000b11a_e_Oct2013_simp.geojson")) %>% 
  st_transform(canada_shp, crs = G_PROJ4DEF)

canada_HR_shp %>% 
  ggplot() +
  geom_sf() +
  theme_map()



canada_prov_shp <- read_sf(file.path("canada_pt_sim.geojson")) %>% 
  st_transform(canada_shp, crs = G_PROJ4DEF)


canada_prov_shp %>% 
  ggplot() +
  geom_sf(alpha = 0, size = 1.25, color = "black") +
  theme_map()


data$prov

points<- 
data$cases %>% 
  st_transform(canada_shp, crs = G_PROJ4DEF)
  # st_coordinates(data$cases) %>% 
  # as_tibble() %>% 
  # bind_cols(., data$cases)
  #   


data$cases %>% group_by(Y) %>% summarise(n = n()) %>% view()
  
p <- 
  canada_shp %>% 
  ggplot() +
  geom_sf() +
  theme_map()

p + 
  geom_sf(data = points, mapping = aes()) +
  scale_color_viridis() +
  scale_fill_viridis()

data$cases %>% view()
fn <- paste0("virihealth_cases_", Sys.Date(), ".tsv")
ret_val[["cases"]] %>% write_tsv(fn)


fn <- paste0("virihealth_by_city_", Sys.Date(), ".tsv")
ret_val[["cases"]] %>% group_by(City, Prov, lat, lon,  lat_min, lat_max, lon_min, lon_max) %>% summarise(N = n()) %>% 
  write_tsv(fn)


fn <- paste0("virihealth_by_prov_", Sys.Date(), ".tsv")
ret_val[["cases"]] %>% group_by(Prov) %>% summarise(N = n()) %>% 
  write_tsv(fn)


lat_lng <- st_coordinates(data$cases)
rbind(lat_lng, data$cases)


data$tests %>% 
  mutate(Tests = as.integer(sub(",", "",Tests)),  Positive= as.integer(sub(",", "",Positive))) %>%
  mutate(Negative = Tests - Positive) %>%
  select(Prov, Negative, Positive, Tests ) %>% 
  mutate(Per = 100*Positive/Tests) %>%
  filter(!Prov %in% c("TOT","RC") ) %>% 
  pivot_longer(cols = c("Negative", "Positive")) %>%
  mutate(Prov = paste0(Prov, "\n Tests =", Tests, "\nPositive=", round(Per, 1), "%")) %>%
  mutate(Prov = fct_reorder(Prov, 1/Tests)) %>%
  ggplot(aes(x=sqrt(Tests)/2, y = value, fill = name, width = sqrt(Tests))) +
  geom_bar(position="fill", stat="identity") + 
  facet_wrap(vars(Prov)) +
  #facet_grid(side1 ~ side2) + 
  coord_polar("y") + 
  labs(title = "COVID-19 tests performed by provinces.", fill = "COVOD-19 test results." ) + 
  theme_void() +
  theme(
        panel.border = element_rect(linetype = "dashed", fill = NA)
        ) 

  
  
  
