

library(xml2)
library(forcats)
library(viridis)
library(tmaptools)
library(sf)
#library(geom_raster)

G_PROJ4DEF <- '+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
G_CRS_CODE <- 'EPSG:3978'


#crs_canada <- "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
G_CRS_WGS <- "+proj=longlat +datum=WGS84"


geocode_OSM_with_cache <- function(vec, file_loc = file.path("~", "OSM_geocoded_strings.tsv")){
  cache <- read_tsv(file_loc)
  
  to_look_up <- vec[!vec %in% cache$query]
  
  new_loc <- tryCatch({
    geocode_OSM(to_look_up, as.data.frame = T)
  }, error = function(e) {
    NULL
  }, finally = {
    print("Done")
  })
  
  if (!is.null(new_loc))
    cache <- bind_rows(cache, new_loc) %>% distinct()
  
  cache %>% write_tsv(file_loc)
  cache %>% filter(query %in% vec)
}


#########################################
#
#
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
  
  
  ret_val[["cases"]] <- rvest::html_table(webpage)[[8]] %>% 
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
    st_as_sf(coords = c("lon", "lat"), crs = crs_WGS) %>%  
    st_transform(crs = crs_out)
  
  
  return(ret_val)
}
