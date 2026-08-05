# Creating map of average maximum summer temperatures across redwood range
# Written by Lily Klinek (lpklinek@ucdavis.edu)


# Data prep ----

# load california boundary
ca <- vect('/Users/lklinek/Desktop/castateboundary/California.shp')

# load redwood boundary
redwood <- vect('/Users/lklinek/Desktop/California (USA) Redwoods Region Composite Conservation Model Score by Watershed/data/commondata/data0/CA_Redwoods_Score_2_9.shp')
# aggregate multiple polygons to one
redwood_ag <- aggregate(redwood)
# reproject redwood to crs of ca
redwood_ag_proj <- project(redwood_ag, crs(ca))



# download gridmet data
ca_clim = getGridMET(ca, 
                     varname = c("tmmx"), 
                     startDate = "2015-01-01",
                     endDate = '2025-01-01')

ca_clim2 <- ca_clim %>% 
  setNames(c("tmax"))
ca_clim2 <- unlist(ca_clim2[[1]])

# get dates from the raster
dates <- time(ca_clim2)

# extract months as numbers (06–09 = Jun–Sep)
summer_idx <- format(dates, "%m") %in% c("06", "07", "08", "09")

# subset raster for summer months
ca_clim_summer <- ca_clim2[[summer_idx]]


# crop and mask
ca_clim_crop <- crop(ca_clim_summer, ca)
ca_clim_mask <- mask(ca_clim_crop, ca)

# take mean of max temp layers
ca_clim_mean <- mean(ca_clim_mask)
# convert to C
ca_clim_mean_C <- ca_clim_mean - 273.15

# test plot
terra::plot(ca_clim_mean_C, main="Mean Max. Temperature, 2015-2020")
plot(redwood_ag_proj, add=TRUE)

# crop and mask climate data to redwood
redwood_climate <- crop(ca_clim_mean_C, redwood_ag_proj)
redwood_climate_mask <- mask(redwood_climate, redwood_ag_proj)

# crop ca to 35.5 lat
crop_ext <- ext(c(-124.5, -118, 35.5, 42))
ca_crop <- crop(ca, crop_ext)

# get basemap options
basemap_main <- maptiles::get_tiles(ca, provider = "CartoDB.PositronNoLabels", crop=TRUE, project=TRUE) 
basemap_main_label <- maptiles::get_tiles(ca, provider = "CartoDB.Positron", crop=TRUE, project=TRUE) 


# palette
pal <- colorNumeric(c(viridis::turbo(n = 100)), values(redwood_climate_mask),
                    na.color = "transparent")
pal_rev <- colorNumeric(c(viridis::turbo(n = 100)), values(redwood_climate_mask),
                        na.color = "transparent", reverse=TRUE)

# site coordinate 
tree_coords <- data.frame(
  name = c("Garcia River Forest"),
  lon = c(-123.425600),
  lat = c(38.978728)
)

# Map code -----

redwood_map <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
  addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
  addRasterImage(redwood_climate_mask,
                 colors=pal
  ) %>%
  addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, options=tileOptions(opacity=0.7)) %>%
  addCircleMarkers(data = tree_coords,
                   lng = ~lon, lat = ~lat,
                   label = ~name,
                   radius = 1,
                   color="black",
                   opacity=1,
                   fillColor = "black",
                   fillOpacity = 1,
                   stroke = TRUE) %>%
  addScaleBar(position = "bottomright",   # add scale bar
              options = scaleBarOptions(maxWidth = 100, 
                                        imperial = TRUE,
                                        updateWhenIdle = TRUE)) %>%
  addLegend(pal=pal_rev, 
            values = values(redwood_climate_mask, na.rm=TRUE), 
            title="Temperature (°C)",
            opacity=0.95,
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  ) 




# Display map -------
redwood_map
