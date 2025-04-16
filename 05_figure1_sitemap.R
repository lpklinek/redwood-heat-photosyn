# Plotting Figure 1: a site map for Garcia River Forest
# Written by Lily Klinek (lpklinek@ucdavis.edu)
# 


# Data and map prep  ------
garcia_vect <- vect('./data/raw/05_site_shapefile/GarciaRiverForest_Boundary-polygon.shp')
# projecting site shapefile
new_crs <-"+proj=longlat +datum=WGS84"
garcia_vect_proj <- terra::project(garcia_vect, new_crs)


# setting layers and source url
grp <- c("USGS Topo", "USGS Imagery Only", "USGS Imagery Topo",
         "USGS Shaded Relief", "Hydrography")
att <- paste0("<a href='https://www.usgs.gov/'>",
              "U.S. Geological Survey</a>")
GetURL <- function(service, host = "basemap.nationalmap.gov") {
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}


# Create site map -----
forest_map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addWMSTiles(GetURL("USGSImageryTopo"), # adding basemap (can edit these lines to select other basemap options from grp list)
              group = grp[3], 
              attribution = att, 
              layers = "0") %>%
  addPolygons(data=garcia_vect_proj, # adding site polygon
              color='black', 
              weight=1, 
              fillOpacity=0.5, 
              fillColor='darkgreen') %>%
  setView(lng = -123.5157259, lat = 38.91669174, zoom = 12) %>% # set map centroid and zoom
  addMiniMap(tiles = providers$USGS.USImagery,  # add map inset
             position = 'topright', 
             width = 130, height = 170,
             toggleDisplay = FALSE,
             zoomLevelOffset = -5.7,
             centerFixed=c(lng=-122.5, lat=38.4)) %>%
  addScaleBar(position = "bottomleft",   # add scale bar
              options = scaleBarOptions(maxWidth = 100, 
                                        imperial = TRUE,
                                        updateWhenIdle = TRUE))


# Display map -------
forest_map
