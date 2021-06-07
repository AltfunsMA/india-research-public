## R 3.5.2
## Alfonso Martinez Arranz # 10 March 2019
## MEMSI-India


## Generating a map of districts/states with density, urban extent and other
## elements helping determing the urban/rural split

plot_UR_split <- function(updated_version = FALSE,
                          # NB: TRUE allows to simply pass through but not NULL
                          filter_state = TRUE,  
                          filter_district = TRUE,
                          size_scalar = 1,
                          legend_scalar = 1.5,
                          legend_fill = "grey90",
                          leg_just = c("left", "top"),
                          leg_x = NULL,
                          leg_y = NULL,
                          legend_omit = FALSE) {
  
  
  #################################################################
  # Need to set the wave, ur_fixes, and ac_map_type variables in the global
  # environment for correct loading of maps and other files
  #################################################################

  ghpath <- "Processed/03_pop_maps/"
  
  # Size scalar
  
  if(isTRUE(filter_state) & !isTRUE(filter_district)) {
    
    size_scalar <- 2
    
  }
  
  # Loading libraries and datasets----
  
  
  library(ggrepel)
  library(raster)
  
  # Ensure dplyr is not masked after runnning this function for the first time. 
  # Throws error if executed when dplyr has already been detached elsewhere 
  # but the tidyverse is still attached (very unlikely circumstances)
  raster_b4_dplyr <- 
    match("package:dplyr", search()) > match("package:raster", search())
  
  if(!is.na(raster_b4_dplyr)) { 
    if(raster_b4_dplyr) {
      detach(package:dplyr)
      require(dplyr)
    }
  }
  library(cptools)

  
  sp_read <- function(string) {

    st_read(paste0(ur_fixes, string, wave, "_", ac_map_type, ".gpkg"))
    
    
    }
  
  # TODO: Make this whole function take a vector of districts and load these
  # dataframes only once per call and never on the Global Environment
  
  if(updated_version) {updated_str <- "updated_w"} else { updated_str <- ""}
  
  if(!exists("AC_urban_rural")) 
  {AC_urban_rural <<- sp_read(paste0("AC_UR_", updated_str))}
  if(!exists("AC_distmap")) 
  {AC_distmap  <<- sp_read("AC_distmap_")}
  
  

  if(!exists("NASA_settlement")) 
  {NASA_settlement <<- 
    read_csv(paste0(ghpath, "NASA_settlement_by_AC_", ac_map_type, ".csv"))}
  # if(!exists("GADM2018_dist")) 
  #   {sfread(GADM2018_dist)}

  if(!exists("GRUMP_urban_extent")) 
    {sfread(GRUMP_urban_extent)}
  if(!exists("GPW2010_dens_2pt5")) 
    {
    GPW2010_dens_2pt5 <<- st_read(paste0(ghpath, "GPW2010_dens_2pt5_", 
                                         ac_map_type, ".gpkg")
                                  )}
  

  
  # TODO: put the following into a loop to avoid problems with loading files every
  # time this function is called
  
  # Selecting geographical references-----
  

  filtrar <- function(df) {

    df %>%
    {if ("STATE" %in% colnames(df)) 
      rename(., ST_NAME = STATE, DIST_NAME = DISTRICT_NAME) else .} %>% 
    {if (filter_state != TRUE)
      filter (., ST_NAME %in% filter_state) else .} %>% 
      {if (filter_district != TRUE) 
      filter (., DIST_NAME %in% filter_district) else .}
    
  }
  
  
  ACfilter <- filtrar(AC_urban_rural %>% {if ("UR_TYPE" %in% colnames(AC_urban_rural)) 
                          rename(., REGION_TYPE = UR_TYPE) else .})
  
  cityfilter <- NASA_settlement %>%
    filtrar() %>%
    mutate(URB_CENTRES = cut(
      POPULATION,
      breaks = c(5000L, 10000L,
                 25000L, 50000L,
                 75000L, 100000L, Inf),
      labels = c(
        "5,001 - 10,000 hab.",
        "10,001 - 25,000 hab.",
        "25,001 - 50,000 hab.",
        "50,001 - 75,000 hab.",
        "75,001 - 100,000 hab.",
        paste("100,001 -", scales::comma(max(POPULATION)), "hab.")
      )
    ))
  

  
  # For labelling, unexplicably using "filterstate" throws an error
  distmapfilter <- filtrar(AC_distmap)
  
  if(nrow(distmapfilter) == 0) {
    
    
    browser()
    
    }
  
  filter_title <- ifelse(filter_state == TRUE, 
                         as.character(
                           paste0(unique(distmapfilter$ST_NAME), 
                                  "_",
                                  filter_district)), 
                         filter_state)

  
  # Filtering the polygons from "urban extent" that intersect with ----
  filterstate <- filtrar(AC_distmap) # Previously was using GADM2018dist, probably mistakenly
  
  
  
  ### Creates a matrix with 1 for TRUE intersects
  intersect_extent_matrix <- st_intersects(x = GRUMP_urban_extent, 
                                           y = filterstate) 
  
  match_extent_vector <- lengths(intersect_extent_matrix) > 0 
  
  rm(intersect_extent_matrix)
  
  extentfilter <- GRUMP_urban_extent %>%
    filter(match_extent_vector)
  
  rm(match_extent_vector)
  
  
  # Filtering the polygons from "density" that intersect with each region----
  
  # Creates a matrix with 1 for TRUE intersects
  intersect_density_matrix <- st_intersects(x = GPW2010_dens_2pt5, 
                                            y = filterstate)
  
  match_density_vector <-  lengths(intersect_density_matrix) > 0 
  
  rm(intersect_density_matrix)
  
  densityfilter <- GPW2010_dens_2pt5 %>%
    filter(match_density_vector)
  
  rm(match_density_vector)
  
  # PLOT --------------------------------------------------------------------
  
  
  # Extracts the limits for plotting because some density maps have odd 
  # dots far away from proper region territory
  statebox <- st_bbox(filterstate)
  
  # Starts map by setting the density values as a background with no borders
  sfplot(densityfilter, fill = "log(Density)", colour = NA) + 
    scale_fill_gradient(low = "grey80", high = "black") +
    
    xlim(statebox$xmin, statebox$xmax) + 
    ylim(statebox$ymin, statebox$ymax) + 
    
    labs(title = filter_title, 
         colour = "Boundaries", 
         fill = "Pop. density (log)", 
         shape = "Urban centres") +
    
    
    # Cities
    geom_point(
      data = cityfilter,
      aes(x = LON_CITY,
          y = LAT_CITY,
          shape = URB_CENTRES),
      fill = "purple",
      colour = "white",
      size = 1 * size_scalar,
      alpha = 0.8
    ) +
    scale_discrete_manual(aesthetics = "shape", values = c(3, 4, 1, 10, 12, 11)) +
    {
      if (filter_state == TRUE)
        geom_text_repel(
          data = cityfilter,
          aes(
            x = LON_CITY,
            y = LAT_CITY,
            label = str_to_title(CITYNAME)
          ),
          colour = "white",
          size = 1.5 * size_scalar
        )
    } +
    
    # Urban extent
    geom_sf(data = extentfilter, aes(colour = "Urban area"), fill = NA, 
            size = 0.35*size_scalar) + 
    
    # Rural vs. urban areas with colour in ur_colour above
    geom_sf(data = ACfilter, aes(colour = REGION_TYPE), fill = NA, 
            size = 0.3*size_scalar) +
    
    # AC_NO labelling
    geom_sf_text(data = ACfilter, aes(label = AC_NO, colour = REGION_TYPE), 
                 size = 1.5*size_scalar) + 
    
    # District borders and labels
    geom_sf(data = distmapfilter, aes(colour = "District"), fill = NA, 
            size = 0.3*size_scalar, 
            linetype = "33") + 
            
    # Manual scale affecting all previous geom_sf with aes "colour"
    scale_colour_manual(values = c("Urban area" = "red", 
                                   "Urban" = "darkgoldenrod3", 
                                   "Rural" = "green", 
                                   "District" = "blue"), 
                        labels = c("District" = "District", 
                                   "Urban" = "Mostly urban constituency",
                                   "Rural" = "Mostly rural constituency", 
                                   "Urban area" = "Urban area")
                        
    ) +
    
    {
      if (filter_district == TRUE)
        geom_text_repel(
          data = distmapfilter,
          aes(x = lon_dist, y = lat_dist, label = DIST_NAME),
          size = 1.5 * size_scalar,
          colour = "blue"
        )
    } +
    # Themes & guides
    theme_minimal() +
    {
      if (!is.null(leg_x) && !is.null(leg_y))
        theme(
          legend.position = c(leg_x, leg_y),
          legend.justification = leg_just,
          legend.text = element_text(size = 3 * legend_scalar),
          legend.title = element_text(size = 3.5 * legend_scalar),
          legend.margin = margin(
            1 * legend_scalar,
            1 * legend_scalar,
            1 * legend_scalar,
            2 * legend_scalar
          ),
          legend.key.width = unit(6, "cm"),
          legend.spacing = unit(0, "pt")
        )
    } + {if (legend_omit) theme(legend.position = "none")} +
    theme(
      panel.background = element_rect(fill = "cyan3"),
      panel.border = element_rect(fill = NA, colour = "transparent"),
      panel.grid = element_blank(),
      panel.grid.major = element_line(colour = "transparent"),
      panel.grid.minor = element_line(colour = "transparent"),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.box.background = element_rect(fill = legend_fill, colour = "black")
    ) +
    guides(
      # To order the appearance of
      colour = guide_legend(
        order = 1,
        keyheight =  unit(1.618 * legend_scalar, "pt"),
        keywidth = unit(1 * legend_scalar, "pt")
      ),
      shape = guide_legend(
        order = 2,
        keyheight = unit(0.5 * legend_scalar, "pt"),
        keywidth = unit(0.3 * legend_scalar, "pt")
      ),
      fill = guide_colourbar(
        order = 3,
        barwidth = 0.3 * legend_scalar,
        barheight = 1.25 * legend_scalar
      )
    )
  
}
