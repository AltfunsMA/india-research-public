## R 3.6.2.
## Alfonso Martinez Arranz 
## 18 March 2020
## MEMSI-India
## Creating clean post-2008 Assembly constituency map 
# for use in the 06CPACUR_cen_geo file

library(cptools)

AC_post08delim <- st_read('Original/ACs/post_2008/AC_post08delim.shp')

find_acno_incomplete(AC_post08delim)

original_dupes <- duplicheck(AC_post08delim) # Some ACs do cut across district boundaries

merge_dupes <- function(st_name, ac_no, dist_name) {
  
AC_post08delim %>% 
  filter(ST_NAME == str_to_upper(st_name), AC_NO == ac_no) %>% 
  mutate(DIST_NAME = str_to_upper(dist_name)) %>% 
  group_by(ST_NAME, AC_NO, DIST_NAME, AC_NAME, PC_NAME, PC_ID) %>% 
  summarise(Shape_Area = sum(Shape_Area))
  
}

dupefix <- list(c("Kerala", "Rajasthan"), 
     c(87, 17), 
     c("Ernakulam", "Bikaner"))

merged <- pmap(dupefix, ~merge_dupes(..1,..2,..3)) %>% 
  do.call(rbind, .)
  
deduped_main <-   AC_post08delim %>% 
  filter(!(ST_NAME == "KERALA" & AC_NO == 87),
         !(ST_NAME == "RAJASTHAN" & AC_NO == 17)) %>% 
  add_row(merged)

clean <- deduped_main %>% 
  filter(AC_NO != 0) %>% 
  filter(!duplicated(data.frame(ST_NAME, DIST_NAME, AC_NO, AC_NAME))) %>% 
  mutate(
    lon_AC = map_dbl(geometry, ~st_centroid(.x)[[1]]),
    lat_AC = map_dbl(geometry, ~st_centroid(.x)[[2]]))


clean_dupes <- duplicheck(clean)

clean %>% 
  include_states("Rajasthan") %>% 
  create_india_map(fill = "DIST_NAME", fill_var_text = "AC_NO")



# (COMMENTED OUT) Testing duplicates and 0 values ------------------------------


# 
# c("JAMMU & KASHMIR", "MAHARASHTRA", "GUJARAT", "ASSAM")
# 
# for (i in unique(AC_map_problem$DIST_NAME)) {
#   
#   AC_maporiginal <- AC_map_original %>% 
#     filter(DIST_NAME == i) %>% 
#     ggplot() +
#     geom_sf(colour = "blue", fill = "yellow", alpha = 0.5) +
#     geom_text(aes(x = lon, y = lat, label = AC_NO))
#   
#   ggsave(AC_maporiginal, filename = paste0("MapsOutput/AC issues/", 
#                                            str_replace(i, "\\s\\*", ""), "_originalAC.png"), dpi = 800)
#   
#   AC_mapclean <- AC_map_clean %>% 
#     filter(DIST_NAME == i) %>% 
#     ggplot() +
#     geom_sf(colour = "blue", fill = "yellow", alpha = 0.5) +
#     geom_text(aes(x = lon, y = lat, label = AC_NO))
#   
#   ggsave(AC_mapclean, filename = paste0("MapsOutput/AC issues/", 
#                                         str_replace(i, "\\s\\*", ""), "_cleanAC.png"), dpi = 800)
#   
# }


# Fixing discrepancies between AC map dataset and other datasets ----------

AC_map2008_ugly_sikkim <- clean %>% 
  arrange(ST_NAME, AC_NO) %>% 
  mutate_at(vars(ST_NAME, DIST_NAME), str_to_title) %>% # With CP and AC elections
  dplyr::select(-OBJECTID, -ST_CODE, -DT_CODE, -AC_NAME, -PC_NO, -STATUS) %>% # simplifying
  mutate(DIST_NAME = if_else(ST_NAME == "Delhi", 
                             str_replace(PC_NAME, "(.*)(\\sDELHI.*)", "\\1"),  # With CP + GADM
                             as.character(DIST_NAME))
  ) %>% 
  mutate(ST_NAME = recode(ST_NAME, "Uttarkhand" = "Uttarakhand", "Orissa" = "Odisha"),
         ST_NAME = if_else(ST_NAME == "Andhra Pradesh" & AC_NO < 120, 
                           "Telangana", as.character(ST_NAME)))




# Quick and dirty fix for Sikkm

# In the AC map the boundaries look very odd, perhaps because 
# they follow the valleys, whereas the districts cover uninhabited
# high mountain terrain


gadm_dist <- st_read("Processed/03_admin_maps/GADM/GADM_dist.gpkg")

# NB: this removes AC information but no AC is expected downdstream from CP2014
# Different solution needed for AC2014.

sk <- gadm_dist %>% 
  include_states("Sikkim") %>% 
  st_join(AC_map2008_ugly_sikkim, largest = T) %>% 
  select(-all_of(names(gadm_dist)))
  

AC_map2008 <- AC_map2008_ugly_sikkim %>% 
  exclude_states("Sikkim") %>% 
  rbind(sk)




write_sf(AC_map2008, "Processed/03_admin_maps/clean_post_acmap.gpkg", 
         delete_layer = TRUE)
