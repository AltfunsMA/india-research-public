## R 4.0.2
## Alfonso Martinez Arranz 
# Last update 12 Jan 2021
# Compute length of perimeter touching other district regions
# Can only be executed as part of the 08_expec_final.R file

cat("Calculating...\n")
cat("...which district-regions touch which other district-regions\n")

touch_list <- st_touches(B3_CPACUR_map) %>% 
  suppressMessages()


cat("...perimeter length across of all district-regions\n")

perimeters <- suppressMessages(st_length(B3_CPACUR_map))


cat("...what percentages of perimeter touch which another district-region\n") #-----

## Converted lapply into for loop for easier debugging. Efficiency gain not worth it

all.length.list <- list()


for (from in 1:length(touch_list)) {
  
  lines <- suppressMessages(st_intersection(B3_CPACUR_map[from,],
                           B3_CPACUR_map[touch_list[[from]],]))


  lines <- st_cast(lines) # In case of multiple geometries

  l_lines <- st_length(lines)

  # Handle zero-length results for isolated/poorly matching polygons
  zl <- function(v)  {
    
    if (length(v) == 0) 0 else v
    
  }

  all.length.list[[from]] <- data.frame(origin = from,
                    perimeter = as.vector(perimeters[from]),
                    touching = zl(touch_list[[from]]),
                    t.length = zl(as.vector(l_lines)),
                    t.pc = zl(as.vector(100*l_lines/perimeters[from])))


}


all.length.df <- do.call("rbind", all.length.list)

# Each polygon must touch more than one other region
stopifnot(nrow(all.length.df) > nrow(B3_CPACUR_map))

write_csv(all.length.df, touch_df_path)
