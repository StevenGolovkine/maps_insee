################################################################################
# Create nice datasets for France
################################################################################

# Load packages
library(tidyverse)

# Load France shapefile
fr <- rnaturalearth::ne_states("france")

fr <- fr |> select(
    name,
    type,
    region,
    provnum_ne,
    latitude,
    longitude,
    gn_a1_code,
    geometry
) |> 
    mutate(depart_code = str_sub(gn_a1_code, 4))

write_rds(fr, './data/france.rds')
