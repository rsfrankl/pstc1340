library(sf)
library(ggplot2)
sf_use_s2(FALSE)  # planar clip in lon/lat

# ---- data ---------------------------------------------------------------
gpkg  <- "data/world_boundaries.gpkg"
layer <- "World Bank Official Boundaries - Admin 0_all_layers"
world <- st_make_valid(st_read(gpkg, layer = layer, quiet = TRUE))

# ---- colors (from gis_style.scss) ---------------------------------------
col_bg   <- "#fbfbfb"  # $body-bg      — ocean / panel
col_land <- "#4e5767"  # heading green — land fill
col_edge <- "gray"  # country borders (thin, = bg)
col_grat <- "#42affa"  # $body-color   — graticule

# ---- projection ---------------------------------------------------------
crs_merc <- "+proj=merc +lon_0=0 +datum=WGS84 +units=m +no_defs"

# Clip to +/-85 lat BEFORE projecting, or Mercator runs to infinity at the poles
bbox_ll <- st_bbox(c(xmin = -180, ymin = -85, xmax = 180, ymax = 85),
                   crs = st_crs(4326))
world_m <- st_transform(st_crop(world, bbox_ll), crs_merc)

grat <- st_graticule(lon = seq(-180, 180, 30), lat = seq(-90, 90, 30),
                     crs = st_crs(4326))
grat <- st_transform(st_crop(grat, bbox_ll), crs_merc)
world_m <- st_simplify(world_m, dTolerance = 10000)  # 10 km tolerance, in meters
lim  <- st_bbox(grat)
# ---- Tissot indicatrices: true circles on the sphere --------------------
ind_radius <- 500000  # great-circle radius of each circle, in meters (500 km)
ind_fill   <- "#C4704A"  # clay — indicatrix fill
ind_edge   <- "#8a3f22"  # darker clay — indicatrix outline

# grid of centre points in lon/lat (stay well clear of the poles)
centres <- expand.grid(lon = seq(-150, 150, 60),
                       lat = seq(-60, 60, 30))
centres <- st_as_sf(centres, coords = c("lon", "lat"), crs = 4326)

# buffer on the SPHERE (s2 on) so radius is a true distance, giving real circles
sf_use_s2(TRUE)
circles_ll <- st_buffer(centres, dist = ind_radius)
sf_use_s2(FALSE)

# reproject with everything else — this is what deforms them
circles_m <- st_transform(circles_ll, crs_merc)

# ---- plot ---------------------------------------------------------------
ggplot() +
  geom_sf(data = world_m, fill = col_land, colour = col_edge, linewidth = 0.1) +
  geom_sf(data = circles_m, fill = ind_fill, colour = ind_edge,
          linewidth = 0.3, alpha = 0.7) +
  geom_sf(data = grat,    colour = col_grat, linewidth = 0.3) +
  coord_sf(xlim = c(lim["xmin"], lim["xmax"]),
           ylim = c(lim["ymin"], lim["ymax"]), expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = col_bg, colour = NA),
        plot.background  = element_rect(fill = col_bg, colour = NA))