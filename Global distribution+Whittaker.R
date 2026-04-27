
# ---- Packages ----
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# if (!require(devtools)) install.packages("devtools")
# devtools::install_github("valentinitnelav/plotbiomes")
library(plotbiomes)
library(raster)
library(grid)
library(gtable)
library(cowplot)
library(scales)

#### 0.PARAMETERS ####

# ---- Final figure export size ----
fig_width  <- 16
fig_height <- 6
fig_dpi    <- 1200

# ---- INTERNAL PANEL SIZE (panel only, not whole plot) ----
map_panel_width_cm   <- 19.0
map_panel_height_cm  <- 10.8

whit_panel_width_cm  <- 14.0
whit_panel_height_cm <- 10.8

# ---- Outer placement on final canvas ----
map_x  <- 0.06
map_y  <- 0.06
map_w  <- 0.42
map_h  <- 0.74

whit_x <- 0.56
whit_y <- 0.06
whit_w <- 0.42
whit_h <- 0.74

# ---- Shared size legend placement ----
leg_x <- -0.12
leg_y <- 0.75
leg_w <- 0.56
leg_h <- 0.22

# ---- Panel labels placement ----
tag_a_x  <- 0.005
tag_b_x  <- 0.565
tag_y    <- 0.86
tag_size <- 18
tag_hjust <- 0
tag_vjust <- 1

# ---- Point style ----
size_breaks <- c(1, 5, 10, 20, 30)
size_range  <- c(2, 8)

pt_fill   <- "navy"
pt_color  <- "white"
pt_alpha  <- 0.7
pt_stroke <- 0.5

# ---- Border / axis line style ----
panel_border_lwd <- 0.6
axis_tick_color  <- "black"
axis_tick_lwd    <- 0.5
axis_text_color  <- "black"

# ---- Plot margins around whole plot object ----
# order: top, right, bottom, left
map_plot_margin  <- margin(12, 20, 18, 24)
whit_plot_margin <- margin(4, 4, 4, 4)

# ---- Axis title / text ----
axis_title_size <- 18
axis_text_size  <- 18

ax_title_x_margin <- 6
ax_title_y_margin <- 6
ax_text_x_margin  <- 2
ax_text_y_margin  <- 2

# ---- Manual endpoint labels/ticks for map ----
end_label_size <- 6.5
tick_len_y     <- 2.5
tick_len_x     <- 22.5

x_end_lab_y <- -93
y_end_lab_x <- -203

# ---- Whittaker axis range ----
whit_x_max            <- 30
whit_x_floor_base     <- -20
whit_y_floor          <- 0
whit_y_min_upper_ref  <- 5000   # now in mm

# ---- Whittaker legend (inside panel) ----
whit_leg_pos_x   <- 0.03
whit_leg_pos_y   <- 0.97
whit_leg_just_x  <- 0
whit_leg_just_y  <- 1
whit_leg_key_cm  <- 0.80
whit_leg_title_size <- 14
whit_leg_text_size  <- 10

# ---- Bottom size legend text sizes ----
size_leg_title_size <- 16
size_leg_text_size  <- 12



#### 1. Read and prepare data ####

df <- read_delim("database_versions/Kplant_0.1.5.csv",
                 delim = "\t", escape_double = FALSE, 
                 trim_ws = TRUE)

df <- df %>%
  mutate(
    si_lat = as.numeric(si_lat),
    si_long = as.numeric(si_long),
    pl_species_corrected = trimws(pl_species_corrected)
  ) %>%
  filter(!is.na(si_lat), !is.na(si_long))

site_df <- df %>%
  group_by(si_lat, si_long) %>%
  summarise(
    n_species_site = n_distinct(pl_species_corrected),
    .groups = "drop"
  )


#### 2. Themes ####

theme_map <- theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = panel_border_lwd),
    
    axis.title.x = element_text(size = axis_title_size, margin = margin(t = ax_title_x_margin)),
    axis.title.y = element_text(size = axis_title_size, margin = margin(r = ax_title_y_margin)),
    
    axis.text.x  = element_text(
      size = axis_text_size,
      colour = axis_text_color,
      margin = margin(t = ax_text_x_margin)
    ),
    axis.text.y  = element_text(
      size = axis_text_size,
      colour = axis_text_color,
      margin = margin(r = ax_text_y_margin)
    ),
    
    axis.ticks = element_line(colour = axis_tick_color, linewidth = axis_tick_lwd),
    
    plot.margin = map_plot_margin
  )

theme_whittaker <- theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = panel_border_lwd),
    
    axis.title.x = element_text(size = axis_title_size, margin = margin(t = ax_title_x_margin)),
    axis.title.y = element_text(size = axis_title_size, margin = margin(r = ax_title_y_margin)),
    
    axis.text.x  = element_text(
      size = axis_text_size,
      colour = axis_text_color,
      margin = margin(t = ax_text_x_margin)
    ),
    axis.text.y  = element_text(
      size = axis_text_size,
      colour = axis_text_color,
      margin = margin(r = ax_text_y_margin)
    ),
    
    axis.ticks = element_line(colour = axis_tick_color, linewidth = axis_tick_lwd),
    
    legend.title = element_text(size = whit_leg_title_size),
    legend.text  = element_text(size = whit_leg_text_size),
    
    plot.margin = whit_plot_margin
  )


#### 3. Panel (a): Global site map ####

world <- ne_countries(scale = "medium", returnclass = "sf")

p_map <- ggplot() +
  geom_sf(
    data = world,
    fill = "grey85",
    color = "white",
    linewidth = 0.6
  ) +
  geom_point(
    data = site_df,
    aes(x = si_long, y = si_lat, size = n_species_site),
    shape  = 21,
    fill   = pt_fill,
    colour = pt_color,
    stroke = pt_stroke,
    alpha  = pt_alpha
  ) +
  scale_size_continuous(
    name   = "Species Number",
    range  = size_range,
    breaks = size_breaks
  ) +
  scale_x_continuous(
    breaks = seq(-180, 180, by = 60),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(-90, 90, by = 30),
    expand = c(0, 0)
  ) +
  coord_sf(
    xlim   = c(-200, 200),
    ylim   = c(-90, 90),
    expand = FALSE,
    clip   = "off"
  ) +
  annotate(
    "segment",
    x = -180, xend = -180,
    y = -90,  yend = -90 - tick_len_y,
    colour = axis_tick_color, linewidth = axis_tick_lwd
  ) +
  annotate(
    "segment",
    x = 180, xend = 180,
    y = -90, yend = -90 - tick_len_y,
    colour = axis_tick_color, linewidth = axis_tick_lwd
  ) +
  annotate(
    "segment",
    x = -180, xend = -180 - tick_len_x,
    y = -90,  yend = -90,
    colour = axis_tick_color, linewidth = axis_tick_lwd
  ) +
  annotate(
    "segment",
    x = -180, xend = -180 - tick_len_x,
    y = 90,   yend = 90,
    colour = axis_tick_color, linewidth = axis_tick_lwd
  ) +
  annotate(
    "text",
    x = -180, y = x_end_lab_y, label = "180°W",
    size = end_label_size, colour = axis_text_color,
    hjust = 0.5, vjust = 1
  ) +
  annotate(
    "text",
    x = 180, y = x_end_lab_y, label = "180°E",
    size = end_label_size, colour = axis_text_color,
    hjust = 0.5, vjust = 1
  ) +
  annotate(
    "text",
    x = y_end_lab_x, y = -90, label = "90°S",
    size = end_label_size, colour = axis_text_color,
    hjust = 1, vjust = 0.5
  ) +
  annotate(
    "text",
    x = y_end_lab_x, y = 90, label = "90°N",
    size = end_label_size, colour = axis_text_color,
    hjust = 1, vjust = 0.5
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_map +
  guides(size = "none")


#### 4. Panel (b): Whittaker biome plot ####

path <- system.file("extdata", "temp_pp.tif", package = "plotbiomes")
temp_pp <- raster::stack(path)
names(temp_pp) <- c("temperature", "precipitation")

pts_sf <- st_as_sf(site_df, coords = c("si_long", "si_lat"), crs = 4326)
points <- as(pts_sf, "Spatial")

extractions <- raster::extract(temp_pp, points, df = TRUE)
extractions$temperature   <- extractions$temperature / 10
extractions$precipitation <- as.numeric(extractions$precipitation)   # keep mm

extractions <- cbind(
  site_df,
  extractions[, c("temperature", "precipitation")]
)

whit_x_min <- floor(min(whit_x_floor_base, extractions$temperature, na.rm = TRUE) / 5) * 5
whit_y_max <- ceiling(max(whit_y_min_upper_ref, extractions$precipitation, na.rm = TRUE) / 100) * 100

p_whittaker <- ggplot() +
  geom_polygon(
    data = Whittaker_biomes,
    aes(x = temp_c, y = precp_cm * 10, fill = biome),   # convert cm to mm
    colour = "gray98",
    linewidth = 0.8
  ) +
  geom_point(
    data = extractions,
    aes(x = temperature, y = precipitation, size = n_species_site),
    shape  = 21,
    fill   = pt_fill,
    colour = pt_color,
    stroke = pt_stroke,
    alpha  = pt_alpha
  ) +
  scale_fill_manual(
    name   = "Whittaker biomes",
    breaks = names(Ricklefs_colors),
    labels = names(Ricklefs_colors),
    values = Ricklefs_colors
  ) +
  scale_size_continuous(
    name   = "Species Number",
    range  = size_range,
    breaks = size_breaks
  ) +
  scale_y_continuous(
    name   = "Precipitation (mm)",
    limits = c(whit_y_floor, whit_y_max),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    name   = expression("Temperature " (degree * C)),
    limits = c(whit_x_min, whit_x_max),
    expand = c(0, 0)
  ) +
  coord_fixed(ratio = 1 / 100, clip = "off") +
  theme_whittaker +
  theme(
    legend.position = c(whit_leg_pos_x, whit_leg_pos_y),
    legend.justification = c(whit_leg_just_x, whit_leg_just_y),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(1, 1, 1, 1),
    legend.key.size = unit(whit_leg_key_cm, "cm")
  ) +
  guides(
    fill = guide_legend(order = 1, ncol = 1),
    size = "none"
  )


#### 5. Shared legend for point size ####

legend_df <- data.frame(
  x = 1,
  y = 1,
  n_species_site = size_breaks
)

p_size_legend <- ggplot(legend_df, aes(x = x, y = y, size = n_species_site)) +
  geom_point(
    shape = 21,
    fill = pt_fill,
    colour = pt_color,
    stroke = pt_stroke,
    alpha = 0,
    show.legend = TRUE
  ) +
  scale_size_continuous(
    name   = "Species Number",
    range  = size_range,
    breaks = size_breaks,
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        shape = 21,
        fill = pt_fill,
        colour = pt_color,
        alpha = 1
      )
    )
  ) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(size = size_leg_title_size),
    legend.text  = element_text(size = size_leg_text_size),
    plot.margin = margin(0, 0, 0, 0)
  )


#### 6. Convert ggplots to gtables and force panel size ####

g_map  <- ggplotGrob(p_map)
g_whit <- ggplotGrob(p_whittaker)

panel_map  <- g_map$layout[g_map$layout$name == "panel", ]
panel_whit <- g_whit$layout[g_whit$layout$name == "panel", ]

g_map$widths[panel_map$l:panel_map$r]     <- unit(map_panel_width_cm,  "cm")
g_whit$widths[panel_whit$l:panel_whit$r]  <- unit(whit_panel_width_cm, "cm")

g_map$heights[panel_map$t:panel_map$b]    <- unit(map_panel_height_cm,  "cm")
g_whit$heights[panel_whit$t:panel_whit$b] <- unit(whit_panel_height_cm, "cm")


#### 7. Final manual composition ####

combined_plot <- ggdraw() +
  draw_grob(
    g_map,
    x = map_x, y = map_y,
    width = map_w, height = map_h
  ) +
  draw_grob(
    g_whit,
    x = whit_x, y = whit_y,
    width = whit_w, height = whit_h
  ) +
  draw_plot(
    p_size_legend,
    x = leg_x, y = leg_y,
    width = leg_w, height = leg_h
  ) +
  draw_label(
    "(a)",
    x = tag_a_x, y = tag_y,
    hjust = tag_hjust, vjust = tag_vjust,
    fontface = "bold", size = tag_size
  ) +
  draw_label(
    "(b)",
    x = tag_b_x, y = tag_y,
    hjust = tag_hjust, vjust = tag_vjust,
    fontface = "bold", size = tag_size
  )

# show
print(combined_plot)

# save
ggsave(
  filename = "plots/Combined_global_Whittaker_panel_fixed.png",
  plot = combined_plot,
  width = fig_width,
  height = fig_height,
  dpi = fig_dpi,
  bg = "white"
)
