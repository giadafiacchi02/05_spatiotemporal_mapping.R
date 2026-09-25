################################################################################
# FUEL PRICES ANALYSIS
# 05_spatiotemporal_mapping.R
#
# What this script does:
#   1) Loads the spatial dataset (s, pts_inside) from 01_data_preparation.R
#      and the daily price matrix (st_daily) from 03_spatiotemporal_matrix.R
#   2) Computes mean E10 price per station before/after the oil-shock
#      structural break identified in 04_price_eda_crude_oil.R
#   3) Flags stations that "reacted" to the shock (price increase above
#      a chosen threshold)
#   4) Produces static maps: PRE vs POST prices, price difference, and
#      reacted vs not-reacted stations
#
# Required input:
#   - spatial_matrices.RData          (contains: s, pts_inside, sp)
#     produced by 01_data_preparation.R
#   - spatio_temporal_matrices.RData  (contains: stdat, stdat_filled,
#                                      stdat_trimmed, st_daily)
#     produced by 03_spatiotemporal_matrix.R
################################################################################

# ------------------------------------------------------------
# 0. SETUP — load spatial objects and spatio-temporal matrix
# ------------------------------------------------------------
setwd("C:/Users/giada/OneDrive/Documenti/Desktop/RCODES THESIS")
load("spatial_matrices.RData")          # loads s, pts_inside, sp
load("spatio_temporal_matrices.RData")  # loads stdat, stdat_filled, stdat_trimmed, st_daily

library(sf)
library(ggplot2)
library(leaflet)
library(dplyr)

# Structural break date (identified in 04_price_eda_crude_oil.R)
break_date <- as.Date("2026-03-01")

# ============================================================
# STEP 1 — COMPUTE PRE / POST MEAN PRICES PER STATION
# ============================================================

dates_daily <- as.Date(rownames(st_daily))

# Boolean index for the two periods
pre_idx  <- dates_daily <  break_date
post_idx <- dates_daily >= break_date

# Column-wise mean for each period (ignoring NAs)
mean_pre  <- colMeans(st_daily[pre_idx,  , drop = FALSE], na.rm = TRUE)
mean_post <- colMeans(st_daily[post_idx, , drop = FALSE], na.rm = TRUE)

# Price difference (post - pre): positive = price increased after shock
price_diff <- mean_post - mean_pre

# ============================================================
# STEP 2 — IDENTIFY STATIONS THAT "REACTED" TO THE OIL SHOCK
# ============================================================

# A station is considered to have reacted if its price increased
# by more than a chosen threshold (here: 1 pence, adjust as needed)
reaction_threshold <- 1.0   # pence

reacted <- price_diff > reaction_threshold

cat("=== Reaction to Oil Shock ===\n")
cat("Total stations analysed: ", length(reacted), "\n")
cat("Stations that REACTED   : ", sum(reacted, na.rm = TRUE),
    paste0("(", round(mean(reacted, na.rm = TRUE) * 100, 1), "%)"), "\n")
cat("Stations that did NOT react:", sum(!reacted, na.rm = TRUE),
    paste0("(", round(mean(!reacted, na.rm = TRUE) * 100, 1), "%)"), "\n")

# ============================================================
# STEP 3 — ATTACH COMPUTED METRICS TO THE SPATIAL OBJECT
# ============================================================

# pts_inside may contain multiple rows per station (one per timestamp).
# We first reproject to WGS84 so that coordinates match those stored
# as column names in st_daily (built from dat$forecourts.location.lon/lat),
# then deduplicate keeping only one row per station.

pts_wgs84 <- st_transform(pts_inside, 4326)

station_key <- paste(pts_wgs84$trading_name,
                     pts_wgs84$postcode,
                     round(st_coordinates(pts_wgs84)[, 1], 6),  # lon
                     round(st_coordinates(pts_wgs84)[, 2], 6),  # lat
                     sep = "=")

# Keep only the first occurrence of each station key
pts_unique        <- pts_wgs84[!duplicated(station_key), ]
station_key_unique <- station_key[!duplicated(station_key)]

cat("=== Spatial join diagnostics ===\n")
cat("Unique stations in pts_inside:", nrow(pts_unique), "\n")
cat("Match trovati in st_daily:    ", sum(station_key_unique %in% colnames(st_daily)), "\n")

# Attach metrics — NA for stations not present in st_daily
pts_map <- pts_unique %>%
  mutate(
    mean_pre   = mean_pre  [match(station_key_unique, names(mean_pre))],
    mean_post  = mean_post [match(station_key_unique, names(mean_post))],
    price_diff = price_diff[match(station_key_unique, names(price_diff))],
    reacted    = reacted   [match(station_key_unique, names(reacted))]
  )

cat("Stazioni con mean_pre  non-NA:", sum(!is.na(pts_map$mean_pre)),  "\n")
cat("Stazioni con mean_post non-NA:", sum(!is.na(pts_map$mean_post)), "\n")

# ============================================================
# STATIC MAPS (ggplot2)
# ============================================================

# Helper: common theme
map_theme <- function() {
  theme_minimal() +
    theme(
      plot.title    = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "grey40"),
      legend.position = "right"
    )
}

# ------------------------------------------------------------
# MAP A + B — PRE and POST side by side, SHARED colour scale
# ------------------------------------------------------------
library(gridExtra)
library(grid)

# Shared scale limits across both periods
price_min <- min(c(pts_map$mean_pre, pts_map$mean_post), na.rm = TRUE)
price_max <- max(c(pts_map$mean_pre, pts_map$mean_post), na.rm = TRUE)

map_A <- ggplot() +
  geom_sf(data = s, fill = "whitesmoke", color = "black") +
  geom_sf(data = pts_map %>% filter(!is.na(mean_pre)),
          aes(color = mean_pre), size = 2, alpha = 0.8) +
  scale_color_gradientn(
    colors = c("green", "yellow", "red"),
    limits = c(price_min, price_max),
    name   = "E10 Price (p)"
  ) +
  map_theme() +
  labs(
    title    = "PRE Oil Shock",
    subtitle = paste("Up to", format(break_date - 1, "%d %B %Y")),
    caption  = ""
  )

map_B <- ggplot() +
  geom_sf(data = s, fill = "whitesmoke", color = "black") +
  geom_sf(data = pts_map %>% filter(!is.na(mean_post)),
          aes(color = mean_post), size = 2, alpha = 0.8) +
  scale_color_gradientn(
    colors = c("green", "yellow", "red"),
    limits = c(price_min, price_max),
    name   = "E10 Price (p)"
  ) +
  map_theme() +
  labs(
    title    = "POST Oil Shock",
    subtitle = paste("From", format(break_date, "%d %B %Y")),
    caption  = ""
  )

# Combine: 1 row, 2 columns
grid.arrange(
  map_A, map_B,
  ncol   = 2,
  top    = textGrob(
    "Mean E10 Price \u2014 PRE vs POST Oil Shock",
    gp = gpar(fontsize = 15, fontface = "bold")
  ),
  bottom = textGrob(
    "Source: UK Government Data",
    gp = gpar(fontsize = 9, col = "grey40")
  )
)

# ------------------------------------------------------------
# MAP C — Price difference (post - pre)
# Diverging palette: purple → gold (no white)
# ------------------------------------------------------------

# Symmetric colour scale centred on 0
max_abs <- max(abs(pts_map$price_diff), na.rm = TRUE)

ggplot() +
  geom_sf(data = s, fill = "whitesmoke", color = "black") +
  geom_sf(data = pts_map %>% filter(!is.na(price_diff)),
          aes(color = price_diff), size = 2, alpha = 0.8) +
  scale_color_gradientn(
    colors = c("#4B0082", "#9B59B6", "#F39C12", "#E67E22", "#E74C3C"),
    limits = c(-max_abs, max_abs),
    name   = "ΔPrice (p)\n(post − pre)"
  ) +
  map_theme() +
  labs(
    title    = "E10 Price Change: POST minus PRE Oil Shock",
    subtitle = paste("Threshold:", format(break_date, "%d %B %Y"),
                     "| Reaction criterion: Δ >", reaction_threshold, "p"),
    caption  = "Source: UK Government Data"
  )

# ------------------------------------------------------------
# MAP D — Stations that REACTED vs NOT REACTED
# ------------------------------------------------------------
ggplot() +
  geom_sf(data = s, fill = "whitesmoke", color = "black") +
  geom_sf(data = pts_map %>% filter(!is.na(reacted)),
          aes(color = factor(reacted,
                             levels = c(TRUE, FALSE),
                             labels = c("Reacted", "Did not react"))),
          size = 2, alpha = 0.8) +
  scale_color_manual(
    values = c("Reacted" = "red", "Did not react" = "steelblue"),
    name   = "Response to shock"
  ) +
  map_theme() +
  labs(
    title    = "Petrol Stations: Reaction to Oil Shock",
    subtitle = paste0("Reaction = price increase > ", reaction_threshold,
                      " p after ", format(break_date, "%d %B %Y"),
                      " | Reacted: ", sum(reacted, na.rm = TRUE),
                      " / ", length(reacted)),
    caption  = "Source: UK Government Data"
  )
