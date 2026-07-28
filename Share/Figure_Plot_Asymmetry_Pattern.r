# ============================================================
# Plot categorical cerebral asymmetry patterns
# in the left hemisphere using the Desikan–Killiany atlas
# ============================================================

library(ggseg)
library(ggplot2)
library(dplyr)


# ============================================================
# 1. Define cortical regions in the left hemisphere
# ============================================================

region_names <- paste0(
  "lh_",
  c(
    "bankssts",
    "caudalanteriorcingulate",
    "caudalmiddlefrontal",
    "cuneus",
    "entorhinal_DELETE",
    "fusiform",
    "inferiorparietal",
    "inferiortemporal",
    "isthmuscingulate",
    "lateraloccipital",
    "lateralorbitofrontal",
    "lingual",
    "medialorbitofrontal",
    "middletemporal",
    "parahippocampal",
    "paracentral",
    "parsopercularis",
    "parsorbitalis",
    "parstriangularis",
    "pericalcarine",
    "postcentral",
    "posteriorcingulate",
    "precentral",
    "precuneus",
    "rostralanteriorcingulate",
    "rostralmiddlefrontal",
    "superiorfrontal",
    "superiorparietal",
    "superiortemporal",
    "supramarginal",
    "frontalpole_DELETE",
    "temporalpole_DELETE",
    "transversetemporal",
    "insula"
  )
)


# ============================================================
# 2. Provide categorical values for each cortical region
# ============================================================

# The values must correspond to the regions listed in
# region_names in exactly the same order.
#
# Category definitions:
# 1 = No asymmetry
# 2 = Right asymmetry
# 3 = Right-to-left
# 4 = Left asymmetry
# 5 = Left-to-right
# 6 = Unclassified
#
# Replace the example values below with the actual study results.

values <- c(
  1, 2, 3, 4, 5, 6, 1, 2, 3, 4,
  5, 6, 1, 2, 3, 4, 5, 6, 1, 2,
  3, 4, 5, 6, 1, 2, 3, 4, 5, 6,
  1, 2, 3, 4
)


# Check whether the number of values matches the number of regions

if (length(values) != length(region_names)) {
  stop(
    paste0(
      "The number of values is ",
      length(values),
      ", whereas the number of cortical regions is ",
      length(region_names),
      ". The two lengths must be identical."
    )
  )
}


# Check whether all values belong to the predefined categories

if (!all(values %in% 1:6)) {
  stop(
    "The values vector may contain only the category codes 1 to 6."
  )
}


# ============================================================
# 3. Define colors and legend labels
# ============================================================

color_map <- c(
  "1" = "#677D9C",
  "2" = "#88CEE6",
  "3" = "#CABAF9",
  "4" = "#FFD19D",
  "5" = "#ED8687",
  "6" = "#91AD5A"
)


label_map <- c(
  "1" = "No asymmetry",
  "2" = "Right asymmetry",
  "3" = "Right-to-left",
  "4" = "Left asymmetry",
  "5" = "Left-to-right",
  "6" = "Unclassified"
)


# ============================================================
# 4. Create the regional data frame
# ============================================================

brain_data <- data.frame(
  label = region_names,
  value = factor(
    as.character(values),
    levels = names(color_map)
  )
)


# Regions with the suffix "_DELETE" are excluded from plotting.
# Their corresponding positions are retained in the original values
# vector so that the input order remains consistent with the full
# Desikan–Killiany region list.

brain_data <- brain_data %>%
  filter(!grepl("_DELETE$", label))


# ============================================================
# 5. Extract the left-hemisphere atlas data
# ============================================================

dk_left <- ggseg::dk$data %>%
  filter(hemi == "left") %>%
  left_join(
    brain_data,
    by = "label"
  )


# ============================================================
# 6. Plot the categorical asymmetry patterns
# ============================================================

p <- ggplot(dk_left) +
  geom_sf(
    aes(fill = value),
    color = "white",
    linewidth = 0.25
  ) +
  scale_fill_manual(
    values = color_map,
    breaks = names(label_map),
    labels = label_map,
    drop = FALSE,
    na.value = "grey90",
    name = NULL
  ) +
  coord_sf(datum = NA) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(
      size = 11,
      family = "Arial"
    ),
    legend.key.height = grid::unit(0.65, "cm"),
    legend.key.width = grid::unit(0.65, "cm"),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    )
  )


# Display the figure

print(p)


# ============================================================
# 7. Save the figure
# ============================================================

ggsave(
  filename = "left_hemisphere_asymmetry_patterns.png",
  plot = p,
  width = 8,
  height = 4.5,
  units = "in",
  dpi = 300,
  bg = "white"
)