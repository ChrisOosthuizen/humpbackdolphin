# ============================================================
# Combine maps with magick
# ============================================================
library(magick)

mapA <- image_read("./figures/Figure1A.png")
mapB <- image_read("./figures/Figure1B.png")
mapC <- image_read("./figures/Figure1C.png")

mapB <- image_trim(mapB)   # only B gets full auto-trim

dpi <- image_info(mapB)$density
dpi <- ifelse(is.na(dpi), 72, as.numeric(strsplit(dpi, "x")[[1]][1]))
margin_px  <- round(0.3 * dpi / 2.54)   # reduced gap — adjust to taste
label_size <- round(dpi * 1.25)
infoB <- image_info(mapB)

#-----------------------------------
# Trim fixed % of white off LEFT and RIGHT only (A and C)
#-----------------------------------
trim_pct <- 0.2   # trim 5% off each side — adjust to taste

crop_width_A  <- round(image_info(mapA)$width * (1 - 2 * trim_pct))
crop_offset_A <- round(image_info(mapA)$width * trim_pct)
mapA <- image_crop(mapA, paste0(crop_width_A, "x", image_info(mapA)$height, "+", crop_offset_A, "+0"))

crop_width_C  <- round(image_info(mapC)$width * (1 - 2 * trim_pct))
crop_offset_C <- round(image_info(mapC)$width * trim_pct)
mapC <- image_crop(mapC, paste0(crop_width_C, "x", image_info(mapC)$height, "+", crop_offset_C, "+0"))

#-----------------------------------
# Resize A and C — increase scale_factor to make top panels larger
#-----------------------------------
scale_factor <- 0.50
col_width <- round(infoB$width * scale_factor)

mapA_small <- image_resize(mapA, paste0(col_width))
mapC_small <- image_resize(mapC, paste0(col_width))

row_height <- image_info(mapA_small)$height

mapA_centered <- image_background(mapA_small, "white", flatten = TRUE)
mapC_centered <- image_background(mapC_small, "white", flatten = TRUE)

#-----------------------------------
# Add panel labels
#-----------------------------------
mapA_labeled <- image_annotate(
  mapA_centered, text = "A", gravity = "northwest",
  location = paste0("+", margin_px/3, "+", margin_px/3),
  size = label_size, font = "Arial", color = "black", weight = 400
)
mapC_labeled <- image_annotate(
  mapC_centered, text = "C", gravity = "northwest",
  location = paste0("+", margin_px/3, "+", margin_px/3),
  size = label_size, font = "Arial", color = "black", weight = 400
)
mapB_labeled <- image_annotate(
  mapB, text = "B", gravity = "northwest",
  location = paste0("+", margin_px/3, "+", margin_px/3),
  size = label_size, font = "Arial", color = "black", weight = 400
)

#-----------------------------------
# Combine A + C into a top row (horizontal gap between them)
#-----------------------------------
gap_h   <- image_blank(margin_px, row_height, color = "white")
top_row <- image_append(c(mapA_labeled, gap_h, mapC_labeled), stack = FALSE)
top_row_width <- image_info(top_row)$width

#-----------------------------------
# Pad B out to match the (now wider) top row width, centered
#-----------------------------------
mapB_labeled <- image_extent(mapB_labeled,
                             geometry = paste0(top_row_width, "x", image_info(mapB_labeled)$height),
                             gravity  = "center")
mapB_labeled <- image_background(mapB_labeled, "white", flatten = TRUE)

#-----------------------------------
# Stack top row above B (vertical gap)
#-----------------------------------
gap_v   <- image_blank(top_row_width, margin_px, color = "white")
stacked <- image_append(c(top_row, gap_v, mapB_labeled), stack = TRUE)

final <- image_border(stacked, color = "white",
                      geometry = paste0(margin_px, "x", margin_px))
#print(final)
image_write(final, "./figures/Figure1.png")
