#!/usr/bin/env Rscript
# Compute beeswarm coordinates for Queens census-tract obesity rates,
# highlighting Meadowmere (Queens Tract 664.01, FIPS 36081066401), and
# export a Datawrapper-ready scatter CSV + a preview PNG.
#
# Run from the project root:  Rscript data/make_beeswarm.R

suppressPackageStartupMessages({
  library(beeswarm)
  library(ggplot2)
  library(ggbeeswarm)
  library(readr)
})

infile  <- "data/queens_obesity_beeswarm.csv"
outfile <- "data/queens_beeswarm_coords.csv"
preview <- "data/queens_beeswarm_preview.png"

df <- read_csv(infile, show_col_types = FALSE)
names(df)[names(df) == "Obesity rate"] <- "obesity"

# --- Compute swarm x-offsets with ggbeeswarm via ggplot_build ---
# Build a geom_beeswarm layer, then read back the computed x/y coordinates.
# PositionBeeswarm keeps rows in input order, so we can bind straight back
# onto df (asserted below).
p_calc <- ggplot(df, aes(x = 0, y = obesity)) +
  geom_beeswarm(cex = 1.6, method = "swarm", size = 1.6)
bd <- ggplot_build(p_calc)$data[[1]]
stopifnot(nrow(bd) == nrow(df),
          identical(round(bd$y, 6), round(df$obesity, 6)))

df$swarm_x <- bd$x - mean(range(bd$x))   # recenter the swarm around 0

# Column order Datawrapper likes: x, y, then metadata
out <- data.frame(
  swarm_x      = round(df$swarm_x, 4),
  obesity      = df$obesity,
  Tract        = df$Tract,
  GEOID        = df$GEOID,
  Highlight    = df$Highlight,
  stringsAsFactors = FALSE
)
write_csv(out, outfile)
cat("Wrote", outfile, "-", nrow(out), "points\n")

# --- Preview PNG (vertical beeswarm, obesity on Y) ---
pal <- c("Meadowmere" = "#f05349", "Other Queens tracts" = "#cfcfcf")
p <- ggplot(out, aes(x = swarm_x, y = obesity, color = Highlight,
                     size = Highlight)) +
  geom_point(alpha = 0.9) +
  scale_color_manual(values = pal) +
  scale_size_manual(values = c("Meadowmere" = 3.2, "Other Queens tracts" = 1.6),
                    guide = "none") +
  labs(title = "Adult obesity across Queens census tracts",
       subtitle = "Each dot = one tract; Meadowmere (Tract 664.01) in red",
       y = "Adult obesity rate (%)", x = NULL, color = NULL) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), legend.position = "top")
ggsave(preview, p, width = 6, height = 7, dpi = 130)
cat("Wrote", preview, "\n")
