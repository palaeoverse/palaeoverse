### In tetrapod-biodiversity.Rmd, we store the output of palaeorotate() in RDS files (see
### motivation in https://github.com/palaeoverse/palaeoverse/pull/224).
###
### The risk is that those stored results become outdated. The objective of this script
### is to run the palaeorotate() calls to get the latest results and compare them to the
### stored output. This throws an error if there are any differences.

library(here)
devtools::load_all()

### All modifications applied to `tetrapods` in the vignette
data(tetrapods)
bins <- time_bins(
     interval = c("Carboniferous", "Permian"),
     rank = "stage",
     scale = "GTS2012",
     plot = FALSE
)
tetrapods <- look_up(tetrapods, int_key = interval_key)

tetrapods$interval_max_ma <- ifelse(
     is.na(tetrapods$interval_max_ma),
     tetrapods$max_ma,
     tetrapods$interval_max_ma
)
tetrapods$interval_min_ma <- ifelse(
     is.na(tetrapods$interval_min_ma),
     tetrapods$min_ma,
     tetrapods$interval_min_ma
)
tetrapods$interval_mid_ma <- (tetrapods$min_ma + tetrapods$max_ma) / 2

colnames(tetrapods)[9:10] <- c("old_max_ma", "old_min_ma")
colnames(tetrapods)[c(35, 37)] <- c("max_ma", "min_ma")

cp_tetrapods <- subset(tetrapods, min_ma > min(bins$min_ma))

maj_tetrapods <- bin_time(
     occdf = cp_tetrapods,
     bins = bins,
     method = "majority"
)


### Original call (as displayed in the vignette)
maj_tetrapods_new <- palaeorotate(
     occdf = maj_tetrapods,
     age = "bin_midpoint",
     method = "point",
     model = "PALEOMAP"
)


### Results that we previously stored
maj_tetrapods_stored <- readRDS(
     here("vignettes/_data/palaeorotate_point_paleomap_tetrapods.rds")
)

if (!identical(maj_tetrapods_new, maj_tetrapods_stored)) {
     stop(
          'Results for the following call have changed compared to stored results:

palaeorotate(occdf = maj_tetrapods, age = "bin_midpoint", method = "point", model = "PALEOMAP")'
     )
}
