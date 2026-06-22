### In phanerozoic-reefs.Rmd, we store the output of palaeorotate() in RDS files (see
### motivation in https://github.com/palaeoverse/palaeoverse/pull/224).
###
### The risk is that those stored results become outdated. The objective of this script
### is to run the palaeorotate() calls to get the latest results and compare them to the
### stored output. This throws an error if there are any differences.

library(here)
devtools::load_all()
source(here("vignettes/check-rds-helpers.R"))

data(reefs)
data(interval_key)

### All modifications applied to `reefs` in the vignette
reefs <- look_up(
  occdf = reefs,
  early_interval = "interval",
  late_interval = "interval",
  int_key = interval_key
)

reefs <- subset(reefs, interval_max_ma <= 541)
bins <- time_bins(interval = "Phanerozoic", rank = "stage")
colnames(reefs)[which(colnames(reefs) == "interval_max_ma")] <- "max_ma"
colnames(reefs)[which(colnames(reefs) == "interval_min_ma")] <- "min_ma"
reefs <- bin_time(occdf = reefs, bins = bins, method = "all")

### Original call (as displayed in the vignette)
reefs_new <- fetch_or_skip(palaeorotate(
  occdf = reefs,
  age = "bin_midpoint",
  method = "point",
  model = "PALEOMAP"
))

### Results that we previously stored
reefs_stored <- readRDS(here("vignettes/_data/palaeorotate_point_paleomap.rds"))

if (!identical(reefs_new, reefs_stored)) {
  stop(
    'Results for the following call have changed compared to stored results:

palaeorotate(occdf = reefs, age = "bin_midpoint", method = "point", model = "PALEOMAP")'
  )
}


### Original call (as displayed in the vignette)
models <- c("GOLONKA", "PALEOMAP", "MERDITH2021")
reefs_new_2 <- fetch_or_skip(palaeorotate(
  # Difference with vignette:
  # in the vignette, we use `occdf = reefs` because the results of the
  # previous palaeorotate() calls are assigned to `reefs` while they are
  # assigned to `reefs_new` here.
  occdf = reefs_new,
  age = "bin_midpoint",
  method = "point",
  model = models
))

### Results that we previously stored
reefs_stored_2 <- readRDS("vignettes/_data/palaeorotate_point_multi_models.rds")

if (!identical(reefs_new_2, reefs_stored_2)) {
  stop(
    'Results for the following call have changed compared to stored results:

palaeorotate(occdf = reefs, age = "bin_midpoint", method = "point", model = models)'
  )
}
