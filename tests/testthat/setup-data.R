# set up data to test plotting
if (suppressPackageStartupMessages(require(divDyn, quietly = TRUE))) {
  stages <- subset(GTS2020, rank == "stage")
  periods <- subset(GTS2020, rank == "period")
  epochs <- subset(GTS2020, rank == "epoch")
  data(corals)
  corals_stages_clean <- subset(corals, stage != "")
  coral_div <- aggregate(cbind(n = genus) ~ stage,
                         data = corals_stages_clean,
                         FUN = function(x) length(x))
  coral_div$stage_age <-
    (stages$max_ma[match(coral_div$stage, stages$interval_name)] +
       stages$min_ma[match(coral_div$stage, stages$interval_name)]) / 2
}
