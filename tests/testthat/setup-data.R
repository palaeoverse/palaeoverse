# set up data to test plotting
if(suppressPackageStartupMessages(require(divDyn, quietly = TRUE))) {
  stages <- deeptime::stages
  periods <- deeptime::periods
  data(corals)
  corals_stages_clean <- subset(corals, stage != "")
  coral_div <- aggregate(cbind(n = genus) ~ stage, data = corals_stages_clean,
                         FUN = function(x) length(x))
  coral_div$stage_age = (stages$max_age[match(coral_div$stage, stages$name)] +
                           stages$min_age[match(coral_div$stage, stages$name)])/2

  coral_div_diet <- aggregate(cbind(n = genus) ~ stage + diet, data = corals_stages_clean,
                               FUN = function(x) length(x))
  coral_div_diet$stage_age = (stages$max_age[match(coral_div_diet$stage, stages$name)] +
                                stages$min_age[match(coral_div_diet$stage, stages$name)])/2

  corals_periods_clean <- subset(corals, period != "")
  coral_div_dis <- aggregate(cbind(n = genus) ~ period + diet, data = corals_periods_clean,
                             FUN = function(x) length(x))
  coral_div_dis$period_age = (periods$max_age[match(coral_div_dis$period, periods$name)] +
                               periods$min_age[match(coral_div_dis$period, periods$name)])/2
  coral_div_dis <- coral_div_dis[rev(order(coral_div_dis$period_age)),, drop = FALSE]
}
