# set up data to test plotting
periods <- subset(GTS2020, rank == "period")
epochs <- subset(GTS2020, rank == "epoch")

reef_df <- look_up(
  occdf = reefs,
  early_interval = "interval",
  late_interval = "interval",
  int_key = interval_key
)
