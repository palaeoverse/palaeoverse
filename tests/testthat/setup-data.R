# set up data to test plotting
stages <- subset(GTS2020, rank == "stage")
periods <- subset(GTS2020, rank == "period")
epochs <- subset(GTS2020, rank == "epoch")

reef_df <- look_up(occdf = reefs, early_interval = "interval",
                   late_interval = "interval", int_key = interval_key)
reef_df <- palaeorotate(occdf = reef_df, age = "interval_mid_ma")
