# Generate h3_info_table (not exported from h3jsr package)
library(devtools)
library(h3jsr)
h3_info_table <- h3jsr::h3_info_table
use_data(h3_info_table, internal = TRUE, overwrite = TRUE)
