# A description of the early tetrapod datasets provided for testing functions:

# Both were initially downloaded from the Paleobiology Database on 14th July 2022
# Both datasets are for the clade 'Tetrapoda', covering a time interval from the
#  Carboniferous to the Early Triassic. These data are relatively well curated
#  (i.e. complete with respect to the literature).

# There are two files, one which is semi-cleaned, and the other which is almost
#  raw.

# early_tetrapods_min.csv is the semi-cleaned version. It includes only body
#  fossils, does not include any identifiers which indicate uncertainty (e.g.
#  cf., aff. ?), and only includes fossils from terrestrial environments.
# Only columns which are likely to be commonly used in analyses are included.

# early_tetrapods_max.csv is 'hard mode'. It has fossils of all preservation
#  types, fossils identified with uncertainty, and all preservational
#  environments. Only columns containing freeform comments have been removed
#  (and that pesky repeated 'cc'/country column).

# The semi-cleaned dataset is probably the best one to write your function
#  around, while the more raw one may be useful for accessing unusual data types
#  and testing your functions on a wider range of expected inputs.
