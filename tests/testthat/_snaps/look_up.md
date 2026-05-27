# look_up() works

    Code
      look_up("Sakmarian")
    Condition
      Error in `look_up()`:
      ! `occdf` should be a dataframe.

---

    Code
      look_up(occdf[names(occdf) != "early_interval"])
    Condition
      Error in `look_up()`:
      ! `early_interval` needs to match a column name of `occdf`

---

    Code
      look_up(occdf, early_interval = "test")
    Condition
      Error in `look_up()`:
      ! `early_interval` needs to match a column name of `occdf`

---

    Code
      look_up(occdf, late_interval = "test")
    Condition
      Error in `look_up()`:
      ! `late_interval` needs to match a column name of `occdf`

---

    Code
      look_up(occdf, int_key = "Sakmarian")
    Condition
      Error in `look_up()`:
      ! `int_key` should be a dataframe.

---

    Code
      look_up(occdf, int_key = custom_key[names(custom_key) != "interval_name"])
    Condition
      Error in `look_up()`:
      ! `int_key` needs to contain the columns "interval_name",
                 "early_stage" and "late_stage"

---

    Code
      look_up(occdf, int_key = FALSE, assign_with_GTS = FALSE)
    Condition
      Error in `look_up()`:
      ! assignment with GTS needs to be enabled if `int_key` is set to `FALSE`

---

    Code
      look_up(occdf[1:10, ], int_key = interval_key)
    Condition
      Error in `look_up()`:
      ! `int_key$min_ma` needs to be of type `numeric`

---

    Code
      look_up(occdf[1:10, ], int_key = interval_key)
    Condition
      Error in `look_up()`:
      ! `int_key$max_ma` needs to be of type `numeric`

---

    Code
      look_up(occdf[1:10, ], int_key = interval_key)
    Condition
      Error in `look_up()`:
      ! `int_key$interval_name`, `int_key$early_stage`, and
                 `int_key$late_stage` needs to be of type `character`

---

    Code
      look_up(occdf[1:10, ], early_interval = 5)
    Condition
      Error in `look_up()`:
      ! `early_interval` needs to be of type `character`

---

    Code
      look_up(occdf[1:10, ], late_interval = 5)
    Condition
      Error in `look_up()`:
      ! `late_interval` needs to be of type `character`

