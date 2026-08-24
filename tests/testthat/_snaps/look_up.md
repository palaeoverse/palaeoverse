# wrong input for argument 'occdf'

    Code
      look_up(1)
    Condition
      Error in `look_up()`:
      ! `occdf` should be a dataframe.

---

    Code
      look_up(NA)
    Condition
      Error in `look_up()`:
      ! `occdf` should be a dataframe.

---

    Code
      look_up(NULL)
    Condition
      Error in `look_up()`:
      ! `occdf` should be a dataframe.

# arguments 'early_interval' and 'late_interval' work

    Code
      look_up(occdf = dat)
    Condition
      Error in `look_up()`:
      ! `early_interval` needs to match a column name of `occdf`

---

    Code
      look_up(occdf = dat, early_interval = "early")
    Condition
      Error in `look_up()`:
      ! `late_interval` needs to match a column name of `occdf`

---

    Code
      look_up(occdf = dat, late_interval = "late")
    Condition
      Error in `look_up()`:
      ! `early_interval` needs to match a column name of `occdf`

---

    Code
      look_up(occdf = dat, early_interval = 1)
    Condition
      Error in `look_up()`:
      ! `early_interval` needs to be of type `character`

---

    Code
      look_up(occdf = dat, early_interval = NA)
    Condition
      Error in `look_up()`:
      ! `early_interval` needs to be of type `character`

---

    Code
      look_up(occdf = dat, early_interval = c("a", "b"))
    Condition
      Error in `look_up()`:
      ! `early_interval` must be of length 1

---

    Code
      look_up(occdf = dat, late_interval = 1)
    Condition
      Error in `look_up()`:
      ! `early_interval` needs to match a column name of `occdf`

---

    Code
      look_up(occdf = dat, early_interval = "early", late_interval = NA)
    Condition
      Error in `look_up()`:
      ! `late_interval` needs to be of type `character`

---

    Code
      look_up(occdf = dat, late_interval = c("a", "b"))
    Condition
      Error in `look_up()`:
      ! `early_interval` needs to match a column name of `occdf`

# argument 'int_key' works

    Code
      look_up(occdf, int_key = 1)
    Condition
      Error in `look_up()`:
      ! `int_key` should be a dataframe.

---

    Code
      look_up(occdf, int_key = c("a", "b"))
    Condition
      Error in `look_up()`:
      ! `int_key` should be a dataframe.

---

    Code
      look_up(occdf, int_key = data.frame(interval_name = c("Induan", "Asselian"),
      early_stage = c("foo1", "foo2")))
    Condition
      Error in `look_up()`:
      ! `int_key` needs to contain the columns "interval_name",
                 "early_stage" and "late_stage"

---

    Code
      look_up(occdf, int_key = data.frame(interval_name = c("Induan", "Asselian"),
      late_stage = c("foo1", "foo2")))
    Condition
      Error in `look_up()`:
      ! `int_key` needs to contain the columns "interval_name",
                 "early_stage" and "late_stage"

---

    Code
      look_up(occdf, int_key = data.frame(interval_name = c("Induan", "Asselian"),
      early_stage = 1:2, late_stage = c("foo1", "foo2")))
    Condition
      Error in `look_up()`:
      ! `int_key$interval_name`, `int_key$early_stage`, and
                 `int_key$late_stage` needs to be of type `character`

---

    Code
      look_up(occdf, int_key = data.frame(interval_name = c("Induan", "Asselian"),
      early_stage = c("foo1", "foo2"), late_stage = c("foo1", "foo2"), max_ma = c("a",
        "b")))
    Condition
      Error in `look_up()`:
      ! `int_key$max_ma` needs to be of type `numeric`

---

    Code
      look_up(occdf, int_key = data.frame(interval_name = c("Induan", "Asselian"),
      early_stage = c("foo1", "foo2"), late_stage = c("foo1", "foo2"), min_ma = c("a",
        "b")))
    Condition
      Error in `look_up()`:
      ! `int_key$min_ma` needs to be of type `numeric`

# argument 'assign_with_GTS' works

    Code
      look_up(occdf, int_key = interval_key, assign_with_GTS = "foo")
    Condition
      Error in `!assign_with_GTS`:
      ! invalid argument type

---

    Code
      look_up(occdf, assign_with_GTS = FALSE)
    Condition
      Error in `look_up()`:
      ! assignment with GTS needs to be enabled if `int_key` is set to `FALSE`

---

    Code
      look_up(occdf, assign_with_GTS = 1)
    Condition
      Error in `look_up()`:
      ! assignment with GTS needs to be enabled if `int_key` is set to `FALSE`

---

    Code
      look_up(occdf, assign_with_GTS = "foo")
    Condition
      Error in `look_up()`:
      ! assignment with GTS needs to be enabled if `int_key` is set to `FALSE`

