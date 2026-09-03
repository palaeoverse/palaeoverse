# wrong input for argument 'occdf'

    Code
      look_up(1)
    Condition
      Error in `look_up()`:
      ! `occdf` must be of class <data.frame>, not the number 1.

---

    Code
      look_up(NA)
    Condition
      Error in `look_up()`:
      ! `occdf` must be of class <data.frame>, not `NA`.

---

    Code
      look_up(NULL)
    Condition
      Error in `look_up()`:
      ! `occdf` must be of class <data.frame>, not `NULL`.

# arguments 'early_interval' and 'late_interval' work

    Code
      look_up(occdf = dat)
    Condition
      Error in `look_up()`:
      ! Column "early_interval" not found in `occdf`.

---

    Code
      look_up(occdf = dat, early_interval = "early")
    Condition
      Error in `look_up()`:
      ! Column "late_interval" not found in `occdf`.

---

    Code
      look_up(occdf = dat, late_interval = "late")
    Condition
      Error in `look_up()`:
      ! Column "early_interval" not found in `occdf`.

---

    Code
      look_up(occdf = dat, early_interval = 1)
    Condition
      Error in `look_up()`:
      ! `early_interval` must be a single string, not the number 1.

---

    Code
      look_up(occdf = dat, early_interval = NA)
    Condition
      Error in `look_up()`:
      ! `early_interval` must be a single string, not `NA`.

---

    Code
      look_up(occdf = dat, early_interval = c("a", "b"))
    Condition
      Error in `look_up()`:
      ! `early_interval` must be a single string, not a character vector.

---

    Code
      look_up(occdf = dat, late_interval = 1)
    Condition
      Error in `look_up()`:
      ! Column "early_interval" not found in `occdf`.

---

    Code
      look_up(occdf = dat, early_interval = "early", late_interval = NA)
    Condition
      Error in `look_up()`:
      ! `late_interval` must be a single string, not `NA`.

---

    Code
      look_up(occdf = dat, late_interval = c("a", "b"))
    Condition
      Error in `look_up()`:
      ! Column "early_interval" not found in `occdf`.

# argument 'int_key' works

    Code
      look_up(occdf, int_key = 1)
    Condition
      Error in `look_up()`:
      ! `int_key` must be of class <data.frame>, not the number 1.

---

    Code
      look_up(occdf, int_key = c("a", "b"))
    Condition
      Error in `look_up()`:
      ! `int_key` must be of class <data.frame>, not a character vector.

---

    Code
      look_up(occdf, int_key = data.frame(interval_name = c("Induan", "Asselian"),
      early_stage = c("foo1", "foo2")))
    Condition
      Error in `look_up()`:
      ! Column "late_stage" not found in `int_key`.

---

    Code
      look_up(occdf, int_key = data.frame(interval_name = c("Induan", "Asselian"),
      late_stage = c("foo1", "foo2")))
    Condition
      Error in `look_up()`:
      ! Column "early_stage" not found in `int_key`.

---

    Code
      look_up(occdf, int_key = data.frame(interval_name = c("Induan", "Asselian"),
      early_stage = 1:2, late_stage = c("foo1", "foo2")))
    Condition
      Error in `look_up()`:
      ! Column "early_stage" in `int_key` must be of class <character>, not <integer>.

---

    Code
      look_up(occdf, int_key = data.frame(interval_name = c("Induan", "Asselian"),
      early_stage = c("foo1", "foo2"), late_stage = c("foo1", "foo2"), max_ma = c("a",
        "b")))
    Condition
      Error in `look_up()`:
      ! Column "max_ma" in `int_key` must be <numeric>, not <character>.

---

    Code
      look_up(occdf, int_key = data.frame(interval_name = c("Induan", "Asselian"),
      early_stage = c("foo1", "foo2"), late_stage = c("foo1", "foo2"), min_ma = c("a",
        "b")))
    Condition
      Error in `look_up()`:
      ! Column "min_ma" in `int_key` must be <numeric>, not <character>.

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
      ! `assign_with_GTS` must be "GTS2020" or "GTS2012" when `int_key = FALSE`.
      x Assignment with GTS is currently disabled.

---

    Code
      look_up(occdf, assign_with_GTS = 1)
    Condition
      Error in `look_up()`:
      ! `assign_with_GTS` must be "GTS2020" or "GTS2012" when `int_key = FALSE`.
      x Assignment with GTS is currently disabled.

---

    Code
      look_up(occdf, assign_with_GTS = "foo")
    Condition
      Error in `look_up()`:
      ! `assign_with_GTS` must be "GTS2020" or "GTS2012" when `int_key = FALSE`.
      x Assignment with GTS is currently disabled.

