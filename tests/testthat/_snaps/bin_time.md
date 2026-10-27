# wrong input for occdf

    Code
      bin_time_mid(occdf = c(50, 20, 10))
    Condition
      Error in `bin_time_mid()`:
      ! `occdf` must be of class <data.frame>, not a double vector.

---

    Code
      bin_time_mid(bins = c(50, 20, 10))
    Condition
      Error in `bin_time_mid()`:
      ! `occdf` must be of class <data.frame>, not absent.

---

    Code
      bin_time_mid(occdf = data.frame(), bins = c(50, 20, 10))
    Condition
      Error in `bin_time_mid()`:
      ! `bins` must be of class <data.frame>, not a double vector.

---

    Code
      bin_time_mid(occdf = data.frame(), bins = data.frame())
    Condition
      Error in `bin_time_mid()`:
      ! Column "min_ma" not found in `occdf`.

---

    Code
      bin_time_mid(occdf = test_occdf, bins = data.frame())
    Condition
      Error in `bin_time_mid()`:
      ! Column "min_ma" not found in `bins`.

---

    Code
      bin_time_mid(bins = mtcars, occdf = c(50, 20, 10))
    Condition
      Error in `bin_time_mid()`:
      ! `occdf` must be of class <data.frame>, not a double vector.

---

    Code
      bin_time_mid(occdf, bins = bins)
    Condition
      Error in `bin_time_mid()`:
      ! Maximum age must be larger than or equal to minimum age.
      i Row(s) of `occdf` where "max_ma" is smaller than "min_ma": 2, 3.

---

    Code
      bin_time_mid(occdf, bins = bins)
    Condition
      Error in `bin_time_mid()`:
      ! Maximum age must be larger than or equal to minimum age.
      i Row(s) of `bins` where "max_ma" is smaller than "min_ma": 2, 3.

# wrong input for reps

    Code
      bin_time_random(occdf, bins = bins, reps = TRUE)
    Condition
      Error in `bin_time_random()`:
      ! `reps` must be a whole number, not `TRUE`.

---

    Code
      bin_time_random(occdf, bins = bins, reps = 0)
    Condition
      Error in `bin_time_random()`:
      ! `reps` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      bin_time_random(occdf, bins = bins, reps = 1.5)
    Condition
      Error in `bin_time_random()`:
      ! `reps` must be a whole number, not the number 1.5.

---

    Code
      bin_time_random(occdf, bins = bins, reps = NA)
    Condition
      Error in `bin_time_random()`:
      ! `reps` must be a whole number, not `NA`.

---

    Code
      bin_time_point(occdf, bins = bins, reps = 0)
    Condition
      Error in `bin_time_point()`:
      ! `reps` must be a whole number larger than or equal to 1, not the number 0.

# wrong input for fun

    Code
      bin_time_point(occdf, bins = bins, fun = NULL)
    Condition
      Error in `bin_time_point()`:
      ! `fun` must be a function, not `NULL`.

---

    Code
      bin_time_point(occdf, bins = bins, fun = 1)
    Condition
      Error in `bin_time_point()`:
      ! `fun` must be a function, not the number 1.

---

    Code
      bin_time_point(occdf, bins = bins, fun = dnorm, x = 1)
    Condition
      Error in `bin_time_point()`:
      ! `x` must not be specified as it is generated internally.

---

    Code
      bin_time_point(occdf, bins = bins, fun = dnorm, test = 1)
    Condition
      Error in `bin_time_point()`:
      ! `test` is not a valid argument for `fun`.

---

    Code
      bin_time_point(occdf, bins = bins, fun = dnorm, test1 = 1, test2 = 1)
    Condition
      Error in `bin_time_point()`:
      ! `test1` and `test2` are not valid arguments for `fun`.

# errors in data for min and max age

    Code
      bin_time_mid(occdf, bins = bins)
    Condition
      Error in `bin_time_mid()`:
      ! Minimum age of occurrence data (-5000) is less than minimum age of bins (0).

---

    Code
      bin_time_mid(occdf, bins = bins)
    Condition
      Error in `bin_time_mid()`:
      ! Maximum age of occurrence data (5000) surpasses maximum age of bins (540).

---

    Code
      bin_time_mid(occdf, bins = bins)
    Condition
      Error in `bin_time_mid()`:
      ! Column "max_ma" in `occdf` must not have missing values.

# bin_time_*() functions error with unnamed args

    Code
      bin_time_majority(occdf = test_occdf, test_bins)
    Condition
      Error in `bin_time_majority()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there is 1 argument that should be named.

---

    Code
      bin_time_majority(test_occdf, test_bins)
    Condition
      Error in `bin_time_majority()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there is 1 argument that should be named.

---

    Code
      bin_time_point(test_occdf, bins = test_bins, reps = 5, fun = dnorm, 0.5, 0.25)
    Condition
      Error in `bin_time_point()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there are 2 arguments that should be named.

# bin_time() is deprecated but still works

    Code
      suppressWarnings(bin_time(test_occdf, bins = test_bins, method = "foo"))
    Condition
      Error in `bin_time()`:
      ! `method` must be one of "mid", "majority", "all", "random", or "point", not "foo".

