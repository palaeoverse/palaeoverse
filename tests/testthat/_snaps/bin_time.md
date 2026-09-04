# wrong input for occdf

    Code
      bin_time(occdf = c(50, 20, 10))
    Condition
      Error in `bin_time()`:
      ! `occdf` must be of class <data.frame>, not a double vector.

---

    Code
      bin_time(bins = c(50, 20, 10))
    Condition
      Error in `bin_time()`:
      ! `occdf` must be of class <data.frame>, not absent.

---

    Code
      bin_time(occdf = data.frame(), bins = c(50, 20, 10))
    Condition
      Error in `bin_time()`:
      ! `bins` must be of class <data.frame>, not a double vector.

---

    Code
      bin_time(occdf = data.frame(), bins = data.frame(), method = "mid")
    Condition
      Error in `bin_time()`:
      ! Column "min_ma" not found in `occdf`.

---

    Code
      bin_time(occdf = data.frame(), bins = data.frame(), method = "mid")
    Condition
      Error in `bin_time()`:
      ! Column "min_ma" not found in `occdf`.

---

    Code
      bin_time(occdf = test_occdf, bins = data.frame(), method = "mid")
    Condition
      Error in `bin_time()`:
      ! Column "min_ma" not found in `bins`.

---

    Code
      bin_time(occdf = test_occdf, bins = data.frame(), method = "mid")
    Condition
      Error in `bin_time()`:
      ! Column "min_ma" not found in `bins`.

---

    Code
      bin_time(bins = mtcars, occdf = c(50, 20, 10))
    Condition
      Error in `bin_time()`:
      ! `occdf` must be of class <data.frame>, not a double vector.

# wrong input for method

    Code
      bin_time(occdf = occdf, bins = bins, method = "foo")
    Condition
      Error in `bin_time()`:
      ! `method` must be one of "mid", "majority", "all", "random", or "point", not "foo".

# wrong input for reps

    Code
      bin_time(occdf = occdf, bins = bins, method = "random", reps = TRUE)
    Condition
      Error in `bin_time()`:
      ! `reps` must be a numeric value, not `TRUE`.

# wrong input for fun

    Code
      bin_time(occdf = occdf, bins = bins, method = "point", fun = NULL)
    Condition
      Error in `bin_time()`:
      ! Setting `method = "point"` requires `fun` to be a function.
      x Problem: `fun` is NULL.

---

    Code
      bin_time(occdf = occdf, bins = bins, method = "point", fun = 1)
    Condition
      Error in `bin_time()`:
      ! Setting `method = "point"` requires `fun` to be a function.
      x Problem: `fun` is numeric.

---

    Code
      bin_time(occdf = occdf, bins = bins, method = "point", fun = dnorm, x = 1)
    Condition
      Error in `bin_time()`:
      ! `x` should not be specified. This is generated internally.

---

    Code
      bin_time(occdf = occdf, bins = bins, method = "point", fun = dnorm, test = 1)
    Condition
      Error in `bin_time()`:
      ! `test` is not a valid argument for the specified function

---

    Code
      bin_time(occdf = occdf, bins = bins, method = "point", fun = dnorm, test1 = 1,
        test2 = 1)
    Condition
      Error in `bin_time()`:
      ! `test1`/`test2` are not valid arguments for the specified function

# errors in data for min and max age

    Code
      bin_time(occdf = occdf, bins = bins)
    Condition
      Error in `bin_time()`:
      ! Minimum age of occurrence data (-5000) is less than minimum age of bins (0).

---

    Code
      bin_time(occdf = occdf, bins = bins)
    Condition
      Error in `bin_time()`:
      ! Maximum age of occurrence data (5000) surpasses maximum age of bins (540).

---

    Code
      bin_time(occdf = occdf, bins = bins)
    Condition
      Error in `bin_time()`:
      ! `max_ma` can't contain NA values.

# bin_time errors with unnamed args

    Code
      bin_time(occdf = test_occdf, test_bins, method = "majority")
    Condition
      Error in `bin_time()`:
      ! All arguments must be named (except for "occdf").
      i Currently, there is 1 argument that should be named.

---

    Code
      bin_time(test_occdf, test_bins, "majority")
    Condition
      Error in `bin_time()`:
      ! All arguments must be named (except for "occdf").
      i Currently, there are 2 arguments that should be named.

---

    Code
      bin_time(occdf = test_occdf, bins = test_bins, method = "point", reps = 5, fun = dnorm,
        0.5, 0.25)
    Condition
      Error in `bin_time()`:
      ! All arguments must be named (except for "occdf").
      i Currently, there are 2 arguments that should be named.

