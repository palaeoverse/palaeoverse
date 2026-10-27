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
      bin_time(occdf = data.frame(), bins = data.frame(), method = bin_time_mid())
    Condition
      Error in `bin_time()`:
      ! Column "min_ma" not found in `occdf`.

---

    Code
      bin_time(occdf = data.frame(), bins = data.frame(), method = bin_time_mid())
    Condition
      Error in `bin_time()`:
      ! Column "min_ma" not found in `occdf`.

---

    Code
      bin_time(occdf = test_occdf, bins = data.frame(), method = bin_time_mid())
    Condition
      Error in `bin_time()`:
      ! Column "min_ma" not found in `bins`.

---

    Code
      bin_time(occdf = test_occdf, bins = data.frame(), method = bin_time_mid())
    Condition
      Error in `bin_time()`:
      ! Column "min_ma" not found in `bins`.

---

    Code
      bin_time(bins = mtcars, occdf = c(50, 20, 10))
    Condition
      Error in `bin_time()`:
      ! `occdf` must be of class <data.frame>, not a double vector.

---

    Code
      bin_time(occdf, bins = bins)
    Condition
      Error in `bin_time()`:
      ! Maximum age must be larger than or equal to minimum age.
      i Row(s) of `occdf` where "max_ma" is smaller than "min_ma": 2, 3.

---

    Code
      bin_time(occdf, bins = bins)
    Condition
      Error in `bin_time()`:
      ! Maximum age must be larger than or equal to minimum age.
      i Row(s) of `bins` where "max_ma" is smaller than "min_ma": 2, 3.

# wrong input for method

    Code
      bin_time(occdf = occdf, bins = bins, method = "foo")
    Condition
      Error in `bin_time()`:
      ! `method` must be a call to one of `bin_time_mid()`, `bin_time_majority()`, `bin_time_all()`, `bin_time_random()`, or `bin_time_point()`, not the string "foo".

---

    Code
      bin_time(occdf = occdf, bins = bins, method = "majority")
    Condition
      Error in `bin_time()`:
      ! `method` must be a call to one of `bin_time_mid()`, `bin_time_majority()`, `bin_time_all()`, `bin_time_random()`, or `bin_time_point()`, not the string "majority".
      i Use `method = bin_time_majority()` instead.

---

    Code
      bin_time(occdf = occdf, bins = bins, method = list(method = "mid"))
    Condition
      Error in `bin_time()`:
      ! `method` must be a call to one of `bin_time_mid()`, `bin_time_majority()`, `bin_time_all()`, `bin_time_random()`, or `bin_time_point()`, not a list.

# method functions error outside of bin_time()

    Code
      bin_time_mid()
    Condition
      Error in `bin_time_mid()`:
      ! `bin_time_mid()` must be used directly in the `method` argument of `bin_time()`.

---

    Code
      bin_time_majority()
    Condition
      Error in `bin_time_majority()`:
      ! `bin_time_majority()` must be used directly in the `method` argument of `bin_time()`.

---

    Code
      bin_time_all()
    Condition
      Error in `bin_time_all()`:
      ! `bin_time_all()` must be used directly in the `method` argument of `bin_time()`.

---

    Code
      bin_time_random(reps = 5)
    Condition
      Error in `bin_time_random()`:
      ! `bin_time_random()` must be used directly in the `method` argument of `bin_time()`.

---

    Code
      bin_time_point(reps = 5, fun = dnorm)
    Condition
      Error in `bin_time_point()`:
      ! `bin_time_point()` must be used directly in the `method` argument of `bin_time()`.

---

    Code
      bin_time_point(reps = -1)
    Condition
      Error in `bin_time_point()`:
      ! `bin_time_point()` must be used directly in the `method` argument of `bin_time()`.

---

    Code
      method <- bin_time_mid()
    Condition
      Error in `bin_time_mid()`:
      ! `bin_time_mid()` must be used directly in the `method` argument of `bin_time()`.

# wrong input for reps

    Code
      bin_time(occdf = occdf, bins = bins, method = bin_time_random(reps = TRUE))
    Condition
      Error in `bin_time_random()`:
      ! `reps` must be a whole number, not `TRUE`.

---

    Code
      bin_time(occdf = occdf, bins = bins, method = bin_time_random(reps = 0))
    Condition
      Error in `bin_time_random()`:
      ! `reps` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      bin_time(occdf = occdf, bins = bins, method = bin_time_random(reps = 1.5))
    Condition
      Error in `bin_time_random()`:
      ! `reps` must be a whole number, not the number 1.5.

---

    Code
      bin_time(occdf = occdf, bins = bins, method = bin_time_random(reps = NA))
    Condition
      Error in `bin_time_random()`:
      ! `reps` must be a whole number, not `NA`.

---

    Code
      bin_time(occdf = occdf, bins = bins, method = bin_time_point(reps = 0))
    Condition
      Error in `bin_time_point()`:
      ! `reps` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      bin_time(occdf = occdf, bins = bins, method = bin_time_random(5))
    Condition
      Error in `bin_time_random()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 5
      i Did you forget to name an argument?

# wrong input for fun

    Code
      bin_time(occdf = occdf, bins = bins, method = bin_time_point(fun = NULL))
    Condition
      Error in `bin_time_point()`:
      ! `fun` must be a function, not `NULL`.

---

    Code
      bin_time(occdf = occdf, bins = bins, method = bin_time_point(fun = 1))
    Condition
      Error in `bin_time_point()`:
      ! `fun` must be a function, not the number 1.

---

    Code
      bin_time(occdf = occdf, bins = bins, method = bin_time_point(fun = dnorm, x = 1))
    Condition
      Error in `bin_time_point()`:
      ! `x` must not be specified as it is generated internally.

---

    Code
      bin_time(occdf = occdf, bins = bins, method = bin_time_point(fun = dnorm, test = 1))
    Condition
      Error in `bin_time_point()`:
      ! `test` is not a valid argument for `fun`.

---

    Code
      bin_time(occdf = occdf, bins = bins, method = bin_time_point(fun = dnorm,
        test1 = 1, test2 = 1))
    Condition
      Error in `bin_time_point()`:
      ! `test1` and `test2` are not valid arguments for `fun`.

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
      ! Column "max_ma" in `occdf` must not have missing values.

# deprecated arguments error

    Code
      bin_time(occdf = test_occdf, bins = test_bins, method = bin_time_random(),
      reps = 5)
    Condition
      Error:
      ! The `reps` argument of `bin_time()` was deprecated in palaeoverse 2.0.0 and is now defunct.
      i Use the `reps` argument of `bin_time_random()` or `bin_time_point()` instead.

---

    Code
      bin_time(occdf = test_occdf, bins = test_bins, method = bin_time_point(), fun = dnorm)
    Condition
      Error:
      ! The `fun` argument of `bin_time()` was deprecated in palaeoverse 2.0.0 and is now defunct.
      i Please use the `fun` argument of `bin_time_point()` instead.

---

    Code
      bin_time(occdf = test_occdf, bins = test_bins, method = bin_time_point(fun = dnorm),
      mean = 0.5, sd = 0.25)
    Condition
      Error:
      ! The `...` argument of `bin_time()` was deprecated in palaeoverse 2.0.0 and is now defunct.
      i Please use the `...` argument of `bin_time_point()` instead.

# bin_time errors with unnamed args

    Code
      bin_time(occdf = test_occdf, test_bins, method = bin_time_majority())
    Condition
      Error in `bin_time()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there is 1 argument that should be named.

---

    Code
      bin_time(test_occdf, test_bins, bin_time_majority())
    Condition
      Error in `bin_time()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there are 2 arguments that should be named.

---

    Code
      bin_time(occdf = test_occdf, bins = test_bins, method = bin_time_point(reps = 5,
        fun = dnorm), 0.5, 0.25)
    Condition
      Error in `bin_time()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there are 2 arguments that should be named.

