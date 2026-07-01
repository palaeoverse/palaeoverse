# wrong input for occdf

    Code
      bin_time(occdf = c(50, 20, 10))
    Condition
      Error in `bin_time()`:
      ! `occdf` should be a dataframe.

---

    Code
      bin_time(bins = c(50, 20, 10))
    Condition
      Error in `bin_time()`:
      ! argument "occdf" is missing, with no default

---

    Code
      bin_time(occdf = data.frame(), bins = c(50, 20, 10))
    Condition
      Error in `bin_time()`:
      ! `bins` should be a dataframe.

---

    Code
      bin_time(occdf = data.frame(), bins = data.frame(), method = "mid")
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      bin_time(occdf = data.frame(), bins = data.frame(), method = "mid")
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      bin_time(mtcars, occdf = c(50, 20, 10))
    Condition
      Error in `bin_time()`:
      ! `occdf` should be a dataframe.

# wrong input for method

    Code
      bin_time(occdf = occdf, bins = bins, method = "foo")
    Condition
      Error in `bin_time()`:
      ! Invalid `method`. Choose either: 
       'all', 'majority', 'random', 'point', or 'mid'.

# wrong input for reps

    Code
      bin_time(occdf = occdf, bins = bins, method = "random", reps = TRUE)
    Condition
      Error in `bin_time()`:
      ! Invalid `reps`. Choose a numeric value.

# wrong input for fun

    Code
      bin_time(occdf = occdf, bins = bins, method = "point", fun = NULL)
    Condition
      Error in `bin_time()`:
      ! `fun` is not a function.

---

    Code
      bin_time(occdf = occdf, bins = bins, method = "point", fun = 1)
    Condition
      Error in `bin_time()`:
      ! `fun` is not a function.

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
      ! Minimum age of occurrence data is less than minimum age of bins.

---

    Code
      bin_time(occdf = occdf, bins = bins)
    Condition
      Error in `bin_time()`:
      ! Maximum age of occurrence data surpasses maximum age of bins.

---

    Code
      bin_time(occdf = occdf, bins = bins)
    Condition
      Error in `bin_time()`:
      ! NA values detected in max_ma or min_ma

