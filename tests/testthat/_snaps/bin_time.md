# bin_time() works with method 'mid'

    Code
      bin_mid[, c("id", "n_bins", "bin_assignment", "bin_midpoint")]
    Output
        id n_bins bin_assignment bin_midpoint
      1  1      2             31          305
      2  2      2             27          265
      3  3      2             25          245
      4  4      3             29          285
      5  5      1             28          275

# bin_time() works with method 'majority'

    Code
      bin_majority[, c("id", "n_bins", "bin_assignment", "bin_midpoint")]
    Output
        id n_bins bin_assignment bin_midpoint
      1  1      2             31          305
      2  2      2             27          265
      3  3      2             25          245
      4  4      3             29          285
      5  5      1             28          275

# bin_time() works with method 'all'

    Code
      bin_all[, c("id", "n_bins", "bin_assignment", "bin_midpoint")]
    Output
         id n_bins bin_assignment bin_midpoint
      1   1      2             30          295
      2   1      2             31          305
      3   2      2             26          255
      4   2      2             27          265
      5   3      2             25          245
      6   3      2             26          255
      7   4      3             28          275
      8   4      3             29          285
      9   4      3             30          295
      10  5      1             28          275

# bin_time() works with method 'random'

    Code
      bin_random[[1]][, c("id", "n_bins", "bin_assignment", "bin_midpoint")]
    Output
        id n_bins bin_assignment bin_midpoint
      1  1      2             31          305
      2  2      2             27          265
      3  3      2             26          255
      4  4      3             29          285
      5  5      1             28          275

# bin_time() works with method 'point'

    Code
      bin_point[[1]][, c("id", "n_bins", "bin_assignment", "point_estimates")]
    Output
        id n_bins bin_assignment point_estimates
      1  1      2             31         302.353
      2  2      2             27         260.895
      3  3      2             25         248.602
      4  4      3             28         279.557
      5  5      1             28         277.640

# user can pass custom function to method 'point'

    Code
      bin_point[[1]][, c("id", "n_bins", "bin_assignment", "point_estimates")]
    Output
        id n_bins bin_assignment point_estimates
      1  1      2             30         299.903
      2  2      2             27         262.273
      3  3      2             25         248.999
      4  4      3             29         286.860
      5  5      1             28         273.313

# user can pass custom function to method 'random'

    Code
      bin_random[[1]][, c("id", "n_bins", "bin_assignment", "bin_midpoint")]
    Output
        id n_bins bin_assignment bin_midpoint
      1  1      2             31          305
      2  2      2             27          265
      3  3      2             26          255
      4  4      3             29          285
      5  5      1             28          275

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

