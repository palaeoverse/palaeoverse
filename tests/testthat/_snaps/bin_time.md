# bin_time() works

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
      bin_time(occdf = data.frame(), bins = data.frame(), method = "assign")
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      bin_time(occdf = data.frame(), bins = data.frame(), method = "mid", reps = TRUE)
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
      bin_time(occdf = data.frame(), bins = data.frame(), method = "mid")
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      bin_time(occdf = occdf, bins = bins, method = "point", fun = NULL)
    Condition
      Error:
      ! object 'occdf' not found

---

    Code
      bin_time(occdf = occdf, bins = bins, method = "point", fun = dnorm, x = 1)
    Condition
      Error:
      ! object 'occdf' not found

---

    Code
      bin_time(occdf = occdf, bins = bins, method = "point", fun = dnorm, test = 1)
    Condition
      Error:
      ! object 'occdf' not found

---

    Code
      bin_time(occdf = occdf, bins = bins, method = "point", fun = dnorm, test1 = 1,
        test2 = 1)
    Condition
      Error:
      ! object 'occdf' not found

---

    Code
      bin_time(occdf = occdf, bins = bins, method = "point", reps = 5, fun = drm,
        mean = 0.5, sd = 0.25)
    Condition
      Error in `bin_time()`:
      ! `fun` is not a function.

---

    Code
      length(bin_time(occdf = occdf, bins = bins, ))
    Condition
      Error in `bin_time()`:
      ! Minimum age of occurrence data is less than minimum age of bins.

---

    Code
      length(bin_time(occdf = occdf, bins = bins, ))
    Condition
      Error in `bin_time()`:
      ! Maximum age of occurrence data surpasses maximum age of bins.

---

    Code
      length(bin_time(occdf = occdf, bins = bins, ))
    Condition
      Error in `bin_time()`:
      ! NA values detected in max_ma or min_ma

