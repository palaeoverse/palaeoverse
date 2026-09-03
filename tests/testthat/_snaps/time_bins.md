# arg 'interval' works

    Code
      time_bins(interval = NA)
    Condition
      Error in `time_bins()`:
      ! `interval` must be `NULL` or of class <character> or <numeric>, not `NA`.

---

    Code
      time_bins(interval = NULL)
    Condition
      Error in `time_bins()`:
      ! `interval` must not be `NULL` when `scale` is "GTS2020" or "GTS2012": define an interval or age range.

---

    Code
      time_bins(interval = "foo")
    Condition
      Error in `time_bins()`:
      ! Unknown interval: "foo".
      i Available intervals are accessible via `GTS2020` and `GTS2012`.

---

    Code
      time_bins(interval = c("Mastrichtian", "foo"))
    Condition
      Error in `time_bins()`:
      ! Unknown intervals: "Mastrichtian" and "foo".
      i Available intervals are accessible via `GTS2020` and `GTS2012`.

---

    Code
      time_bins(interval = c("Mastrichtian", NA))
    Condition
      Error in `time_bins()`:
      ! Unknown intervals: "Mastrichtian" and NA.
      i Available intervals are accessible via `GTS2020` and `GTS2012`.

---

    Code
      time_bins(interval = letters[1:3])
    Condition
      Error in `time_bins()`:
      ! `interval` must be `NULL` or of length 1 or 2, not 3.

---

    Code
      time_bins(interval = character(0))
    Condition
      Warning in `min()`:
      no non-missing arguments to min; returning Inf
      Warning in `max()`:
      no non-missing arguments to max; returning -Inf
      Error in `time_bins()`:
      ! No intervals are available for the defined `interval` range.

---

    Code
      time_bins(interval = data.frame())
    Condition
      Error in `time_bins()`:
      ! `interval` must be `NULL` or of class <character> or <numeric>, not a <data.frame> object.

---

    Code
      time_bins(interval = 1:3)
    Condition
      Error in `time_bins()`:
      ! `interval` must be `NULL` or of length 1 or 2, not 3.

---

    Code
      time_bins(interval = -1, plot = TRUE)
    Condition
      Error in `time_bins()`:
      ! Minimum `interval` value (-1) is less than the minimum available interval (0).

---

    Code
      time_bins(interval = 700)
    Condition
      Error in `time_bins()`:
      ! No intervals are available for the defined `interval` range.

---

    Code
      time_bins(interval = numeric(0))
    Condition
      Warning in `max()`:
      no non-missing arguments to max; returning -Inf
      Warning in `min()`:
      no non-missing arguments to min; returning Inf
      Warning in `min()`:
      no non-missing arguments to min; returning Inf
      Warning in `max()`:
      no non-missing arguments to max; returning -Inf
      Error in `time_bins()`:
      ! No intervals are available for the defined `interval` range.

---

    Code
      time_bins(interval = c(10000, 100))
    Condition
      Error in `time_bins()`:
      ! Maximum `interval` value (10000) is greater than the maximum available interval (4600).

# arg 'rank' works

    Code
      time_bins(interval = "Mesozoic", rank = "stages")
    Condition
      Error in `time_bins()`:
      ! `rank` must be one of "stage", "epoch", "period", "era", or "eon", not "stages".
      i Did you mean "stage"?

---

    Code
      time_bins(interval = "Mesozoic", rank = c("stage", "period"))
    Condition
      Error in `time_bins()`:
      ! `rank` must be a single string, not a character vector.

---

    Code
      time_bins(interval = "Mesozoic", rank = NA)
    Condition
      Error in `time_bins()`:
      ! `rank` must be a single string, not `NA`.

---

    Code
      time_bins(interval = "Mesozoic", rank = character(0))
    Condition
      Error in `time_bins()`:
      ! `rank` must be a single string, not an empty character vector.

# arg 'size' works

    Code
      out <- time_bins(interval = c("Fortunian", "Meghalayan"), size = 200)
    Message
      Target duration of equal length time bins was set to 200 Myr.
      i 3 time bins were generated.
      i Mean length: 180.33 Myr
      i Standard deviation: 3.59 Myr

---

    Code
      out <- time_bins(interval = c("Fortunian", "Meghalayan"), size = 6)
    Message
      Target duration of equal length time bins was set to 6 Myr.
      i 90 time bins were generated.
      i Mean length: 6.01 Myr
      i Standard deviation: 3.31 Myr

---

    Code
      time_bins(interval = "Mesozoic", size = "ten")
    Condition
      Error in `time_bins()`:
      ! `size` must be a number or `NULL`, not the string "ten".

---

    Code
      time_bins(interval = "Mesozoic", size = numeric(0))
    Condition
      Error in `time_bins()`:
      ! `size` must be a number or `NULL`, not an empty numeric vector.

---

    Code
      time_bins(interval = "Mesozoic", size = NA)
    Condition
      Error in `time_bins()`:
      ! `size` must be a number or `NULL`, not `NA`.

---

    Code
      time_bins(interval = "Mesozoic", size = 1:2)
    Condition
      Error in `time_bins()`:
      ! `size` must be a number or `NULL`, not an integer vector.

# arg 'n_bins' works

    Code
      time_bins(interval = "Mesozoic", n_bins = "ten")
    Condition
      Error in `time_bins()`:
      ! `n_bins` must be a whole number or `NULL`, not the string "ten".

---

    Code
      time_bins(interval = "Mesozoic", n_bins = numeric(0))
    Condition
      Error in `time_bins()`:
      ! `n_bins` must be a whole number or `NULL`, not an empty numeric vector.

---

    Code
      time_bins(interval = "Mesozoic", n_bins = NA)
    Condition
      Error in `time_bins()`:
      ! `n_bins` must be a whole number or `NULL`, not `NA`.

---

    Code
      time_bins(interval = "Mesozoic", n_bins = 1:2)
    Condition
      Error in `time_bins()`:
      ! `n_bins` must be a whole number or `NULL`, not an integer vector.

---

    Code
      time_bins(n_bins = 200)
    Condition
      Error in `time_bins()`:
      ! `n_bins` (200) must not be greater than the number of intervals (102).

# arg 'assign' works

    Code
      time_bins(interval = "Mesozoic", assign = 40)
    Condition
      Error in `time_bins()`:
      ! All values of `assign` must be within the specified time interval range (66 to 251.902).

---

    Code
      time_bins(interval = "Mesozoic", assign = -40)
    Condition
      Error in `time_bins()`:
      ! Age estimates for `assign` must be non-negative values.
      i You can transform your data using `abs()`.

---

    Code
      time_bins(interval = "Mesozoic", assign = "30")
    Condition
      Error in `time_bins()`:
      ! `assign` must be <numeric>, not the string "30".

---

    Code
      time_bins(interval = "Mesozoic", assign = NA)
    Condition
      Error in `time_bins()`:
      ! `assign` must be <numeric>, not `NA`.

---

    Code
      time_bins(interval = "Mesozoic", assign = 1:2)
    Condition
      Error in `time_bins()`:
      ! All values of `assign` must be within the specified time interval range (66 to 251.902).

# arg 'scale' works

    Code
      time_bins(interval = "Mesozoic", scale = "foo")
    Condition
      Error in `time_bins()`:
      ! `scale` must match a built-in or Macrostrat time scale.

---

    Code
      time_bins(interval = "Mesozoic", scale = 1)
    Condition
      Error in `time_bins()`:
      ! `scale` must be a single string, not the number 1.

---

    Code
      time_bins(interval = "Mesozoic", scale = NA)
    Condition
      Error in `time_bins()`:
      ! `scale` must be a single string, not `NA`.

---

    Code
      time_bins(interval = "Mesozoic", scale = character(0))
    Condition
      Error in `time_bins()`:
      ! `scale` must be a single string, not an empty character vector.

---

    Code
      time_bins(scale = scale, size = 15)
    Condition
      Error in `time_bins()`:
      ! Column "interval_name" not found in `scale`.

# arg 'plot' works

    Code
      time_bins(interval = "Mesozoic", plot = "TRUE")
    Condition
      Error in `time_bins()`:
      ! `plot` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    Code
      time_bins(interval = "Mesozoic", plot = NA)
    Condition
      Error in `time_bins()`:
      ! `plot` must be `TRUE` or `FALSE`, not `NA`.

---

    Code
      time_bins(interval = "Mesozoic", plot = logical(0))
    Condition
      Error in `time_bins()`:
      ! `plot` must be `TRUE` or `FALSE`, not an empty logical vector.

