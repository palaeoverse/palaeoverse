# time_bins() works

    Code
      time_bins(interval = c("Mastrichtian", "Danian"))
    Condition
      Error in `time_bins()`:
      ! Check spelling of specified intervals. Available intervals are accessible via GTS2020 and GTS2012.

---

    Code
      time_bins(interval = "Mastrichtian", scale = "GTS2012", plot = TRUE)
    Condition
      Error in `time_bins()`:
      ! Check spelling of specified intervals. Available intervals are accessible via GTS2020 and GTS2012.

---

    Code
      time_bins(interval = "Mastrichtian", scale = "2012", plot = TRUE)
    Condition
      Error:
      ! `name` does not match a built-in or Macrostrat time scale.

---

    Code
      time_bins(interval = 700, scale = "GTS2020", plot = TRUE)
    Condition
      Error in `time_bins()`:
      ! No intervals are available for the defined interval range.

---

    Code
      time_bins(interval = -1, plot = TRUE)
    Condition
      Error in `time_bins()`:
      ! minimum `interval` value is less than available intervals

---

    Code
      time_bins(interval = data.frame())
    Condition
      Error in `time_bins()`:
      ! `interval` must be NULL or of class 'character' or 'numeric'

---

    Code
      time_bins(interval = c(50, 10, 20), plot = TRUE)
    Condition
      Error in `time_bins()`:
      ! `interval` must be a 'character' or 'numeric' vector of length 1 or 2 or NULL.

---

    Code
      time_bins(interval = "Mesozoic", plot = "TRUE")
    Condition
      Error in `time_bins()`:
      ! `plot` should be logical (TRUE/FALSE).

---

    Code
      time_bins(interval = "Mesozoic", assign = 40)
    Condition
      Error in `time_bins()`:
      ! One or more ages is outside the specified time interval range

---

    Code
      time_bins(interval = "Mesozoic", assign = -40)
    Condition
      Error in `time_bins()`:
      ! Age estimates for `assign` should be non-negative values. Hint: You can transform your data using abs().

---

    Code
      time_bins(interval = "Mesozoic", assign = "30")
    Condition
      Error in `time_bins()`:
      ! `assign` should be a numeric

---

    Code
      time_bins(interval = "Mesozoic", size = "ten")
    Condition
      Error in `time_bins()`:
      ! `size` should be a 'numeric' or NULL.

---

    Code
      time_bins(interval = "Mesozoic", n_bins = "eleven")
    Condition
      Error in `time_bins()`:
      ! `size` should be a 'numeric' or NULL.

---

    Code
      time_bins(interval = "Mesozoic", rank = "stages")
    Condition
      Error in `time_bins()`:
      ! `rank` must be either: stage, epoch, period, era, eon, or NULL.

---

    Code
      time_bins(interval = "Mesozoic", rank = c("stage", "period"))
    Condition
      Error in `time_bins()`:
      ! `rank` must be of length 1.

---

    Code
      time_bins(interval = "Mesozoic", scale = 1)
    Condition
      Error in `time_bins()`:
      ! `scale` must be either:
       The name of an in-built time scale (e.g. 'GTS2020'), the name of a Macrostrat time scale (see details), or a `data.frame`.

---

    Code
      time_bins(interval = NULL, scale = "GTS2020")
    Condition
      Error in `time_bins()`:
      ! `interval` is NULL. You must define an interval/age range.

---

    Code
      time_bins(interval = c(10000, 100), scale = "GTS2020")
    Condition
      Error in `time_bins()`:
      ! maximum `interval` value is greater than available intervals

---

    Code
      time_bins(n_bins = 200)
    Condition
      Error in `time_bins()`:
      ! `n_bins` can't be greater than the number of intervals.

---

    Code
      time_bins(scale = scale, size = 15)
    Condition
      Error in `time_bins()`:
      ! `scale` does not contain named columns: 'interval_name', 'max_ma', and 'min_ma'.

