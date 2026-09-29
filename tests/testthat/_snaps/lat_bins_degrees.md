# argument 'size' works

    Code
      lat_bins_degrees(size = 100)
    Condition
      Error in `lat_bins_degrees()`:
      ! `size` must be a number between 0 and 90, not the number 100.

---

    Code
      lat_bins_degrees(size = numeric(0))
    Condition
      Error in `lat_bins_degrees()`:
      ! `size` must be a number, not an empty numeric vector.

---

    Code
      lat_bins_degrees(size = c(10, 20))
    Condition
      Error in `lat_bins_degrees()`:
      ! `size` must be a number, not a double vector.

# arguments 'min' and 'max' work

    Code
      lat_bins_degrees(min = 500)
    Condition
      Error in `lat_bins_degrees()`:
      ! `min` must be a number between -90 and 90, not the number 500.

---

    Code
      lat_bins_degrees(min = "a")
    Condition
      Error in `lat_bins_degrees()`:
      ! `min` must be a number, not the string "a".

---

    Code
      lat_bins_degrees(min = NA)
    Condition
      Error in `lat_bins_degrees()`:
      ! `min` must be a number, not `NA`.

---

    Code
      lat_bins_degrees(min = numeric(0))
    Condition
      Error in `lat_bins_degrees()`:
      ! `min` must be a number, not an empty numeric vector.

---

    Code
      lat_bins_degrees(min = c(1, 2))
    Condition
      Error in `lat_bins_degrees()`:
      ! `min` must be a number, not a double vector.

---

    Code
      lat_bins_degrees(max = 500)
    Condition
      Error in `lat_bins_degrees()`:
      ! `max` must be a number between -90 and 90, not the number 500.

---

    Code
      lat_bins_degrees(max = "a")
    Condition
      Error in `lat_bins_degrees()`:
      ! `max` must be a number, not the string "a".

---

    Code
      lat_bins_degrees(max = NA)
    Condition
      Error in `lat_bins_degrees()`:
      ! `max` must be a number, not `NA`.

---

    Code
      lat_bins_degrees(max = numeric(0))
    Condition
      Error in `lat_bins_degrees()`:
      ! `max` must be a number, not an empty numeric vector.

---

    Code
      lat_bins_degrees(max = c(1, 2))
    Condition
      Error in `lat_bins_degrees()`:
      ! `max` must be a number, not a double vector.

---

    Code
      lat_bins_degrees(min = 30, max = 10)
    Condition
      Error in `lat_bins_degrees()`:
      ! `min` must be less than `max`.

---

    Code
      lat_bins_degrees(min = 30, max = 30)
    Condition
      Error in `lat_bins_degrees()`:
      ! `min` must be less than `max`.

# argument 'fit' works

    Code
      lat_bins_degrees(fit = 100)
    Condition
      Error in `lat_bins_degrees()`:
      ! `fit` must be `TRUE` or `FALSE`, not the number 100.

---

    Code
      lat_bins_degrees(fit = logical(0))
    Condition
      Error in `lat_bins_degrees()`:
      ! `fit` must be `TRUE` or `FALSE`, not an empty logical vector.

---

    Code
      lat_bins_degrees(fit = NA)
    Condition
      Error in `lat_bins_degrees()`:
      ! `fit` must be `TRUE` or `FALSE`, not `NA`.

---

    Code
      lat_bins_degrees(fit = c(TRUE, TRUE))
    Condition
      Error in `lat_bins_degrees()`:
      ! `fit` must be `TRUE` or `FALSE`, not a logical vector.

# lat_bins errors with unnamed args

    Code
      lat_bins_degrees(10, -90)
    Condition
      Error in `lat_bins_degrees()`:
      ! All arguments must be named.
      i Currently, there are 2 arguments that should be named.

---

    Code
      lat_bins_degrees(size = 10, -90)
    Condition
      Error in `lat_bins_degrees()`:
      ! All arguments must be named.
      i Currently, there is 1 argument that should be named.

---

    Code
      lat_bins_degrees(10, -90, 90)
    Condition
      Error in `lat_bins_degrees()`:
      ! All arguments must be named.
      i Currently, there are 3 arguments that should be named.

---

    Code
      lat_bins_degrees(10, -90, max = 90)
    Condition
      Error in `lat_bins_degrees()`:
      ! All arguments must be named.
      i Currently, there are 2 arguments that should be named.

# lat_bins_degrees plotting with extra args works

    Code
      plot(lat_bins_degrees(size = 40), col = "foo")
    Condition
      Error in `plot()`:
      ! Argument `col` must be an object of class <character> of length 2, not the string "foo".

---

    Code
      plot(lat_bins_degrees(size = 40), col = 1)
    Condition
      Error in `plot()`:
      ! Argument `col` must be an object of class <character> of length 2, not the number 1.

---

    Code
      plot(lat_bins_degrees(size = 40), col = NA)
    Condition
      Error in `plot()`:
      ! Argument `col` must be an object of class <character> of length 2, not `NA`.

---

    Code
      plot(lat_bins_degrees(size = 40), type = "foo", xlim = "foo", ylim = "foo")
    Condition
      Error in `plot()`:
      ! Cannot pass arguments `type`, `xlim`, and `ylim` when calling `plot()` on an object of class <palaeoverse_lat_bins_degrees>.
      i These arguments are already set by `plot()` internally.

# plot is deprecated but still works

    Code
      lat_bins_degrees(plot = "6")
    Condition
      Warning:
      The `plot` argument of `lat_bins_degrees()` is deprecated as of palaeoverse 2.0.0.
      i Please use `plot()` on the output of this function instead.
      Error in `lat_bins_degrees()`:
      ! `plot` must be `TRUE` or `FALSE`, not the string "6".

