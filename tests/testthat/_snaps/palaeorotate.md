# arg 'occdf' works

    Code
      palaeorotate(occdf = 10)
    Condition
      Error in `palaeorotate()`:
      ! `occdf` must be of class <data.frame>, not the number 10.

---

    Code
      palaeorotate(occdf = NA)
    Condition
      Error in `palaeorotate()`:
      ! `occdf` must be of class <data.frame>, not `NA`.

---

    Code
      palaeorotate(occdf = data.frame(lng = 10, lat = 5))
    Condition
      Error in `palaeorotate()`:
      ! Column "age" not found in `occdf`.

# input checks for longitude

    Code
      palaeorotate(occdf = data.frame(lng = 210, lat = 40, age = 25))
    Condition
      Error in `palaeorotate()`:
      ! All values of column "lng" in `occdf` must be between -180 and 180.
      i Value(s) outside the range: 210.

---

    Code
      palaeorotate(occdf = data.frame(lng = NA, lat = 40, age = 25))
    Condition
      Error in `palaeorotate()`:
      ! Column "lng" in `occdf` must be <numeric>, not <logical>.

---

    Code
      palaeorotate(occdf = data.frame(lng = "a", lat = 40, age = 25))
    Condition
      Error in `palaeorotate()`:
      ! Column "lng" in `occdf` must be <numeric>, not <character>.

# input checks for latitude

    Code
      palaeorotate(occdf = data.frame(lng = 160, lat = 200, age = 25))
    Condition
      Error in `palaeorotate()`:
      ! All values of column "lat" in `occdf` must be between -90 and 90.
      i Value(s) outside the range: 200.

---

    Code
      palaeorotate(occdf = data.frame(lng = 40, lat = NA, age = 25))
    Condition
      Error in `palaeorotate()`:
      ! Column "lat" in `occdf` must be <numeric>, not <logical>.

---

    Code
      palaeorotate(occdf = data.frame(lng = 40, lat = "a", age = 25))
    Condition
      Error in `palaeorotate()`:
      ! Column "lat" in `occdf` must be <numeric>, not <character>.

# input checks values for age

    Code
      palaeorotate(occdf = data.frame(lng = 160, lat = 40, age = -1))
    Condition
      Error in `palaeorotate()`:
      ! All values of column "age" in `occdf` must be between 0 and Inf.
      i Value(s) outside the range: -1.

---

    Code
      palaeorotate(occdf = data.frame(lng = 160, lat = 40, age = NA))
    Condition
      Error in `palaeorotate()`:
      ! Column "age" in `occdf` must be <numeric>, not <logical>.

---

    Code
      palaeorotate(occdf = data.frame(lng = 160, lat = 40, age = "a"))
    Condition
      Error in `palaeorotate()`:
      ! Column "age" in `occdf` must be <numeric>, not <character>.

# arg 'model' works

    Code
      palaeorotate(occdf = occdf, method = "point", model = NA)
    Condition
      Error in `palaeorotate()`:
      ! `model` must be a character vector, not `NA`.

---

    Code
      palaeorotate(occdf = occdf, method = "point", model = character(0))
    Condition
      Error in `palaeorotate()`:
      ! `model` must select at least one model.

---

    Code
      palaeorotate(occdf = occdf, method = "point", model = "MULLER2022")
    Condition
      Error in `palaeorotate()`:
      ! Selected model "MULLER2022" has recently been removed as it is not in a palaeomagnetic reference frame.
      i See `palaeorotate()` (`?palaeoverse::palaeorotate()`) for available models.

---

    Code
      palaeorotate(occdf = occdf, method = "point", model = "GPlates")
    Condition
      Error in `palaeorotate()`:
      ! `model` must be one of "MERDITH2021", "MATTHEWS2016_pmag_ref", "TorsvikCocks2017", "GOLONKA", or "PALEOMAP", not "GPlates".

# arg 'method' works

    Code
      palaeorotate(occdf = occdf, method = "foo")
    Condition
      Error in `palaeorotate()`:
      ! `method` must be one of "point" or "grid", not "foo".

---

    Code
      palaeorotate(occdf = occdf, method = NA)
    Condition
      Error in `palaeorotate()`:
      ! `method` must be a single string, not `NA`.

---

    Code
      palaeorotate(occdf = occdf, method = character(0))
    Condition
      Error in `palaeorotate()`:
      ! `method` must be a single string, not an empty character vector.

---

    Code
      palaeorotate(occdf = occdf, method = c("point", "grid"))
    Condition
      Error in `palaeorotate()`:
      ! `method` must be a single string, not a character vector.

# arg 'uncertainty' works

    Code
      palaeorotate(occdf = dat, uncertainty = "GOONTHEN")
    Condition
      Error in `palaeorotate()`:
      ! `uncertainty` must be `TRUE` or `FALSE`, not the string "GOONTHEN".

---

    Code
      palaeorotate(occdf = dat, uncertainty = character(0))
    Condition
      Error in `palaeorotate()`:
      ! `uncertainty` must be `TRUE` or `FALSE`, not an empty character vector.

---

    Code
      palaeorotate(occdf = dat, uncertainty = 1)
    Condition
      Error in `palaeorotate()`:
      ! `uncertainty` must be `TRUE` or `FALSE`, not the number 1.

# arg 'round' works

    Code
      palaeorotate(occdf = occdf, round = TRUE)
    Condition
      Error in `palaeorotate()`:
      ! `round` must be a whole number or `NULL`, not `TRUE`.

---

    Code
      palaeorotate(occdf = occdf, round = NA)
    Condition
      Error in `palaeorotate()`:
      ! `round` must be a whole number or `NULL`, not `NA`.

---

    Code
      palaeorotate(occdf = occdf, round = numeric(0))
    Condition
      Error in `palaeorotate()`:
      ! `round` must be a whole number or `NULL`, not an empty numeric vector.

---

    Code
      palaeorotate(occdf = occdf, round = 1:2)
    Condition
      Error in `palaeorotate()`:
      ! `round` must be a whole number or `NULL`, not an integer vector.

