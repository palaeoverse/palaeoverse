# arg 'occdf' works

    Code
      palaeorotate_point(occdf = 10)
    Condition
      Error in `palaeorotate_point()`:
      ! `occdf` must be of class <data.frame>, not the number 10.

---

    Code
      palaeorotate_point(occdf = NA)
    Condition
      Error in `palaeorotate_point()`:
      ! `occdf` must be of class <data.frame>, not `NA`.

---

    Code
      palaeorotate_point(occdf = data.frame(lng = 10, lat = 5))
    Condition
      Error in `palaeorotate_point()`:
      ! Column "age" not found in `occdf`.

# palaeorotate_point() errors with unnamed args

    Code
      palaeorotate_point(occdf, "lng")
    Condition
      Error in `palaeorotate_point()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there is 1 argument that should be named.

---

    Code
      palaeorotate_point(occdf = occdf, "lng")
    Condition
      Error in `palaeorotate_point()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there is 1 argument that should be named.

---

    Code
      palaeorotate_point(occdf, "lng", "lat")
    Condition
      Error in `palaeorotate_point()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there are 2 arguments that should be named.

---

    Code
      palaeorotate_point(occdf, "lng", lat = "lat")
    Condition
      Error in `palaeorotate_point()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there is 1 argument that should be named.

# input checks for longitude

    Code
      palaeorotate_point(occdf = data.frame(lng = 210, lat = 40, age = 25))
    Condition
      Error in `palaeorotate_point()`:
      ! All values of column "lng" in `occdf` must be between -180 and 180.
      i Value(s) outside the range: 210.

---

    Code
      palaeorotate_point(occdf = data.frame(lng = NA, lat = 40, age = 25))
    Condition
      Error in `palaeorotate_point()`:
      ! Column "lng" in `occdf` must be of class <numeric>, not <logical>.

---

    Code
      palaeorotate_point(occdf = data.frame(lng = "a", lat = 40, age = 25))
    Condition
      Error in `palaeorotate_point()`:
      ! Column "lng" in `occdf` must be of class <numeric>, not <character>.

# input checks for latitude

    Code
      palaeorotate_point(occdf = data.frame(lng = 160, lat = 200, age = 25))
    Condition
      Error in `palaeorotate_point()`:
      ! All values of column "lat" in `occdf` must be between -90 and 90.
      i Value(s) outside the range: 200.

---

    Code
      palaeorotate_point(occdf = data.frame(lng = 40, lat = NA, age = 25))
    Condition
      Error in `palaeorotate_point()`:
      ! Column "lat" in `occdf` must be of class <numeric>, not <logical>.

---

    Code
      palaeorotate_point(occdf = data.frame(lng = 40, lat = "a", age = 25))
    Condition
      Error in `palaeorotate_point()`:
      ! Column "lat" in `occdf` must be of class <numeric>, not <character>.

# input checks values for age

    Code
      palaeorotate_point(occdf = data.frame(lng = 160, lat = 40, age = -1))
    Condition
      Error in `palaeorotate_point()`:
      ! All values of column "age" in `occdf` must be positive.
      i Value(s) outside the range: -1.

---

    Code
      palaeorotate_point(occdf = data.frame(lng = 160, lat = 40, age = NA))
    Condition
      Error in `palaeorotate_point()`:
      ! Column "age" in `occdf` must be of class <numeric>, not <logical>.

---

    Code
      palaeorotate_point(occdf = data.frame(lng = 160, lat = 40, age = "a"))
    Condition
      Error in `palaeorotate_point()`:
      ! Column "age" in `occdf` must be of class <numeric>, not <character>.

# arg 'model' works

    Code
      palaeorotate_point(occdf = occdf, model = NA)
    Condition
      Error in `palaeorotate_point()`:
      ! `model` must be a character vector, not `NA`.

---

    Code
      palaeorotate_point(occdf = occdf, model = character(0))
    Condition
      Error in `palaeorotate_point()`:
      ! `model` must select at least one model.

---

    Code
      palaeorotate_point(occdf = occdf, model = "MULLER2022")
    Condition
      Error in `palaeorotate_point()`:
      ! Selected model "MULLER2022" has recently been removed as it is not in a palaeomagnetic reference frame.
      i See `palaeorotate()` (`?palaeoverse::palaeorotate()`) for available models.

---

    Code
      palaeorotate_point(occdf = occdf, model = "GPlates")
    Condition
      Error in `palaeorotate_point()`:
      ! `model` must be one of "MERDITH2021", "MATTHEWS2016_pmag_ref", "TorsvikCocks2017", "GOLONKA", or "PALEOMAP", not "GPlates".

# palaeorotate() is deprecated but still works

    Code
      suppressWarnings(palaeorotate(occdf = occdf, method = "foo"))
    Condition
      Error in `palaeorotate()`:
      ! `method` must be one of "point" or "grid", not "foo".

---

    Code
      suppressWarnings(palaeorotate(occdf = occdf, method = NA))
    Condition
      Error in `palaeorotate()`:
      ! `method` must be a single string, not `NA`.

---

    Code
      suppressWarnings(palaeorotate(occdf = occdf, method = character(0)))
    Condition
      Error in `palaeorotate()`:
      ! `method` must be a single string, not an empty character vector.

---

    Code
      suppressWarnings(palaeorotate(occdf = occdf, method = c("point", "grid")))
    Condition
      Error in `palaeorotate()`:
      ! `method` must be a single string, not a character vector.

# arg 'uncertainty' works

    Code
      palaeorotate_point(occdf = dat, uncertainty = "GOONTHEN")
    Condition
      Error in `palaeorotate_point()`:
      ! `uncertainty` must be `TRUE` or `FALSE`, not the string "GOONTHEN".

---

    Code
      palaeorotate_point(occdf = dat, uncertainty = character(0))
    Condition
      Error in `palaeorotate_point()`:
      ! `uncertainty` must be `TRUE` or `FALSE`, not an empty character vector.

---

    Code
      palaeorotate_point(occdf = dat, uncertainty = 1)
    Condition
      Error in `palaeorotate_point()`:
      ! `uncertainty` must be `TRUE` or `FALSE`, not the number 1.

# arg 'round' works

    Code
      palaeorotate_point(occdf = occdf, round = TRUE)
    Condition
      Error in `palaeorotate_point()`:
      ! `round` must be a whole number or `NULL`, not `TRUE`.

---

    Code
      palaeorotate_point(occdf = occdf, round = NA)
    Condition
      Error in `palaeorotate_point()`:
      ! `round` must be a whole number or `NULL`, not `NA`.

---

    Code
      palaeorotate_point(occdf = occdf, round = numeric(0))
    Condition
      Error in `palaeorotate_point()`:
      ! `round` must be a whole number or `NULL`, not an empty numeric vector.

---

    Code
      palaeorotate_point(occdf = occdf, round = 1:2)
    Condition
      Error in `palaeorotate_point()`:
      ! `round` must be a whole number or `NULL`, not an integer vector.

# good error message if GPlates or Zenodo are not available

    Code
      palaeorotate_point(occdf = occdf, model = "PALEOMAP")
    Condition
      Error in `palaeorotate_point()`:
      ! GPlates Web Service is not available.
      i Either the website is down or you are not connected to the internet.

---

    Code
      palaeorotate_grid(occdf = occdf, model = "PALEOMAP")
    Condition
      Error in `palaeorotate_grid()`:
      ! Zenodo is not available.
      i Either the website is down or you are not connected to the internet.

