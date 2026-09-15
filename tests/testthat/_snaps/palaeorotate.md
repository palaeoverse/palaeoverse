# arg 'data' works

    Code
      palaeorotate(data = 10)
    Condition
      Error in `palaeorotate()`:
      ! `data` must be of class <data.frame>, not the number 10.

---

    Code
      palaeorotate(data = NA)
    Condition
      Error in `palaeorotate()`:
      ! `data` must be of class <data.frame>, not `NA`.

---

    Code
      palaeorotate(data = data.frame(lng = 10, lat = 5))
    Condition
      Error in `palaeorotate()`:
      ! Column "age" not found in `data`.

# palaeorotate errors with unnamed args

    Code
      palaeorotate(data, "lng")
    Condition
      Error in `palaeorotate()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

---

    Code
      palaeorotate(data = data, "lng")
    Condition
      Error in `palaeorotate()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

---

    Code
      palaeorotate(data, "lng", "lat")
    Condition
      Error in `palaeorotate()`:
      ! All arguments must be named (except for "data").
      i Currently, there are 2 arguments that should be named.

---

    Code
      palaeorotate(data, "lng", lat = "lat")
    Condition
      Error in `palaeorotate()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

# input checks for longitude

    Code
      palaeorotate(data = data.frame(lng = 210, lat = 40, age = 25))
    Condition
      Error in `palaeorotate()`:
      ! All values of column "lng" in `data` must be between -180 and 180.
      i Value(s) outside the range: 210.

---

    Code
      palaeorotate(data = data.frame(lng = NA, lat = 40, age = 25))
    Condition
      Error in `palaeorotate()`:
      ! Column "lng" in `data` must be <numeric>, not <logical>.

---

    Code
      palaeorotate(data = data.frame(lng = "a", lat = 40, age = 25))
    Condition
      Error in `palaeorotate()`:
      ! Column "lng" in `data` must be <numeric>, not <character>.

# input checks for latitude

    Code
      palaeorotate(data = data.frame(lng = 160, lat = 200, age = 25))
    Condition
      Error in `palaeorotate()`:
      ! All values of column "lat" in `data` must be between -90 and 90.
      i Value(s) outside the range: 200.

---

    Code
      palaeorotate(data = data.frame(lng = 40, lat = NA, age = 25))
    Condition
      Error in `palaeorotate()`:
      ! Column "lat" in `data` must be <numeric>, not <logical>.

---

    Code
      palaeorotate(data = data.frame(lng = 40, lat = "a", age = 25))
    Condition
      Error in `palaeorotate()`:
      ! Column "lat" in `data` must be <numeric>, not <character>.

# input checks values for age

    Code
      palaeorotate(data = data.frame(lng = 160, lat = 40, age = -1))
    Condition
      Error in `palaeorotate()`:
      ! All values of column "age" in `data` must be positive.
      i Value(s) outside the range: -1.

---

    Code
      palaeorotate(data = data.frame(lng = 160, lat = 40, age = NA))
    Condition
      Error in `palaeorotate()`:
      ! Column "age" in `data` must be <numeric>, not <logical>.

---

    Code
      palaeorotate(data = data.frame(lng = 160, lat = 40, age = "a"))
    Condition
      Error in `palaeorotate()`:
      ! Column "age" in `data` must be <numeric>, not <character>.

# arg 'model' works

    Code
      palaeorotate(data = data, method = "point", model = NA)
    Condition
      Error in `palaeorotate()`:
      ! `model` must be a character vector, not `NA`.

---

    Code
      palaeorotate(data = data, method = "point", model = character(0))
    Condition
      Error in `palaeorotate()`:
      ! `model` must select at least one model.

---

    Code
      palaeorotate(data = data, method = "point", model = "MULLER2022")
    Condition
      Error in `palaeorotate()`:
      ! Selected model "MULLER2022" has recently been removed as it is not in a palaeomagnetic reference frame.
      i See `palaeorotate()` (`?palaeoverse::palaeorotate()`) for available models.

---

    Code
      palaeorotate(data = data, method = "point", model = "GPlates")
    Condition
      Error in `palaeorotate()`:
      ! `model` must be one of "MERDITH2021", "MATTHEWS2016_pmag_ref", "TorsvikCocks2017", "GOLONKA", or "PALEOMAP", not "GPlates".

# arg 'method' works

    Code
      palaeorotate(data = data, method = "foo")
    Condition
      Error in `palaeorotate()`:
      ! `method` must be one of "point" or "grid", not "foo".

---

    Code
      palaeorotate(data = data, method = NA)
    Condition
      Error in `palaeorotate()`:
      ! `method` must be a single string, not `NA`.

---

    Code
      palaeorotate(data = data, method = character(0))
    Condition
      Error in `palaeorotate()`:
      ! `method` must be a single string, not an empty character vector.

---

    Code
      palaeorotate(data = data, method = c("point", "grid"))
    Condition
      Error in `palaeorotate()`:
      ! `method` must be a single string, not a character vector.

# arg 'uncertainty' works

    Code
      palaeorotate(data = dat, uncertainty = "GOONTHEN")
    Condition
      Error in `palaeorotate()`:
      ! `uncertainty` must be `TRUE` or `FALSE`, not the string "GOONTHEN".

---

    Code
      palaeorotate(data = dat, uncertainty = character(0))
    Condition
      Error in `palaeorotate()`:
      ! `uncertainty` must be `TRUE` or `FALSE`, not an empty character vector.

---

    Code
      palaeorotate(data = dat, uncertainty = 1)
    Condition
      Error in `palaeorotate()`:
      ! `uncertainty` must be `TRUE` or `FALSE`, not the number 1.

# arg 'round' works

    Code
      palaeorotate(data = data, round = TRUE)
    Condition
      Error in `palaeorotate()`:
      ! `round` must be a whole number or `NULL`, not `TRUE`.

---

    Code
      palaeorotate(data = data, round = NA)
    Condition
      Error in `palaeorotate()`:
      ! `round` must be a whole number or `NULL`, not `NA`.

---

    Code
      palaeorotate(data = data, round = numeric(0))
    Condition
      Error in `palaeorotate()`:
      ! `round` must be a whole number or `NULL`, not an empty numeric vector.

---

    Code
      palaeorotate(data = data, round = 1:2)
    Condition
      Error in `palaeorotate()`:
      ! `round` must be a whole number or `NULL`, not an integer vector.

# good error message if GPlates or Zenodo are not available

    Code
      palaeorotate(data = data, model = "PALEOMAP")
    Condition
      Error in `palaeorotate()`:
      ! GPlates Web Service is not available.
      i Either the website is down or you are not connected to the internet.

---

    Code
      palaeorotate(data = data, model = "PALEOMAP", method = "grid")
    Condition
      Error in `palaeorotate()`:
      ! Zenodo is not available.
      i Either the website is down or you are not connected to the internet.

