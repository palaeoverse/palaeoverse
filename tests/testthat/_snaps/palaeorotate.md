# arg 'occdf' works

    Code
      palaeorotate(occdf = 10)
    Condition
      Error in `palaeorotate()`:
      ! Please supply `occdf` as a data.frame.

---

    Code
      palaeorotate(occdf = NA)
    Condition
      Error in `palaeorotate()`:
      ! Please supply `occdf` as a data.frame.

---

    Code
      palaeorotate(occdf = data.frame(lng = 10, lat = 5))
    Condition
      Error in `palaeorotate()`:
      ! Defined `lng`, `lat`, or `age` not found in `occdf`.

# input checks for longitude

    Code
      palaeorotate(occdf = data.frame(lng = 210, lat = 40, age = 25))
    Condition
      Error in `palaeorotate()`:
      ! `lng` values should be >= -180° and <= 180°.

---

    Code
      palaeorotate(occdf = data.frame(lng = NA, lat = 40, age = 25))
    Condition
      Error in `palaeorotate()`:
      ! `lng`, `lat` and `age` should be of class numeric.

---

    Code
      palaeorotate(occdf = data.frame(lng = "a", lat = 40, age = 25))
    Condition
      Error in `palaeorotate()`:
      ! `lng`, `lat` and `age` should be of class numeric.

# input checks for latitude

    Code
      palaeorotate(occdf = data.frame(lng = 160, lat = 200, age = 25))
    Condition
      Error in `palaeorotate()`:
      ! `lat` values should be >= -90° and <= 90°.

---

    Code
      palaeorotate(occdf = data.frame(lng = 40, lat = NA, age = 25))
    Condition
      Error in `palaeorotate()`:
      ! `lng`, `lat` and `age` should be of class numeric.

---

    Code
      palaeorotate(occdf = data.frame(lng = 40, lat = "a", age = 25))
    Condition
      Error in `palaeorotate()`:
      ! `lng`, `lat` and `age` should be of class numeric.

# input checks values for age

    Code
      palaeorotate(occdf = data.frame(lng = 160, lat = 40, age = -1))
    Condition
      Error in `palaeorotate()`:
      ! `age` contains negative values. Input ages should be positive.

---

    Code
      palaeorotate(occdf = data.frame(lng = 160, lat = 40, age = NA))
    Condition
      Error in `palaeorotate()`:
      ! `lng`, `lat` and `age` should be of class numeric.

---

    Code
      palaeorotate(occdf = data.frame(lng = 160, lat = 40, age = "a"))
    Condition
      Error in `palaeorotate()`:
      ! `lng`, `lat` and `age` should be of class numeric.

# arg 'model' works

    Code
      palaeorotate(occdf = occdf, method = "point", model = NA)
    Condition
      Error in `palaeorotate()`:
      ! Unavailable model(s). Choose one from the following: 
      MERDITH2021, MATTHEWS2016_pmag_ref, TorsvikCocks2017, GOLONKA, PALEOMAP

---

    Code
      palaeorotate(occdf = occdf, method = "point", model = character(0))
    Condition
      Error in `palaeorotate()`:
      ! `model` should have length > 1.

---

    Code
      palaeorotate(occdf = occdf, method = "point", model = "MULLER2022")
    Condition
      Error in `palaeorotate()`:
      ! Selected model(s) (MULLER2022) have recently been removed as they are not in a palaeomagnetic reference frame. See details for available models.

---

    Code
      palaeorotate(occdf = occdf, method = "point", model = "GPlates")
    Condition
      Error in `palaeorotate()`:
      ! Unavailable model(s). Choose one from the following: 
      MERDITH2021, MATTHEWS2016_pmag_ref, TorsvikCocks2017, GOLONKA, PALEOMAP

# arg 'method' works

    Code
      palaeorotate(occdf = occdf, method = "foo")
    Condition
      Error in `palaeorotate()`:
      ! `method` should be either 'grid' or 'point'.

---

    Code
      palaeorotate(occdf = occdf, method = NA)
    Condition
      Error in `palaeorotate()`:
      ! `method` should be either 'grid' or 'point'.

---

    Code
      palaeorotate(occdf = occdf, method = character(0))
    Condition
      Error in `palaeorotate()`:
      ! `method` should have length 1.

---

    Code
      palaeorotate(occdf = occdf, method = c("point", "grid"))
    Condition
      Error in `palaeorotate()`:
      ! `method` should have length 1.

# arg 'uncertainty' works

    Code
      palaeorotate(occdf = dat, uncertainty = "GOONTHEN")
    Condition
      Error in `palaeorotate()`:
      ! `uncertainty` should be of class logical (TRUE/FALSE).

---

    Code
      palaeorotate(occdf = dat, uncertainty = character(0))
    Condition
      Error in `palaeorotate()`:
      ! `uncertainty` should be of class logical (TRUE/FALSE).

---

    Code
      palaeorotate(occdf = dat, uncertainty = 1)
    Condition
      Error in `palaeorotate()`:
      ! `uncertainty` should be of class logical (TRUE/FALSE).

# arg 'round' works

    Code
      palaeorotate(occdf = occdf, round = TRUE)
    Condition
      Error in `palaeorotate()`:
      ! `round` should be NULL or of class numeric.

---

    Code
      palaeorotate(occdf = occdf, round = NA)
    Condition
      Error in `palaeorotate()`:
      ! `round` should be NULL or of class numeric.

---

    Code
      palaeorotate(occdf = occdf, round = numeric(0))
    Condition
      Error in `FUN()`:
      ! invalid second argument of length 0

