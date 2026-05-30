# palaeorotate() error handling works

    Code
      palaeorotate(occdf = 10)
    Condition
      Error in `palaeorotate()`:
      ! Please supply `occdf` as a data.frame.

---

    Code
      palaeorotate(occdf = data.frame(lng = 10, lat = 5))
    Condition
      Error in `palaeorotate()`:
      ! Defined `lng`, `lat`, or `age` not found in `occdf`.

---

    Code
      palaeorotate(occdf = data.frame(lng = 10, lat = 5, age = TRUE))
    Condition
      Error in `palaeorotate()`:
      ! `lng`, `lat` and `age` should be of class numeric.

---

    Code
      palaeorotate(occdf = data.frame(lng = 10, lat = 5, age = -5))
    Condition
      Error in `palaeorotate()`:
      ! `age` contains negative values. Input ages should be positive.

---

    Code
      palaeorotate(occdf = data.frame(lng = 10, lat = 95, age = 25))
    Condition
      Error in `palaeorotate()`:
      ! `lat` values should be >= -90° and <= 90°.

---

    Code
      palaeorotate(occdf = data.frame(lng = 210, lat = 40, age = 25))
    Condition
      Error in `palaeorotate()`:
      ! `lng` values should be >= -180° and <= 180°.

---

    Code
      palaeorotate(occdf = data.frame(lng = 10, lat = 40, age = 25), method = "API")
    Condition
      Error in `palaeorotate()`:
      ! `method` should be either 'grid' or 'point'.

---

    Code
      palaeorotate(occdf = data.frame(lng = 110, lat = 40, age = 25), round = TRUE)
    Condition
      Error in `palaeorotate()`:
      ! `round` should be NULL or of class numeric.

---

    Code
      palaeorotate(occdf = data.frame(lng = 110, lat = 40, age = 25), uncertainty = "GOONTHEN")
    Condition
      Error in `palaeorotate()`:
      ! `uncertainty` should be of class logical (TRUE/FALSE).

# palaeorotate() point method works

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

