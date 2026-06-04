# lat_bins_area works

    Code
      lat_bins_area(n = "10")
    Condition
      Error in `lat_bins_area()`:
      ! `n` should be a numeric.

---

    Code
      lat_bins_area(max = 100)
    Condition
      Error in `lat_bins_area()`:
      ! `max` should be less than 90 and more than -90.

---

    Code
      lat_bins_area(min = 90, max = -90)
    Condition
      Error in `lat_bins_area()`:
      ! `min` should be less than `max`.

---

    Code
      lat_bins_area(min = 100)
    Condition
      Error in `lat_bins_area()`:
      ! `min` should be less than 90 and more than -90.

---

    Code
      lat_bins_area(plot = "TRUE")
    Condition
      Error in `lat_bins_area()`:
      ! `plot` should be logical (TRUE/FALSE).

---

    Code
      lat_bins_area(r = "Earth")
    Condition
      Error in `lat_bins_area()`:
      ! `r` should be a numeric.

---

    Code
      lat_bins_area(n = 3.5)
    Condition
      Error in `lat_bins_area()`:
      ! `n` should be an integer (whole number).

