# lat_bins_degrees() works

    Code
      lat_bins_degrees(size = 100)
    Condition
      Error in `lat_bins_degrees()`:
      ! `size` should be more than 0 and less than or equal to 90

---

    Code
      lat_bins_degrees(max = 100)
    Condition
      Error in `lat_bins_degrees()`:
      ! `max` should be less than 90 and more than -90

---

    Code
      lat_bins_degrees(min = 100)
    Condition
      Error in `lat_bins_degrees()`:
      ! `min` should be less than 90 and more than -90

---

    Code
      lat_bins_degrees(max = 10, min = 20)
    Condition
      Error in `lat_bins_degrees()`:
      ! `min` should be less than `max`

---

    Code
      lat_bins_degrees(fit = 1)
    Condition
      Error in `lat_bins_degrees()`:
      ! `fit` should be logical (TRUE/FALSE)

---

    Code
      lat_bins_degrees(size = "10")
    Condition
      Error in `lat_bins_degrees()`:
      ! `size` should be a numeric

---

    Code
      lat_bins_degrees(plot = "TRUE")
    Condition
      Error in `lat_bins_degrees()`:
      ! `plot` should be logical (TRUE/FALSE)

