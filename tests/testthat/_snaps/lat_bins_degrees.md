# lat_bins_degrees() basic usage works

    Code
      lat_bins_degrees()
    Output
         bin min mid max
      1    1  80  85  90
      2    2  70  75  80
      3    3  60  65  70
      4    4  50  55  60
      5    5  40  45  50
      6    6  30  35  40
      7    7  20  25  30
      8    8  10  15  20
      9    9   0   5  10
      10  10 -10  -5   0
      11  11 -20 -15 -10
      12  12 -30 -25 -20
      13  13 -40 -35 -30
      14  14 -50 -45 -40
      15  15 -60 -55 -50
      16  16 -70 -65 -60
      17  17 -80 -75 -70
      18  18 -90 -85 -80

# argument 'size' works

    Code
      lat_bins_degrees(size = 100)
    Condition
      Error in `lat_bins_degrees()`:
      ! `size` should be more than 0 and less than or equal to 90

---

    Code
      lat_bins_degrees(size = numeric(0))
    Condition
      Error in `lat_bins_degrees()`:
      ! `size` must have length 1.

---

    Code
      lat_bins_degrees(size = c(10, 20))
    Condition
      Error in `lat_bins_degrees()`:
      ! `size` must have length 1.

# arguments 'min' and 'max' work

    Code
      lat_bins_degrees(min = 500)
    Condition
      Error in `lat_bins_degrees()`:
      ! `min` should be less than 90 and more than -90

---

    Code
      lat_bins_degrees(min = "a")
    Condition
      Error in `lat_bins_degrees()`:
      ! `min` should be less than 90 and more than -90

---

    Code
      lat_bins_degrees(min = NA)
    Condition
      Error in `lat_bins_degrees()`:
      ! `min` should be less than 90 and more than -90

---

    Code
      lat_bins_degrees(min = numeric(0))
    Condition
      Error in `lat_bins_degrees()`:
      ! `min` must have length 1.

---

    Code
      lat_bins_degrees(min = c(1, 2))
    Condition
      Error in `lat_bins_degrees()`:
      ! `min` must have length 1.

---

    Code
      lat_bins_degrees(max = 500)
    Condition
      Error in `lat_bins_degrees()`:
      ! `max` should be less than 90 and more than -90

---

    Code
      lat_bins_degrees(max = "a")
    Condition
      Error in `lat_bins_degrees()`:
      ! `max` should be less than 90 and more than -90

---

    Code
      lat_bins_degrees(max = NA)
    Condition
      Error in `lat_bins_degrees()`:
      ! `max` should be less than 90 and more than -90

---

    Code
      lat_bins_degrees(max = numeric(0))
    Condition
      Error in `lat_bins_degrees()`:
      ! `max` must have length 1.

---

    Code
      lat_bins_degrees(max = c(1, 2))
    Condition
      Error in `lat_bins_degrees()`:
      ! `max` must have length 1.

---

    Code
      lat_bins_degrees(min = 30, max = 10)
    Condition
      Error in `lat_bins_degrees()`:
      ! `min` should be less than `max`

---

    Code
      lat_bins_degrees(min = 30, max = 30)
    Condition
      Error in `lat_bins_degrees()`:
      ! `min` should be less than `max`

# argument 'fit' works

    Code
      lat_bins_degrees(fit = 100)
    Condition
      Error in `lat_bins_degrees()`:
      ! `fit` should be logical (TRUE/FALSE)

---

    Code
      lat_bins_degrees(fit = logical(0))
    Condition
      Error in `lat_bins_degrees()`:
      ! `fit` must have length 1.

---

    Code
      lat_bins_degrees(fit = NA)
    Condition
      Error in `lat_bins_degrees()`:
      ! `fit` should be logical (TRUE/FALSE)

---

    Code
      lat_bins_degrees(fit = c(TRUE, TRUE))
    Condition
      Error in `lat_bins_degrees()`:
      ! `fit` must have length 1.

# argument 'plot' works

    Code
      lat_bins_degrees(plot = 100)
    Condition
      Error in `lat_bins_degrees()`:
      ! `plot` should be logical (TRUE/FALSE)

---

    Code
      lat_bins_degrees(plot = logical(0))
    Condition
      Error in `lat_bins_degrees()`:
      ! `plot` must have length 1.

---

    Code
      lat_bins_degrees(plot = NA)
    Condition
      Error in `lat_bins_degrees()`:
      ! `plot` should be logical (TRUE/FALSE)

---

    Code
      lat_bins_degrees(plot = c(TRUE, TRUE))
    Condition
      Error in `lat_bins_degrees()`:
      ! `plot` must have length 1.

