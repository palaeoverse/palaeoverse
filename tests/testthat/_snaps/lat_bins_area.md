# lat_bins_area works

    Code
      lat_bins_area(n = 6)
    Output
        bin       min       mid       max         area area_prop
      1   1  41.81031  65.90516  90.00000 8.501075e+13 0.1666667
      2   2  19.47122  30.64077  41.81031 8.501075e+13 0.1666667
      3   3   0.00000   9.73561  19.47122 8.501075e+13 0.1666667
      4   4 -19.47122  -9.73561   0.00000 8.501075e+13 0.1666667
      5   5 -41.81031 -30.64077 -19.47122 8.501075e+13 0.1666667
      6   6 -90.00000 -65.90516 -41.81031 8.501075e+13 0.1666667

---

    Code
      lat_bins_area(n = 6, min = 0, max = 90)
    Output
        bin       min       mid       max         area area_prop
      1   1 56.442690 73.221345 90.000000 4.250537e+13 0.1666667
      2   2 41.810315 49.126503 56.442690 4.250537e+13 0.1666667
      3   3 30.000000 35.905157 41.810315 4.250537e+13 0.1666667
      4   4 19.471221 24.735610 30.000000 4.250537e+13 0.1666667
      5   5  9.594068 14.532644 19.471221 4.250537e+13 0.1666667
      6   6  0.000000  4.797034  9.594068 4.250537e+13 0.1666667

# lat_bins_area errors with wrong inputs

    Code
      lat_bins_area(n = "10")
    Condition
      Error in `lat_bins_area()`:
      ! `n` should be a numeric.

---

    Code
      lat_bins_area(n = -1)
    Condition
      Error in `data.frame()`:
      ! arguments imply differing number of rows: 3, 1, 0

---

    Code
      lat_bins_area(n = numeric(0))
    Condition
      Error in `if (n%%1 != 0) ...`:
      ! argument is of length zero

---

    Code
      lat_bins_area(n = 3.5)
    Condition
      Error in `lat_bins_area()`:
      ! `n` should be an integer (whole number).

---

    Code
      lat_bins_area(max = 100)
    Condition
      Error in `lat_bins_area()`:
      ! `max` should be less than 90 and more than -90.

---

    Code
      lat_bins_area(max = numeric(0))
    Condition
      Error in `if (max > 90 || max < -90) ...`:
      ! missing value where TRUE/FALSE needed

---

    Code
      lat_bins_area(min = 100)
    Condition
      Error in `lat_bins_area()`:
      ! `min` should be less than 90 and more than -90.

---

    Code
      lat_bins_area(min = numeric(0))
    Condition
      Error in `if (min > 90 || min < -90) ...`:
      ! missing value where TRUE/FALSE needed

---

    Code
      lat_bins_area(min = 90, max = -90)
    Condition
      Error in `lat_bins_area()`:
      ! `min` should be less than `max`.

---

    Code
      lat_bins_area(plot = "TRUE")
    Condition
      Error in `lat_bins_area()`:
      ! `plot` should be logical (TRUE/FALSE).

---

    Code
      lat_bins_area(plot = logical(0))
    Condition
      Error in `if (plot) ...`:
      ! argument is of length zero

---

    Code
      lat_bins_area(r = "Earth")
    Condition
      Error in `lat_bins_area()`:
      ! `r` should be a numeric.

---

    Code
      lat_bins_area(r = numeric(0))
    Condition
      Error in `data.frame()`:
      ! arguments imply differing number of rows: 12, 0

