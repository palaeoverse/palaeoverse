# lat_bins_area works

    Code
      lat_bins_area()
    Output
         bin        min        mid        max         area  area_prop
      1    1  56.442690  73.221345  90.000000 4.250537e+13 0.08333333
      2    2  41.810315  49.126503  56.442690 4.250537e+13 0.08333333
      3    3  30.000000  35.905157  41.810315 4.250537e+13 0.08333333
      4    4  19.471221  24.735610  30.000000 4.250537e+13 0.08333333
      5    5   9.594068  14.532644  19.471221 4.250537e+13 0.08333333
      6    6   0.000000   4.797034   9.594068 4.250537e+13 0.08333333
      7    7  -9.594068  -4.797034   0.000000 4.250537e+13 0.08333333
      8    8 -19.471221 -14.532644  -9.594068 4.250537e+13 0.08333333
      9    9 -30.000000 -24.735610 -19.471221 4.250537e+13 0.08333333
      10  10 -41.810315 -35.905157 -30.000000 4.250537e+13 0.08333333
      11  11 -56.442690 -49.126503 -41.810315 4.250537e+13 0.08333333
      12  12 -90.000000 -73.221345 -56.442690 4.250537e+13 0.08333333

---

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

# lat_bins_area errors if min == max

    Code
      lat_bins_area(min = 90, max = 90)
    Condition
      Error in `lat_bins_area()`:
      ! `min` must be less than `max`.

# lat_bins_area errors with unnamed args

    Code
      lat_bins_area(10, 1)
    Condition
      Error in `lat_bins_area()`:
      ! All arguments must be named.
      i Currently, there are 2 arguments that should be named.

---

    Code
      lat_bins_area(n = 10, 1)
    Condition
      Error in `lat_bins_area()`:
      ! All arguments must be named.
      i Currently, there is 1 argument that should be named.

---

    Code
      lat_bins_area(10, 1, 2)
    Condition
      Error in `lat_bins_area()`:
      ! All arguments must be named.
      i Currently, there are 3 arguments that should be named.

---

    Code
      lat_bins_area(10, 1, max = 2)
    Condition
      Error in `lat_bins_area()`:
      ! All arguments must be named.
      i Currently, there are 2 arguments that should be named.

# partial matching of argument names is forbidden

    Code
      lat_bins_area(10, mi = 1)
    Condition
      Error in `lat_bins_area()`:
      ! Argument names must be fully written.
      i Partially matched argument name: "mi"

---

    Code
      lat_bins_area(10, mi = 1, ma = 2)
    Condition
      Error in `lat_bins_area()`:
      ! Argument names must be fully written.
      i Partially matched argument names: "mi" and "ma"

# lat_bins_area errors with wrong inputs

    Code
      lat_bins_area(n = "10")
    Condition
      Error in `lat_bins_area()`:
      ! `n` must be a whole number, not the string "10".

---

    Code
      lat_bins_area(n = -1)
    Condition
      Error in `lat_bins_area()`:
      ! `n` must be a whole number larger than or equal to 1, not the number -1.

---

    Code
      lat_bins_area(n = numeric(0))
    Condition
      Error in `lat_bins_area()`:
      ! `n` must be a whole number, not an empty numeric vector.

---

    Code
      lat_bins_area(n = 3.5)
    Condition
      Error in `lat_bins_area()`:
      ! `n` must be a whole number, not the number 3.5.

---

    Code
      lat_bins_area(max = 100)
    Condition
      Error in `lat_bins_area()`:
      ! `max` must be a number between -90 and 90, not the number 100.

---

    Code
      lat_bins_area(max = numeric(0))
    Condition
      Error in `lat_bins_area()`:
      ! `max` must be a number, not an empty numeric vector.

---

    Code
      lat_bins_area(min = 100)
    Condition
      Error in `lat_bins_area()`:
      ! `min` must be a number between -90 and 90, not the number 100.

---

    Code
      lat_bins_area(min = numeric(0))
    Condition
      Error in `lat_bins_area()`:
      ! `min` must be a number, not an empty numeric vector.

---

    Code
      lat_bins_area(min = 90, max = -90)
    Condition
      Error in `lat_bins_area()`:
      ! `min` must be less than `max`.

---

    Code
      lat_bins_area(plot = "TRUE")
    Condition
      Error in `lat_bins_area()`:
      ! `plot` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    Code
      lat_bins_area(plot = logical(0))
    Condition
      Error in `lat_bins_area()`:
      ! `plot` must be `TRUE` or `FALSE`, not an empty logical vector.

---

    Code
      lat_bins_area(r = "Earth")
    Condition
      Error in `lat_bins_area()`:
      ! `r` must be a number, not the string "Earth".

---

    Code
      lat_bins_area(r = numeric(0))
    Condition
      Error in `lat_bins_area()`:
      ! `r` must be a number, not an empty numeric vector.

---

    Code
      lat_bins_area(r = -1)
    Condition
      Error in `lat_bins_area()`:
      ! `r` must be a number larger than or equal to 0, not the number -1.

