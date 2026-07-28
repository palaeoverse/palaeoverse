# bin_space error handling

    Code
      bin_space(occdf = matrix(tetrapods))
    Condition
      Error in `bin_space()`:
      ! `occdf` must be a data frame, not a list matrix.

---

    Code
      bin_space(occdf = tetrapods, spacing = NA)
    Condition
      Error in `bin_space()`:
      ! `spacing` must be a numeric value, not `NA`.

---

    Code
      bin_space(occdf = tetrapods, spacing = 1:2)
    Condition
      Error in `bin_space()`:
      ! `spacing` must be of length 1, not 2.

---

    Code
      bin_space(occdf = tetrapods, sub_grid = 1:2)
    Condition
      Error in `bin_space()`:
      ! `sub_grid` must be of length 1, not 2.

---

    Code
      bin_space(occdf = tetrapods, spacing = 1000, sub_grid = NA)
    Condition
      Error in `bin_space()`:
      ! `sub_grid` must be a numeric value or `NULL`, not `NA`.

---

    Code
      bin_space(occdf = tetrapods, return = "TRUE")
    Condition
      Error in `bin_space()`:
      ! `return` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    Code
      bin_space(occdf = tetrapods, lng = "long", lat = "latit")
    Condition
      Error in `bin_space()`:
      ! Column "latit" not found in `occdf`.

---

    Code
      bin_space(occdf = tetrapods, spacing = 1000, sub_grid = 1000)
    Condition
      Error in `bin_space()`:
      ! `spacing` and `sub_grid` values result in the same resolution.
      i Update `spacing` and/or `sub_grid` accordingly.

---

    Code
      bin_space(occdf = occdf)
    Condition
      Error in `bin_space()`:
      ! All values of column "lat" in `occdf` must be between -90 and 90.
      i Value(s) outside the range: 94.

---

    Code
      bin_space(occdf = occdf)
    Condition
      Error in `bin_space()`:
      ! Column "lat" in `occdf` must be numeric, not <character>.

---

    Code
      bin_space(occdf = occdf)
    Condition
      Error in `bin_space()`:
      ! All values of column "lng" in `occdf` must be between -180 and 180.
      i Value(s) outside the range: 184.

---

    Code
      bin_space(occdf = occdf)
    Condition
      Error in `bin_space()`:
      ! Column "lng" in `occdf` must be numeric, not <character>.

