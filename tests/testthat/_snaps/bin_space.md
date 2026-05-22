# bin_space error handling

    Code
      bin_space(occdf = matrix(tetrapods))
    Condition
      Error in `bin_space()`:
      ! occdf should be of class dataframe

---

    Code
      bin_space(occdf = tetrapods, spacing = NA)
    Condition
      Error in `bin_space()`:
      ! `spacing` should be of class numeric

---

    Code
      bin_space(occdf = tetrapods, spacing = 1000, sub_grid = NA)
    Condition
      Error in `bin_space()`:
      ! `sub_grid` should be of class numeric or NULL

---

    Code
      bin_space(occdf = tetrapods, return = "TRUE")
    Condition
      Error in `bin_space()`:
      ! `return` should be logical (TRUE/FALSE)

---

    Code
      bin_space(occdf = tetrapods, lng = "long", lat = "latit")
    Condition
      Error in `bin_space()`:
      ! input column names do not exist in `occdf`

---

    Code
      bin_space(occdf = tetrapods, spacing = 1000, sub_grid = 1000)
    Condition
      Error in `bin_space()`:
      ! `spacing` and `sub_grid` values result in the same resolution.
          Update `spacing` and/or `sub_grid` accordingly.

---

    Code
      bin_space(occdf = occdf)
    Condition
      Error in `bin_space()`:
      ! Latitudinal coordinates should be more than -90 and less than 90

---

    Code
      bin_space(occdf = occdf)
    Condition
      Error in `bin_space()`:
      ! input coordinates are not of class numeric

---

    Code
      bin_space(occdf = occdf)
    Condition
      Error in `bin_space()`:
      ! Longitudinal coordinates should be more than -180 and less than 180

---

    Code
      bin_space(occdf = occdf)
    Condition
      Error in `bin_space()`:
      ! input coordinates are not of class numeric

