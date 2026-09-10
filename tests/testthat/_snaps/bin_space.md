# bin_space errors with unnamed args

    Code
      bin_space(data, "lng")
    Condition
      Error in `bin_space()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

---

    Code
      bin_space(data = data, "lng")
    Condition
      Error in `bin_space()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

---

    Code
      bin_space(data, "lng", "lat")
    Condition
      Error in `bin_space()`:
      ! All arguments must be named (except for "data").
      i Currently, there are 2 arguments that should be named.

---

    Code
      bin_space(data, "lng", lat = "lat")
    Condition
      Error in `bin_space()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

# bin_space error handling

    Code
      bin_space(data = matrix(tetrapods))
    Condition
      Error in `bin_space()`:
      ! `data` must be of class <data.frame>, not a list matrix.

---

    Code
      bin_space(data = tetrapods, spacing = NA)
    Condition
      Error in `bin_space()`:
      ! `spacing` must be of class <numeric>, not `NA`.

---

    Code
      bin_space(data = tetrapods, spacing = 1:2)
    Condition
      Error in `bin_space()`:
      ! `spacing` must be of length 1, not 2.

---

    Code
      bin_space(data = tetrapods, sub_grid = 1:2)
    Condition
      Error in `bin_space()`:
      ! `sub_grid` must be of length 1, not 2.

---

    Code
      bin_space(data = tetrapods, spacing = 1000, sub_grid = NA)
    Condition
      Error in `bin_space()`:
      ! `sub_grid` must be of class <numeric> or `NULL`, not `NA`.

---

    Code
      bin_space(data = tetrapods, return = "TRUE")
    Condition
      Error in `bin_space()`:
      ! `return` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    Code
      bin_space(data = tetrapods, lng = "long", lat = "latit")
    Condition
      Error in `bin_space()`:
      ! Column "latit" not found in `data`.

---

    Code
      bin_space(data = tetrapods, spacing = 1000, sub_grid = 1000)
    Condition
      Error in `bin_space()`:
      ! `spacing` and `sub_grid` values result in the same resolution.
      i Update `spacing` and/or `sub_grid` accordingly.

---

    Code
      bin_space(data = data)
    Condition
      Error in `bin_space()`:
      ! All values of column "lat" in `data` must be between -90 and 90.
      i Value(s) outside the range: 94.

---

    Code
      bin_space(data = data)
    Condition
      Error in `bin_space()`:
      ! Column "lat" in `data` must be <numeric>, not <character>.

---

    Code
      bin_space(data = data)
    Condition
      Error in `bin_space()`:
      ! All values of column "lng" in `data` must be between -180 and 180.
      i Value(s) outside the range: 184.

---

    Code
      bin_space(data = data)
    Condition
      Error in `bin_space()`:
      ! Column "lng" in `data` must be <numeric>, not <character>.

