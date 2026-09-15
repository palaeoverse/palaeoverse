# bin_space errors with unnamed args

    Code
      bin_space(occdf, space_bins(1000), "lng")
    Condition
      Error in `bin_space()`:
      ! All arguments must be named (except for "occdf").
      i Currently, there are 2 arguments that should be named.

---

    Code
      bin_space(occdf = occdf, space_bins(1000), "lng")
    Condition
      Error in `bin_space()`:
      ! All arguments must be named (except for "occdf").
      i Currently, there are 2 arguments that should be named.

---

    Code
      bin_space(occdf, space_bins(1000), "lng", "lat")
    Condition
      Error in `bin_space()`:
      ! All arguments must be named (except for "occdf").
      i Currently, there are 3 arguments that should be named.

---

    Code
      bin_space(occdf, space_bins(1000), "lng", lat = "lat")
    Condition
      Error in `bin_space()`:
      ! All arguments must be named (except for "occdf").
      i Currently, there are 2 arguments that should be named.

# bin_space error handling

    Code
      bin_space(occdf = matrix(tetrapods))
    Condition
      Error in `bin_space()`:
      ! `occdf` must be of class <data.frame>, not a list matrix.

---

    Code
      bin_space(occdf = tetrapods, bins = NA)
    Condition
      Error in `bin_space()`:
      ! `bins` must be of class <palaeo_space_bins> or <sfc_POLYGON>.
      i Hint: you can create space bins with `space_bins()`.

---

    Code
      bin_space(occdf = tetrapods, bins = 1:2)
    Condition
      Error in `bin_space()`:
      ! `bins` must be of class <palaeo_space_bins> or <sfc_POLYGON>.
      i Hint: you can create space bins with `space_bins()`.

---

    Code
      bin_space(occdf = tetrapods, bins = space_bins(1000), lng = "long", lat = "latit")
    Condition
      Error in `bin_space()`:
      ! Column "latit" not found in `occdf`.

---

    Code
      bin_space(occdf, space_bins(1000))
    Condition
      Error in `bin_space()`:
      ! All arguments must be named (except for "occdf").
      i Currently, there is 1 argument that should be named.

---

    Code
      bin_space(occdf, space_bins(1000))
    Condition
      Error in `bin_space()`:
      ! All arguments must be named (except for "occdf").
      i Currently, there is 1 argument that should be named.

---

    Code
      bin_space(occdf, space_bins(1000))
    Condition
      Error in `bin_space()`:
      ! All arguments must be named (except for "occdf").
      i Currently, there is 1 argument that should be named.

---

    Code
      bin_space(occdf, space_bins(1000))
    Condition
      Error in `bin_space()`:
      ! All arguments must be named (except for "occdf").
      i Currently, there is 1 argument that should be named.

# plot argument works

    Code
      bin_space(occdf = occdf, bins = space_bins(1000), plot = "foo")
    Condition
      Error in `bin_space()`:
      ! `plot` must be `TRUE` or `FALSE`, not the string "foo".

---

    Code
      bin_space(occdf = occdf, bins = space_bins(1000), plot = logical(0))
    Condition
      Error in `bin_space()`:
      ! `plot` must be `TRUE` or `FALSE`, not an empty logical vector.

---

    Code
      bin_space(occdf = occdf, bins = space_bins(1000), plot = 1)
    Condition
      Error in `bin_space()`:
      ! `plot` must be `TRUE` or `FALSE`, not the number 1.

