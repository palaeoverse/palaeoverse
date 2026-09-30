# bin_space errors with unnamed args

    Code
      bin_space(occdf, space_bins(spacing = 1000), "lng")
    Condition
      Error in `bin_space()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there are 2 arguments that should be named.

---

    Code
      bin_space(occdf = occdf, space_bins(spacing = 1000), "lng")
    Condition
      Error in `bin_space()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there are 2 arguments that should be named.

---

    Code
      bin_space(occdf, space_bins(spacing = 1000), "lng", "lat")
    Condition
      Error in `bin_space()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there are 3 arguments that should be named.

---

    Code
      bin_space(occdf, space_bins(spacing = 1000), "lng", lat = "lat")
    Condition
      Error in `bin_space()`:
      ! All arguments must be named (except for `occdf`).
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
      ! `bins` must be of class <palaeoverse_space_bins>.
      i Hint: use `space_bins()` to create the spatial bins.

---

    Code
      bin_space(occdf = tetrapods, bins = 1:2)
    Condition
      Error in `bin_space()`:
      ! `bins` must be of class <palaeoverse_space_bins>.
      i Hint: use `space_bins()` to create the spatial bins.

---

    Code
      bin_space(occdf = tetrapods, bins = space_bins(spacing = 1000), lng = "long",
      lat = "latit")
    Message
      Average spacing between adjacent cells in the primary grid was set to 725.17 km.
      i H3 resolution: 1
    Condition
      Error in `bin_space()`:
      ! Column "latit" not found in `occdf`.

---

    Code
      bin_space(occdf, bins = space_bins(spacing = 1000))
    Message
      Average spacing between adjacent cells in the primary grid was set to 725.17 km.
      i H3 resolution: 1
    Condition
      Error in `bin_space()`:
      ! All values of column "lat" in `occdf` must be between -90 and 90.
      i Value(s) outside the range: 94.

---

    Code
      bin_space(occdf, bins = space_bins(spacing = 1000))
    Message
      Average spacing between adjacent cells in the primary grid was set to 725.17 km.
      i H3 resolution: 1
    Condition
      Error in `bin_space()`:
      ! Column "lat" in `occdf` must be of class <numeric>, not <character>.

---

    Code
      bin_space(occdf, bins = space_bins(spacing = 1000))
    Message
      Average spacing between adjacent cells in the primary grid was set to 725.17 km.
      i H3 resolution: 1
    Condition
      Error in `bin_space()`:
      ! All values of column "lng" in `occdf` must be between -180 and 180.
      i Value(s) outside the range: 184.

---

    Code
      bin_space(occdf, bins = space_bins(spacing = 1000))
    Message
      Average spacing between adjacent cells in the primary grid was set to 725.17 km.
      i H3 resolution: 1
    Condition
      Error in `bin_space()`:
      ! Column "lng" in `occdf` must be of class <numeric>, not <character>.

# plot argument works

    Code
      bin_space(occdf = occdf, bins = space_bins(spacing = 1000), plot = "foo")
    Message
      Average spacing between adjacent cells in the primary grid was set to 725.17 km.
      i H3 resolution: 1
    Condition
      Error in `bin_space()`:
      ! `plot` must be `TRUE` or `FALSE`, not the string "foo".

---

    Code
      bin_space(occdf = occdf, bins = space_bins(spacing = 1000), plot = logical(0))
    Message
      Average spacing between adjacent cells in the primary grid was set to 725.17 km.
      i H3 resolution: 1
    Condition
      Error in `bin_space()`:
      ! `plot` must be `TRUE` or `FALSE`, not an empty logical vector.

---

    Code
      bin_space(occdf = occdf, bins = space_bins(spacing = 1000), plot = 1)
    Message
      Average spacing between adjacent cells in the primary grid was set to 725.17 km.
      i H3 resolution: 1
    Condition
      Error in `bin_space()`:
      ! `plot` must be `TRUE` or `FALSE`, not the number 1.

# spacing, sub_grid, and return give good error messages

    Code
      bin_space(occdf = occdf, spacing = 1000)
    Condition
      Error in `bin_space()`:
      ! The `spacing` argument of `bin_space()` is no longer used as of palaeoverse 2.0.0.
      i Pass the output of `space_bins()` to the `bins` argument instead.

---

    Code
      bin_space(occdf = occdf, sub_grid = 1000)
    Condition
      Error in `bin_space()`:
      ! The `sub_grid` argument of `bin_space()` is no longer used as of palaeoverse 2.0.0.
      i Pass the output of `space_bins()` to the `bins` argument instead.

---

    Code
      bin_space(occdf = occdf, bins = space_bins(spacing = 1000), return = TRUE)
    Condition
      Error in `bin_space()`:
      ! The `return` argument of `bin_space()` is no longer used as of palaeoverse 2.0.0.
      i Pass the output of `space_bins()` to the `bins` argument instead.

