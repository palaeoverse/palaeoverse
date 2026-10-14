# bin_space() overwrites existing cell columns

    Code
      out <- bin_space(bin_space(occdf, bins = space_bins(spacing = 1000)), bins = space_bins(
        spacing = 900))
    Message
      i H3 resolution: 1
      i Average spacing between adjacent cells: 725.17 km.
      i Average cell area: 609,788.4 sq. km.
      i H3 resolution: 1
      i Average spacing between adjacent cells: 725.17 km.
      i Average cell area: 609,788.4 sq. km.
      Overwriting columns "cell_ID_1", "cell_centroid_lat_1", and "cell_centroid_lng_1".

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
      i H3 resolution: 1
      i Average spacing between adjacent cells: 725.17 km.
      i Average cell area: 609,788.4 sq. km.
    Condition
      Error in `bin_space()`:
      ! Column "latit" not found in `occdf`.

---

    Code
      bin_space(occdf, bins = space_bins(spacing = 1000))
    Message
      i H3 resolution: 1
      i Average spacing between adjacent cells: 725.17 km.
      i Average cell area: 609,788.4 sq. km.
    Condition
      Error in `bin_space()`:
      ! All values of column "lat" in `occdf` must be between -90 and 90.
      i Value(s) outside the range: 94.

---

    Code
      bin_space(occdf, bins = space_bins(spacing = 1000))
    Message
      i H3 resolution: 1
      i Average spacing between adjacent cells: 725.17 km.
      i Average cell area: 609,788.4 sq. km.
    Condition
      Error in `bin_space()`:
      ! Column "lat" in `occdf` must be of class <numeric>, not <character>.

---

    Code
      bin_space(occdf, bins = space_bins(spacing = 1000))
    Message
      i H3 resolution: 1
      i Average spacing between adjacent cells: 725.17 km.
      i Average cell area: 609,788.4 sq. km.
    Condition
      Error in `bin_space()`:
      ! All values of column "lng" in `occdf` must be between -180 and 180.
      i Value(s) outside the range: 184.

---

    Code
      bin_space(occdf, bins = space_bins(spacing = 1000))
    Message
      i H3 resolution: 1
      i Average spacing between adjacent cells: 725.17 km.
      i Average cell area: 609,788.4 sq. km.
    Condition
      Error in `bin_space()`:
      ! Column "lng" in `occdf` must be of class <numeric>, not <character>.

# plot() works with several resolutions

    Code
      plot(out)
    Condition
      Error in `plot()`:
      ! `x` must contain at least one column named "cell_ID_<resolution>".

# bin_space plotting with extra args works

    Code
      plot(out, add = TRUE)
    Condition
      Error in `plot()`:
      ! Cannot pass argument `add` when calling `plot()` on an object of class <palaeoverse_bin_space>.
      i This argument is already set by `plot()` internally.

# bin_space plotting with custom colours works

    Code
      plot(out, col = c("red", "blue", "green"))
    Condition
      Error in `plot()`:
      ! Argument `col` must be of class <character> and of length 1 or 2 (i.e. the number of resolutions in `x`).

---

    Code
      plot(out, col = 1)
    Condition
      Error in `plot()`:
      ! Argument `col` must be of class <character> and of length 1 or 2 (i.e. the number of resolutions in `x`).

# plot is deprecated but still works

    Code
      bin_space(occdf = occdf, bins = space_bins(spacing = 1000), plot = "foo")
    Message
      i H3 resolution: 1
      i Average spacing between adjacent cells: 725.17 km.
      i Average cell area: 609,788.4 sq. km.
    Condition
      Warning:
      The `plot` argument of `bin_space()` is deprecated as of palaeoverse 2.0.0.
      i Please use `plot()` on the output of this function instead.
      Error in `bin_space()`:
      ! `plot` must be `TRUE` or `FALSE`, not the string "foo".

---

    Code
      bin_space(occdf = occdf, bins = space_bins(spacing = 1000), plot = logical(0))
    Message
      i H3 resolution: 1
      i Average spacing between adjacent cells: 725.17 km.
      i Average cell area: 609,788.4 sq. km.
    Condition
      Warning:
      The `plot` argument of `bin_space()` is deprecated as of palaeoverse 2.0.0.
      i Please use `plot()` on the output of this function instead.
      Error in `bin_space()`:
      ! `plot` must be `TRUE` or `FALSE`, not an empty logical vector.

---

    Code
      bin_space(occdf = occdf, bins = space_bins(spacing = 1000), plot = 1)
    Message
      i H3 resolution: 1
      i Average spacing between adjacent cells: 725.17 km.
      i Average cell area: 609,788.4 sq. km.
    Condition
      Warning:
      The `plot` argument of `bin_space()` is deprecated as of palaeoverse 2.0.0.
      i Please use `plot()` on the output of this function instead.
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

