# axis_geo() error handling

    Code
      axis_geo(height = c(0.5, 0.5))
    Condition
      Error in `axis_geo()`:
      ! `height` must be a single numeric value per scale.

---

    Code
      axis_geo(fill = 5)
    Condition
      Error in `axis_geo()`:
      ! `fill` must be a single character value per scale or `NULL`.

---

    Code
      axis_geo(lab = "true")
    Condition
      Error in `axis_geo()`:
      ! `lab` must be a single logical value per scale.

---

    Code
      axis_geo(lab_col = 42)
    Condition
      Error in `axis_geo()`:
      ! `lab_col` must be a single character value per scale or `NULL`.

---

    Code
      axis_geo(lab_size = "big")
    Condition
      Error in `axis_geo()`:
      ! `lab_size` must be a single numeric value per scale.

---

    Code
      axis_geo(rot = NULL)
    Condition
      Error in `axis_geo()`:
      ! `rot` must be a single numeric value per scale.

---

    Code
      axis_geo(abbr = c("true", 1))
    Condition
      Error in `axis_geo()`:
      ! `abbr` must be a single logical value per scale.

---

    Code
      axis_geo(skip = c(1, 2, 3))
    Condition
      Error in `axis_geo()`:
      ! All values of `skip` must be of class <character> or `NULL`.

---

    Code
      axis_geo(center_end_labels = c(FALSE, TRUE))
    Condition
      Error in `axis_geo()`:
      ! `center_end_labels` must be a single logical value per scale.

---

    Code
      axis_geo(autofit = c(FALSE, TRUE))
    Condition
      Error in `axis_geo()`:
      ! `autofit` must be a single logical value per scale.

---

    Code
      axis_geo(bord_col = TRUE)
    Condition
      Error in `axis_geo()`:
      ! All values of `bord_col` must be of class <character> or `NULL`.

---

    Code
      axis_geo(lty = 7)
    Condition
      Error in `axis_geo()`:
      ! All values of `lty` must be of class <character> or `NULL`.

---

    Code
      axis_geo(lwd = "thin")
    Condition
      Error in `axis_geo()`:
      ! All values of `lwd` must be of class <numeric> or `NULL`.

---

    Code
      axis_geo(side = 5)
    Condition
      Error in `axis_geo()`:
      ! `side` must be 1, 2, 3, or 4.

---

    Code
      axis_geo(phylo = TRUE)
    Condition
      Error in `axis_geo()`:
      ! `axis_geo()` is not available for unrooted plots.
      i Try `ape::add.scale.bar()` instead.

---

    Code
      axis_geo(phylo = TRUE)
    Condition
      Error in `axis_geo()`:
      ! `axis_geo()` is not meaningful for radial or fan plots.

---

    Code
      axis_geo(phylo = TRUE)
    Condition
      Error in `axis_geo()`:
      ! `axis_geo()` is not meaningful for radial or fan plots.

