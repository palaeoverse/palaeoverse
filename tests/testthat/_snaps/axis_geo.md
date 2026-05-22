# axis_geo() error handling

    Code
      axis_geo(height = c(0.5, 0.5))
    Condition
      Error:
      ! Invalid value supplied for height, must be a single numeric value
               per scale

---

    Code
      axis_geo(fill = 5)
    Condition
      Error:
      ! Invalid value supplied for fill, must be character (or NULL)

---

    Code
      axis_geo(lab = "true")
    Condition
      Error:
      ! Invalid value supplied for lab, must be a single logical value per
               scale

---

    Code
      axis_geo(lab_color = 42)
    Condition
      Error in `rect()`:
      ! plot.new has not been called yet

---

    Code
      axis_geo(lab_size = "big")
    Condition
      Error:
      ! Invalid value supplied for lab_size, must be numeric

---

    Code
      axis_geo(rot = NULL)
    Condition
      Error:
      ! Invalid value supplied for rot, must be a single numeric value per
               scale

---

    Code
      axis_geo(abbr = c("true", 1))
    Condition
      Error:
      ! Invalid value supplied for abbr, must be a single numeric value per
               scale

---

    Code
      axis_geo(skip = c(1, 2, 3))
    Condition
      Error:
      ! Invalid value supplied for skip, must be character (or NULL)

---

    Code
      axis_geo(center_end_labels = c(FALSE, TRUE))
    Condition
      Error:
      ! Invalid value supplied for center_end_labels, must be a single logical
               value per scale

---

    Code
      axis_geo(autofit = c(FALSE, TRUE))
    Condition
      Error:
      ! Invalid value supplied for autofit, must be a single logical value
               per scale

---

    Code
      axis_geo(bord_col = TRUE)
    Condition
      Error:
      ! Invalid value supplied for bord_col, must be character (or NULL)

---

    Code
      axis_geo(lty = 7)
    Condition
      Error:
      ! Invalid value supplied for lty, must be character (or NULL)

---

    Code
      axis_geo(lwd = "thin")
    Condition
      Error:
      ! Invalid value supplied for lwd, must be numeric (or NULL)

---

    Code
      axis_geo(side = 5)
    Condition
      Error:
      ! Invalid value supplied for side, must be 1, 2, 3, or 4

