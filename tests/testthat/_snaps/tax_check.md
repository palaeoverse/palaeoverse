# tax_check() accepts taxon names, no groups

    Code
      tax_check(name = "genus", group = TRUE)
    Condition
      Error in `tax_check()`:
      ! argument "taxdf" is missing, with no default

---

    Code
      tax_check(1)
    Condition
      Error in `tax_check()`:
      ! Please supply `taxdf` as a data.frame with named columns, containing
               taxon names, and optionally their higher classification

---

    Code
      tax_check(data.frame())
    Condition
      Error in `tax_check()`:
      ! Please supply `taxdf` as a data.frame with named columns, containing
               taxon names, and optionally their higher classification

---

    Code
      tax_check(testdf, name = 1)
    Condition
      Error in `tax_check()`:
      ! Please specify `name` as a single column name in `taxdf`

---

    Code
      tax_check(testdf, name = c("one", "name"))
    Condition
      Error in `tax_check()`:
      ! Please specify `name` as a single column name in `taxdf`

---

    Code
      tax_check(testdf, name = "age")
    Condition
      Error in `tax_check()`:
      ! The `name` column in `taxdf` must contain data of class character and
               at least one entry that is not NA or empty

---

    Code
      tax_check(testdf, "genus", group = TRUE)
    Condition
      Error in `tax_check()`:
      ! Please specify `group` as a single column name in `taxdf`

---

    Code
      tax_check(testdf, "genus", group = c("a", "group"))
    Condition
      Error in `tax_check()`:
      ! Please specify `group` as a single column name in `taxdf`

---

    Code
      tax_check(testdf, "genus", group = "agroup")
    Condition
      Error in `tax_check()`:
      ! Please specify `group` as a single column name in `taxdf`

---

    Code
      tax_check(testdf, "genus", group = "age")
    Condition
      Error in `tax_check()`:
      ! The `group` column in `taxdf` must contain data of class character

---

    Code
      tax_check(testdf, "genus", dis = "max")
    Condition
      Error in `tax_check()`:
      ! `dis` must be a single numeric, greater than 0 and less than 1

---

    Code
      tax_check(testdf, "genus", dis = 0)
    Condition
      Error in `tax_check()`:
      ! `dis` must be a single numeric, greater than 0 and less than 1

---

    Code
      tax_check(testdf, "genus", dis = FALSE)
    Condition
      Error in `tax_check()`:
      ! `dis` must be a single numeric, greater than 0 and less than 1

---

    Code
      tax_check(testdf, "genus", start = "1")
    Condition
      Error in `tax_check()`:
      ! `start` must be a single positive integer, or zero

---

    Code
      tax_check(testdf, "genus", start = c(1, 3))
    Condition
      Error in `tax_check()`:
      ! `start` must be a single positive integer, or zero

---

    Code
      tax_check(testdf, "genus", start = TRUE)
    Condition
      Error in `tax_check()`:
      ! `start` must be a single positive integer, or zero

---

    Code
      tax_check(testdf, "genus", verbose = "TRUE")
    Condition
      Error in `tax_check()`:
      ! `verbose` must be a single logical value

