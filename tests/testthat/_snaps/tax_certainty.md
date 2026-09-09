# throws error for missing required arguments

    Code
      tax_certainty(taxdf = 1, name = "foo")
    Condition
      Error in `tax_certainty()`:
      ! `taxdf` must be of class <data.frame>, not the number 1.

---

    Code
      tax_certainty()
    Condition
      Error in `tax_certainty()`:
      ! `taxdf` must be of class <data.frame>, not absent.

---

    Code
      tax_certainty(taxdf = tetrapods)
    Condition
      Error in `tax_certainty()`:
      ! `name` must be a single string, not absent.

# tax_certainty() basic behavior

    Code
      tax_certainty(taxdf = data.frame(), name = "identified_name")
    Condition
      Error in `tax_certainty()`:
      ! Column "identified_name" not found in `taxdf`.

# arg 'name' works

    Code
      tax_certainty(taxdf = occdf, name = "foo")
    Condition
      Error in `tax_certainty()`:
      ! Column "foo" not found in `taxdf`.

---

    Code
      tax_certainty(taxdf = occdf, name = NULL)
    Condition
      Error in `tax_certainty()`:
      ! `name` must be a single string, not `NULL`.

# arg 'terms' works

    Code
      tax_certainty(taxdf = occdf, name = "identified_name", terms = 1)
    Condition
      Error in `tax_certainty()`:
      ! `terms` must be of class <list> or `NULL`, not the number 1.

# arg 'append' works

    Code
      tax_certainty(taxdf = occdf, name = "identified_name", append = 1)
    Condition
      Error in `tax_certainty()`:
      ! `append` must be `TRUE` or `FALSE`, not the number 1.

---

    Code
      tax_certainty(taxdf = occdf, name = "identified_name", append = NA)
    Condition
      Error in `tax_certainty()`:
      ! `append` must be `TRUE` or `FALSE`, not `NA`.

