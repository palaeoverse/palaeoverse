# tax_certainty() basic behavior

    Code
      tax_certainty(taxdf = data.frame(), name = "identified_name")
    Condition
      Error in `tax_certainty()`:
      ! `names` is not a named column in `taxdf`.

# arg 'name' works

    Code
      tax_certainty(taxdf = occdf, name = "foo")
    Condition
      Error in `tax_certainty()`:
      ! `names` is not a named column in `taxdf`.

---

    Code
      tax_certainty(taxdf = occdf, name = NULL)
    Condition
      Error in `.subset2()`:
      ! attempt to select less than one element in get1index

# arg 'terms' works

    Code
      tax_certainty(taxdf = occdf, name = "identified_name", terms = 1)
    Condition
      Error in `tax_certainty()`:
      ! `terms` must be of class list or NULL.

# arg 'append' works

    Code
      tax_certainty(taxdf = occdf, name = "identified_name", append = 1)
    Condition
      Error in `tax_certainty()`:
      ! `append` must be of class logical (TRUE/FALSE).

---

    Code
      tax_certainty(taxdf = occdf, name = "identified_name", append = NA)
    Condition
      Error in `tax_certainty()`:
      ! `append` must be of class logical (TRUE/FALSE).

