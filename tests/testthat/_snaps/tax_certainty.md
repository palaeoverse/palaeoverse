# throws error for missing required arguments

    Code
      tax_certainty(data = 1, name = "foo")
    Condition
      Error in `tax_certainty()`:
      ! `data` must be of class <data.frame>, not the number 1.

---

    Code
      tax_certainty()
    Condition
      Error in `tax_certainty()`:
      ! `data` must be of class <data.frame>, not absent.

---

    Code
      tax_certainty(data = tetrapods)
    Condition
      Error in `tax_certainty()`:
      ! `name` must be a single string, not absent.

# tax_certainty errors with unnamed args

    Code
      tax_certainty(data, "identified_name")
    Condition
      Error in `tax_certainty()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_certainty(data = data, "identified_name")
    Condition
      Error in `tax_certainty()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_certainty(data, "identified_name", NULL)
    Condition
      Error in `tax_certainty()`:
      ! All arguments must be named (except for "data").
      i Currently, there are 2 arguments that should be named.

---

    Code
      tax_certainty(data, "identified_name", terms = NULL)
    Condition
      Error in `tax_certainty()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

# tax_certainty() basic behavior

    Code
      tax_certainty(data = data.frame(), name = "identified_name")
    Condition
      Error in `tax_certainty()`:
      ! Column "identified_name" not found in `data`.

# arg 'name' works

    Code
      tax_certainty(data = data, name = "foo")
    Condition
      Error in `tax_certainty()`:
      ! Column "foo" not found in `data`.

---

    Code
      tax_certainty(data = data, name = NULL)
    Condition
      Error in `tax_certainty()`:
      ! `name` must be a single string, not `NULL`.

# arg 'terms' works

    Code
      tax_certainty(data = data, name = "identified_name", terms = 1)
    Condition
      Error in `tax_certainty()`:
      ! `terms` must be of class <list> or `NULL`, not the number 1.

# arg 'append' works

    Code
      tax_certainty(data = data, name = "identified_name", append = 1)
    Condition
      Error in `tax_certainty()`:
      ! `append` must be `TRUE` or `FALSE`, not the number 1.

---

    Code
      tax_certainty(data = data, name = "identified_name", append = NA)
    Condition
      Error in `tax_certainty()`:
      ! `append` must be `TRUE` or `FALSE`, not `NA`.

