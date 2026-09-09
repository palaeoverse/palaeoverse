# basic behavior works

    Code
      tax_check(data.frame(genus = c("Automaton", "Automaton2")))
    Condition
      Warning:
      Non-letter characters present in the taxon names.
    Output
      $synonyms
        group   greater     lesser count_greater count_lesser
      1     A Automaton Automaton2             1            1
      
      $non_letter_name
      [1] "Automaton2"
      
      $non_letter_group
      NULL
      

---

    Code
      tax_check(taxdf = data.frame())
    Condition
      Error in `tax_check()`:
      ! Column "genus" not found in `taxdf`.

---

    Code
      tax_check(taxdf = 1)
    Condition
      Error in `tax_check()`:
      ! `taxdf` must be of class <data.frame>, not the number 1.

---

    Code
      tax_check(taxdf = data.frame(genus = c(NA, "")))
    Condition
      Error in `tax_check()`:
      ! Column "genus" in `taxdf` must have at least one entry that is not NA or empty.

# tax_check errors with unnamed args

    Code
      tax_check(dat, "genus")
    Condition
      Error in `tax_check()`:
      ! All arguments must be named (except for "taxdf").
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_check(taxdf = dat, "genus")
    Condition
      Error in `tax_check()`:
      ! All arguments must be named (except for "taxdf").
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_check(dat, "genus", NULL)
    Condition
      Error in `tax_check()`:
      ! All arguments must be named (except for "taxdf").
      i Currently, there are 2 arguments that should be named.

---

    Code
      tax_check(dat, "genus", group = NULL)
    Condition
      Error in `tax_check()`:
      ! All arguments must be named (except for "taxdf").
      i Currently, there is 1 argument that should be named.

# arg 'name' works

    Code
      tax_check(taxdf = dat)
    Condition
      Error in `tax_check()`:
      ! Column "genus" not found in `taxdf`.

---

    Code
      tax_check(taxdf = dat, name = "nonexistent")
    Condition
      Error in `tax_check()`:
      ! Column "nonexistent" not found in `taxdf`.

---

    Code
      tax_check(taxdf = dat, name = 1)
    Condition
      Error in `tax_check()`:
      ! `name` must be a single string, not the number 1.

---

    Code
      tax_check(taxdf = dat, name = NULL)
    Condition
      Error in `tax_check()`:
      ! `name` must be a single string, not `NULL`.

---

    Code
      tax_check(taxdf = dat, name = character(0))
    Condition
      Error in `tax_check()`:
      ! `name` must be a single string, not an empty character vector.

---

    Code
      tax_check(taxdf = dat, name = "")
    Condition
      Error in `tax_check()`:
      ! Column "" not found in `taxdf`.

# arg 'group' works

    Code
      tax_check(data.frame(genus = c("Automaton", "Automaton"), family = c("Foo",
        "Examplidae2")), group = "family")
    Condition
      Warning:
      Non-letter characters present in the group names.
    Output
      $synonyms
      NULL
      
      $non_letter_name
      NULL
      
      $non_letter_group
      [1] "Examplidae2"
      

---

    Code
      tax_check(taxdf = dat, group = "nonexistent")
    Condition
      Error in `tax_check()`:
      ! Column "nonexistent" not found in `taxdf`.

---

    Code
      tax_check(taxdf = dat, group = 1)
    Condition
      Error in `tax_check()`:
      ! `group` must be a single string, not the number 1.

---

    Code
      tax_check(taxdf = dat, group = character(0))
    Condition
      Error in `tax_check()`:
      ! `group` must be a single string, not an empty character vector.

---

    Code
      tax_check(taxdf = dat, group = "")
    Condition
      Error in `tax_check()`:
      ! Column "" not found in `taxdf`.

# arg 'dis' works

    Code
      tax_check(taxdf = dat, dis = 1)
    Condition
      Error in `tax_check()`:
      ! `dis` must be greater than 0 and less than 1.

---

    Code
      tax_check(taxdf = dat, dis = 0)
    Condition
      Error in `tax_check()`:
      ! `dis` must be greater than 0 and less than 1.

---

    Code
      tax_check(taxdf = dat, dis = c(0.5, 0.6))
    Condition
      Error in `tax_check()`:
      ! `dis` must be a number, not a double vector.

---

    Code
      tax_check(taxdf = dat, dis = "a")
    Condition
      Error in `tax_check()`:
      ! `dis` must be a number, not the string "a".

---

    Code
      tax_check(taxdf = dat, dis = numeric(0))
    Condition
      Error in `tax_check()`:
      ! `dis` must be a number, not an empty numeric vector.

---

    Code
      tax_check(taxdf = dat, dis = NULL)
    Condition
      Error in `tax_check()`:
      ! `dis` must be a number, not `NULL`.

# arg 'start' works

    Code
      tax_check(taxdf = dat, start = -1)
    Condition
      Error in `tax_check()`:
      ! `start` must be a whole number larger than or equal to 0, not the number -1.

---

    Code
      tax_check(taxdf = dat, start = numeric(0))
    Condition
      Error in `tax_check()`:
      ! `start` must be a whole number, not an empty numeric vector.

---

    Code
      tax_check(taxdf = dat, start = "a")
    Condition
      Error in `tax_check()`:
      ! `start` must be a whole number, not the string "a".

---

    Code
      tax_check(taxdf = dat, start = NULL)
    Condition
      Error in `tax_check()`:
      ! `start` must be a whole number, not `NULL`.

# arg 'verbose' works

    Code
      tax_check(taxdf = dat, verbose = 1)
    Condition
      Error in `tax_check()`:
      ! `verbose` must be `TRUE` or `FALSE`, not the number 1.

---

    Code
      tax_check(taxdf = dat, verbose = numeric(0))
    Condition
      Error in `tax_check()`:
      ! `verbose` must be `TRUE` or `FALSE`, not an empty numeric vector.

---

    Code
      tax_check(taxdf = dat, verbose = "a")
    Condition
      Error in `tax_check()`:
      ! `verbose` must be `TRUE` or `FALSE`, not the string "a".

---

    Code
      tax_check(taxdf = dat, verbose = NULL)
    Condition
      Error in `tax_check()`:
      ! `verbose` must be `TRUE` or `FALSE`, not `NULL`.

