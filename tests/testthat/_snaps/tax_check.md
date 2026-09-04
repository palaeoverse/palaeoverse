# basic behavior works

    Code
      tax_check(taxdf = data.frame())
    Condition
      Error in `tax_check()`:
      ! Please supply `taxdf` as a data.frame with named columns, containing
               taxon names, and optionally their higher classification

---

    Code
      tax_check(taxdf = 1)
    Condition
      Error in `tax_check()`:
      ! Please supply `taxdf` as a data.frame with named columns, containing
               taxon names, and optionally their higher classification

# tax_check errors with unnamed args

    Code
      tax_check(dat, "genus")
    Condition
      Error in `tax_check()`:
      ! All arguments must be named.
      i Currently, there are 2 arguments that should be named.

---

    Code
      tax_check(taxdf = dat, "genus")
    Condition
      Error in `tax_check()`:
      ! All arguments must be named.
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_check(dat, "genus", NULL)
    Condition
      Error in `tax_check()`:
      ! All arguments must be named.
      i Currently, there are 3 arguments that should be named.

---

    Code
      tax_check(dat, "genus", group = NULL)
    Condition
      Error in `tax_check()`:
      ! All arguments must be named.
      i Currently, there are 2 arguments that should be named.

# arg 'name' works

    Code
      tax_check(taxdf = dat)
    Condition
      Error in `tax_check()`:
      ! Please specify `name` as a single column name in `taxdf`

---

    Code
      tax_check(taxdf = dat, name = "nonexistent")
    Condition
      Error in `tax_check()`:
      ! Please specify `name` as a single column name in `taxdf`

---

    Code
      tax_check(taxdf = dat, name = 1)
    Condition
      Error in `tax_check()`:
      ! Please specify `name` as a single column name in `taxdf`

---

    Code
      tax_check(taxdf = dat, name = NULL)
    Condition
      Error in `tax_check()`:
      ! Please specify `name` as a single column name in `taxdf`

---

    Code
      tax_check(taxdf = dat, name = character(0))
    Condition
      Error in `tax_check()`:
      ! Please specify `name` as a single column name in `taxdf`

---

    Code
      tax_check(taxdf = dat, name = "")
    Condition
      Error in `tax_check()`:
      ! Please specify `name` as a single column name in `taxdf`

# arg 'group' works

    Code
      tax_check(taxdf = dat, group = "nonexistent")
    Condition
      Error in `tax_check()`:
      ! Please specify `group` as a single column name in `taxdf`

---

    Code
      tax_check(taxdf = dat, group = 1)
    Condition
      Error in `tax_check()`:
      ! Please specify `group` as a single column name in `taxdf`

---

    Code
      tax_check(taxdf = dat, group = character(0))
    Condition
      Error in `tax_check()`:
      ! Please specify `group` as a single column name in `taxdf`

---

    Code
      tax_check(taxdf = dat, group = "")
    Condition
      Error in `tax_check()`:
      ! Please specify `group` as a single column name in `taxdf`

# arg 'dis' works

    Code
      tax_check(taxdf = dat, dis = 1)
    Condition
      Error in `tax_check()`:
      ! `dis` must be a single numeric, greater than 0 and less than 1

---

    Code
      tax_check(taxdf = dat, dis = 0)
    Condition
      Error in `tax_check()`:
      ! `dis` must be a single numeric, greater than 0 and less than 1

---

    Code
      tax_check(taxdf = dat, dis = "a")
    Condition
      Error in `tax_check()`:
      ! `dis` must be a single numeric, greater than 0 and less than 1

---

    Code
      tax_check(taxdf = dat, dis = numeric(0))
    Condition
      Error in `tax_check()`:
      ! `dis` must be a single numeric, greater than 0 and less than 1

---

    Code
      tax_check(taxdf = dat, dis = NULL)
    Condition
      Error in `tax_check()`:
      ! `dis` must be a single numeric, greater than 0 and less than 1

# arg 'start' works

    Code
      tax_check(taxdf = dat, start = -1)
    Condition
      Error in `tax_check()`:
      ! `start` must be a single positive integer, or zero

---

    Code
      tax_check(taxdf = dat, start = numeric(0))
    Condition
      Error in `tax_check()`:
      ! `start` must be a single positive integer, or zero

---

    Code
      tax_check(taxdf = dat, start = "a")
    Condition
      Error in `tax_check()`:
      ! `start` must be a single positive integer, or zero

# arg 'verbose' works

    Code
      tax_check(taxdf = dat, verbose = 1)
    Condition
      Error in `tax_check()`:
      ! `verbose` must be a single logical value

---

    Code
      tax_check(taxdf = dat, verbose = numeric(0))
    Condition
      Error in `tax_check()`:
      ! `verbose` must be a single logical value

---

    Code
      tax_check(taxdf = dat, verbose = "a")
    Condition
      Error in `tax_check()`:
      ! `verbose` must be a single logical value

---

    Code
      tax_check(taxdf = dat, verbose = NULL)
    Condition
      Error in `tax_check()`:
      ! `verbose` must be a single logical value

