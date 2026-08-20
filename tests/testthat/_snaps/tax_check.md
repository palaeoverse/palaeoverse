# basic behavior works

    Code
      tax_check(data.frame())
    Condition
      Error in `tax_check()`:
      ! Please supply `taxdf` as a data.frame with named columns, containing
               taxon names, and optionally their higher classification

---

    Code
      tax_check(1)
    Condition
      Error in `tax_check()`:
      ! Please supply `taxdf` as a data.frame with named columns, containing
               taxon names, and optionally their higher classification

# arg 'name' works

    Code
      tax_check(dat)
    Condition
      Error in `tax_check()`:
      ! Please specify `name` as a single column name in `taxdf`

---

    Code
      tax_check(dat, name = "nonexistent")
    Condition
      Error in `tax_check()`:
      ! Please specify `name` as a single column name in `taxdf`

---

    Code
      tax_check(dat, name = 1)
    Condition
      Error in `tax_check()`:
      ! Please specify `name` as a single column name in `taxdf`

---

    Code
      tax_check(dat, name = NULL)
    Condition
      Error in `tax_check()`:
      ! Please specify `name` as a single column name in `taxdf`

---

    Code
      tax_check(dat, name = character(0))
    Condition
      Error in `tax_check()`:
      ! Please specify `name` as a single column name in `taxdf`

---

    Code
      tax_check(dat, name = "")
    Condition
      Error in `tax_check()`:
      ! Please specify `name` as a single column name in `taxdf`

# arg 'group' works

    Code
      tax_check(dat, group = "nonexistent")
    Condition
      Error in `tax_check()`:
      ! Please specify `group` as a single column name in `taxdf`

---

    Code
      tax_check(dat, group = 1)
    Condition
      Error in `tax_check()`:
      ! Please specify `group` as a single column name in `taxdf`

---

    Code
      tax_check(dat, group = character(0))
    Condition
      Error in `tax_check()`:
      ! Please specify `group` as a single column name in `taxdf`

---

    Code
      tax_check(dat, group = "")
    Condition
      Error in `tax_check()`:
      ! Please specify `group` as a single column name in `taxdf`

# arg 'dis' works

    Code
      tax_check(dat, dis = 1)
    Condition
      Error in `tax_check()`:
      ! `dis` must be a single numeric, greater than 0 and less than 1

---

    Code
      tax_check(dat, dis = 0)
    Condition
      Error in `tax_check()`:
      ! `dis` must be a single numeric, greater than 0 and less than 1

---

    Code
      tax_check(dat, dis = "a")
    Condition
      Error in `tax_check()`:
      ! `dis` must be a single numeric, greater than 0 and less than 1

---

    Code
      tax_check(dat, dis = numeric(0))
    Condition
      Error in `tax_check()`:
      ! `dis` must be a single numeric, greater than 0 and less than 1

---

    Code
      tax_check(dat, dis = NULL)
    Condition
      Error in `tax_check()`:
      ! `dis` must be a single numeric, greater than 0 and less than 1

# arg 'start' works

    Code
      tax_check(dat, start = -1)
    Condition
      Error in `tax_check()`:
      ! `start` must be a single positive integer, or zero

---

    Code
      tax_check(dat, start = numeric(0))
    Condition
      Error in `tax_check()`:
      ! `start` must be a single positive integer, or zero

---

    Code
      tax_check(dat, start = "a")
    Condition
      Error in `tax_check()`:
      ! `start` must be a single positive integer, or zero

# arg 'verbose' works

    Code
      tax_check(dat, verbose = 1)
    Condition
      Error in `tax_check()`:
      ! `verbose` must be a single logical value

---

    Code
      tax_check(dat, verbose = numeric(0))
    Condition
      Error in `tax_check()`:
      ! `verbose` must be a single logical value

---

    Code
      tax_check(dat, verbose = "a")
    Condition
      Error in `tax_check()`:
      ! `verbose` must be a single logical value

---

    Code
      tax_check(dat, verbose = NULL)
    Condition
      Error in `tax_check()`:
      ! `verbose` must be a single logical value

