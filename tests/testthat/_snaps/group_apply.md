# error handling for argument 'occdf'

    Code
      group_apply(group = "cc", fun = nrow)
    Condition
      Error in `group_apply()`:
      ! argument "occdf" is missing, with no default

---

    Code
      group_apply(occdf = 1, group = "cc", fun = nrow)
    Condition
      Error in `group_apply()`:
      ! `occdf` should be a dataframe

---

    Code
      group_apply(occdf = data.frame(), group = "cc", fun = nrow)
    Condition
      Error in `group_apply()`:
      ! Supplied `group` is not a named column in `occdf`

# error handling for argument 'group'

    Code
      group_apply(occdf = occdf, fun = nrow)
    Condition
      Error in `group_apply()`:
      ! argument "group" is missing, with no default

---

    Code
      group_apply(occdf = occdf, group = NULL, fun = nrow)
    Condition
      Error in `group_apply()`:
      ! Supplied `group` is not a named column in `occdf`

---

    Code
      group_apply(occdf = occdf, group = "foo", fun = nrow)
    Condition
      Error in `group_apply()`:
      ! Supplied `group` is not a named column in `occdf`

---

    Code
      group_apply(occdf = occdf, group = 1, fun = nrow)
    Condition
      Error in `group_apply()`:
      ! Supplied `group` is not a named column in `occdf`

---

    Code
      group_apply(occdf = occdf, group = c("cc", "foobar"), fun = nrow)
    Condition
      Error:
      ! object 'foobar' not found

# error handling for argument 'fun'

    Code
      group_apply(occdf = occdf, group = "cc", fun = "tax_range_time")
    Condition
      Error in `group_apply()`:
      ! Supplied `fun` is not a function

---

    Code
      group_apply(occdf = occdf, group = "cc", fun = foobar)
    Condition
      Error:
      ! object 'foobar' not found

---

    Code
      group_apply(occdf = occdf, group = "cc", fun = tax_range_time, not_an_argument = "test")
    Condition
      Error in `group_apply()`:
      ! `not_an_argument` is not a valid argument for the specified function

---

    Code
      group_apply(occdf = occdf, group = "cc", fun = tax_range_time,
        not_an_argument1 = "test", not_an_argument2 = "test")
    Condition
      Error in `group_apply()`:
      ! `not_an_argument1`/`not_an_argument2` are not valid arguments for the specified function

