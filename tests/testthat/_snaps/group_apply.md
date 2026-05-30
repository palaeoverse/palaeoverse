# group_apply works

    Code
      group_apply(occdf = 1, group = c("cc"), fun = tax_range_time)
    Condition
      Error in `group_apply()`:
      ! `occdf` should be a dataframe

---

    Code
      group_apply(occdf = occdf, group = c("cc"), fun = "tax_range_time")
    Condition
      Error in `group_apply()`:
      ! Supplied `fun` is not a function

---

    Code
      group_apply(occdf = occdf, group = c("test"), fun = tax_range_time)
    Condition
      Error in `group_apply()`:
      ! Supplied `group` is not a named column in `occdf`

---

    Code
      group_apply(occdf = occdf, fun = tax_range_time)
    Condition
      Error in `group_apply()`:
      ! argument "group" is missing, with no default

---

    Code
      group_apply(occdf = occdf, group = c("cc"), fun = tax_range)
    Condition
      Error:
      ! object 'tax_range' not found

---

    Code
      group_apply(occdf = occdf, group = c("cc"), fun = tax_range_time,
      not_an_argument = "test")
    Condition
      Error in `group_apply()`:
      ! `not_an_argument` is not a valid argument for the specified function

---

    Code
      group_apply(occdf = occdf, group = c("cc"), fun = tax_range_time,
      not_an_argument1 = "test", not_an_argument2 = "test")
    Condition
      Error in `group_apply()`:
      ! `not_an_argument1`/`not_an_argument2` are not valid arguments for the specified function

