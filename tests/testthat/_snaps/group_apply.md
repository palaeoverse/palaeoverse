# error handling for argument 'occdf'

    Code
      group_apply(group = "cc", fun = nrow)
    Condition
      Error in `group_apply()`:
      ! `occdf` must be a data frame, not absent.

---

    Code
      group_apply(occdf = 1, group = "cc", fun = nrow)
    Condition
      Error in `group_apply()`:
      ! `occdf` must be a data frame, not the number 1.

---

    Code
      group_apply(occdf = data.frame(), group = "cc", fun = nrow)
    Condition
      Error in `group_apply()`:
      ! Column "cc" not found in `occdf`.

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
      ! `group` must specify at least one column.

---

    Code
      group_apply(occdf = occdf, group = "foo", fun = nrow)
    Condition
      Error in `group_apply()`:
      ! Column "foo" not found in `occdf`.

---

    Code
      group_apply(occdf = occdf, group = 1, fun = nrow)
    Condition
      Error in `group_apply()`:
      ! `group` must be a character vector, not the number 1.

---

    Code
      group_apply(occdf = occdf, group = c("cc", "foobar"), fun = nrow)
    Condition
      Error in `group_apply()`:
      ! Column "foobar" not found in `occdf`.

---

    Code
      group_apply(occdf = occdf, group = c("cc", "foobar", "foobar2"), fun = nrow)
    Condition
      Error in `group_apply()`:
      ! Columns "foobar" and "foobar2" not found in `occdf`.

---

    Code
      group_apply(occdf = occdf, group = c("cc", "foo"), fun = nrow)
    Condition
      Error in `group_apply()`:
      ! Column "foo" not found in `occdf`.

# error handling for argument 'fun'

    Code
      group_apply(occdf = occdf, group = "cc", fun = "tax_range_time")
    Condition
      Error in `group_apply()`:
      ! `fun` must be a function, not the string "tax_range_time".

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
      ! `not_an_argument` is not a valid argument for the specified function `tax_range_time`

---

    Code
      group_apply(occdf = occdf, group = "cc", fun = tax_range_time,
        not_an_argument1 = "test", not_an_argument2 = "test")
    Condition
      Error in `group_apply()`:
      ! `not_an_argument1` and `not_an_argument2` are not valid arguments for the specified function `tax_range_time`

