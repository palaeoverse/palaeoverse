# error handling for argument 'data'

    Code
      group_apply(group = "cc", fun = nrow)
    Condition
      Error in `group_apply()`:
      ! `data` must be of class <data.frame>, not absent.

---

    Code
      group_apply(data = 1, group = "cc", fun = nrow)
    Condition
      Error in `group_apply()`:
      ! `data` must be of class <data.frame>, not the number 1.

---

    Code
      group_apply(data = data.frame(), group = "cc", fun = nrow)
    Condition
      Error in `group_apply()`:
      ! Column "cc" not found in `data`.

# group_apply errors with unnamed args

    Code
      group_apply(data, group = "cc", nrow)
    Condition
      Error in `group_apply()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

---

    Code
      group_apply(data, "cc", nrow)
    Condition
      Error in `group_apply()`:
      ! All arguments must be named (except for "data").
      i Currently, there are 2 arguments that should be named.

---

    Code
      group_apply(data, "cc", fun = tax_range_time, "family")
    Condition
      Error in `group_apply()`:
      ! All arguments must be named (except for "data").
      i Currently, there are 2 arguments that should be named.

---

    Code
      group_apply(data, "cc", fun = tax_range_time, name = "family")
    Condition
      Error in `group_apply()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

# error handling for argument 'group'

    Code
      group_apply(data = data, fun = nrow)
    Condition
      Error in `group_apply()`:
      ! `group` must be a character vector, not absent.

---

    Code
      group_apply(data = data, group = NULL, fun = nrow)
    Condition
      Error in `group_apply()`:
      ! `group` must be a character vector, not `NULL`.

---

    Code
      group_apply(data = data, group = "foo", fun = nrow)
    Condition
      Error in `group_apply()`:
      ! Column "foo" not found in `data`.

---

    Code
      group_apply(data = data, group = 1, fun = nrow)
    Condition
      Error in `group_apply()`:
      ! `group` must be a character vector, not the number 1.

---

    Code
      group_apply(data = data, group = c("cc", "foobar"), fun = nrow)
    Condition
      Error in `group_apply()`:
      ! Column "foobar" not found in `data`.

---

    Code
      group_apply(data = data, group = c("cc", "foobar", "foobar2"), fun = nrow)
    Condition
      Error in `group_apply()`:
      ! Columns "foobar" and "foobar2" not found in `data`.

---

    Code
      group_apply(data = data, group = c("cc", "foo"), fun = nrow)
    Condition
      Error in `group_apply()`:
      ! Column "foo" not found in `data`.

# error handling for argument 'fun'

    Code
      group_apply(data = data, group = "cc", fun = "tax_range_time")
    Condition
      Error in `group_apply()`:
      ! `fun` must be a function, not the string "tax_range_time".

---

    Code
      group_apply(data = data, group = "cc", fun = foobar)
    Condition
      Error:
      ! object 'foobar' not found

---

    Code
      group_apply(data = data, group = "cc", fun = tax_range_time, not_an_argument = "test")
    Condition
      Error in `group_apply()`:
      ! `not_an_argument` is not a valid argument for the specified function `tax_range_time()`.

---

    Code
      group_apply(data = data, group = "cc", fun = tax_range_time, not_an_argument1 = "test",
        not_an_argument2 = "test")
    Condition
      Error in `group_apply()`:
      ! `not_an_argument1` and `not_an_argument2` are not valid arguments for the specified function `tax_range_time()`.

