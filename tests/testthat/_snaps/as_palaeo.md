# as_palaeo() errors if provided values don't exist in the data

    Code
      as_palaeo(dat, lat = "foo")
    Condition
      Error in `as_palaeo()`:
      ! Column "foo" not found in `x`.

---

    Code
      as_palaeo(dat, lon = "foo")
    Condition
      Error in `as_palaeo()`:
      ! Column "foo" not found in `x`.

# as_palaeo() custom print method works

    Code
      dat2
    Output
      A dataframe with 1 rows and 2 columns
      
      Attributes:               
      - Latitude: lat

# args must be named

    Code
      as_palaeo(dat, "foo")
    Condition
      Error in `as_palaeo()`:
      ! All arguments must be named (except for "x").
      i Currently, there is 1 argument that should be named.

---

    Code
      as_palaeo(dat, "foo", lat = "bar")
    Condition
      Error in `as_palaeo()`:
      ! All arguments must be named (except for "x").
      i Currently, there is 1 argument that should be named.

---

    Code
      as_palaeo(x = dat, "foo", lat = "bar")
    Condition
      Error in `as_palaeo()`:
      ! All arguments must be named (except for "x").
      i Currently, there is 1 argument that should be named.

