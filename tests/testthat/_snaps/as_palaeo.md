# as_palaeo() errors if provided values don't exist in the data

    Code
      as_palaeo(dat, lat = "foo")
    Condition
      Error in `as_palaeo()`:
      ! Column "foo" not found in `data`.

---

    Code
      as_palaeo(dat, lon = "foo")
    Condition
      Error in `as_palaeo()`:
      ! Column "foo" not found in `data`.

# as_palaeo() custom print method works

    Code
      dat2
    Message
      A dataframe with 1 row and 2 columns.
      i Attributes:
        * Latitude: "lat"
    Output
      
        lat long
      1   1    2

---

    Code
      dat2
    Message
      A dataframe with 1 row and 2 columns.
      i Attributes:
        * Longitude: "lon"
        * Latitude: "lat"
    Output
      
        lat lon
      1   1   2

# args must be named

    Code
      as_palaeo(dat, "foo")
    Condition
      Error in `as_palaeo()`:
      ! All arguments must be named (except for `data`).
      i Currently, there is 1 argument that should be named.

---

    Code
      as_palaeo(dat, "foo", lat = "bar")
    Condition
      Error in `as_palaeo()`:
      ! All arguments must be named (except for `data`).
      i Currently, there is 1 argument that should be named.

---

    Code
      as_palaeo(data = dat, "foo", lat = "bar")
    Condition
      Error in `as_palaeo()`:
      ! All arguments must be named (except for `data`).
      i Currently, there is 1 argument that should be named.

