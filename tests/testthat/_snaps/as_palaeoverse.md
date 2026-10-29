# as_palaeoverse() errors if some arguments are not provided

    Code
      as_palaeoverse(dat)
    Condition
      Error in `as_palaeoverse()`:
      ! All arguments must be provided.
      x Missing arguments: `lat`, `lon`.
      i You can find pre-filled templates at `?as_palaeoverse()`.

---

    Code
      as_palaeoverse(dat, lat = "lat")
    Condition
      Error in `as_palaeoverse()`:
      ! All arguments must be provided.
      x Missing argument: `lon`.
      i You can find pre-filled templates at `?as_palaeoverse()`.

---

    Code
      as_palaeoverse(lat = "lat", lon = "long")
    Condition
      Error in `as_palaeoverse()`:
      ! All arguments must be provided.
      x Missing argument: `data`.
      i You can find pre-filled templates at `?as_palaeoverse()`.

# as_palaeoverse() errors if provided values don't exist in the data

    Code
      as_palaeoverse(dat, lat = "foo", lon = "long")
    Condition
      Error in `as_palaeoverse()`:
      ! Column "foo" not found in `data`.

---

    Code
      as_palaeoverse(dat, lat = "lat", lon = "foo")
    Condition
      Error in `as_palaeoverse()`:
      ! Column "foo" not found in `data`.

# as_palaeoverse() custom print method works

    Code
      dat2
    Output
        lat long
      1   1    2
      
    Message
      A dataframe with 1 row and 2 columns.
      i Attributes:
        * lon: "long"
        * lat: "lat"

---

    Code
      dat2
    Output
        lat lon
      1   1   2
      
    Message
      A dataframe with 1 row and 2 columns.
      i Attributes:
        * lon: "lon"
        * lat: "lat"

---

    Code
      dat2
    Output
      # A tibble: 1 x 2
          lat   lon
        <dbl> <dbl>
      1     1     2
      
    Message
      A dataframe with 1 row and 2 columns.
      i Attributes:
        * lon: "lon"
        * lat: "lat"

# args must be named

    Code
      as_palaeoverse(dat, "foo")
    Condition
      Error in `as_palaeoverse()`:
      ! All arguments must be named (except for `data`).
      i Currently, there is 1 argument that should be named.

---

    Code
      as_palaeoverse(dat, "foo", lat = "bar")
    Condition
      Error in `as_palaeoverse()`:
      ! All arguments must be named (except for `data`).
      i Currently, there is 1 argument that should be named.

---

    Code
      as_palaeoverse(data = dat, "foo", lat = "bar")
    Condition
      Error in `as_palaeoverse()`:
      ! All arguments must be named (except for `data`).
      i Currently, there is 1 argument that should be named.

