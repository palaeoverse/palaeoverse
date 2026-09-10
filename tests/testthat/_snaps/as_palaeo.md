# as_palaeo() errors if provided values don't exist in the data

    Code
      as_palaeo(dat, lat = "foo")
    Condition
      Error in `as_palaeo()`:
      ! Column "foo" doesn't exist in `dat`.

---

    Code
      as_palaeo(dat, lon = "foo")
    Condition
      Error in `as_palaeo()`:
      ! Column "foo" doesn't exist in `dat`.

# as_palaeo() custom print method works

    Code
      dat2
    Output
      A dataframe with 1 rows and 2 columns
      
      Attributes:               
      - Latitude: lat

