# space_bins works

    Code
      space_bins(spacing = 1000)
    Output
      Geometry set for 842 features 
      Geometry type: eo_space_bins
      Dimension:     XY
      Bounding box:  xmin: -179.7613 ymin: -87.80824 xmax: 179.9534 ymax: 87.82362
      Geodetic CRS:  WGS 84
      First 5 geometries:
    Message
      POLYGON ((40.0114 75.01232, 55.65377 76.66744, ...
      POLYGON ((13.88006 80.34595, 33.23853 83.59755,...
      POLYGON ((67.04817 73.26747, 82.21912 72.97478,...
      POLYGON ((61.13368 80.94452, 89.55466 80.74027,...
      POLYGON ((31.83128 68.92996, 41.0975 70.96472, ...

---

    Code
      space_bins(spacing = 1000.2)
    Output
      Geometry set for 842 features 
      Geometry type: eo_space_bins
      Dimension:     XY
      Bounding box:  xmin: -179.7613 ymin: -87.80824 xmax: 179.9534 ymax: 87.82362
      Geodetic CRS:  WGS 84
      First 5 geometries:
    Message
      POLYGON ((40.0114 75.01232, 55.65377 76.66744, ...
      POLYGON ((13.88006 80.34595, 33.23853 83.59755,...
      POLYGON ((67.04817 73.26747, 82.21912 72.97478,...
      POLYGON ((61.13368 80.94452, 89.55466 80.74027,...
      POLYGON ((31.83128 68.92996, 41.0975 70.96472, ...

# spacing has no default value

    Code
      space_bins()
    Condition
      Error in `space_bins()`:
      ! `spacing` must be a number, not absent.

# partial matching of argument names is forbidden

    Code
      space_bins(sp = 1000)
    Condition
      Error in `space_bins()`:
      ! Argument names must be fully written.
      i Partially matched argument name: "sp"

# space_bins errors with wrong inputs

    Code
      space_bins(spacing = "10")
    Condition
      Error in `space_bins()`:
      ! `spacing` must be a number, not the string "10".

---

    Code
      space_bins(spacing = -1)
    Condition
      Error in `space_bins()`:
      ! `spacing` must be a number larger than or equal to 0, not the number -1.

---

    Code
      space_bins(spacing = numeric(0))
    Condition
      Error in `space_bins()`:
      ! `spacing` must be a number, not an empty numeric vector.

---

    Code
      space_bins(spacing = NULL)
    Condition
      Error in `space_bins()`:
      ! `spacing` must be a number, not `NULL`.

---

    Code
      space_bins(spacing = NA)
    Condition
      Error in `space_bins()`:
      ! `spacing` must be a number, not `NA`.

