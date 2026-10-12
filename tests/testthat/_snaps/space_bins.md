# arg spacing works

    Code
      space_bins(spacing = 1000)
    Message
      i Average spacing between adjacent cells: 725.17 km.
      i Average cell area: 609,788.4 sq. km.
      i H3 resolution: 1
    Output
      Geometry set for 842 features 
      Geometry type: eoverse_space_bins
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
    Message
      i Average spacing between adjacent cells: 725.17 km.
      i Average cell area: 609,788.4 sq. km.
      i H3 resolution: 1
    Output
      Geometry set for 842 features 
      Geometry type: eoverse_space_bins
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
      space_bins(spacing = "10")
    Condition
      Error in `space_bins()`:
      ! `spacing` must be a number, not the string "10".

---

    Code
      space_bins(spacing = -1)
    Condition
      Error in `space_bins()`:
      ! `spacing` must be greater than 0.

---

    Code
      space_bins(spacing = 0)
    Condition
      Error in `space_bins()`:
      ! `spacing` must be greater than 0.

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

# arg resolution works

    Code
      space_bins(resolution = 1)
    Message
      i Average spacing between adjacent cells: 725.17 km.
      i Average cell area: 609,788.4 sq. km.
    Output
      Geometry set for 842 features 
      Geometry type: eoverse_space_bins
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
      space_bins(resolution = 16)
    Condition
      Error in `space_bins()`:
      ! `resolution` must be a whole number between 0 and 15.

---

    Code
      space_bins(resolution = 15.1)
    Condition
      Error in `space_bins()`:
      ! `resolution` must be a whole number, not the number 15.1.

---

    Code
      space_bins(resolution = "10")
    Condition
      Error in `space_bins()`:
      ! `resolution` must be a whole number, not the string "10".

---

    Code
      space_bins(resolution = -1)
    Condition
      Error in `space_bins()`:
      ! `resolution` must be a whole number between 0 and 15.

---

    Code
      space_bins(resolution = numeric(0))
    Condition
      Error in `space_bins()`:
      ! `resolution` must be a whole number, not an empty numeric vector.

---

    Code
      space_bins(resolution = NULL)
    Condition
      Error in `space_bins()`:
      ! `resolution` must be a whole number, not `NULL`.

---

    Code
      space_bins(resolution = NA)
    Condition
      Error in `space_bins()`:
      ! `resolution` must be a whole number, not `NA`.

# arg area works

    Code
      space_bins(area = 1e+05)
    Message
      i Average spacing between adjacent cells: 274.09 km.
      i Average cell area: 86,801.78 sq. km.
      i H3 resolution: 2
    Output
      Geometry set for 5882 features 
      Geometry type: eoverse_space_bins
      Dimension:     XY
      Bounding box:  xmin: -179.9789 ymin: -89.07415 xmax: 179.9829 ymax: 89.07415
      Geodetic CRS:  WGS 84
      First 5 geometries:
    Message
      POLYGON ((36.42872 77.68575, 43.86177 78.00915,...
      POLYGON ((30.78838 80.33975, 40.19373 80.81642,...
      POLYGON ((48.58085 76.68007, 55.65377 76.66744,...
      POLYGON ((46.68072 79.52276, 55.75077 79.55833,...
      POLYGON ((29.20747 75.65094, 35.20921 76.15221,...

---

    Code
      space_bins(area = "10")
    Condition
      Error in `space_bins()`:
      ! `area` must be a number, not the string "10".

---

    Code
      space_bins(area = -1)
    Condition
      Error in `space_bins()`:
      ! `area` must be greater than 0.

---

    Code
      space_bins(area = 0)
    Condition
      Error in `space_bins()`:
      ! `area` must be greater than 0.

---

    Code
      space_bins(area = NA)
    Condition
      Error in `space_bins()`:
      ! `area` must be a number, not `NA`.

# space_bins() must take one of spacing, resolution, or area

    Code
      space_bins()
    Condition
      Error in `space_bins()`:
      ! One of `spacing`, `resolution`, or `area` must be supplied.

---

    Code
      space_bins(spacing = 1000, resolution = 1)
    Condition
      Error in `space_bins()`:
      ! Exactly one of `spacing`, `resolution`, or `area` must be supplied.
      x `spacing` and `resolution` were supplied together.

---

    Code
      space_bins(spacing = 1000, area = 1e+05)
    Condition
      Error in `space_bins()`:
      ! Exactly one of `spacing`, `resolution`, or `area` must be supplied.
      x `spacing` and `area` were supplied together.

# space_bins() forbids unnamed args

    Code
      space_bins(1000)
    Condition
      Error in `space_bins()`:
      ! All arguments must be named.
      i Currently, there is 1 argument that should be named.

# partial matching of argument names is forbidden

    Code
      space_bins(sp = 1000)
    Condition
      Error in `space_bins()`:
      ! Argument names must be fully written.
      i Partially matched argument name: `sp`

