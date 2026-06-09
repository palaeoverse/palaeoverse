# basic behavior works

    Code
      out[, c("early_interval", "late_interval", "early_stage", "late_stage",
        "interval_max_ma", "interval_mid_ma", "interval_min_ma")]
    Output
         early_interval late_interval early_stage late_stage interval_max_ma
      3          Induan     Olenekian      Induan  Olenekian         251.902
      19       Asselian     Sakmarian    Asselian  Sakmarian         298.900
      20       Asselian     Sakmarian    Asselian  Sakmarian         298.900
      21       Asselian     Sakmarian    Asselian  Sakmarian         298.900
      22       Asselian     Sakmarian    Asselian  Sakmarian         298.900
         interval_mid_ma interval_min_ma
      3          249.551           247.2
      19         294.500           290.1
      20         294.500           290.1
      21         294.500           290.1
      22         294.500           290.1

# wrong input for argument 'occdf'

    Code
      look_up(1)
    Condition
      Error in `look_up()`:
      ! `occdf` should be a dataframe.

---

    Code
      look_up(NA)
    Condition
      Error in `look_up()`:
      ! `occdf` should be a dataframe.

---

    Code
      look_up(NULL)
    Condition
      Error in `look_up()`:
      ! `occdf` should be a dataframe.

# look_up() warns if some intervals couldn't be matched

    Code
      out[, c("early_interval", "late_interval", "early_stage", "late_stage",
        "interval_max_ma", "interval_mid_ma", "interval_min_ma")]
    Output
        early_interval late_interval early_stage late_stage interval_max_ma
      1     Missourian      Gzhelian        <NA>   Gzhelian              NA
      3         Induan     Olenekian      Induan  Olenekian         251.902
        interval_mid_ma interval_min_ma
      1              NA           298.9
      3         249.551           247.2

# arguments 'early_interval' and 'late_interval' work

    Code
      look_up(occdf = occdf)
    Condition
      Error in `look_up()`:
      ! `early_interval` needs to match a column name of `occdf`

---

    Code
      look_up(occdf = occdf, early_interval = "early")
    Condition
      Error in `look_up()`:
      ! `late_interval` needs to match a column name of `occdf`

---

    Code
      look_up(occdf = occdf, late_interval = "late")
    Condition
      Error in `look_up()`:
      ! `early_interval` needs to match a column name of `occdf`

---

    Code
      look_up(occdf = occdf, early_interval = 1)
    Condition
      Error in `look_up()`:
      ! `early_interval` needs to be of type `character`

---

    Code
      look_up(occdf = occdf, early_interval = NA)
    Condition
      Error in `look_up()`:
      ! `early_interval` needs to be of type `character`

---

    Code
      look_up(occdf = occdf, early_interval = c("a", "b"))
    Condition
      Error in `if (!early_interval %in% colnames(occdf)) ...`:
      ! the condition has length > 1

---

    Code
      look_up(occdf = occdf, late_interval = 1)
    Condition
      Error in `look_up()`:
      ! `early_interval` needs to match a column name of `occdf`

---

    Code
      look_up(occdf = occdf, late_interval = NA)
    Condition
      Error in `look_up()`:
      ! `early_interval` needs to match a column name of `occdf`

---

    Code
      look_up(occdf = occdf, late_interval = c("a", "b"))
    Condition
      Error in `look_up()`:
      ! `early_interval` needs to match a column name of `occdf`

# argument 'int_key' works

    Code
      look_up(occdf, int_key = my_interval)
    Output
         early_interval late_interval early_stage late_stage interval_max_ma
      3          Induan     Olenekian        foo1  Olenekian              NA
      19       Asselian     Sakmarian        foo2  Sakmarian              NA
      20       Asselian     Sakmarian        foo2  Sakmarian              NA
      21       Asselian     Sakmarian        foo2  Sakmarian              NA
      22       Asselian     Sakmarian        foo2  Sakmarian              NA
         interval_mid_ma interval_min_ma
      3               NA           247.2
      19              NA           290.1
      20              NA           290.1
      21              NA           290.1
      22              NA           290.1

---

    Code
      look_up(occdf, int_key = data.frame(interval_name = c("Induan", "Asselian"),
      early_stage = c("foo1", "foo2")))
    Condition
      Error in `look_up()`:
      ! `int_key` needs to contain the columns "interval_name",
                 "early_stage" and "late_stage"

---

    Code
      look_up(occdf, int_key = data.frame(interval_name = c("Induan", "Asselian"),
      late_stage = c("foo1", "foo2")))
    Condition
      Error in `look_up()`:
      ! `int_key` needs to contain the columns "interval_name",
                 "early_stage" and "late_stage"

---

    Code
      look_up(occdf, int_key = 1)
    Condition
      Error in `look_up()`:
      ! `int_key` should be a dataframe.

---

    Code
      look_up(occdf, int_key = c("a", "b"))
    Condition
      Error in `look_up()`:
      ! `int_key` should be a dataframe.

# argument 'assign_with_GTS' works

    Code
      look_up(occdf, assign_with_GTS = "GTS2012")
    Output
         early_interval late_interval early_stage late_stage interval_max_ma
      3          Induan     Olenekian      Induan  Olenekian           252.2
      19       Asselian     Sakmarian    Asselian  Sakmarian           298.9
      20       Asselian     Sakmarian    Asselian  Sakmarian           298.9
      21       Asselian     Sakmarian    Asselian  Sakmarian           298.9
      22       Asselian     Sakmarian    Asselian  Sakmarian           298.9
         interval_mid_ma interval_min_ma
      3            249.7           247.2
      19           294.5           290.1
      20           294.5           290.1
      21           294.5           290.1
      22           294.5           290.1

---

    Code
      out[, c("early_interval", "late_interval", "early_stage", "late_stage")]
    Output
         early_interval late_interval early_stage late_stage
      30      Virgilian     Virgilian    Gzhelian   Asselian
      31       Wolfcamp      Wolfcamp   Sakmarian    Roadian
      32       Wolfcamp      Wolfcamp   Sakmarian    Roadian
      33       Wolfcamp      Wolfcamp   Sakmarian    Roadian
      34      Urzhumian     Urzhumian     Wordian    Wordian
      35      Urzhumian     Urzhumian     Wordian    Wordian
      36      Urzhumian     Urzhumian     Wordian    Wordian
      37      Urzhumian     Urzhumian     Wordian    Wordian
      38      Urzhumian     Urzhumian     Wordian    Wordian
      45     Brigantian    Brigantian      Visean     Visean
      46     Brigantian    Brigantian      Visean     Visean

---

    Code
      look_up(occdf, assign_with_GTS = FALSE)
    Condition
      Error in `look_up()`:
      ! assignment with GTS needs to be enabled if `int_key` is set to `FALSE`

---

    Code
      look_up(occdf, assign_with_GTS = 1)
    Condition
      Error in `look_up()`:
      ! assignment with GTS needs to be enabled if `int_key` is set to `FALSE`

---

    Code
      look_up(occdf, assign_with_GTS = "foo")
    Condition
      Error in `look_up()`:
      ! assignment with GTS needs to be enabled if `int_key` is set to `FALSE`

# no error is produced when pre-Phanerozoic intervals are included

    Code
      out[, c("interval", "early_stage", "late_stage", "interval_max_ma",
        "interval_mid_ma", "interval_min_ma")]
    Output
               interval early_stage late_stage interval_max_ma interval_mid_ma
      1    Meso-archean        <NA>       <NA>              NA              NA
      4111    Ediacaran        <NA>       <NA>              NA              NA
      4178    Ediacaran        <NA>       <NA>              NA              NA
           interval_min_ma
      1                 NA
      4111              NA
      4178              NA

