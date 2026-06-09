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
      look_up(occdf = occdf, early_interval = "early", late_interval = NA)
    Condition
      Error in `look_up()`:
      ! `late_interval` needs to be of type `character`

---

    Code
      look_up(occdf = occdf, late_interval = c("a", "b"))
    Condition
      Error in `look_up()`:
      ! `early_interval` needs to match a column name of `occdf`

# warn if 'late_interval' values have been filled with 'early_interval' ones

    Code
      look_up(occdf)
    Condition
      Warning in `look_up()`:
      `NA`, `""` or `" "` entries from `late_interval` have been
                  filled in with the corresponding `early_interval` entries
    Output
        early_interval late_interval early_stage late_stage interval_max_ma
      2     Capitanian          <NA>  Capitanian Capitanian         265.100
      3         Induan     Olenekian      Induan  Olenekian         251.902
        interval_mid_ma interval_min_ma
      2         262.100           259.1
      3         249.551           247.2

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
      look_up(occdf, int_key = data.frame(interval_name = c("Induan", "Asselian"),
      early_stage = 1:2, late_stage = c("foo1", "foo2")))
    Condition
      Error in `look_up()`:
      ! `int_key$interval_name`, `int_key$early_stage`, and
                 `int_key$late_stage` needs to be of type `character`

---

    Code
      look_up(occdf, int_key = data.frame(interval_name = c("Induan", "Asselian"),
      early_stage = c("foo1", "foo2"), late_stage = c("foo1", "foo2"), max_ma = c("a",
        "b")))
    Condition
      Error in `look_up()`:
      ! `int_key$max_ma` needs to be of type `numeric`

---

    Code
      look_up(occdf, int_key = data.frame(interval_name = c("Induan", "Asselian"),
      early_stage = c("foo1", "foo2"), late_stage = c("foo1", "foo2"), min_ma = c("a",
        "b")))
    Condition
      Error in `look_up()`:
      ! `int_key$min_ma` needs to be of type `numeric`

# int_key works with columns 'min_ma' and 'max_ma'

    Code
      out[, c("early_interval", "late_interval", "early_stage", "late_stage",
        "max_ma", "interval_mid_ma", "min_ma", "interval_max_ma", "interval_min_ma")]
    Output
           early_interval late_interval early_stage late_stage max_ma interval_mid_ma
      1        Missourian      Gzhelian  Kasimovian   Gzhelian  305.9             375
      59       Capitanian Wuchiapingian  Capitanian       <NA>  265.1              NA
      554      Capitanian Wuchiapingian  Capitanian       <NA>  265.1              NA
      1957       Gzhelian      Asselian    Gzhelian       <NA>  303.7              NA
      2031     Missourian     Virgilian  Kasimovian       <NA>  305.9              NA
           min_ma interval_max_ma interval_min_ma
      1    298.90             400             350
      59   254.17             300              NA
      554  254.17             300              NA
      1957 295.50             360              NA
      2031 298.90             400              NA

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

