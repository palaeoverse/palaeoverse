# basic behavior works

    Code
      phylo_check(tree = data.frame())
    Condition
      Error in `phylo_check()`:
      ! Phylogeny must be a phylo object

---

    Code
      phylo_check(tree = 1)
    Condition
      Error in `phylo_check()`:
      ! Phylogeny must be a phylo object

---

    Code
      phylo_check(tree = NA)
    Condition
      Error in `phylo_check()`:
      ! Phylogeny must be a phylo object

---

    Code
      phylo_check()
    Condition
      Error in `phylo_check()`:
      ! argument "tree" is missing, with no default

# phylo_check errors with unnamed args

    Code
      phylo_check(1, "a")
    Condition
      Error in `phylo_check()`:
      ! All arguments must be named.
      i Currently, there are 2 arguments that should be named.

---

    Code
      phylo_check(tree = 1, "a")
    Condition
      Error in `phylo_check()`:
      ! All arguments must be named.
      i Currently, there is 1 argument that should be named.

---

    Code
      phylo_check(1, "a", "full_table")
    Condition
      Error in `phylo_check()`:
      ! All arguments must be named.
      i Currently, there are 3 arguments that should be named.

---

    Code
      phylo_check(1, "a", out = "full_table")
    Condition
      Error in `phylo_check()`:
      ! All arguments must be named.
      i Currently, there are 2 arguments that should be named.

# arg 'list' works

    Code
      phylo_check(tree = tree, list = c("foo.bar"))
    Condition
      Error in `phylo_check()`:
      ! Taxon names should not contain punctuation except spaces or
               underscores

---

    Code
      phylo_check(tree = tree)
    Condition
      Error in `phylo_check()`:
      ! argument "list" is missing, with no default

# arg 'out' works

    Code
      phylo_check(tree = tree, list = list, out = "foo")
    Condition
      Error in `phylo_check()`:
      ! out must either be 'full_table', 'diff_table', 'counts' or 'tree'

---

    Code
      phylo_check(tree = tree, list = list, out = 1)
    Condition
      Error in `phylo_check()`:
      ! out must either be 'full_table', 'diff_table', 'counts' or 'tree'

---

    Code
      phylo_check(tree = tree, list = list, out = NA)
    Condition
      Error in `phylo_check()`:
      ! `out` must be of length 1.

---

    Code
      phylo_check(tree = tree, list = list, out = NULL)
    Condition
      Error in `phylo_check()`:
      ! `out` must be of length 1.

# arg 'sort' works

    Code
      phylo_check(tree = tree, list = list, sort = "foo")
    Condition
      Error in `phylo_check()`:
      ! sort must either be 'az' or 'presence'

---

    Code
      phylo_check(tree = tree, list = list, sort = 1)
    Condition
      Error in `phylo_check()`:
      ! sort must either be 'az' or 'presence'

---

    Code
      phylo_check(tree = tree, list = list, sort = NA)
    Condition
      Error in `phylo_check()`:
      ! `sort` must be of length 1.

---

    Code
      phylo_check(tree = tree, list = list, sort = NULL)
    Condition
      Error in `phylo_check()`:
      ! `sort` must be of length 1.

