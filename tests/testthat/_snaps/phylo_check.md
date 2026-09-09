# basic behavior works

    Code
      phylo_check(tree = data.frame())
    Condition
      Error in `phylo_check()`:
      ! `tree` must be of class <phylo>, not a <data.frame> object.

---

    Code
      phylo_check(tree = 1)
    Condition
      Error in `phylo_check()`:
      ! `tree` must be of class <phylo>, not the number 1.

---

    Code
      phylo_check(tree = NA)
    Condition
      Error in `phylo_check()`:
      ! `tree` must be of class <phylo>, not `NA`.

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
      ! Taxon names in `list` must not contain punctuation other than spaces or underscores.
      i Invalid name(s): "foo.bar".

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
      ! `out` must be one of "full_table", "diff_table", "counts", or "tree", not "foo".

---

    Code
      phylo_check(tree = tree, list = list, out = 1)
    Condition
      Error in `phylo_check()`:
      ! `out` must be a single string, not the number 1.

---

    Code
      phylo_check(tree = tree, list = list, out = NA)
    Condition
      Error in `phylo_check()`:
      ! `out` must be a single string, not `NA`.

---

    Code
      phylo_check(tree = tree, list = list, out = NULL)
    Condition
      Error in `phylo_check()`:
      ! `out` must be a single string, not `NULL`.

---

    Code
      phylo_check(tree, list, out = c("counts", "tree"))
    Condition
      Error in `phylo_check()`:
      ! `out` must be a single string, not a character vector.

# arg 'sort' works

    Code
      phylo_check(tree = tree, list = list, sort = "foo")
    Condition
      Error in `phylo_check()`:
      ! `sort` must be one of "presence" or "az", not "foo".

---

    Code
      phylo_check(tree = tree, list = list, sort = 1)
    Condition
      Error in `phylo_check()`:
      ! `sort` must be a single string, not the number 1.

---

    Code
      phylo_check(tree = tree, list = list, sort = NA)
    Condition
      Error in `phylo_check()`:
      ! `sort` must be a single string, not `NA`.

---

    Code
      phylo_check(tree = tree, list = list, sort = NULL)
    Condition
      Error in `phylo_check()`:
      ! `sort` must be a single string, not `NULL`.

---

    Code
      phylo_check(tree, list, sort = c("presence", "az"))
    Condition
      Error in `phylo_check()`:
      ! `sort` must be a single string, not a character vector.

