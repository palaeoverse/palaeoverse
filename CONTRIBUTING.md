# Contributing to palaeoverse

At `palaeoverse` we have adopted a set of [structures and standards](https://palaeoverse.org/articles/structure-and-standards.html) to follow for contributing to the development of `palaeoverse`. If you would like to contribute to the `palaeoverse` toolkit, we strongly advise reading this document first. If you plan to contribute a function to `palaeoverse`, you should first raise an [issue](https://github.com/palaeoverse-community/palaeoverse/issues) via the GitHub repository. This way the development team can assess whether the function is suitable or needed in the `palaeoverse` toolkit prior to submission.

## How to contribute

### Minor changes

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly on GitHub, provided it is done so in the source file. This means you’ll need to edit `roxygen2` comments in the `.R` file, not the `.Rd` file.

### Substantial changes

If you would like to make a substantial change, you should first file an issue and make sure someone from the development team agrees that it’s needed. If you’ve found a bug, please file an issue that illustrates the bug with a reproducible example.

### Pull request process

You (the contributor) should clone the desired repository (i.e. [the palaeoverse R package](https://github.com/palaeoverse-community/palaeoverse)) to your personal computer. Before changes are made, you should switch to a new git branch (i.e., not the main branch). When your changes are complete, you can submit your changes for merging via a [pull request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests) (“PR”) on GitHub. Note that a complete pull request should include a succinct description ([see function template](https://github.com/palaeoverse-community/palaeoverse/blob/main/pull_request_template.md)) of what the code changes do, proper documentation (via [roxygen2](https://roxygen2.r-lib.org)), and unit tests (via `testthat`). Only the description is required for the initial pull request and code review (see below), but pull requests will not be merged until they contain complete documentation and tests.

If you are not comfortable with git/GitHub, you can reach out to one of the core developers ([see collaborators](https://github.com/palaeoverse-community/palaeoverse)) via email and they can make a pull request on your behalf. However, you will be expected to respond to any reviewer comments on GitHub (see below).

If you don't feel comfortable implementing changes yourself, you can submit a bug report or feature request as a GitHub issue in the proper repository (e.g. [palaeoverse issues](https://github.com/palaeoverse-community/palaeoverse/issues)).

## Code review
All pull requests must be reviewed by two core developers of palaeoverse ([see collaborators](https://github.com/palaeoverse-community/palaeoverse)) before merging. The review process will ensure that contributions 1) meet the standards and expectations as described above, 2) successfully perform the functions that they claim to perform, and 3) don't break any other parts of the codebase.

Submitting a pull request for one of the palaeoverse R packages will automatically initiate an [R CMD check]( https://r-pkgs.org/check.html), [lintr check](https://lintr.r-lib.org/index.html), and [test coverage check](https://github.com/r-lib/covr) via GitHub Actions. While these checks will conduct some automatic review to ensure the package has not been broken by the new code and that the code matches the style guide (see above), a manual review is still required before the pull request can be merged.

Reviewers may have questions while reviewing your pull request. You are expected to respond to any of these questions via GitHub. If fixes and/or changes are required, you are expected to make these changes. If the required changes are minor enough, reviewers may make them for you, but this should not be expected. If you have any questions or lack the background to make the required changes, you should work with the reviewer to determine a plan of attack.

## Code of Conduct

Please note that by contributing to `palaeoverse` you agree to our [Code of Conduct](https://palaeoverse.org/CODE_OF_CONDUCT.html).
