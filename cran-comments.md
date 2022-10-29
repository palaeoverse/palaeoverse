# Resubmission

*Please do not start the description with "A package", package name, title or similar.*
**DONE.**

*Please omit the redundant "A Toolkit" at the start of your title.*
**DONE.**

*Please always write package names, software names and API (application programming interface) names in single quotes in title and description. e.g: --> 'R'.*
**DONE. This text has been removed.**

*If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form*
*authors (year) <doi:...>*
*authors (year) <arXiv:...>*
*authors (year, ISBN:...)*
*or if those are not available: <https:...>*
*with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking. (If you want to add a title as well please put it in quotes: "Title").*
**NOT IMPLEMENTED. The package does not provide implementation of any one specific published method. References are provided within function documentation where relevant for related work. This conforms with other packages such as 'chronosphere' or 'divDyn'. However, I have now pointed towards the associated preprint for users.**

*Please provide a link to the used datasets to the description field of your DESCRIPTION file in the form <http:...> or <https:...> with angle brackets for auto-linking and no space after 'http:' and 'https:'.*
**DONE.**

*Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)*
*Missing Rd-tags:*
     *axis_geo.Rd: \value*
**DONE.**

*\dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user. Does not seem necessary. Please replace \dontrun with \donttest. -> palaeorotate.Rd*
**DONE.**

*Please unwrap the examples if they are executable in < 5 sec, or replace dontrun{} with \donttest{}.*
**NOT IMPLEMENTED. Example has run time longer than 5 sec.**

*Please always make sure to reset to user's options(), working directory or par() after you changed it in examples and vignettes and demos. -> man/axis_geo.Rd; man/phylo_check.Rd*
*e.g.:*
*oldpar <- par(mfrow = c(1,2))*
*...*
*par(oldpar)*
**DONE.**

The outstanding note relates to first time submission.

# Test environments (with Github Actions)
Windows 10.0.20348 (x86_64-w64-mingw32): R 4.2.1

Mac OS X 12.6 (x86_64-apple-darwin17.0): R 4.2.1

Ubuntu-release 20.04.5 (x86_64-pc-linux-gnu): R 4.2.1 

Ubuntu-devel 20.04.5 (x86_64-pc-linux-gnu): r83164

Ubuntu-oldrel 20.04.5 (x86_64-pc-linux-gnu): R 4.1.3 

## R CMD check results
There were no ERRORs or WARNINGs.

There is one NOTE:
- First time submission.

## Downstream dependencies
There are currently no downstream dependencies for this package.
