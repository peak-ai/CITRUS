### Second submission
> Please add some more details about the package functionality and implemented methods in your Description text.

The Description text has been updated with more detail.

> If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form authors (year) <doi:...>...

There are no references to include here. The methods applied are all built in-house.

> Please write TRUE and FALSE instead of T and F. (Please don't use 'T' or 'F' as vector names.)

All uses of `T` and `F` have been changed to `TRUE` or `FALSE`.

> Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means.

This has now been added to all exported functions.

> You write information messages to the console that cannot be easily suppressed. It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object. Instead of print()/cat() rather use message()/warning() or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console. (except for print, summary, interactive functions)

`print()` statements have all been replaced by appropriate applications of `warning()` or `message()`. All messages are also put behind `if(verbose)` conditions.

> Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies. In your examples/vignettes/tests you can write to tempdir().

Oversight on our side. We believe it was caused by a `saveouput = TRUE` argument in one of our unit tests. This has been set to `FALSE` now.

## Test environments
* Mac OS X 10.15.7, R-devel
* Ubuntu 20.04.3 LTS, R-devel

## R CMD check results

0 errors | 0 warnings | 1 notes

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Dom Clarke <dom.clarke@peak.ai>'

New submission

## Reverse dependencies

None
