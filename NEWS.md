# AnomalyDetection 2.0.1

* Swapped out `stringr` dependency for a `stringi` dependency since stringr also
  imports (and, hence, forces installation of) `glue` and `magrittr` which are not needed.
  `lubridate` still requires `stringr` but this is step one in the eventual removal
  of `lubridate`. The ultimate goal is to have this dependent only on `stringi`
  and `stats` and (possibly) even only just `stats`.
* Added package `prefix::` to all imported functions to improve package
  safety & readability.
* Added `stats` to the `Imports:` in `DESCRIPTION`.
* Changed tex formatting in comments to markdown and added `@md` roxgen tags
  where needed.
* Trimmed more long lines and reformatted a number of sections to improve
  readability.
* Added in URL references to the cited USENIX and Technometrics papers references.
* Cleaned up some status, warning and error messages
* Updated `Depends:` R version to require 3.0.0 given `lubridate`'s min R version.

# AnomalyDetection 2.0.0

* Added in PR <https://github.com/twitter/AnomalyDetection/pull/98/> (@gggodhwani)
* Added in PR <https://github.com/twitter/AnomalyDetection/pull/93> (@nujnimka)
* Added in PR <https://github.com/twitter/AnomalyDetection/pull/69> (@randakar)
* Added in PR <https://github.com/twitter/AnomalyDetection/pull/44 (@nicolasmiller)
* PR <https://github.com/twitter/AnomalyDetection/pull/92> (@caijun) inherently resolved
  since we return `POSIXct` objects in the data frames now
* Removed plotting code
* Removed tests that tested plotting code
* Updated tests since we only return data frames now
* Updated package to conform to modern CRAN standards
* Added a `NEWS.md` file to track changes to the package.
