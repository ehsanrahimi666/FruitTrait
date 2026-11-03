## Test environments

* local: Ubuntu 24.04, R 4.4.3
* GitHub Actions: windows-latest, macos-latest and ubuntu-latest (R release)

## R CMD check results

0 errors | 0 warnings | 2 notes

* New submission. Installed size is about 6 MB, mostly the example and gallery
  photographs and the validation data used by the examples, tests and vignette.
  (Further notes in the local check concerned the build environment only: the
  current time could not be verified offline, and HTML Tidy was not installed.)

* This is a new release.
* Examples that need the optional Python back end or internet access are
  wrapped in `\dontrun{}` or `\donttest{}`; all other examples run in a few
  seconds.
* Tests of the Python back end run only when the environment variable
  `FRUITTRAIT_TEST_BACKEND` is `"true"`; tests that query GBIF or iNaturalist
  are skipped on CRAN, on continuous integration and when offline.
