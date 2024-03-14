## Test Environments

* local Windows 11 install, R 4.3.3
* via GitHub Actions:
  * macos-latest (release)
  * windows-latest (release)
  * ubuntu-latest (release)
  * ubuntu-latest (devel)
  * ubuntu-latest (oldrel-1)

## Submission summary

### First submission

```
Flavor: r-devel-linux-x86_64-debian-gcc, r-devel-windows-x86_64
Check: CRAN incoming feasibility, Result: NOTE
  Maintainer: 'Liang Zhang <psychelzh@outlook.com>'

  New submission

  Possibly misspelled words in DESCRIPTION:
    Connectome (2:8)
    connectome (8:5)

  Found the following (possibly) invalid URLs:
    URL: https://cran.r-project.org/web/packages/Rfast/index.html
      From: README.md
      Status: 200
      Message: OK
      CRAN URL not in canonical form
    The canonical URL of the CRAN page for a package is
      https://CRAN.R-project.org/package=pkgname

  The Title field should be in title case. Current version is:
  'Connectome predictive modelling in R'
  In title case that is:
  'Connectome Predictive Modelling in R'

  The Description field should not start with the package name,
    'This package' or similar.
```

These are corrected and begin a new submission. And "connectome" is correct spelling so I keep it as is.

### Second submission

The main issue for this submission is to enhance the description of the package based on the feedback from the first submission. The following changes are made:

* Added a simple description of the method and doi link to the reference paper.
* Fixed issue of package name in LICENSE file.

### Third submission

This submission enhance more details in DESCRIPTION file:

* Reference now in correct format.
* Correct package name quotes.

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is the first submission of this package.
