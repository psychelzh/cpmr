
## Test Environments

* local Windows 11 install, R 4.4.0
* via GitHub Actions:
  * macos-latest (release)
  * windows-latest (release)
  * ubuntu-latest (release)
  * ubuntu-latest (devel)
  * ubuntu-latest (oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

The NOTE is as follows:

```
❯ checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''
```

As noted in [this issue](https://github.com/r-hub/rhub/issues/560), this is a known issue with the `rhub` package and can be safely ignored.
