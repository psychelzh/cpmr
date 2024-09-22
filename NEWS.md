# cpmr (development version)

## New features

* Added `summary()` method to summarize the results of the CPM analysis.
* Added `tidy()` method to tidy the results of the CPM analysis.

## Enhancements

* Added `params` to `cpm()` output to store the input arguments (#14).
* Let `"sum"` be the default value for `return_edges` argument.
* Let the first two dimensions of `edges` in the output be edges and networks, respectively.
* Polish the print method of the `cpm` class.

# cpmr 0.0.9

## New features

* Added support for row/column matrix as input for behavior and confounds data.

## Maintenance

* Added more data checks to ensure the input data are in the correct format.

# cpmr 0.0.8

* Added `return_edges` argument to optionally set how to return edges in the output.

# cpmr 0.0.7

* Convert back to older version of confounds treating.

# cpmr 0.0.6

* Ensure confounds regression are now only used in feature selection.

# cpmr 0.0.5

* Fixed confounds treatment. Now confounds are used in feature selection but not in model fitting.

# cpmr 0.0.4

* Ensure sparsity threshold method work as expect.
* Some other improvements in code quality.

# cpmr 0.0.3

* Keep observation names in the output.
* Check if observation names match between neural data and behavioral data.

# cpmr 0.0.2

* Added support for confounding variables.

# cpmr 0.0.1

* Initial commit to r-universe.
