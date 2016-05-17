# Change Log
All notable changes to this project will be documented in this file, as of core.matrix 0.50.0
This project adheres to [Semantic Versioning](http://semver.org/).

## [0.52.0]
### Added
- minimum and maximum functions
- Indexed ranking functionality
- slice-map function

### Fixed
- Add back deprecated except-columns function (was breaking current Incanter release)

## [0.51.0]
### Added
- add-outer-product! API function

## [0.50.2] - 2016-03-31
### Fixed
- Fix issue with c.c.m.stats/sample-normal to create proper gaussians

## [0.50.1] - 2016-03-31
### Fixed
- Fix issue with c.c.m.stats/sd

## [0.50.0] - 2016-03-07
### Added
- Experimental ClojureScript support
- BLAS-like API for matrix computations (clojure.core.matrix.blas)

### Changed
- Add magnitude, magnitude-squared and deprecate length, length-squared
