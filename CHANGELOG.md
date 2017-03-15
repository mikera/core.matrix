# Change Log
All notable changes to this project will be documented in this file, as of core.matrix 0.50.0
This project adheres to [Semantic Versioning](http://semver.org/).

## [0.58.0] - 
### Enhanced
- Better implementation for magnitude #309
- Throw error on failure to set implementation

### Fixed
- Clarified docstrings

## [0.57.0] - 2016-11-17
### Added
- Neanderthal implementation reference
- Weka implementation reference

### Fixed
- Fixed persistent vector coercion issue #302

## [0.56.0] - 2016-10-03
### Changed
- Improvements to dataset representations. Should be non-breaking if user did not rely on implementation details :-)

### Added
- Add dataset/emap-columns function
- Allow printing of column names in pprint functionality

### Fixed
- Improve docstrings
- Improved implementations for double[][] arrays
- Fixed Classcast exception on creating dataset #289
- Fixed issue with p-norm #292

## [0.55.0] - 2016-09-06
### Added
- Add filter-slices API function
- Implement generalised N-dimensional transpose functionality
- Improve broadcasting docs and default implementation

### Fixed
- Improve docstrings
- Fix for default mutable matrix construction

## [0.54.0] - 2016-08-16
### Fixed
- Improve error messages
- Change generation of test jar to output with "tests" classifier

## [0.53.1] - 2016-08-12
### Fixed
- Improve error messages

## [0.53.0] - 2016-08-12
### Added
- References to ND4J implementation

### Fixed
- Reinstate c.c.m.utils/error? for backwards compatibility

## [0.52.2] - 2016-06-16

## [0.52.1] - 2016-06-08
### Added
- core.matrix now has a CHANGELOG

### Fixed
- Update dependencies for Clojure 1.9.0 Alpha, latest Clojurescript version

## [0.52.0] - 2016-05-07
### Added
- minimum and maximum functions
- Indexed ranking functionality
- slice-map function

### Fixed
- Add back deprecated except-columns function (was breaking current Incanter release)

## [0.51.0] - 2016-04-01
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
