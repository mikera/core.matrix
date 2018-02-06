# Change Log
All notable changes to this project will be documented in this file, as of core.matrix 0.50.0
This project adheres to [Semantic Versioning](http://semver.org/).

## [0.62.0] - 2018-02-06
### Enhanced
- Update for Clojure 1.9.0 release
### Fixed
- General refactoring and performance oprimisations
- Fixed CLJS tests to run on Nashorn 
- Improvements to dataset behaviour

## [0.61.0] - 2017-09-02
### Added
- add-emap! and set-emap! API functions

## [0.60.3] - 2017-05-18
### Fixed
- New upload to Clojars attempting to resolve deployment issue (see: https://github.com/clojars/clojars-web/issues/640)
- Problem with pm for double arrays see: https://github.com/mikera/core.matrix/issues/317

## [0.60.2] - 2017-05-17
### Note
- Partial failure to deploy with Clojars - DO NOT USE

## [0.60.1] - 2017-05-16
### Note
- Partial failure to deploy with Clojars - DO NOT USE
### Added
- Implement rmse, r-squared and person correlation in c.c.m/stats (thanks Kiran!)
### Fixed
- Improve default handling of mmul to fall back to inner-product for ND arrays

## [0.60.0] - 2017-05-16 
### Note
- Partial failure to deploy with Clojars - DO NOT USE
### Added
- Implement rmse, r-squared and person correlation in c.c.m/stats (thanks Kiran!)
### Fixed
- Improve default handling of mmul to fall back to inner-product for ND arrays

## [0.59.0] - 2017-05-01
### Added
- Implemented validate-shape #306

### Fixed
- Fix for #311 unnecessary coercion of column types in join-rows
- Improved docs

## [0.58.0] - 2017-03-15
### Enhanced
- Better implementation for magnitude #309
- Throw error on failure to set implementation

### Fixed
- Clarified docstrings

## [0.57.0] - 2016-11-17
### Enhanced
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
- Improved implementations for 2D double arrays
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
