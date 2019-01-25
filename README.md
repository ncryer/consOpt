Priority Threat Management by Integer Linear Programming
==========================================================

This package implements a method to optimally allocate resources to conservation efforts, as described in Chadés et. al. (2015), _Benefits of integrating complementarity into priority threat management_. *Conservation Biology*, 29: 525-536. [doi:10.1111/cobi.12413](https://doi.org/10.1111/cobi.12413) 
========================================================================================================================================================

## Development roadmap 

Currently, this codebase implements the Integer Linear Programming algorithm described in (publication). The code recovers correct results on a number of test datasets, however the API is not yet stable and documentation is lacking. Additionally, little to no effort has been spent on unit testing or code quality control, as the focus until now has been on functionality first and foremost. 
 
As such, the development goals for version 1.0 (CRAN release) is proposed as follows: 

- [ ] Establishment of an "official" test dataset with known results for automated testing
- [ ] Extensive refactoring and quality control of current code
- [ ] Establishment of a suite of unit tests and Continuous Integration via CircleCI or similar
- [ ] Extensive documentation of the API (for the end user) and the internals of the codebase, as well as a "how to contribute" guide 

Additionally, to facilitate later development of tools that enable non-technical users to use the algorithm (>1.0)
- [ ] Implementing a parallel version of Optimize() which takes advantage of multiple CPU cores, drastically speeding up computation
- [ ] Implementing a set of Command Line Interface utilities allowing end-users to pass spreadsheet files (properly formatted) directly  
 
## Developer log: 
 
Forthcoming... 
