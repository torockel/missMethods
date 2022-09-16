## Test environments
- local windows 10, R 4.2.1
- macOS builder 
- win-builder (devel)
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)


## R CMD check results

0 errors | 0 warnings | 0 note

R-hub generated one note, which occurred on no other test environment:
  Found the following (possibly) invalid DOIs:
    DOI: 10.1093/bioinformatics/bth499
      From: DESCRIPTION
      Status: Forbidden
      Message: 403
    DOI: 10.1177/0013164418805532
      From: DESCRIPTION
      Status: Service Unavailable
   
I checked the DOIs (on https://www.doi.org/). They are valid: 
https://doi.org/10.1093/bioinformatics/bth499
https://doi.org/10.1177%2F0013164418805532


## revdepcheck results

I checked 3 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * I saw 0 new problems
 * I failed to check 0 packages
