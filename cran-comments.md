## General
* This release fixes a bug with the function sample in the out-of-bag predictions.

## Test environments
* Local, macOS Sierra 10.12.6, R-3.4.2
* Ubuntu 12.04 (on travis-ci), R-3.1, R-3.2, R-oldrel, R-release, R-devel
* Windows Server 2012 R2 (x64) (on AppVeyor-ci), R 3.4.2
* Rhub
  * Debian Linux, R-devel, GCC ASAN/UBSAN ok
  * Fedora Linux, R-devel, clang, gfortran ok
  * Ubuntu Linux 16.04 LTS, R-release, GCC ok

## R CMD check results

0 ERRORs | 0 WARNINGs | 0  NOTES.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of quantregForest, all packages passed the checks.
