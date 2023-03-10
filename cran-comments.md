## Test environments

- Local MacOS install, R 4.2.0
- R hub
    - Fedora Linux, R-devel, clang, gfortran
    - Ubuntu Linux 20.04.1 LTS, R-release, GCC
    - Windows Server 2022, R-devel, 64 bit

## R CMD check results

- Local `R CMD check`: 0 errors | 0 warnings | 0 notes
- R hub: 
    - Fedora Linux, R-devel, clang, gfortran
        - 0 errors | 0 warnings | 2 notes
        - NOTE: New submission
        - NOTE: Possibly misspelled words in DESCRIPTION (Dockerfiles, pracpac, renv, usethis). These are correctly spelled.
    - Ubuntu Linux 20.04.1 LTS, R-release, GCC
        - 0 errors | 0 warnings | 2 notes
        - Same notes as above
    - Windows Server 2022, R-devel, 64 bit
        - 0 errors | 0 warnings | 2 notes
        - Same notes as above
