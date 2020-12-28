# Revision history for r-glpk-phonetic-languages-ukrainian-durations

## 0.1.0.0 -- 2020-11-07

* First version. Released on an unsuspecting world.

## 0.1.1.0 -- 2020-11-07

* First version revised A. Dependency boundaries changed.

## 0.1.2.0 -- 2020-11-07

* First version revised B. Fixed issue with being not compiled for the GHC-7.8* series of compiler.

## 0.1.3.0 -- 2020-11-09

* First version revised C. Some code improvements. Some documentation improvements.

## 0.1.4.0 -- 2020-11-10

* First version revised D. Added printing for the ready inner in the getBFst' function tuple. Fixed issue with inexact documentation. Added lists-flines as
a new leightweight dependency. Improved printing system for various OSes.

## 0.2.0.0 -- 2020-11-10

* Second version. Added a new function answer2 that allows to change the randomization boundaries for the model (this allows, e. g. to research accents). Added the possibility to
specify up to 4 randomization boundaries as the 4 first command line arguments (otherwise, the default values are used). Please, be aware of possible violating the dependencies
between durations of the sounds because of inappropriately chosen randomization parameters (sometimes the dependencies can violate also for the appropriately chosen durations
though it less common in such a case).

## 0.2.1.0 -- 2020-11-10

* Second version revised A. Made narrower the default possible randomization parameters (now, the default behaviour is like for 0.1.4.0 version).

## 0.2.2.0 -- 2020-12-03

* Second version revised B. Extended the README.md with some basic examples of the pldUkr usage.

