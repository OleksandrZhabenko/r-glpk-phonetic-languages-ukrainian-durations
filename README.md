The library and the executable is intended to use the functionality of the :

1) R programming language https://www.r-project.org/

2) Rglpk library https://cran.r-project.org/web/packages/Rglpk/index.html

3) GNU GLPK library https://www.gnu.org/software/glpk/glpk.html

For more information, please, see the documentation for them.

* Some examples.

Let in the file words.txt you have the Ukrainian words and their durations in seconds as pairs separated with whitespace 
at the lines. 

Then the following commands can be variants of the usage of the pldUkr executable. 

1) pldUkr 0.01 0.01 0.1 0.2 words.txt 1.8 -1 0.01 0.01 1.5 1.5 1.5 1.5 1.5 1.5 | R --quiet --no-save 

THis variant tries to minimize the duration of the soft sign (-1 as the second parameter after the file name), tries 
to reduce the influence of the volatile 'X' and 'Y' (the third and the fourth parameters as 0.01) and tries to somewhat 
make longer the 6 Ukrainian vowels (all the consonants have by that the default coefficients equal to 1.0).

2) pldUkr 0.01 0.01 0.1 0.2 words.txt 1.8 -1 0.01 0.01 2.5 2.5 2.5 2.5 2.5 2.5 | R --quiet --no-save 

The same as the 1), but with more prolongation of the vowels.

3) pldUkr 0.01 0.01 0.1 0.2 words.txt 1.4 -1 0.01 0.01 2.5 2.5 2.5 2.5 2.5 2.5 | R --quiet --no-save 

If the 2) gives you the not NULL result, then it is recommended to minimize the first parameter after the file name. 

4) pldUkr 0.01 0.01 0.1 0.2 words.txt 1.6 -1 0.01 0.01 2.5 2.5 2.5 2.5 2.5 2.5 | R --quiet --no-save 

If the previous gives NULL result, then try to somewhat increase the first parameter after the file name.

It is needed to be said that after the vowel sounds there are the coefficients for the voiced and, afterwards, for the 
voiceless sounds. If omitted, they are equal to the default 1.0.
