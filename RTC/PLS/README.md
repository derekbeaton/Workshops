Almost everything you need to know about Partial Least Squares
================

# Overview

This is a short workshop on Partial Least Squares (PLS) to provide about
90% of all things you could possibly need to know about PLS (I’ve made
up the 90%, but it feels about right). This workshop is **reboot** of a
previous one, which you can find \[here\]
(<https://github.com/derekbeaton/Workshops/tree/master/RTC/Oct2017>).
This version of the PLS workshop is:

-   Shorter than before, and a bit more focused

-   Has way less math (or the math is more implicit)

-   Spends some time on PLS correlation (as we typically see in
    Neuroimaging) and PLS regression (what most non-neuroimagers think
    of when they hear “PLS”)

All materials for Part 1 are contained in this repository, including a
copy of a toy data set (synthesized data from the ONDRI project:
<https://github.com/ondri-nibs/toy_data>). The slides contain high level
overviews of PLSC and PLSR, but really do benefit from the video and
animations. For a dive into PLS code, see the /R/ directory which
contains:

-   baby\_plss.R: highly useful yet minimal implementations of PLSC and
    PLSR in base R

-   DATA\_PLSC\_make\_figures.R and PLSR\_make\_figures.R: Some code I
    used to make figures for the slides

-   EXAMPLE\_PLSC\_TExPosition.R: PLSC with permutation and bootstrap
    with the `TExPosition` and `TInPosition` packages

-   EXAMPLE\_PLSR\_pls.R: PLSR with (internal) cross-validation, plus
    comparisons with `lm()`, with the `pls` package.

-   EXAMPLE\_PLSR\_tidymodels.R: PLSR with repeated K-fold via
    `tidymodels`, which also makes use of `plsr()` from the `pls`
    package

-   EXAMPLE\_PLSC\_PLSR\_gpls.R: A few PLS examples to highlight just
    how similar (or in some cases *identical*) different PLS techniques
    are. This is from the `GPLS` package I wrote.
