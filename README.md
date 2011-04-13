#**This is an active integration branch for version 0.6**

This means wild things may happen at any moment.  History may get re-written.
Upstream rebases may occur.  **Pull at your own peril.**


# tikzDevice

---

## Description

The tikzDevice package provides a graphics device for R that enables direct
output of graphics in a LaTeX-friendly way.  Plotting commands issued by R
functions are transformed into LaTeX code blocks.  These blocks are interpreted
with the help of TikZ-- a graphics library for TeX and friends written by Till
Tantau.

The tikzDevice supports three main modes of output:

  - Figure chunks: placed in .tex files and suitable for inclusion in
    LaTeX documents via the \input{} command.

  - Stand alone figures: Complete LaTeX documents containing figure
    code that can be compiled into stand-alone images.  Pages are
    cropped to the size of the figure using the LaTeX preview package.

  - Console output: TikZ code is returned directly to the R console
    as a character vector for further manipulation.


## Beta Notice

The tikzDevice is currently flagged as a beta work.  The package is
reasonably stable and has been used by the authors to produce graphics
for academic publications for over a year.  The reason for beta status
is that there are several open design issues- two of which are:

  - Providing support for UTF8 text.

  - Supporting TeX variants other than LaTeX.

Resolving these issues may require changes to the tikzDevice that break
backwards compatibility with previous versions.  The beta flag is a reminder
that such changes may occur- although we will strive to avoid them if possible.

The beta flag will be removed upon release of version 1.0. At this time the
tikzDevice will switch to [semantic versioning][1] and changes that
break backwards compatibility will happen rarely and will incur a major release.

  [1]: http://www.semver.org


## Obtaining the Package

Stable versions of the tikzDevice may be downloaded from CRAN:

    install.packages( 'tikzDevice' )

Development builds may be obtained from R-Forge:

    install.packages( 'tikzDevice',
      repos='http://r-forge.r-project.net' )

Bleeding-edge source code is available from GitHub:

    git clone git://github.com/Sharpie/RTikZDevice.git



## Reporting Bugs and Getting Help

The tikzDevice has a dedicated mailing list courtesy of R-Forge.  The
mailing list is the easiest way to get answers for questions related
to usage:

  tikzdevice-bugs @at@ lists.r-forge.r-project.org

The mailing list may also be accessed through Google Groups:

  https://groups.google.com/forum/#!forum/tikzdevice


Primary development takes place on GitHub.  Bugs and feature requests
may be made by opening issues at the primary repository:

  https://github.com/Sharpie/RTikZDevice/issues

Adventurous users are encouraged to fork the repository and contribute
to the development of the package!


## Latest Changes
*See the [CHANGELOG][2] for changes that occurred in previous releases*

  [2]:https://github.com/Sharpie/RTikZDevice/blob/master/CHANGELOG.md


---

### Version: 0.6.0

---

#### New Features

- Unicode Support!!!! XeLaTeX may now be used calculate metrics and widths for
  Unicode characters. PdfLaTeX remains the default LaTeX compiler, but this may
  be changed by setting the global option `tikzDefaultEngine` to `xetex`.

- New global option `tikzXelatexPackages` which contains packages necessary to
  use unicode characters with xelatex.  Specifically, the fontspec and the
  xunicode packages as well as the xetex option to the preview package.

- New global option `tikzUnicodeMetricPackages` which contains the packages
  necessary to calculate metrics for multibyte unicode characters with xelatex.

- New function anyMultibyteUTF8Characters() which will check if the given
  string contains any multibyte unicode characters.  Exposed in the package
  namespace since it is general and may be useful in other applications.

- The TikZ device now fully supports the `Raster` graphics primitive that was
  added in R 2.11.0 and no longer throws "not implemented" warnings when this
  functionality is used. This is accompilshed by writing raster images to PNG
  files, `Rplots_ras#.png`, which are then included in the main TeX file
  `Rplots.tex`.

- The TikZ device now fully supports the `polypath` graphics primitive that was
  added in R 2.12.0 and no longer throws "not implemented" warnings when this
  functionality is used.


#### Bug Fixes

- Fixed a bug where the `lwd` parameter used to control line widths was
  declared by tikzDevice to be of type `int` when it is actually a `double`.
  This was causing line widths to be ignored or miscalculated. Many thanks to
  Baptiste Auguie for reporting this issue.


#### Depreciation Notices

- Versions of R < 2.11.0 are no longer supported due to lack of required
  functions for handling Unicode strings.


#### Behind the Scenes

- New Makefile for executing common development tasks.

- Package documentation now handled by `roxygen`.  Many thanks to Hadley
  Wickham and Yihui Xie for the `Rd2roxygen` package which facilitated this
  switch.

- Package test suite completely overhauled and now based on Hadley Wickham's
  `test_that` unit testing framework.

