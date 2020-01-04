# Sophocles *Oedipus Tyrannos*

## Online Text

[Link to Sophocles’ *Oedipus Tyrannos* online.](http://folio2.furman.edu/ot/pages/)

## Version 

**v.1.0.0**. Status: active development; see `releases.md`.

## Overview

This is a presentation of the Greek text of Sophocles, <i>Oedipus Tyrannos</i>, by Christopher Blackwell of the Furman University Department of Classics. The source Edition is:  Francis Storr, ed. and trans., <i>Sophocles. Vol 1: Oedipus the King. Oedipus at Colonus. Antigone. With an English translation by F. Storr</i> The Loeb Classical Library, 20 (London; New York. William Heinemann Ltd.; The Macmillan Company) 1912. The machine-readable text by the <a href="http://www.perseus.tufts.edu/hopper/text?doc=Perseus:text:1999.01.0191">Perseus Digital Library</a>, Gregory Crane, Editor. These pages were generated using the <a href="http://cite-architecture.org">CITE Architecture</a>, by Christopher Blackwell and Neel Smith. The source-code is on <a href="https://github.com/Eumaeus/Oedipus_2019">GitHub</a>. The CITE Architecture, ©2001–2020, Christopher Blackwell &amp; Neel Smith. 

The comprehensive syntactic analysis of Sophocles’ play was done by <b>Francesco Mambrini</b>.

<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a>

## Details

I made this both to serve as a text for an upper-level Greek class at Furman, in Spring of 2020, and as a test-bed for developing some generic tools and frameworks for the [CITE Arcitecture](http://cite-architecture.org), and for solving some problems with canonically citable texts of drama. A blog post or article describing the details will be forthcoming. — CWB 1/3/2020.

## Contents and Building

- `cex/` contains a CEX file with an Edition and two Analytical Exemplars of the *OT*. It also conains a greatly abridged CEX file for testing.
-	`data/` contains an index to short-definitions in the *LSJ* lexicon. `ot_all.csv` is the dataset from which the CEX files (above) and the HTML pages are generated. There is an abdridged version for testing.
- `src/` contains Scala code that generates the HTML pages. It is really not well documenting, since this was a test-bed for more formal libraries to come.
- `project/` and `build.sbt` are specific to the SBT build system. Don't mess with them.
- `templates` contains template files for the HTML pages, one for the `index.html` page (containing the table of contents), and one for each of the pages for the play's text.
- `html/` contains all necessary CSS and JS files. When pages are generated, they go in `html/pages/`.

To build your own set of pages:

- Have [SBT](https://www.scala-sbt.org) installed (which requires a Java JDK).
- Clone this repo.
- At its top-level (the level where `build.sbt` is):
	- `sbt console` 
	- Wait for it to download dependencies, which may take a while.
	- `:load make_pages.sc`
	- The build process takes *many minutes*.

You can edit `make_pages.sc` to specify a target number of lines per HTML page. The default is 25.

