Input one or more files. The parser will guess the file format and file type of each uploaded file.

<br/>

**Meaures** file(s) can be in [cg12][], [gp1][], [yg][], or [bioscreen][]. 

<br/>

**Design** files can be in [plater][] or generic .csv format.

<br/>

**Matching of multiple files** is as follows:

<br/>

If 2 files are uploaded and one is a design file, the design will be joined onto the run measures file.

<br/>

if more than 2 files are uploaded and there is 1 design file, the design will be joined onto *each* of the run measures files.

<br/>

if more than 2 files are uploaded and there is more than 1 design file, meausures and designs are joined by common name: **run1**_design.csv would match to measures file **run1**.txt.

[cg12]: https://github.com/npjc/readcg12
[gp1]: https://github.com/npjc/readgp1
[yg]: https://github.com/npjc/readyg
[bioscreen]: https://github.com/npjc/readbioscreen
[plater]: https://cran.r-project.org/web/packages/plater/vignettes/plater-basics.html
