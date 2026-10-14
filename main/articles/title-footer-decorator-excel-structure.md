# Excel structure for \`title_footer_decorator\`

This vignette documents the exact structure expected by
[`title_footer_decorator()`](https://phuse-org.github.io/uteals/reference/title_footer_decorator.md)
for the Excel specification file. The decorator expects a sheet that
lists table identifiers and, for each table, a `TITLE` row and zero or
more `FOOTNOTE` rows.

The function expects the titles to be in the first sheet named `Sheet1`.
The column names must be the first row of the file (i.e. `TABLE ID`,
`IDENTIFIER`, `TEXT`).

Below is an example of a table that would work for the decorator. The
`DO_NOT_DELETE` values in the first row are intentional and have to be
part of the file.

\
`example_excel`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
``   `TABLE ID`  ```=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`    ``"DO_NOT_DELETE"``,`\
`    ``"TSFAE01A"``, ``"TSFAE01A"``, ``"TSFAE01A"``, ``"TSFAE01A"``, ``"TSFAE01A"``, ``"TSFAE01A"``, ``"TSFAE01A"``,`\
`    ``"TSFAE01B"``, ``"TSFAE01B"``, ``"TSFAE01B"``, ``"TSFAE01B"``, ``"TSFAE01B"``, ``"TSFAE01B"``, ``"TSFAE01B"`\
`  ``)``,`\
`  IDENTIFIER ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`    ``"DO_NOT_DELETE"``,`\
`    ``"TITLE"``, ``"FOOTNOTE1"``, ``"FOOTNOTE2"``, ``"FOOTNOTE3"``, ``"FOOTNOTE4"``, ``"FOOTNOTE5"``, ``"FOOTNOTE6"``,`\
`    ``"TITLE"``, ``"FOOTNOTE1"``, ``"FOOTNOTE2"``, ``"FOOTNOTE3"``, ``"FOOTNOTE4"``, ``"FOOTNOTE5"``, ``"FOOTNOTE6"`\
`  ``)``,`\
`  TEXT ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`    ``"DO_NOT_DELETE"``,`\
`    ``"Some title"``, ``"First part of footnote"``, ``"Another part of footnote"``, ``"Another part of footnote"``,`\
`    ``"Another part of footnote"``, ``"Another part of footnote"``, ``"Another part of footnote"``,`\
`    ``"Some title"``, ``"First part of footnote"``, ``"Another part of footnote"``, ``"Another part of footnote"``,`\
`    ``"Another part of footnote"``, ``"Another part of footnote"``, ``"Another part of footnote"`\
`  ``)``,`\
`  stringsAsFactors ``=`` ``FALSE``,`\
`  check.names ``=`` ``FALSE`\
`)`\
\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``example_excel``)`

| TABLE ID      | IDENTIFIER    | TEXT                     |
|:--------------|:--------------|:-------------------------|
| DO_NOT_DELETE | DO_NOT_DELETE | DO_NOT_DELETE            |
| TSFAE01A      | TITLE         | Some title               |
| TSFAE01A      | FOOTNOTE1     | First part of footnote   |
| TSFAE01A      | FOOTNOTE2     | Another part of footnote |
| TSFAE01A      | FOOTNOTE3     | Another part of footnote |
| TSFAE01A      | FOOTNOTE4     | Another part of footnote |
| TSFAE01A      | FOOTNOTE5     | Another part of footnote |
| TSFAE01A      | FOOTNOTE6     | Another part of footnote |
| TSFAE01B      | TITLE         | Some title               |
| TSFAE01B      | FOOTNOTE1     | First part of footnote   |
| TSFAE01B      | FOOTNOTE2     | Another part of footnote |
| TSFAE01B      | FOOTNOTE3     | Another part of footnote |
| TSFAE01B      | FOOTNOTE4     | Another part of footnote |
| TSFAE01B      | FOOTNOTE5     | Another part of footnote |
| TSFAE01B      | FOOTNOTE6     | Another part of footnote |
