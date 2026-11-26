# Toggling Input Visibility with \`input_visibility_transformator\`

`input_visibility_transformator` is a `teal_transform_module` that
automatically discovers the Shiny inputs rendered in a module’s encoding
panel and adds a collapsible **Toggle Input Visibility** control. Users
can show or hide individual inputs without any changes to the underlying
module code.

## When to use it

- A module exposes many inputs but only a subset are relevant for a
  given study.
- You want to reduce visual clutter by hiding advanced or rarely-used
  controls (e.g. grade filters, confidence interval settings) by default
  while keeping them accessible.

## Key arguments

| Argument | Type | Description |
|----|----|----|
| `label` | `character(1)` | Label shown in the transformator accordion (default: `"Toggle Input Visibility"`). |
| `hidden_by_default` | `character` | Vector of label substrings (case-insensitive). Any discovered input whose label contains one of these strings is hidden on first render. |

## Setup

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`uteals`](https://github.com/phuse-org/uteals)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal`](https://insightsengineering.github.io/teal/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.data`](https://insightsengineering.github.io/teal.data/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.modules.clinical`](https://insightsengineering.github.io/teal.modules.clinical/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`

## Example application

The app below registers `input_visibility_transformator` on three
`teal.modules.clinical` modules, each with a different set of inputs
hidden by default.

\
`data`` ``<-`` `[`teal_data`](https://rdrr.io/pkg/teal.data/man/teal_data.html)`(``)`\
`data`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(``data``, ``{`\
`  `[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`\
`  ``ADSL`` ``<-`` ``tmc_ex_adsl`\
`  ``.lbls_adae`` ``<-`` `[`col_labels`](https://rdrr.io/pkg/teal.data/man/col_labels.html)`(``tmc_ex_adae``)`\
`  ``ADAE`` ``<-`` ``tmc_ex_adae`` ``|>`` `[`mutate_if`](https://dplyr.tidyverse.org/reference/mutate_all.html)`(``is.character``, ``as.factor``)`\
`  `[`col_labels`](https://rdrr.io/pkg/teal.data/man/col_labels.html)`(``ADAE``)`` ``<-`` ``.lbls_adae`\
`  ``ADLB`` ``<-`` ``tmc_ex_adlb`\
`}``)`\
[`join_keys`](https://rdrr.io/pkg/teal.data/man/join_keys.html)`(``data``)`` ``<-`` ``default_cdisc_join_keys``[`[`names`](https://rdrr.io/r/base/names.html)`(``data``)``]`\
\
`ADSL`` ``<-`` ``data``[[``"ADSL"``]``]`\
`ADAE`` ``<-`` ``data``[[``"ADAE"``]``]`\
`ADLB`` ``<-`` ``data``[[``"ADLB"``]``]`\
\
`app`` ``<-`` ``teal``::`[`init`](https://rdrr.io/pkg/teal/man/init.html)`(`\
`  data ``=`` ``data``,`\
`  modules ``=`` ``teal``::`[`modules`](https://rdrr.io/pkg/teal/man/teal_modules.html)`(`\
`    ``# ── Adverse events by term: no inputs hidden, full toggle panel ────────`\
`    `[`tm_t_events`](https://insightsengineering.github.io/teal.modules.clinical/latest-tag/reference/tm_t_events.html)`(`\
`      label ``=`` ``"Adverse Event Table"``,`\
`      dataname ``=`` ``"ADAE"``,`\
`      arm_var ``=`` `[`choices_selected`](https://rdrr.io/pkg/teal.transform/man/choices_selected.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"ARM"``, ``"ARMCD"``)``, ``"ARM"``)``,`\
`      llt ``=`` `[`choices_selected`](https://rdrr.io/pkg/teal.transform/man/choices_selected.html)`(`\
`        choices ``=`` `[`variable_choices`](https://rdrr.io/pkg/teal.transform/man/variable_choices.html)`(``ADAE``, `[`c`](https://rdrr.io/r/base/c.html)`(``"AETERM"``, ``"AEDECOD"``)``)``,`\
`        selected ``=`` ``"AEDECOD"`\
`      ``)``,`\
`      hlt ``=`` `[`choices_selected`](https://rdrr.io/pkg/teal.transform/man/choices_selected.html)`(`\
`        choices ``=`` `[`variable_choices`](https://rdrr.io/pkg/teal.transform/man/variable_choices.html)`(``ADAE``, `[`c`](https://rdrr.io/r/base/c.html)`(``"AEBODSYS"``, ``"AESOC"``)``)``,`\
`        selected ``=`` ``"AEBODSYS"`\
`      ``)``,`\
`      add_total ``=`` ``TRUE``,`\
`      event_type ``=`` ``"adverse event"``,`\
`      transformators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`        `[`input_visibility_transformator`](https://phuse-org.github.io/uteals/reference/input_visibility_transformator.md)`(``)`\
`      ``)`\
`    ``)``,`\
\
`    ``# ── Demographics summary: hide the "show arm variable" and "useNA" ─────`\
`    `[`tm_t_summary`](https://insightsengineering.github.io/teal.modules.clinical/latest-tag/reference/tm_t_summary.html)`(`\
`      label ``=`` ``"Demographics"``,`\
`      dataname ``=`` ``"ADSL"``,`\
`      arm_var ``=`` `[`choices_selected`](https://rdrr.io/pkg/teal.transform/man/choices_selected.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"ARM"``, ``"ARMCD"``)``, ``"ARM"``)``,`\
`      add_total ``=`` ``TRUE``,`\
`      summarize_vars ``=`` `[`choices_selected`](https://rdrr.io/pkg/teal.transform/man/choices_selected.html)`(`\
`        choices ``=`` `[`variable_choices`](https://rdrr.io/pkg/teal.transform/man/variable_choices.html)`(``ADSL``, `[`c`](https://rdrr.io/r/base/c.html)`(``"SEX"``, ``"RACE"``, ``"BMRKR2"``, ``"EOSDY"``, ``"DCSREAS"``, ``"AGE"``)``)``,`\
`        selected ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"SEX"``, ``"RACE"``, ``"AGE"``)`\
`      ``)``,`\
`      useNA ``=`` ``"ifany"``,`\
`      transformators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`        `[`input_visibility_transformator`](https://phuse-org.github.io/uteals/reference/input_visibility_transformator.md)`(`\
`          hidden_by_default ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"show arm"``, ``"missing data"``)`\
`        ``)`\
`      ``)`\
`    ``)``,`\
\
`    ``# ── Lab summary by visit: hide numeric stats and denominator ──────────`\
`    `[`tm_t_summary_by`](https://insightsengineering.github.io/teal.modules.clinical/latest-tag/reference/tm_t_summary_by.html)`(`\
`      label ``=`` ``"Lab Summary by Visit"``,`\
`      dataname ``=`` ``"ADLB"``,`\
`      arm_var ``=`` `[`choices_selected`](https://rdrr.io/pkg/teal.transform/man/choices_selected.html)`(`\
`        choices ``=`` `[`variable_choices`](https://rdrr.io/pkg/teal.transform/man/variable_choices.html)`(``ADSL``, `[`c`](https://rdrr.io/r/base/c.html)`(``"ARM"``, ``"ARMCD"``)``)``,`\
`        selected ``=`` ``"ARM"`\
`      ``)``,`\
`      add_total ``=`` ``TRUE``,`\
`      by_vars ``=`` `[`choices_selected`](https://rdrr.io/pkg/teal.transform/man/choices_selected.html)`(`\
`        choices ``=`` `[`variable_choices`](https://rdrr.io/pkg/teal.transform/man/variable_choices.html)`(``ADLB``, `[`c`](https://rdrr.io/r/base/c.html)`(``"PARAM"``, ``"AVISIT"``)``)``,`\
`        selected ``=`` ``"AVISIT"`\
`      ``)``,`\
`      summarize_vars ``=`` `[`choices_selected`](https://rdrr.io/pkg/teal.transform/man/choices_selected.html)`(`\
`        choices ``=`` `[`variable_choices`](https://rdrr.io/pkg/teal.transform/man/variable_choices.html)`(``ADLB``, `[`c`](https://rdrr.io/r/base/c.html)`(``"AVAL"``, ``"CHG"``)``)``,`\
`        selected ``=`` ``"AVAL"`\
`      ``)``,`\
`      useNA ``=`` ``"ifany"``,`\
`      paramcd ``=`` `[`choices_selected`](https://rdrr.io/pkg/teal.transform/man/choices_selected.html)`(`\
`        choices ``=`` `[`value_choices`](https://rdrr.io/pkg/teal.transform/man/value_choices.html)`(``ADLB``, ``"PARAMCD"``, ``"PARAM"``)``,`\
`        selected ``=`` ``"ALT"`\
`      ``)``,`\
`      transformators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`        `[`input_visibility_transformator`](https://phuse-org.github.io/uteals/reference/input_visibility_transformator.md)`(`\
`          hidden_by_default ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"numeric"``, ``"denominator"``)`\
`        ``)`\
`      ``)`\
`    ``)`\
`  ``)`\
`)`\
\
`if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``shiny``::`[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``app``$``ui``, ``app``$``server``)`

## How it works

1.  After the module’s encoding panel renders, a JavaScript snippet
    walks the sidebar and assigns a stable `data-vis-key` attribute to
    each input container. A key is derived from the element’s ID or its
    DOM path when it is first seen and is never reassigned, so keys stay
    stable across re-discovery passes even when siblings are hidden.
2.  The discovered inputs are sent back to R via `Shiny.setInputValue`.
3.  R renders a checkbox list inside the accordion panel of the
    transformator with Show all / Hide all links.
4.  Toggling a checkbox sends a custom Shiny message that sets
    `element.style.display` on the corresponding DOM node — the input is
    hidden visually but remains fully functional in the background.
5.  Inputs whose labels match any entry in `hidden_by_default` start
    hidden. Their boxes start unchecked; check a box (or use Show all)
    to reveal the input.
6.  Accordion sections (e.g. “Additional table settings”) are treated as
    parent items. Toggling a parent cascades to all its children.
7.  A `MutationObserver` watches the sidebar for late-rendered elements
    (e.g. arm variable buckets) and triggers re-discovery automatically.

## Notes

- The transformator passes data through **unchanged** — it has no effect
  on the analysis or the output table.
- `hidden_by_default` matching is **case-insensitive** and uses
  substring matching, so `"grade"` will match both `"Grade filter"` and
  `"High grade"`.
- Each module instance gets its own independent visibility state; hiding
  an input in one tab does not affect other tabs.
