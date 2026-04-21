# Change X-axis scaling decorator for [`teal.modules.clinical::tm_g_forest_rsp`](https://insightsengineering.github.io/teal.modules.clinical/latest-tag/reference/tm_g_forest_rsp.html).

**\[experimental\]** A function to create a UI component for selecting a
transform function for the forest plot x axis.

## Usage

``` r
forestplot_x_decorator()
```

## Value

[`teal::teal_transform_module`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_transform_module.html)
Returns a modified plot object with the transformation applied.

## Details

The module creates a UI with a radio control for selecting the transform
function. The selected transformation function is applied to the forest
plot to update the plot's axis and annotations accordingly.

## Examples

``` r
library(teal.modules.clinical)
#> Loading required package: teal
#> Loading required package: shiny
#> Loading required package: teal.data
#> Loading required package: teal.code
#> Loading required package: teal.slice
#> 
#> You are using teal version 1.1.0
#> 
#> Attaching package: ‘teal’
#> The following objects are masked from ‘package:teal.slice’:
#> 
#>     as.teal_slices, teal_slices
#> Loading required package: teal.transform
#> Loading required package: tern
#> Loading required package: rtables
#> Loading required package: formatters
#> 
#> Attaching package: ‘formatters’
#> The following object is masked from ‘package:base’:
#> 
#>     %||%
#> Loading required package: magrittr
#> 
#> Attaching package: ‘rtables’
#> The following object is masked from ‘package:utils’:
#> 
#>     str
data <- teal.data::teal_data()
data <- within(data, {
  ADSL <- teal.modules.clinical::tmc_ex_adsl
  ADRS <- teal.modules.clinical::tmc_ex_adrs |>
    dplyr::mutate(AVALC = tern::d_onco_rsp_label(AVALC) |>
      formatters::with_label("Character Result/Finding")) |>
    dplyr::filter(PARAMCD != "OVRINV" | AVISIT == "FOLLOW UP")
})
teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADRS <- data[["ADRS"]]

arm_ref_comp <- list(
  ARM = list(
    ref = "B: Placebo",
    comp = c("A: Drug X", "C: Combination")
  ),
  ARMCD = list(
    ref = "ARM B",
    comp = c("ARM A", "ARM C")
  )
)

app <- teal::init(
  data = data,
  modules = teal::modules(
    teal.modules.clinical::tm_g_forest_rsp(
      label = "Forest Response",
      dataname = "ADRS",
      arm_var = choices_selected(
        variable_choices(ADSL, c("ARM", "ARMCD")),
        "ARMCD"
      ),
      arm_ref_comp = arm_ref_comp,
      paramcd = choices_selected(
        value_choices(ADRS, "PARAMCD", "PARAM"),
        "INVET"
      ),
      subgroup_var = choices_selected(
        variable_choices(ADSL, names(ADSL)),
        c("BMRKR2", "SEX")
      ),
      strata_var = choices_selected(
        variable_choices(ADSL, c("STRATA1", "STRATA2")),
        "STRATA2"
      ),
      plot_height = c(600L, 200L, 2000L),
      decorators = list(
        plot = forestplot_x_decorator()
      ),
      default_responses = list(
        BESRSPI = list(
          rsp = c("Stable Disease (SD)", "Not Evaluable (NE)"),
          levels = c(
            "Complete Response (CR)", "Partial Response (PR)", "Stable Disease (SD)",
            "Progressive Disease (PD)", "Not Evaluable (NE)"
          )
        ),
        INVET = list(
          rsp = c("Complete Response (CR)", "Partial Response (PR)"),
          levels = c(
            "Complete Response (CR)", "Not Evaluable (NE)", "Partial Response (PR)",
            "Progressive Disease (PD)", "Stable Disease (SD)"
          )
        ),
        OVRINV = list(
          rsp = c("Progressive Disease (PD)", "Stable Disease (SD)"),
          levels = c("Progressive Disease (PD)", "Stable Disease (SD)", "Not Evaluable (NE)")
        )
      )
    )
  )
)
#> Initializing tm_g_forest_rsp
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
