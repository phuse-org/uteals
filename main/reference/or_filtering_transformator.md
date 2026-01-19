# Apply Logical AND/OR filter transformations

**\[experimental\]** This transformator provides users with flexible,
dynamic filtering capabilities for datasets.

Each instance of the transformator operates on a **single dataset**.
Users can define multiple filter blocks, where:

- Conditions within each block are combined with **logical AND**.

- Multiple blocks are combined with **logical OR**.

This allows creating complex filter expressions like:

`(Condition1 AND Condition2) OR (Condition3 AND Condition4)`

To apply filtering across **multiple datasets**, users can instantiate
multiple instances of this module, each configured for a different
dataset. Each module call is independent and manages filters for its
specific dataset.

- **Supported Data Types & Expressions:**

  - Supports filtering on `character`, `factor`, and `numeric` columns.

  - Conditions can use operators: `==`, `!=`, `<`, `>`, `<=`, `>=`.

  - Conditions are specified as simple expressions, e.g.,
    `columnA == 'value'` `columnB != 5` `columnC >= 10`

  - Each block's conditions are combined with **AND**.

  - Multiple blocks are combined with **OR**.

- **Features:**

  - **Add Multiple OR Blocks:** Dynamically add new blocks for
    alternative conditions.

  - **Add Conditions:** Within each block, add multiple conditions, with
    duplicate prevention.

  - **Preview Filter Expression:** Generate and display the current
    combined filter expression.

  - **Remove Conditions:** Remove individual conditions within a block.

  - **Expression Generation:** The resulting expression can be directly
    used with
    [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
    or similar functions.

- **Usage Pattern:**

  - Call the module multiple times with different dataset names to
    filter multiple datasets independently.

  - Each call manages its own filter state and expression.

  - Users can build complex filters per dataset and apply or combine
    them as needed.

## Usage

``` r
or_filtering_transformator(dataname)
```

## Arguments

- dataname:

  (`character(1)`) Name of the dataset to filter. Pass a single dataset
  name as a string.

## Value

[`teal::teal_transform_module`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_transform_module.html)

## Examples

``` r

app <- teal::init(
  data = teal.data::teal_data(IRIS = iris),
  modules = teal::modules(teal::example_module(
    transformators = list(or_filtering_transformator("IRIS"))
  ))
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
