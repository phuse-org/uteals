# View model for [`or_filtering_transformator()`](https://github.com/phuse-org/uteals/reference/or_filtering_transformator.md).

View model for
[`or_filtering_transformator()`](https://github.com/phuse-org/uteals/reference/or_filtering_transformator.md).

View model for
[`or_filtering_transformator()`](https://github.com/phuse-org/uteals/reference/or_filtering_transformator.md).

## Public fields

- `block_objects`:

  Block objects.

- `r_vals`:

  Reactive values.

- `alt_id`:

  The id of the last alternative.

- `block_conditions`:

  Conditions as strings.

- `choices_for_columns`:

  Choices for the columns.

- `final_filter_expr`:

  Final filtering expression.

- `final_exp`:

  Final expression for `teal.data`. Initializes the object

## Methods

### Public methods

- [`filtering_transformator_model$new()`](#method-filtering_transformator_model-new)

- [`filtering_transformator_model$add_alternative()`](#method-filtering_transformator_model-add_alternative)

- [`filtering_transformator_model$is_duplicate_condition()`](#method-filtering_transformator_model-is_duplicate_condition)

- [`filtering_transformator_model$clone()`](#method-filtering_transformator_model-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    filtering_transformator_model$new(data, dataname)

#### Arguments

- `data`:

  the reactive data object from `teal`.

- `dataname`:

  `character(1)` the name of the dataset.

------------------------------------------------------------------------

### Method `add_alternative()`

#### Usage

    filtering_transformator_model$add_alternative()

#### Returns

invisibly self.

------------------------------------------------------------------------

### Method `is_duplicate_condition()`

#### Usage

    filtering_transformator_model$is_duplicate_condition(cond_str, block_cond_list)

#### Arguments

- `cond_str`:

  `character(1)` added condition.

- `block_cond_list`:

  `character(1)` list of all conditions.

#### Returns

`logical(1)` whether the added condition is a duplicate.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    filtering_transformator_model$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
