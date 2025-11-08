# Add a condition to a BlockConditions object

Add a condition to a BlockConditions object

## Usage

``` r
addCondition(object, variable, operator, value)
```

## Arguments

- object:

  A `BlockConditions` object.

- variable:

  A character string specifying the variable/column name.

- operator:

  A character string specifying the operator (e.g., "==", "!=", "\<",
  "\>", "\<=", "\>=").

- value:

  The value to compare against.

## Value

An updated `BlockConditions` object with the new condition added.
