# SMHvalidation (development version)

-   fix bug in `check_df_values_required()` dealing with model task containing
    multiple output types.
-   minor fixes: read partition of one file, improve output error message 
    of invalid or missing variables combination, fix `n_decimal` comparison

# SMHvalidation 1.1.0

-   add `store_msg_val()` function to store `validate_submission()` function
    output in a character vector with a slightly different format (#39)
-   add `ignore_val_err` parameter in the `validate_submission()` function to 
    ignore stopping the validation if any `[valid_vals]` error (#40)
-   add parameter `store_msg_val()` function to remove valid check (and non 
    compound id check) on output of the function (#44)
-   fix message output style by removing unnecessary new line and output an 
    error if a value is incorrect instead of failure (#46)
-   fix required value test (#48)

# SMHvalidation 1.0.0

-   *BREAKING CHANGE*: changes functions `validate_submission()` and
    `generate_validation_plots()` behavior, output and parameters to
    adapt to hubverse v.5 schema update (see
    [hubverse schema releases](https://github.com/hubverse-org/schemas/releases)
    for information) (#37)
-   *BREAKING CHANGE*: update visualization function to adapt to the new format,
    and update the output to fix minor issues (#15)
-   *BREAKING CHANGE*: Integrate
    [hubValidations](https://hubverse-org.github.io/hubValidations/)
    function in `validate_submission()` and follow the same output style 
    (#30, #24)
-   *BREAKING CHANGE*: deprecate `pull_gs_data()` and `test_` functions.
-   add function to test file path associated with a partitioned submission
    (use hubverse functions for not partitioned files) (#5)

# SMHvalidation 0.1.2

-   Add an optional parameter `r_schema` to validate_submission()
    function, to allow to read partition files with a specific schema.
    If none provided, the schema will be created from the `js_def` JSON
    file. (only for partition files)

# SMHvalidation 0.1.1

-   Add parameter `verbose` to validate_submission() function to add
    information about the sample pairing information in output report.
    **only available for submission with samples output type** and add
    internal parameters extract from `js_def` to always print the
    pairing information of one or multiple columns in details.
    (parameter `verbose` need to be in the js_def\` file, see vignette
    for more information)
-   Update sample testing to be able to test sub-group pairing
    information
-   Update test on horizon behavior: submission with additional weeks
    now returns an error instead of a warning.
-   Add test on required values (returns an error if any values are
    missing)

# SMHvalidation 0.1.0

-   Added a `NEWS.md` file to track changes to the package.
-   Add more test and minor fixes
-   Add function to validate `race_ethinicity` column
-   Update functions to accept partitioned files and add optional
    parameter (`round_id`, etc.), see vignettes for more information
