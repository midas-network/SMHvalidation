# SMHvalidation 0.1.2

* Add an optional parameter `r_schema` to validate_submission() function, to 
allow to read partition files with a specific schema. If none provided, the 
schema will be created from the `js_def` JSON file. (only for partition files)

# SMHvalidation 0.1.1

* Add parameter `verbose` to validate_submission() function to add information 
about the sample pairing information in output report. 
**only available for submission with samples output type** and add internal 
parameters extract from `js_def` to always print the pairing information of 
one or multiple columns in details. (parameter `verbose` need to be in the `
`js_def` file, see vignette for more information)
* Update sample testing to be able to test sub-group pairing information
* Update test on horizon behavior: submission with additional weeks now returns 
an error instead of a warning.
* Add test on required values (returns an error if any values are missing)

# SMHvalidation 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Add more test and minor fixes
* Add function to validate `race_ethinicity` column
* Update functions to accept partitioned files and add optional parameter 
(`round_id`, etc.), see vignettes for more information
