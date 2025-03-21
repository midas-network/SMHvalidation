
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

# SMHvalidation

R package containing functions to validate and visualize Scenario Modeling Hub 
submissions, and to pull data from the R package
`covidcast` (output in the Scenario Modeling Hub (SMH) standard format). 
For more information on the Scenario Modeling Hub and/or on how to participate,
please consult the Scenario Modeling Hub 
[GitHub repository](https://github.com/midas-network/covid19-scenario-modeling-hub)
or [website](https://scenariomodelinghub.org/)

## Installation

The package is currently only available on GitHub, to install it please
follow the next steps:

```{r}
install.packages("remotes")
remotes::install_github("midas-network/SMHvalidation") 
```

or it can be manually installed by directly cloning/forking/downloading
the package from GitHub.

```{r}
library(SMHvalidation)
```

## Validate submission:

#### Description

The main validation function, `validate_submission()`, can be used to
validate Scenario Modeling Hub submissions. The function runs multiple
checks, please look at the vignette ("Validation Checks")
containing the list of all the tests with detailed information.

The function internally runs all the different validation checks
functions (`test_column()`, `test_scenario()`, `test_origindate()`,
`test_quantiles()`, `test_val()`, `test_target()`, `test_location()`, etc.) on
SMH submissions and prints information about the results of each tests
on the submission: warning(s), error(s) or if all the tests were
successful.

For more information, each function has a documentation associated
accessible with `?validate_submission()` for example.

#### Usage

To test a submission file, it's necessary to provide 2 arguments:

-   the path of the file to test (CSV, ZIP or GZ file format)
-   path to JSON file containing round definitions: names of columns,
    target names, ... following the 
    [`tasks.json` Hubverse format](https://hubdocs.readthedocs.io/en/latest/user-guide/hub-config.html#hub-model-task-configuration-tasks-json-file)

```{r}
js_def <- "PATH/TO/ROUND/tasks.json"
validate_submission("PATH/TO/SUBMISSION", js_def)
```

As a warning:

-   The vast majority of submissions done in 2021 will return an error
    on the model_projection_date column as the rules and tests on this
    column have been made and put in place only since 2022 (COVID-19 -
    round 12).

-   Also, the cumulative observed data for the location 39 has been
    corrected after COVID-19 round 12 publication so all the submissions
    might have an error saying:\
    `"Error: Some value(s) are inferior than the last observed cumulative death count. Please check location(s): 39 "`

These 2 errors can be ignored.

## Pull "truth data":

For more information, please feel free to look at the documentation of
the function (`?pull_gs_data`)

Currently, the function only pull COVID-19 data.

#### Description

The `SMHvalidation` package contains the function
`pull_gs_data()` that download and compile data to output weekly data on
cumulative and incidence number of cases and deaths, and hospitalization
incidence:

-   For the cumulative numbers: use the `confirmed_cumulative_num` and
    `deaths_cumulative_num` signals from `jhu-csse`. As the data are
    expressed per day and the Scenario Modeling Hub use cumulative data
    by epiweek, the last day of each epiweek is selected for each
    location and the values are added between all states to obtains the
    US level data.

-   For the incidence numbers: use the `confirmed_incidence_num` and
    `deaths_incidence_num` signals from `jhu-csse` and
    `confirmed_admissions_covid_1d` from `hhs`. As the data are
    expressed per day and the Scenario Modeling Hub use cumulative data
    by epiweek, the sum of the incidences is calculated by epiweek and
    by location, and the values are added between all states to obtains
    the US level data.

#### Source

For the cases and deaths data, the data are coming from the JHU CSSE
group and for the hospitalization incidence, the data are extracted from
the HealthData.gov COVID-19 Reported Patient Impact and Hospital
Capacity by State Daily and Timeseries datasets.

To access all these data we use the `covidcast` R package. A package
using the COVIDcast Epidata API maintained by the Delphi Research Group
at Carnegie Mellon University. Please see
[here](https://cran.r-project.org/web/packages/covidcast/index.html),
for more information.

#### Usage

```{r}
# Store the output in an object
lst_gs <- pull_gs_data()

# Look at the sctructure of the object
str(lst_gs)

# Print the output
lst_gs
```

## Code license

The overall project is available under an open-source MIT license.

## Contributing

Please feel free to open an issue if you identify any issue with the
package or would like to suggest an idea/improvement. We also welcome
any pull-request.

## Funding

Scenario modeling groups are supported through grants to the
contributing investigators.

The Scenario Modeling Hub site is supported by the [MIDAS Coordination
Center](https://midasnetwork.us/), NIGMS Grant U24GM132013 to the
University of Pittsburgh.
