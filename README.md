
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

# SMHvalidation

R package containing functions to validate and visualize Scenario Modeling Hub 
submissions.
For more information on the Scenario Modeling Hub and/or on how to participate,
please consult the Scenario Modeling Hub 
[GitHub repository](https://github.com/midas-network/covid19-scenario-modeling-hub)
or [website](https://scenariomodelinghub.org/).

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
containing detailed information.

The function internally runs different validation checks
functions from 
[hubValidations package](https://hubverse-org.github.io/hubValidations/) on
SMH submissions and prints detail information about the results of each tests
on the submission.

For more information, please consult the documentation associated with the
function: `?validate_submission()` for example.

#### Usage

To test a submission file, it's necessary to provide 2 arguments:

-   `path`: the path to the submissions file (or folder for partitioned data) 
    to test. PQT, PARQUET, CSV, ZIP (not partitioned), or GZ (not partitioned) 
    file format are accepted. The path should be relative to the path of the hub
    containing the file to check

-   `hub_path`: path to the repository contains the submissions files and hub
    config files including the `tasks.json` file in the hubverse format (version
    5.0 accepted), please find information on the `tasks.json` format and 
    definition on the [hubverse website](https://hubdocs.readthedocs.io/en/latest/user-guide/hub-config.html#hub-model-task-configuration-tasks-json-file)

```{r}
validate_submission(path, hub_path)
```

As a warning, the vast majority of submissions done before 2025 might returns 
false error with the new format of the `validate_submission()` function (v1.0). 
We advice to use previous version for validation. 

## Code license

The overall project is available under an open-source MIT license.

## Contributing

Please feel free to open an issue if you identify any issue with the
package or would like to suggest an idea/improvement. We also welcome
any pull-request.

## Funding

Scenario modeling groups are supported through grants to the
contributing investigators.

The Scenario Modeling Hub site is supported by the 
[MIDAS Coordination Center](https://midasnetwork.us/), NIGMS Grants U24GM132013 
(2019-2024) and R24GM153920 (2024-2029) to the University of Pittsburgh.
