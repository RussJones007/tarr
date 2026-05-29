# tarr

`tarr` is an internal R package for Tarrant County public health and epidemiology workflows. It combines case-line-list import and harmonization, spatial clustering and mapping, MMWR date utilities, text classification helpers, and a set of small analyst-oriented tools used in routine reporting.

## What The Package Covers

### Case Line List Import And Harmonization

-   `nbs_import()` reads NBS case line list exports and returns processed data.
-   `nbs_process()` formats NBS data, standardizes fields, calculates derived variables, and filters to the case statuses used for analysis.
-   `epitrax_import()` reads EpiTrax line list exports, adds derived date and age fields, and supports a simple parquet cache.
-   `epitrax_2_nbs_format()` converts EpiTrax field names and types so EpiTrax records can be combined with NBS-shaped data.
-   `baseData()`, `baseNBSUpdate()`, and `construct_nbs_base()` support maintaining a reusable base case dataset built from NBS and current-year EpiTrax exports.

### Spatial Data, Clustering, And Mapping

-   `makeAddress()` builds cleaned address strings from line list data.
-   `correct_city()` standardizes city names used in addresses and mapping workflows.
-   `load_tarrant_spatial()` loads commonly used Tarrant County spatial layers.
-   `scan_cluster()` applies density-based clustering to `sf` point data using `dbscan`, `hdbscan`, or `optics`.
-   `mapCluster()`, `get_cluster_polys()`, and the `clustered` S3 methods support plotting and working with clustering results.
-   `map_theme` provides a package-specific map theme for ggplot output.
-   `geo_sets` stores the paths to common Tarrant County spatial datasets.

### Epidemiology Date Helpers

-   `mmwrWeek()`, `mmwrWeekBegin()`, `mmwrWeekEnd()`, `mmwrWeekMonth()`, and `mmwrYearFirstEndDate()` provide MMWR week calculations based on CDC rules.
-   These functions support line list processing and reporting by turning event dates into epidemiologically useful week values.

### Text Classification

-   `make_classifier()` is a general regex-based classifier factory.
-   `classifier_patterns()` retrieves or updates the stored true/false pattern sets for a classifier.
-   `case_interview_classifier()` is a ready-made classifier for identifying whether note text indicates a case interview occurred.

### Analyst Utilities

-   `read.clip()` and `write.clip()` move tabular data between R and the clipboard.
-   `bar_wrap()` wraps another function with a progress bar for repeated calls.
-   String and normalization helpers such as `only_alpha()`, `only_digits()`, `only_pattern()`, `mash_name()`, and `mash_phone()`
    for special text pricessing
-   `simplify_term()` simplifies disease names for reporting.
-   `epi_curve()` provides epidemic curve plotting for `incidence2` objects.

## Package Behavior

The package no longer attaches companion packages on startup. Load `rage` or `tarr.pop` directly when you need their age-group or population workflows.

The package also includes path and file-selection infrastructure intended for the local reporting environment:

-   `paths` contains named paths used by some workflows.
-   `tarr.filters` contains file filter definitions for use interactive import routines.

## Key Dependencies

The package relies heavily on:

-   `dplyr`, `tidyr`, `purrr`, `stringr`, `stringi`, and `janitor` for data wrangling
-   `sf`, `dbscan`, `tmap`, `ggplot2`, `ggthemes`, and `tidyterra` for spatial analysis and mapping
-   `lubridate` and `incidence2` for epidemiology date and curve workflows
-   `clipr` for clipboard integration
-   `nanoparquet` and `arrow`-adjacent tooling for parquet-based caching

## Example Workflows

### Import NBS Data

``` r
library(tarr)

cases <- nbs_import("NBS_line_list.csv")
```

### Import EpiTrax Data And Convert To NBS Shape

``` r
library(tarr)

epitrax_cases <- epitrax_import("EpiTrax_cases.csv")
combined_ready <- epitrax_2_nbs_format(epitrax_cases)
```

### Calculate MMWR Information

``` r
library(tarr)

mmwrWeek(as.Date("2025-01-15"))
mmwrWeekEnd(weekNum = 3, yr = 2025)
```

### Cluster Point Data

``` r
library(tarr)
library(sf)

pts <- st_as_sf(
  synthetic_outbreak,
  coords = c("lon", "lat"),
  crs = 4326
)

clusters <- scan_cluster(pts, minPts = 7)
plot(clusters)
```

### Classify Encounter Text

``` r
library(tarr)

notes <- c(
  "Interview completed with patient",
  "Left voicemail for nurse"
)

case_interview_classifier(notes, return_detail = TRUE)
```

## Included Data And Objects

The package includes several supporting objects used by epidemiologists and by the package itself, including:

-   `geo_sets` for common spatial layers
-   `paths` for frequently used directories
-   `synthetic_outbreak` for examples and testing
-   simplified disease-name mappings and other lookup-style objects in `R/Misc.r`

## Development Notes

-   Tests are under `tests/testthat/`.
-   Some workflows assume access to local or shared-drive data resources that are not bundled in the package repository.

## Repository

-   Source: <https://github.com/RussJones007/tarr>
-   Issues: <https://github.com/RussJones007/tarr/issues>
