# KPLANT Database — Data control and figure code

This repository contains the R code used for **data control** and for **generating
the figures and technical-validation analyses** of the KPLANT database, a global
compilation of whole-plant hydraulic conductance (*k*<sub>plant</sub>)
measurements.

The published database itself (`KPLANT_database.csv`, the data-extraction
template, and per-dataset quality-control reports) is archived on Zenodo and
released under CC BY 4.0. **This repository contains code only; the data are
on Zenodo.**

## Data paper

> Zhao, Y., Mencuccini, M., *et al.*, Flo, V. KPLANT: a global database of plant
> water transport capacity. *Scientific Data*, *in preparation* (DOI pending).

The database compiles 3,873 observations from 356 datasets, covering 429
species across 245 genera and 100 families, sampled on 6 continents and 7
plant growth forms (tree, shrub, liana, graminoid, forb, stem succulent, fern),
spanning 1969 to the present. *k*<sub>plant</sub> is provided in original
units together with standardised expressions on five normalisation bases
(leaf area, sapwood area, wood area, ground area, and whole-plant).

## Repository contents

| File | Purpose |
| --- | --- |
| `species_name_check.R` | Resolves species names against the GBIF backbone via the `taxize` R package and writes `species_names_clean.csv`. Run this first. |
| `Read_data.R` | Reads the per-dataset Excel files in `data/`, applies harmonisation (column renaming, taxonomic overrides, type coercion, unit rounding) and writes the consolidated tab-delimited database file. |
| `digitalization_error_evaluation.R` | Technical-validation analysis: fits a linear mixed model to the digitisation error obtained from 10 synthetic plots digitised by 4 independent observers, as reported in the data paper. |
| `Kplant_database.Rproj` | RStudio project file. |
| `LICENSE` | MIT licence (code only). |

The source Excel files (`data/`) and the consolidated database (`KPLANT_database.csv`)
are **not** distributed in this repository; they are part of the Zenodo deposit.

## Installation

Clone the repository and open the RStudio project:

```bash
git clone https://github.com/vflo/Kplant_database.git
```

Required R packages:

```r
install.packages(c("readxl", "dplyr", "purrr", "tidyr", "readr",
                   "stringr", "ggplot2", "taxize"))
```

Tested with R ≥ 4.3.

## Usage

The pipeline expects the per-dataset Excel files (downloaded from the Zenodo
deposit) to be placed in a `data/` folder at the project root. The recommended
order is:

1. **Resolve taxonomy** — run `species_name_check.R` to query GBIF and write
   `species_names_clean.csv`. This step requires an internet connection and is
   sensitive to the GBIF backbone version at the time of execution.
2. **Build the consolidated database** — run `Read_data.R` to ingest the Excel
   files, join the resolved taxonomy, apply manual taxonomic and contributor
   corrections, standardise variable names and types, and write the
   tab-delimited output. The current versioned output is `Kplant_0.1.5.csv`;
   the file is renamed to `KPLANT_database.csv` for the Zenodo release.
3. **Reproduce the digitisation-error validation** — run
   `digitalization_error_evaluation.R` to reproduce the linear mixed-model
   analysis of WebPlotDigitizer error reported in the Technical Validation
   section of the data paper.

End users who only want to **analyse the database** do not need to run any of
this code: download `KPLANT_database.csv` directly from Zenodo and load it,
e.g.:

```r
library(readr)
library(dplyr)

kplant <- read_tsv("KPLANT_database.csv")

kplant |>
  filter(!is.na(k_plant_leaf), pl_growth_form == "tree") |>
  glimpse()
```

Variable definitions and units are listed in Tables 1–5 and S1–S6 of the data
paper.

## Related repositories

- [`vflo/Kwp_QC`](https://github.com/vflo/Kwp_QC) — automated quality-control
  procedure that generates the per-dataset HTML reports archived on Zenodo.

## License

Code in this repository is released under the [MIT License](LICENSE).
The data archived on Zenodo are released under
[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

## Contact

Víctor Flo — CREAF — `v.flo@creaf.uab.cat`
