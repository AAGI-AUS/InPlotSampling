# Generate SBS PPS sample indices.

Generate SBS PPS sample indices.

## Usage

``` r
get_sbs_pps_sample_indices(population, n, with_unique_pps = FALSE)
```

## Arguments

- population:

  Population data frame to be sampled with 2 columns.

  1.  Halton numbers

  2.  Size measurements of population units

- n:

  Sample sizes (SBS sample size, PPS sample size).

- with_unique_pps:

  A boolean to specify whether to use unique indices for pps sample or
  not.

## Value

A named list of:

- sbs_pps_indices: sample indices

- sbs_indices: sbs sample indices

- pps_indices: pps sample indices

- sizes_wo_sbs: measured sizes without sbs sample
