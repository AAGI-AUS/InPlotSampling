# Generate a heat map of SBS PPS sample of the provided population.

Generate a heat map of SBS PPS sample of the provided population.

## Usage

``` r
sbs_pps_heatmap(pop, sbs_indices, pps_indices)
```

## Arguments

- pop:

  Population data frame to be sampled with 5 columns.

  1.  Halton numbers

  2.  X1-coordinate of population unit

  3.  X2-coordinate of population unit

  4.  Size measurements of population units

  5.  Inclusion probabilities

- sbs_indices:

  Indices of SBS sample.

- pps_indices:

  Indices of PPS sample.

## Value

Heat map of the sample.
