# Coombe Vineyard Data from 2019 Season.

Measurements taken on the Coombe Research Vineyard, University of
Adelaide Waite Campus after the 2019 season.

## Usage

``` r
coombe2019
```

## Format

A data frame with 352 rows and 11 variables:

- vine_id:

  Identifier for the individual vines (1-352)

- rootstock:

  The rootstock that the vine is growing on (8 levels)

- row:

  The row in the vineyard (10-20)

- panel:

  The panel of the vines. A pair of vines is grouped into a panel (1-16)

- trunk_circ_18:

  The trunk circumference of the vine in 2018, measured in cm at
  watering height (~20 cm above the ground).

- trunk_circ_19:

  The trunk circumference of the vine in 2019, measured in cm at
  watering height (~20 cm above the ground).

- count_shoots:

  Check this?

- non_count_shoots:

  Check this?

- total_shoots:

  Sum of `count_shoot` and `non_count_shoot`.

- pruning_weight:

  Weight of the material removed during pruning in Kg. Check this?

- cordon_length:

  The length of the cordon in cm
