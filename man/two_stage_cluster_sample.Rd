% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/two_stage_cluster_sample.R
\name{two_stage_cluster_sample}
\alias{two_stage_cluster_sample}
\title{Generate two-stage cluster sampling on the population provided.}
\usage{
two_stage_cluster_sample(
  pop,
  sampling_strategies,
  n,
  H,
  replace,
  ni,
  Hi,
  replace_i
)
}
\arguments{
\item{pop}{Population that will be sampled with these ordered columns:
\enumerate{
\item Parent id: an index to denotes the parent of the record
\item Parent auxiliary parameter: an auxiliary parameter for ranking parents
\item Child auxiliary parameter: an auxiliary parameter for ranking children
}}

\item{sampling_strategies}{(first stage sampling strategy, second stage sampling strategy), e.g.,
\code{c('srs', 'jps')}.
\itemize{
\item \code{'srs'}: simple random sampling without replacement
\item \code{'jps'}: JPS sampling
}}

\item{n}{Number of samples in the first stage.}

\item{H}{Set size for each ranking group in the first stage.}

\item{replace}{A boolean which specifies whether to sample with replacement or not in the first stage
(applicable only for JPS sampling).}

\item{ni}{Number(s) of samples in the second stage. Can be a single number or a vector of \code{n} numbers.}

\item{Hi}{Set size for each ranking group in the second stage. Can be a single number or a vector of \code{n}
numbers.}

\item{replace_i}{A boolean which specifies whether to sample with replacement or not in the second stage
(applicable only for JPS sampling).}
}
\value{
A matrix with ranks from each ranker.
}
\description{
Generate two-stage cluster sampling on the population provided.
}
\examples{
set.seed(112)
parent_size <- 300
child_size <- 50
# the number of samples to be ranked in each set
H <- 3

sampling_strategies <- c("jps", "jps")
replace <- FALSE
mu <- 10
sigma <- 4
n <- 4

parent_indices <- rep(1:parent_size, child_size)
parent_aux <- abs(qnorm(1:parent_size / (parent_size + 1), mu, sigma) + 5 * rnorm(parent_size, 0, 1))
child_aux <- abs(parent_aux + 10 * rnorm(parent_size * child_size, 0, 1))

population <- cbind(parent_indices, rep(parent_aux, child_size), child_aux)
two_stage_cluster_sample(population, sampling_strategies, n, H, replace, 6, 3, FALSE)
#>       parent_id parent_rank child_id  child_aux child_rank
#>  [1,]       201           1     7101  2.2349453          1
#>  [2,]       201           1    12801  9.7175545          3
#>  [3,]       201           1     6501  7.9207230          1
#>  [4,]       201           1     9801  5.7644835          2
#>  [5,]       201           1    10701 13.8089335          3
#>  [6,]       201           1     3501  0.3598331          1
#>  [7,]       254           2     8654 17.3059292          3
#>  [8,]       254           2    11354 15.0837335          2
#>  [9,]       254           2     9254  6.0103919          2
#> [10,]       254           2     2954 12.7011502          2
#> [11,]       254           2    14954  5.1158133          2
#> [12,]       254           2    13754  5.8931551          1
#> [13,]        74           1     8474  4.3393349          1
#> [14,]        74           1     9674 15.0512523          2
#> [15,]        74           1     6674 12.9022479          3
#> [16,]        74           1      674  2.9209174          2
#> [17,]        74           1     7274  7.2500468          3
#> [18,]        74           1     6374  7.0925954          1
#> [19,]       223           3     9223 28.4694257          3
#> [20,]       223           3      223  4.4001977          1
#> [21,]       223           3     9823 22.8676415          3
#> [22,]       223           3    11923 26.4531048          3
#> [23,]       223           3      823 20.8714211          2
#> [24,]       223           3     9523  8.1783058          1

}
