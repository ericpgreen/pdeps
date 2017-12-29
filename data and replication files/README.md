All contents of this repository (and associated website) are licensed under the [CC-BY license](https://creativecommons.org/licenses/by/4.0/). Each published paper has its own citation. To cite the data, please use:

Green, E.P. (2017). PDEPS data repository. [![DOI](https://zenodo.org/badge/115718962.svg)](https://zenodo.org/badge/latestdoi/115718962)

* * * 

To replicate papers and analyses:

1. Clone this repository.
2. Open the `manuscript.Rnw` file in `replication/JOURNAL` in RStudio. 
3. Packrat should prompt you to get the package versions used when the analysis was published.
4. `manuscript.Rnw` will run `master/analysis.R`, which loads data from `master/input`, runs `master/scaleScores.R`, and outputs to `master/output`.