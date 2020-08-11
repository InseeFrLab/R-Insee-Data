# insee 0.9.0

* all data are cached, all queries are then run one per R session
* drop rsdmx dependency
* progress bar addition in `get_insee` function
* `get_insee_idbank` splits automatically a big query into several queries of 400-idbank each
* `search_insee` function : find datasets with a keyword
* `add_insee_title` function : find INSEE title of an unlimited amount of idbanks
* `split_title` function : split quickly the titles provided with the data
* new vignette
* new pkgdown website : https://hadrilec.github.io/insee/

# insee 0.8.0

* Bug fix in `get_insee_title` function
* more hands-on examples 

# insee 0.7.0

`insee 0.7.0` first version on CRAN
