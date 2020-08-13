# insee 0.9.0

* progress bar addition in `get_insee` function
* all data is cached, and then all queries are run once per R session
* `get_insee_idbank` splits automatically a big query into several queries of 400-idbank each
* `search_insee` function : find datasets with a keyword
* `add_insee_title` function : find INSEE title of an unlimited amount of idbanks
* `split_title` function : split quickly the titles provided with the data
* new vignette
* new pkgdown website : https://hadrilec.github.io/insee/
* rsdmx and tidyr dependencies dropped

# insee 0.8.0

* Bug fix in `get_insee_title` function
* more hands-on examples 

# insee 0.7.0

* first version on CRAN
