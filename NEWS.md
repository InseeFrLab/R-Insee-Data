# insee 1.0.0

* if dataset names are provided, get clean metadata with `get_idbank_list` (code breaks likely). The metadata is then stored locally on the computer. The update is automatically done every 3 months, and can be manually triggered
* use of dim columns in the idbank list is then deprecated
* add metadata to raw data with the function `add_insee_metadata`
* get columns' name with the function `get_column_title`
* `search_insee` function is now using the idbank and the dataset lists
* option to choose the separator with the `split_title` function
* vignette : deaths and births example added
* backup idbank list stored inside the package
* readsdmx dependency is used optionally but not by default
* package now officially supported by INSEE, source code and issue reports are moved to INSEE's official GitHub page

# insee 0.9.0

* all data is cached, and then all queries are run once per R session
* RapidXML C++ library is used by default in `get_insee` function through readsdmx package
* `get_insee_idbank` splits automatically a big query into several queries of 400-idbank each
* `search_insee` function : find datasets with a keyword
* `add_insee_title` function : find INSEE title of an unlimited amount of idbanks
* `split_title` function : split quickly the titles provided with the data
* pkgdown website : https://hadrilec.github.io/insee/
* code coverage with unit tests > 90%
* rsdmx and tidyr dependencies dropped, readsdmx added

# insee 0.8.0

* Bug fix in `get_insee_title` function
* more hands-on examples 

# insee 0.7.0

* first version on CRAN
