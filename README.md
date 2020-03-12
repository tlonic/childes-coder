# childes-coder

childes-coder is a utility for coding data from [CHILDES](https://childes.talkbank.org/) corpora. It is written in R, using the [shiny](https://shiny.rstudio.com/) package, and relies on [childes-db](http://childes-db.stanford.edu/).

![Screenshot of childes-coder](https://github.com/tlonic/childes-coder/raw/master/images/screenshot.png)

## Getting Started

These instructions will help you set up an instance of childes-coder.

### Prerequisites

To install childes-coder, you need [R](https://www.r-project.org/), as well as the following R packages:

- dplyr
- childesr
- DBI
- pool
- shiny
- shinyjs
- yaml

You can run the `packages.R` script to install these.

You may also want to use [shiny-server](https://rstudio.com/products/shiny/shiny-server/), which provides an easy platform to run Shiny apps. The instructions below will assume you are using shiny-server.

Finally, you will need a MySQL server to store [childes-db](http://childes-db.stanford.edu/) and your responses database.

### Installing

First, you will need to make sure that you have [childes-db](http://childes-db.stanford.edu/) set up on a local MySQL database. Instructions for doing this can be found [here](http://childes-db.stanford.edu/releases.html). Note that instructions on the linked page are for version 0.1.0 of the database, which is an old version. If you replace 0.1.0 with 0.1.2 (http://childes-db-archive.s3.amazonaws.com/childes-db-version-0.1.2.sql.gz) you can download the most recent version of childes-db, which is required for `childesr`.

Next, you will need to copy `config.yaml`, `server.R`, `ui.R`, and `global.R` into a shiny-server app folder. The location of the shiny-server app folders will depend on your particular installation of shiny-server. For Linux installations, it may be located at /srv/shiny-server.

### Configuration

All of childes-coder's configuration is done in the `config.yaml` file. Provided is an example configuration for a corpus study of all instances of the tokens 'even' and 'especially', spoken by subjects marked as 'Target_Child' in CHILDES, in the 'Eng-NA' collection of corpora. The search section of the configuration is where you specify which tokens you would like to code. You can specify in this configuration section anything which is a valid argument for the `get_tokens` function in `childesr` (see [the childesr manual](https://cran.r-project.org/web/packages/childesr/childesr.pdf) for details. 

TODO: Expand configuration walkthrough.

### Citing childes-coder



