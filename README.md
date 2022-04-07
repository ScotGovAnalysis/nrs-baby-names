Baby names
================

A R shiny app showing baby names trends in Scotland since 1974. Go to
[the app](https://scotland.shinyapps.io/nrs-baby-names/).

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

![Screenshot of the baby names shiny
app](https://github.com/DataScienceScotland/baby_names/blob/master/Screenshot.png)

## How it works

Tidy up the user input and split into a vector of valid names. Plot
these names and display a list of similar names by [Levenshtein
distance](https://en.wikipedia.org/wiki/Levenshtein_distance). Each of
these similar names is an `ActionLink` that updates the plot.

## How to update

1.  Make a copy of the most recent files
2.  Update the CSV file
      - Check for names that contain characters that aren’t letters,
        dashes or apostrophes (regex: `[^A-Za-z'-]`)
      - User input is split by these characters in `server.R`
        (e.g. commas and spaces)
3.  Update the code
      - `tickvals` in the plot
      - Update the `Download the data` link
      - Check and update the top names for the default display in `ui.R` & `server.R`
4.  Create a [feather
    file](https://blog.rstudio.com/2016/03/29/feather/) from the CSV
    file
      - There is a script for this
      - This format is used to reduce the load time of the app. In
        future the data could be uploaded to statistics.gov.scot. This
        would allow the app to use the API instead.
5. The day before launch, [scale up performance](https://www.rstudio.com/products/shinyapps/shinyapps-io-performance-tuning/) noting the original values you used
6. Once demand has returned back to normal (usually after a few days) scale performance back down by returning the settings in step 5 back to normal

## Licence

This repository is available under the [Open Government Licence
v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).
