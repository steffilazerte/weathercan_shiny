# `weathercan` and Shiny

This app demonstrates how data downloaded via `weathercan` can be explored through 
a Shiny app.

You can view the app live here: <https://steffilazerte.shinyapps.io/weathercan_shiny/>

Alternatively, you can modify the data and display by playing around with the
code in this repository.

- The data is prepared in `get_data.R` with the following packages:
    ```r 
    install.packages("weathercan", "dplyr", "stringr", "sf", "janitor",
                     "rnaturalearth")
    ```

- The shiny app relies on the following packages:

    ```
    install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "leaflet", 
                       "sf", "glue", "DT", "ggiraph"))
    ```

- `DEPLOY.R` is a quick deployment to shinyapps.io (provided you have setup access)