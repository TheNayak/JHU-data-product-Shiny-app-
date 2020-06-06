This small Shiny application demonstrates Shiny's automatic UI updates.

There are three elements in this app:

1. The interactive world map which is a plotly choropleth map showing up to date COVID-19 cases per country.

2. The bar chart shoes up to date total confirmed COVID 19 cases of top 20 countries worst affected.

3. The line plot that takes the input from the list of countries and shows the statistics for that country.

Use the *Select the country* list and notice how the line plot is automatically re-evaluated causing a new distribution to be generated and the plot to be rendered.

The data is scraped from the following sources:

- The Covid time series from [https://datahub.io/core/covid-19](https://datahub.io/core/covid-19)
- The data for map from [https://www.worldometers.info/coronavirus/](https://www.worldometers.info/coronavirus/)
- The country codes required for map from [here.](https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv)
- The current statistics displayed above bar plot from [here.](https://www.worldometers.info/coronavirus/)



