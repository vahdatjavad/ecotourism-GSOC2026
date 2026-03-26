
# Ecotourism GSoC 2026 Shiny App

This repository contains my submission work for the **GSoC 2026 Ecotourism project**.

The app combines the three requested tasks into one Shiny application:

- **Easy Task**: interactive tutorial questions based on joining occurrence, weather, and tourism data
- **Medium Task**: wildlife occurrence explorer with an Australia map and filtering tools
- **Hard Task**: wildlife holiday planner that recommends where and when to go based on historical sightings and weather patterns

## Project idea

The app is designed to help users explore the best locations and times to spot wildlife across Australia using the `ecotourism` package.

Users can:
- explore organism sightings on a map
- compare patterns across months and years
- view linked weather summaries
- plan a wildlife holiday by choosing a date, state, and organism
- get ranked recommendations for places with strong historical sighting patterns

## Datasets used

This app uses datasets from the `ecotourism` package, including:

- `glowworms`
- `gouldian_finch`
- `manta_rays`
- `orchids`
- `weather`
- `weather_stations`
- `tourism_quarterly`
- `tourism_region`

## App structure

### Easy Task
A guided tutorial with editable code and runnable outputs for joining:
- organism occurrence data
- weather data
- tourism data

### Medium Task
An interactive wildlife explorer with:
- organism filter
- month filter
- record type filter
- Australia sightings map
- state summary table
- monthly, yearly, and weather plots

### Hard Task
A wildlife holiday planner where users choose:
- a future travel date
- a state or territory
- a target organism

The app then returns:
- best recommended locations
- typical weather for that time of year
- expected best viewing time
- alternative organisms that may be easier to spot

## How to run locally

Install the required packages and run the app:

```r
install.packages(c("shiny", "dplyr", "ggplot2", "leaflet", "DT", "ecotourism"))
shiny::runApp("app.R")
