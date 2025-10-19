# dwhr

> Dashboard library for data warehouse data

dwhr is an R package that provides a framework for building interactive data warehouse dashboards using Shiny. It implements a star schema architecture that makes it easy to create hierarchical, drillable visualizations with dimensions, measures, and multiple presentation types.

## Features

- **Star Schema Architecture**: Build dashboards around fact tables and dimension views
- **Hierarchical Navigation**: Drill down and up through dimension levels
- **Multiple Presentation Types**:
  - Interactive DataTables with formatting, sparklines, and colorbars
  - Highcharts/Highstock visualizations
  - Form controls (select inputs, date ranges, sliders)
- **Reactive Filtering**: Dimensions automatically filter fact data
- **Derived Measures**: Custom aggregations (sum, count, median, mean, etc.)
- **Performance Optimization**: Built-in caching for large datasets
- **Pipe-Friendly**: Chain operations with magrittr's `%>%` operator

## Installation

```r
# Install from local source
devtools::install()

# Or using R CMD
R CMD INSTALL .
```

## Dependencies

Required packages:
- shiny (>= 1.0.4)
- shinyjs (>= 2.0)
- shinyjqui (>= 0.3.2)
- data.table (>= 1.10.4-3)
- DT (>= 0.4)
- highcharter (>= 0.5.0)
- RODBC (>= 1.3-13)

## Quick Start

### UI (ui.R)

```r
library(shiny)
library(dwhr)

fluidPage(
    dwhrInit(),  # Required: initialize dwhr
    getDimUI(starId = 's1', dim = 'per')
)
```

### Server (server.R)

```r
library(shiny)
library(magrittr)
library(dwhr)

# Load dimension data
per <- read.csv("data/ds_d_periode.txt",
                header = FALSE, sep = ";",
                col.names = c("maandId", "level1Label", "level2Label"))

# Create fact table
facts <- data.frame(
    maandId = per$maandId[sample(1:nrow(per), 10000, replace = TRUE)],
    num1 = runif(10000, 100, 200))

function(input, output, session) {

    authenticate(session)  # Required: authenticate session

    s1 <- new.star(starId = 's1',
                   session = session,
                   facts = facts) %>%
        addDimView(
            dim = 'per',
            name = 'Periode',
            data = per,
            levelNames = c('Alle perioden', 'Jaar', 'Maand'),
            initLevel = 2,
            initParent = '2017',
            useLevels = c(0, 1, 2)) %>%
        addMeasure(
            dim = 'per',
            factColumn = c('num1', 'num1', 'num1'),
            viewColumn = c('sum_col', 'count_col', 'median_col'),
            fun = c('sum', 'dcount', 'median'),
            as = c('Sum', 'Count', 'Median'),
            format = c('decimal2', 'decimal2', 'euro2')) %>%
        addPresentation(
            dim = 'per',
            type = 'dataTable',
            as = 'Table View',
            isDefault = TRUE,
            dataTableOpts = list(
                measures = list(
                    list(viewColumn = 'sum_col', colorBarColor1 = '#f7fcb9')
                ))) %>%
        renderDims(input, output)
}
```

## Core Concepts

### Star Schema

A **star object** contains:
- **Facts**: The central data table with measures and foreign keys
- **Dimensions**: Hierarchical views that slice and filter the facts
- **Measures**: Aggregated calculations on fact columns
- **Presentations**: How data is displayed (tables, charts, controls)

### Workflow

1. Create a star schema with `new.star()`
2. Add dimension views with `addDimView()`
3. Define measures with `addMeasure()`
4. Add visualizations with `addPresentation()`
5. Start rendering with `renderDims()`

### Dimension Levels

Dimensions support hierarchical levels (e.g., Year → Quarter → Month):
- Users can drill down/up through levels
- Selections at higher levels filter lower levels
- Configurable via `levelNames` and `useLevels`

## Examples

The package includes 15 example applications in `inst/examples/`:

- `01SimpleTable` - Basic dataTable presentation
- `02DerrivedMeasure` - Custom aggregation functions
- `03SortColumn` - Sortable columns
- `04DataTableStyle 1` - Conditional formatting
- `05DataTableStyle 2` - Advanced styling
- `06SelectableLevels` - Level selection controls
- `07MoreDimViews` - Multiple dimensions
- `08DataFromDb` - Database integration
- `09ColumnChart` - Highcharts visualization
- `10PresentationSplit` - Multiple presentations per dimension
- And more...

Run an example:
```r
shiny::runApp("inst/examples/01SimpleTable")
```

## Documentation

View function documentation in R:
```r
?new.star
?addDimView
?addMeasure
?addPresentation
```

## Development

```r
# Load package for development
devtools::load_all()

# Generate documentation
devtools::document()

# Build package
R CMD build .
```

## License

MIT License - see [LICENSE.md](LICENSE.md) for details

## Authors

Pieter Timmerman - Rest In Peaces old friend \
Howard Ching Chung

## Version

Current version: 1.6.2

