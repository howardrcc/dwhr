hcOptsBaseColumn <- list(
    dashboard = list(
        useUpdate = TRUE),
    chart = list(
        zoomType = 'xy',
        #animation = list(duration = 2000,easing = 'easeOutBounce'),
        animation = FALSE,
        # panning = TRUE,
        # panKey = 'shift',
        # borderColor = 'black',
        # borderRadius = 10,
        # borderWidth = 1,
        spacing = list(10,10,10,10),
        style = list(fontFamily= 'sans-serif')),
    title = list(
        text = '',
        useHTML = TRUE,
        align = 'left',
        y = 20,
        style = list(
            fontFamily = 'sans-serif',
            fontSize = '13px')),
    xAxis = list(
        # scrollbar = list(
        #     enabled = TRUE,
        #     showFull = FALSE
        # ),
        labels = list(
            autoRotation = NULL,
            useHTML = TRUE,
            style = list(
                fontSize = '9px', 
                fontWeight = 'bold', 
                whiteSpace = 'normal'))),
    yAxis = list(
        list(
            lineWidth = 1,
            lineColor = 'black',
            title = list( text = ''),
            plotLines = list(
                list(
                    color = 'black',
                    width = 1,
                    value = 0,
                    zIndex = 2)))),
    tooltip = list(
        enabled = TRUE,
        useHTML = TRUE),
    legend = list(
        enabled = TRUE,
        layout = 'vertical',
        itemStyle = list(
            fontSize = '9px')),
    plotOptions = list(
        column = list(
            grouping = FALSE,
            borderRadius = 5,
            borderWidth = 1,
            pointPadding = 0.05,
            groupPadding = 0.1),
        series = list(
            shadow = FALSE,
            animation = FALSE,
            #animation = list(duration = 2000,easing = 'easeOutBounce'),
            dataLabels = list(
                style = list(
                    fontSize = '9px',
                    fontweight = 'bold')))
    )
)