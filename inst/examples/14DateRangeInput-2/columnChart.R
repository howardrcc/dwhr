#
# highcharts optie-definitie voor een columnChart
# zie http://api.highcharts.com/highcharts
#
columnChartOpts = list(
    dashboard = list(
        useUpdate = TRUE),
    chart = list(
        zoomType = 'xy',
        height = 240,
        #animation = list(duration = 2000,easing = 'easeOutBounce'),
        animation = FALSE,
        panning = TRUE,
        panKey = 'shift',
        # borderColor = 'black',
        # borderRadius = 10,
        # borderWidth = 1,
        spacing = list(0,0,0,0),
        style = list(fontFamily= 'sans-serif')),
    title = list(
        text = 'Test123',
        align = 'left',
        style = list(
            fontFamily = 'sans-serif',
            fontSize = '13px')),
    xAxis = list(
        labels = list(
            autoRotation = NULL,
            style = list(
                fontSize = '8px', 
                fontWeight = 'normal', 
                whiteSpace = 'normal'))),
    yAxis = list(
        lineWidth = 1,
        lineColor = 'black',
        plotLines = list(
            list(
                color = 'black',
                width = 1,
                value = 0,
                zIndex = 10))),
    tooltip = list(
        enabled = TRUE),
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
                    fontweight = 'bold')))))
    