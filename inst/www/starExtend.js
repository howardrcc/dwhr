/**
 * Easing function from https://github.com/danro/easing-js/blob/master/easing.js
 */
Math.easeOutBounce = function (pos) {
    if ((pos) < (1 / 2.75)) {
        return (7.5625 * pos * pos);
    }
    if (pos < (2 / 2.75)) {
        return (7.5625 * (pos -= (1.5 / 2.75)) * pos + 0.75);
    }
    if (pos < (2.5 / 2.75)) {
        return (7.5625 * (pos -= (2.25 / 2.75)) * pos + 0.9375);
    }
    return (7.5625 * (pos -= (2.625 / 2.75)) * pos + 0.984375);
};


ttPointFormatter = function(point) {
    
    if ( typeof point.ttXtraData == "undefined"  || point.ttXtraData === null || point.ttXtraData === '') 
        return null;
 
    return point.ttXtraData;

};

toggleAccordion = function(e,id) {
    var panel = $('#'.concat(id))[0];  
  
    if (panel !== null) {
        if (panel.style.maxHeight === "0px"){
            $(e).find('.glyphicon').removeClass('glyphicon-chevron-right');
            $(e).find('.glyphicon').addClass('glyphicon-chevron-down');
            panel.style.maxHeight = null;
        } else {
            $(e).find('.glyphicon').removeClass('glyphicon-chevron-down');
            $(e).find('.glyphicon').addClass('glyphicon-chevron-right');
            panel.style.maxHeight = "0px";
        }     
    }
};

rowGroupEvent = function(gdim,rowGroup) {

    Shiny.onInputChange(gdim.concat('RowGroupEvent'),{
        r: Math.random(),
        rowGroup: rowGroup});    
    
};

clearPlotbands = function(chart,id,color) {

    var len = chart.xAxis[0].plotLinesAndBands.length;
    var newPb = [];

    for (i = 0; i < len; i++) {
        var pb = chart.xAxis[0].plotLinesAndBands[i].options;

        if (pb.id == id) {
            pb.color = color;
        } else {
            pb.color = 'rgba(0,0,0,0)';
        }

        newPb.push(pb);
    }


    for (i = 0; i < newPb.length; i++) {
        chart.xAxis[0].removePlotBand(i);
        chart.xAxis[0].addPlotBand(newPb[i]);
    }

};

countSelected = function(chart,color) {
    
    var len = chart.xAxis[0].plotLinesAndBands.length;
    var cnt = 0;

    for (i = 0; i < len; i++) {
        var pb = chart.xAxis[0].plotLinesAndBands[i].options;

        if (pb.color == color) {
            cnt = cnt + 1;
        } 
    }
    
    return(cnt);
};

clearSelection = function(serie,id,color) {

  var len = serie.data.length;
  if (serie.type == 'pie') {
      for (i = 0; i< len; i++) {
          if(i == id) {
              serie.data[i].slice(true);
          } else {
              serie.data[i].slice(false);
          }
      }
   } else {
     if (serie.type == 'treemap') {
        for (i = 0; i< len; i++) {
            if( i == id) {
                serie.data[i].update({color: color});
            } else {
                serie.data[i].update({color: serie.data[i].orgColor});
            }
        }
     }
   }
};

pointSingleSelect = function(dim,point,event,selectable,unSelectable,drillable,color) {
    var container = '#'.concat(dim,'DimChart');
    var eventName = dim.concat('HighchartClick');
    var chart = $(container).highcharts();
    var drill = false;
    var select = false;
    var unSelect = false;

    if (!event.ctrlKey) {

        if(chart.xAxis[0].plotLinesAndBands.length > 0) {

            var pbColor = chart.xAxis[0].plotLinesAndBands[point.x].options.color;

            cnt = countSelected(chart,color);
            
            if (pbColor == color && unSelectable && cnt == 1) {
                unSelect = true;
                clearPlotbands(chart,-1,color);
            }

            if ((cnt > 1 || pbColor != color) && selectable) {
                clearPlotbands(chart,point.x,color);
                select = true;
            }
        }

        if (point.series.type == 'treemap') {

            var pColor = point.color;
            if(pColor == color && unSelectable) {
                unSelect = true;
                clearSelection(point.series,-1,color);
            }

            if(pColor != color && selectable) {
                clearSelection(point.series,point.x,color);
                select = true;
            }

        }

        if (point.series.type == 'pie') {

            var pSliced = point.sliced;
            if(pSliced && unSelectable) {
                unSelect = true;
                clearSelection(point.series,-1);
            }

            if(!pSliced && selectable) {
                clearSelection(point.series,point.x);
                select = true;
            }
        }


    } else {

        drill = drillable;
    }

    Shiny.onInputChange(eventName,{
        r: Math.random(),
        data: point.x,
        drill: drill,
        select: select,
        unSelect: unSelect,
        id: point.id});
};


plotBandSingleSelect = function(dim,plotBand,event,selectable,unSelectable,drillable,color) {
    var container = '#'.concat(dim,'DimChart');
    var eventName = dim.concat('HighchartPbClick');
    var data = plotBand.options.from;
    var chart = $(container).highcharts();
    var drill = false;
    var select = false;
    var unSelect = false;

    if (!event.ctrlKey) {

        var pBColor = plotBand.options.color;
        
        cnt = countSelected(chart,color);
            
        if (pBColor == color && unSelectable && cnt == 1) {
            unSelect = true;
            clearPlotbands(chart,-1,color);
        }

        if ((cnt > 1 || pBColor != color) && selectable) {
            select = true;
            clearPlotbands(chart,plotBand.options.id,color);
        }

    } else {

        drill = drillable;
    }

    Shiny.onInputChange(eventName,{
        r: Math.random(),
        data: data,
        drill: drill,
        select: select,
        unSelect: unSelect});


};


shinyjs.dumpToConsole = function(params) {
    for (i = 0; i < params.trace.length; i++) {
        console.log(params.trace[i]);
    }
};

shinyjs.updateTitle = function(params) {
    var container = '#' + params.dim + 'DimChart';
    var chart = $(container).highcharts();

    chart.setTitle(params.titleOpts,null,params.redraw);

};

shinyjs.updateSeriesData = function(params) {
    var container = '#'.concat(params.dim,'DimChart');
    var chart = $(container).highcharts();

    for (i = 0; i < params.seriesData.length; i++) {
        chart.series[i].setData(params.seriesData[i],params.redraw,false,false);
    }


};

shinyjs.updateSeriesOpts = function(params) {
    var container = '#'.concat(params.dim,'DimChart');
    var chart = $(container).highcharts();

    for (i = 0; i < params.seriesOpts.length; i++) {
        chart.series[i].update(params.seriesOpts[i],params.redraw);
    }
};

shinyjs.updateXAxis = function(params) {
    var container = '#' + params.dim + 'DimChart';
    var chart = $(container).highcharts();

    chart.xAxis[params.axis].update(params.xAxisOpts,params.redraw);

};

shinyjs.updateYAxis = function(params) {
    var container = '#' + params.dim + 'DimChart';
    var chart = $(container).highcharts();
    chart.yAxis[params.axis].update(params.yAxisOpts,params.redraw);

};

shinyjs.updateXPlotBands = function(params) {

  //  $.unblockUI();
    var container = '#'.concat(params.dim,'DimChart');
    var chart = $(container).highcharts();
    var len = chart.xAxis[0].plotLinesAndBands.length;

    for ( i = 0; i < len; i++) {
        chart.xAxis[0].removePlotBand(i);
    }

    for ( i = 0; i < params.plotBands.length; i++) {
        var pb = params.plotBands[i];
        pb.events.click = eval('(' + pb.events.click + ')');
        chart.xAxis[0].addPlotBand(pb);
    }


};

shinyjs.redraw = function(params) {
    container = '#'.concat(params.dim,'DimChart');
    chart = $(container).highcharts();
    chart.reflow();
    chart.redraw(params.animate);

};


shinyjs.blockUI = function(params) {
    $("#app-content").block({ message: null, overlayCSS: { backgroundColor: params.backgroundColor, opacity:params.opacity }, timeout:params.timeout});
    $('.tooltip').tooltip('destroy');
};

shinyjs.tooltip = function(params) {
    
    var trigger;
    
    if (params === null) {
        trigger = 'hover';
    } else {
        trigger = params.trigger;
    }
    $('.tooltip').tooltip('destroy');
    $('[data-toggle="tooltip"]').tooltip({trigger: trigger, delay: {show: 0}});
};

shinyjs.popover = function(params) {
    
    if (params === null) {
        trigger = 'hover';
    } else {
        trigger = params.trigger;
    }
    
    $('[data-toggle="popover"]').popover({trigger: trigger, html: true, delay: {show: 200}});
};

shinyjs.hideDim = function(params) {
    var container = '#'.concat(params.dim,'Dimensie');
    $(container).fadeTo(0,0).addClass("hide-db");
};

shinyjs.showDim = function(params) {
    var container = '#'.concat(params.dim,'Dimensie');
    $(container).removeClass("hide-db");
    $(container).fadeTo(500,1);
};


shinyjs.searchDT = function(params) {

    var datatable = $('#' + params.id).dataTable().api();
    datatable.draw();
};

shinyjs.updateDT = function(params) {

    var datatable = $('#' + params.id).dataTable().api();

    var el = $( '<div></div>' );
    el.html(params.container);

    if ($('thead', el).length > 0) {
        $('thead', el).find('th').each(function(index) { $(datatable.column(index).header()).html($(this).text());});
    }

    if ($('tfoot', el).length > 0) {
        $('tfoot', el).find('th').each(function(index) { $(datatable.column(index).footer()).html($(this).text());});
    }

    datatable.clear();
    datatable.rows.add(params.tab);
    if (params.selected !== null) {
        $('#' + params.dim + 'Dim').data('datatable').shinyMethods.selectCells([[params.selected,1]]);
    }
    datatable.columns.adjust().draw();
    datatable.page(params.page).draw(false);
    $('.tooltip').tooltip('destroy');
    $('[data-toggle="tooltip"]').tooltip({trigger : 'hover'});
};

batchStateFinished = false; 