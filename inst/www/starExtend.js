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

addSpark = function(selector, sparkOpts, xaxisValue) {
    
    var e = $(selector);
    
    for (i = 0; i < e.length; i++) {
        
        var arr = JSON.parse("[" + e[i].textContent + "]");

        if ((typeof sparkOpts.addXaxis[0] !== 'undefined') && sparkOpts.addXaxis[0]) {
            var minVal = Math.min.apply(null,arr);
        
            if (minVal > 0) {
                minVal = 0;
            }
        
            var maxVal = Math.max.apply(null,arr);
        
            if (maxVal < 0) {
                maxVal = 0;
            }
        
            sparkOpts.chartRangeMin = minVal;
            sparkOpts.chartRangeMax = maxVal;
            sparkOpts.composite = true;
            
            if (xaxisValue === null)
                xaxisValue = 0;
        
            var axis = Array.apply(null, new Array(arr.length)).map(Number.prototype.valueOf,xaxisValue);
            
            axisOpts = {
                type: 'line',
                chartRangeMin: minVal,
                chartRangeMax: maxVal,
                lineColor: 'black',
                fillColor:'rgba(0,0,0,0)',
                lineWidth: 1,
                spotColor: false,
                minSpotColor: false,
                maxSpotColor: false,
                tooltipFormat: ''};
            
            if (typeof sparkOpts.defaultPixelsPerValue !== 'undefined')
                axisOpts.defaultPixelsPerValue = sparkOpts.defaultPixelsPerValue;
            
            $(e[i]).sparkline(axis, axisOpts);
            
        }
        
        $(e[i]).sparkline(arr,sparkOpts);
        
    }
};


ttPointFormatter = function(point) {
    
    if ( typeof point.ttXtraData == "undefined"  || point.ttXtraData === null || point.ttXtraData === '') 
        return null;
 
    return point.ttXtraData;

};

toggleAccordion = function(e,id,accordionId) {
    var panel = $('#'.concat(id))[0];  
    var acc = $('#'.concat(accordionId))[0];  
  
    if (panel !== null) {
        
        if (e === null) {
            e = acc;
        }
        
        if (e !== null) {
            if (panel.style.maxHeight === "0px"){
                $(e).find('.glyphicon').removeClass('glyphicon-chevron-right');
                $(e).find('.glyphicon').addClass('glyphicon-chevron-down');
                panel.style.maxHeight = '9999px';
                Shiny.onInputChange(id,{toggleState: 1});
            } else {
                $(e).find('.glyphicon').removeClass('glyphicon-chevron-down');
                $(e).find('.glyphicon').addClass('glyphicon-chevron-right');
                panel.style.maxHeight = "0px";
                Shiny.onInputChange(id,{toggleState: 0});
            }     
        }
    }
};

setAccordion = function(state,id,accordionId) {
    var panel = $('#'.concat(id))[0];  
    var e = $('#'.concat(accordionId))[0];  
  
    if (panel !== null && e !== null) {
        if (state) {
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
 
clearPlotbands = function(chart) {

    var len = chart.xAxis[0].plotLinesAndBands.length;
    var newPb = [];

    for (i = 0; i < len; i++) {
        var pb = chart.xAxis[0].plotLinesAndBands[i].options;
        pb.color = 'rgba(0,0,0,0)';
        newPb.push(pb);
    }

    for (i = 0; i < newPb.length; i++) {
        chart.xAxis[0].removePlotBand(i);
        chart.xAxis[0].addPlotBand(newPb[i]);
    }
    
    for (j = 0; j < chart.series.length; j++) {
       serie = chart.series[j];

       if (serie.type == 'pie') {
          len = serie.data.length;
          for (i = 0; i< len; i++) {
               serie.data[i].slice(false);
          }
       } 
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

getSelected = function(series,color) {

  var len = series.data.length;
  var selected  = [];
  chart = series.chart;
  var i;
  
  if (series.chart.xAxis[0].plotLinesAndBands.length > 0) {
      for (i = 0; i < len; i++) {
          var pb = series.chart.xAxis[0].plotLinesAndBands[i].options;
          if (pb.color == color) {
              selected.push({id: series.data[i].id});
          } 
      }
  } else {
      if (series.type == 'pie') {
          for (i = 0; i < len; i++) {
              if (series.data[i].sliced) {
                  selected.push({id: series.data[i].id});
              }
          }
      } else {
          if (series.type == 'treemap' || series.type == 'packedbubble' || series.type == 'heatmap') {
              for (i = 0; i < len; i++) {
                  if (series.data[i].color == color) {
                      selected.push({id: series.data[i].id});
                  }
              }
          } 
      }
  }
  return(selected);
};

togglePlotband = function(chart,id,color) {
   
    if (chart.xAxis[0].plotLinesAndBands.length > 0) {
        var len = chart.xAxis[0].plotLinesAndBands.length;
        var newPb = [];
        for (i = 0; i < len; i++) {
            var pb = chart.xAxis[0].plotLinesAndBands[i].options;
            if (pb.id == id) {  
                if (pb.color == color) {
                    pb.color = 'rgba(0,0,0,0)';  
                } else {
                    pb.color = color;  
                }
            }
            newPb.push(pb);
        }
        for (i = 0; i < newPb.length; i++) {
            chart.xAxis[0].removePlotBand(i);
            chart.xAxis[0].addPlotBand(newPb[i]);
        }
    }
    
    for (j = 0; j < chart.series.length; j++) {
       serie = chart.series[j];

       if (serie.type == 'pie') {
           serie.data[id].slice(!serie.data[id].sliced);
       } 
    }
    
};

toggleSelection = function(point,color) {
    var series = point.series;
    chart = series.chart;
  
    if (chart.xAxis[0].plotLinesAndBands.length > 0) {
        var len = chart.xAxis[0].plotLinesAndBands.length;
        var newPb = [];
        for (i = 0; i < len; i++) {
            var pb = chart.xAxis[0].plotLinesAndBands[i].options;
            if (pb.id == point.index) {  
                if (pb.color == color) {
                    pb.color = 'rgba(0,0,0,0)';  
                } else {
                    pb.color = color;  
                }
            }
            newPb.push(pb);
        }
        for (i = 0; i < newPb.length; i++) {
            chart.xAxis[0].removePlotBand(i);
            chart.xAxis[0].addPlotBand(newPb[i]);
        }
    } 
    
    for (j = 0; j < chart.series.length; j++) {
       serie = chart.series[j];

        if (serie.type == 'pie') {
            serie.data[point.index].slice(!serie.data[point.index].sliced);
        } else {
            if (serie.type == 'treemap' || serie.type == 'packedbubble' || serie.type == 'heatmap') {
                if (serie.data[point.index].color == color) {
                    serie.data[point.index].update({color: serie.data[point.index].orgColor});
                } else {
                    serie.data[point.index].update({color: color});
                }
            }
        }
    }
};

clearSelection = function(chart) {

    if (chart.xAxis[0].plotLinesAndBands.length > 0) {
        var len = chart.xAxis[0].plotLinesAndBands.length;
        var newPb = [];

        for (i = 0; i < len; i++) {
            var pb = chart.xAxis[0].plotLinesAndBands[i].options;
            pb.color = 'rgba(0,0,0,0)';
            newPb.push(pb);
        }

        for (i = 0; i < newPb.length; i++) {
            chart.xAxis[0].removePlotBand(i);
            chart.xAxis[0].addPlotBand(newPb[i]);
        }
    }
    
    for (j = 0; j < chart.series.length; j++) {
       serie = chart.series[j];
            
       var len = serie.data.length;
       if (serie.type == 'pie') {
           for (i = 0; i< len; i++) {
               serie.data[i].slice(false);
           }
       } else {
           if (serie.type == 'treemap' || serie.type == 'packedbubble' || serie.type == 'heatmap') {
               for (i = 0; i< len; i++) {
                   if (serie.data[i].color != serie.data[i].orgColor) {
                       serie.data[i].update({color: serie.data[i].orgColor});
                   }
              }
          }
      }
   }
};

pointSingleSelect = function(dim,point,event,selectable,unSelectable,drillable,color,multi) {
    var container = '#'.concat(dim,'DimChart');
    var eventName = dim.concat('HighchartClick');
    var chart = point.series.chart;
    var drill = false;
    var select = false;
    var unSelect = false;

    var curId = point.id;
        
    if (point.series.type == 'areaspline') {
        curId = point.series.options.id; 
    }
        
    if (!event.ctrlKey) {
      
        var curSel = getSelected(point.series,color);
                
        if (curSel.length == 1) {
            if (curSel[0].id == curId && unSelectable) {
                unSelect = true;
            }
            if (curSel[0].id != curId) {
                select = true;
                if (!multi) {
                    clearSelection(point.series.chart);
                }
            }
        } else {
            if (selectable)
                select = true;
        }
        if (select || unSelect)
            toggleSelection(point,color); 
    } else {
        drill = drillable;
    }
    
    Shiny.onInputChange(eventName,{
        r: Math.random(),
        drill: drill,
        select: select,
        unSelect: unSelect,
        id: curId});
};


plotBandSingleSelect = function(dim,plotBand,event,selectable,unSelectable,drillable,color,multi) {
    var container = '#'.concat(dim,'DimChart');
    var eventName = dim.concat('HighchartPbClick');
    var data = plotBand.options.from;    
    var chart = $(container).highcharts();
    var id = plotBand.options.id;
    var drill = false;
    var select = false;
    var unSelect = false;
    
    if (!event.ctrlKey) {

        var pBColor = plotBand.options.color;
        
        cnt = countSelected(chart,color);
            
        if (cnt == 1) {
            if (pBColor == color && unSelectable) {
                unSelect = true;
            }
            if (pBColor != color) {
                select = true;
                if (!multi) {
                    clearPlotbands(chart);
                }
            }
        } else {
            if (selectable)
                select = true;
        }
        
        if (select || unSelect)
            togglePlotband(chart,id,color); 

    } else {

        drill = drillable;
    }

    Shiny.onInputChange(eventName,{
        r: Math.random(),
        data: data,
        drill: drill,
        select: select,
        unSelect: unSelect,
        id : id}); 
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
};

shinyjs.tooltip = function(params) {
    $('[data-toggle="tooltip"]').uitooltip({track: true, show: false, hide: false});
};

shinyjs.popover = function(params) {
    
    trigger = 'hover';
    
    if (params !== null) {
        if (params.trigger !== null) {
            trigger = params.trigger;
        }
    }
    
    if (trigger == 'manual') {
        $('[data-toggle="popover"]').popover({
            trigger: "manual",
            html: true,
            animation: false,
            container: "body"
        })  
        .on("mouseenter", function() {
            var _this = this;
            $(this).popover("show");
            $(".popover").on("mouseleave", function() {
                $(_this).popover('hide');
            });
        }).on("mouseleave", function() {
            var _this = this;
            setTimeout(function() {
                if (!$(".popover:hover").length) {
                    $(_this).popover("hide");
                }
            }, 200);
        });
    } else {
        $('[data-toggle="popover"]').popover({trigger: trigger, html: true, delay: {show: 200}});
    }
};

shinyjs.hideDim = function(params) {
    var container = '#'.concat(params.dim,'Dimensie');
    $(container).fadeTo(0,0).addClass("hide-db");
};

shinyjs.showDim = function(params) {
    var container = '#'.concat(params.dim,'Dimensie');
    $(container).removeClass("hide-db");
    $(container).fadeTo(100,1);
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
 };

batchStateFinished = false; 

shinyjs.init = function() {
   
  
    $(document).on('shiny:idle', function(event) {
        $("#app-content").unblock()
        
    });
    
    $(window).resize(function(){
        if (typeof Shiny.onInputChange != 'undefined') {
            Shiny.onInputChange('windowHeight',{height: window.innerHeight});
            Shiny.onInputChange('windowWidth',{width: window.innerWidth});
        }
    });
    
    $.blockUI.defaults.onUnblock = resetUnblock;
    
    $.widget.bridge('uitooltip', $.ui.tooltip);
}

shinyjs.hcSetHeight = function(params) {
    container = '#'.concat(params.gdim,'DimChart');
    body = '#'.concat(params.gdim,'DimBody');
    chart = $(container).highcharts();
    
    $(body).css("height",params.height);
    $(container).css("height","100%");
    
    if (typeof chart != 'undefined' && params.source != 'chartChange') {
        chart.setSize(null,params.height,false);
    }
    
}

resetUnblock = function(e,opts) {
    // ivm conflict tussen blockui en introjs
    $(e).css("position","")  
}

