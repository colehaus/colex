/* @flow */
$(function() {
'use strict';

var plotOptions = { font: { size: 8 }
                  , shadowSize: 0
                  , yaxis: { tickLength: 5 }
                  , xaxis: { tickLength: 5 }
                  , legend: { backgroundColor: 'rgba(0, 0, 0, 0)'
                            , color: colors.bodyText }
                  , grid: { backgroundColor: null
                          , color: colors.bodyText
                          }
                  , colors: colors.chroma.slice(1)
                  };
var interval = [-1, 1];

var plotMcmcHist = function (jq, paramData, conf, preds) {
    var plotOpts = $.extend(true, {}, plotOptions);
    var data = [];
    if (conf.log === true) {
        var logTrans = { ticks: [0, 0.01, 0.1, 1, 10, 100]
                       , transform: function (v) { return Math.log(v + 0.001); }
                       , inverseTransform: function (v) { return Math.exp(v); }
                       , tickDecimals: 2
                       };
        $.extend(plotOpts.xaxis, logTrans);
        $.extend(plotOpts.yaxis, logTrans);
    }

    var lims = function(l) {
        if (conf.log === true) {
            return { min: l[0] / 2
                   , max: l[1] * 2
                   };
        } else {
            return { min: l[0]
                   , max: l[1]
                   };
        }
    };
    if (typeof conf.xlims === 'object' && conf.xlims !== null) {
        $.extend(plotOpts.xaxis, lims(conf.xlims));
    }
    if (typeof conf.ylims === 'object' && conf.ylims !== null) {
        $.extend(plotOpts.yaxis, lims(conf.ylims));
    }

    var l = (preds || []).length || 0;
    if (l !== 0) {
        preds.forEach(function(pred) {
            data.push({ data: pred
                        // chroma[0] with alpha
                       , color: 'rgba(165, 170, 204, 0.7)'
                      // , color: 'rgba(215, 214, 230, 0.5)'
                      });
        });
        data[0].label = 'Posterior Prediction';
    }
    if (paramData.length !== 0) {
        var barData = plot.histogramCounts(paramData);
        var width = barData.length > 1 ? barData[1][0] - barData[0][0] : 0.05;
        data.push({ data: barData
                  , bars: { show: true
                          , align: "center"
                          , barWidth: width
                          }
                  , color: 1
                  });
    }

    var percSmallerLarger = function (comp, data) {
        var percLarger = jStat.mean(jStat.map(data, function(x) {
            return x >= comp ? 1 : 0;
        }));
        return [1 - percLarger, percLarger];
    };
    if (typeof conf.compValue === 'number') {
        var compPerc = percSmallerLarger(conf.compValue, paramData);
        data.push({ data: [[conf.compValue, 0], [conf.compValue, Infinity]]
                  , label: "" + (compPerc[0] * 100).toPrecision(3) + "% < " +
                           conf.compValue + " < " + (compPerc[1] * 100).toPrecision(3) + "%"
                  , lines: { lineWidth: 2 }
                  , color: 2
                  });
    }

    var boundedI = function (c, x) {
        var l = x.length;
        x = x.sort(function(a, b){ return a - b; });
        var nbrPoints = Math.floor(x.length * c);
        var upper = [x[l - 1 - nbrPoints], interval[1]];
        var lower = [interval[0], x[nbrPoints]];
        return -upper[0] < lower[1] ? upper : lower;
    };
    var HDI = function (c, x) {
        x = x.sort(function(a, b){ return a - b; });
        var nbrPoints = Math.floor(x.length * c);
        var minWidth = [jStat.min(x), jStat.max(x)];
        for(var i = 0, l = x.length - nbrPoints; i < l; i++) {
            var width = x[i + nbrPoints] - x[i];
            if (width < minWidth[1] - minWidth[0]) {
                minWidth = [x[i], x[i + nbrPoints]];
            }
        }
        return minWidth;
    };
    if (typeof conf.di !== 'undefined' && conf.di !== null) {
        var intervAdd = function(c, di) {
            data.push({ data: [ [di[0], plot.getHeight(di[0] === interval[0] ? di[1] : di[0], barData)]
                              , [di[1], plot.getHeight(di[1] === interval[1] ? di[0] : di[1], barData)]
                              ]
                      , label: c.toPrecision(2).slice(2) + '% ' + conf.di + ' ('+
                               di[0].toPrecision(3) + ', ' + di[1].toPrecision(3) +')'
                      , lines: { lineWidth: 5 }
                      , color: 3
                      });
        };
        if (conf.di === 'BI') {
            [0.95, 0.99].forEach(function(c) {
                var di = boundedI(c, paramData);
                intervAdd(c, di);
            });
        } else {
            [0.95].forEach(function(c) {
                var di = HDI(c, paramData);
                intervAdd(c, di);
            });
        }
    }
    if (typeof paramData.length !== 'undefined' && paramData.length !== 0) {
        var mean = jStat.mean(paramData);
        data.push({ data: [[mean, 0]]
                  , label: 'Mean: ' + mean.toPrecision(3)
                  , points: { show: true }
                  , color: 4
                  });
    }
    return $.plot(jq, data, plotOpts);
};

var best = function(ds) {
    var showResult = function(chain) {
        inputGraph(mcmc.posterior_predictive_check(chain));
        progress(0.925);

        $('#stat-out').show();
        plotMcmcHist( $("#mean > div")
                    , plot.twoDArrayCol(chain, 2)
                    , {di: 'BI', comp: 0, xlims: [-1, 1]}
                    );
        progress(0.95);

        var a = plot.twoDArrayCol(chain, 0);
        var b = plot.twoDArrayCol(chain, 1);
        var xlims = plot.xLims([a, b]);
        var ylims = plot.yHistLims([a, b]);
        plotMcmcHist($("#alpha > div"), a, { di: 'HDI', xlims: xlims, ylims: ylims, log: true });
        progress(0.975);
        plotMcmcHist($("#beta > div"), b, { di: 'HDI', xlims: xlims, ylims: ylims, log: true });
        progress(1);
    };
    var progress = function(x) {
        $('progress').attr('value', x);
    };

    $(".analyze").html('Re-analyze');
    mcmc.run_BEST(ds, 20000, 20000, progress, showResult);
};

var freq = function(ds) {
    var l = ds.length;
    var dof = l - 1;
    if (l < 30) {
        alert('We need at least 30 votes for this calculation. You need at least ' + (30 - l) + ' more.');
        return undefined;
    }

    var mean = jStat.mean(ds);
    var stdev = jStat.stdev(ds);
    var cis = plot.sampleFunc(-1, 1, function(mu) {
        return jStat.studentt.pdf((mean - mu) / stdev * Math.sqrt(l), dof);
    });
    var ci = function(conf) {
        var bound = jStat.studentt.inv(conf, dof) * stdev / Math.sqrt(l);
        var y = jStat.studentt.pdf(jStat.studentt.inv(conf, dof), dof);
        if (mean > 0) {
            return [y, mean - bound, Infinity];
        } else {
            return [y, -Infinity, mean + bound];
        }
    };
    var data = [{data: cis
               , lines: { show: true }
               , color: 1
               },{
                 data: [[mean, 0]]
               , label: "Mean: " + mean.toPrecision(3)
               , points: { show: true }
               , color: 4
               }];
    [0.95, 0.99].forEach(function(conf) {
        var bs = ci(conf);
        var y = bs[0];
        var lb = bs[1];
        var ub = bs[2];
        data.push({ data: [[lb, y], [ub, y]]
                  , label: conf.toPrecision(2).slice(2) + "% CI ("+
                           lb.toPrecision(3) + ", " + ub.toPrecision(3) +")"
                  , lines: { lineWidth: 5 }
                  , color: 3
                  });
    });
    $('#stat-out').show();
    $.plot($('#freq > div'), data, plotOptions);
    $('progress').attr('value', 1);
};

var getData = function(check) {
    var stringToNums = function (s) {
        s = s.replace(/[^-1234567890.]+$/, '').replace(/^[^-1234567890.]+/, '');
        return jStat.map(s.split(/[^-1234567890.]+/), function(x) {
            var f = parseFloat(x);
            if (isNaN(f)) {
                throw "NaN";
            }
            // If we let in 0 or 1, we end up taking log(0) later
            var eps = 0.001;
            if (f <= 0) {
                return eps;
            }
            if (f >= 1) {
                return 1 - eps;
            }
            return f;
        });
    };

    try {
        var y1 = stringToNums($("#data1").val());
        var y2 = stringToNums($("#data2").val());
    } catch(err) {
        alert("ERROR: Data not supplied for both groups or not formatted correctly.");
        return;
    }
    var dif = y2.length - y1.length;
    if (dif !== 0 && check === true) {
        var err = " Since we're supposed to pair data, that's bad.";
        if (dif > 0) {
            alert("You have " + dif + " more data points for Proposal 2 than for Proposal 1." + err);
            return;
        } else {
            alert("You have " + -dif + " more data points for Proposal 1 than for Proposal 2." + err);
            return;
        }
    }
    var ds = [];
    var l = Math.min(y1.length, y2.length);
    for (var i = 0; i < l; i++) {
        ds.push(y2[i] - y1[i]);
    }
    return [y1, y2, ds];
};

var inputGraph = function(pred) {
    var ys = getData();
    if (typeof ys === 'undefined') { return; }
    var ylims = plot.yHistLims(ys);
    plotMcmcHist($('#preview1 > div'), ys[0], { xlims: [0, 1], ylims: ylims });
    plotMcmcHist($('#preview2 > div'), ys[1], { xlims: [0, 1], ylims: ylims });
    plotMcmcHist($('#diff > div'), ys[2], { xlims: [-1, 1] }, pred);
};

$(".act > .analyze").click(function() {
    var d = getData(true);
    if (typeof d === 'undefined') { return; }
    if ($('#best').closest('li').hasClass('open')) {
        best(d[2]);
    } else {
        freq(d[2]);
    }
});
inputGraph();
$(".preview").click(inputGraph);

(function() {
    var plotOpts = { grid: { show: false }
                   , colors: [colors.value[3]]
                   };

    $.plot($('#non-norm'),
           [{ bars: { show: true }
           , data: [[-2, 4], [-1, 1], [0, 0], [1, 1], [2, 4]]
           }], plotOpts);
    $.plot($('#sym1'),
           [{ bars: { show: true }
           , data: [[-2, 1], [-1, 0], [0, 0], [1, 2], [2, 0]]
           }], plotOpts);
    $.plot($('#sym2'),
           [{ bars: { show: true }
           , data: [[-2, 0], [-1, 0], [0, 3], [1, 0], [2, 0]]
           }], plotOpts);
})();

});
