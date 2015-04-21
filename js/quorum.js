'use strict';

var _slicedToArray = function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i['return']) _i['return'](); } finally { if (_d) throw _e; } } return _arr; } else { throw new TypeError('Invalid attempt to destructure non-iterable instance'); } };

(function ($, colors, plot, mcmc, jStat) {

  var plotOptions = {
    font: { size: 8 },
    shadowSize: 0,
    yaxis: { tickLength: 5 },
    xaxis: { tickLength: 5 },
    legend: {
      backgroundColor: 'rgba(0, 0, 0, 0)',
      color: colors.bodyText
    },
    grid: {
      backgroundColor: null,
      color: colors.bodyText
    },
    colors: colors.chroma.slice(1)
  };
  var interval = [-1, 1];

  var plotMcmcHist = function plotMcmcHist(jq, paramData, conf) {
    var preds = arguments[3] === undefined ? [] : arguments[3];

    var barData = undefined;
    var plotOpts = $.extend(true, {}, plotOptions);
    var data = [];
    if (conf.log === true) {
      var logTrans = {
        ticks: [0, 0.01, 0.1, 1, 10, 100],
        transform: function transform(v) {
          return Math.log(v + 0.001);
        },
        inverseTransform: function inverseTransform(v) {
          return Math.exp(v);
        },
        tickDecimals: 2
      };
      $.extend(plotOpts.xaxis, logTrans);
      $.extend(plotOpts.yaxis, logTrans);
    }

    var lims = function lims(_ref) {
      var _ref2 = _slicedToArray(_ref, 2);

      var min = _ref2[0];
      var max = _ref2[1];

      if (conf.log === true) {
        return { min: min / 2, max: max * 2 };
      } else {
        return { min: min, max: max };
      }
    };
    if (typeof conf.xlims === 'object' && conf.xlims !== null) {
      $.extend(plotOpts.xaxis, lims(conf.xlims));
    }
    if (typeof conf.ylims === 'object' && conf.ylims !== null) {
      $.extend(plotOpts.yaxis, lims(conf.ylims));
    }

    if (preds.length !== 0) {
      preds.forEach(function (pred) {
        data.push({
          data: pred,
          // chroma[0] with alpha
          color: 'rgba(165, 170, 204, 0.7)'
          // color: 'rgba(215, 214, 230, 0.5)'
        });
      });
      data[0].label = 'Posterior Prediction';
    }
    if (paramData.length !== 0) {
      barData = plot.histogramCounts(paramData);
      var width = barData.length > 1 ? barData[1][0] - barData[0][0] : 0.05;
      data.push({
        data: barData,
        bars: {
          show: true,
          align: 'center',
          barWidth: width
        },
        color: 1
      });
    }

    var percSmallerLarger = function percSmallerLarger(comp, data) {
      var percLarger = jStat.mean(jStat.map(data, function (x) {
        return x >= comp ? 1 : 0;
      }));
      return [1 - percLarger, percLarger];
    };
    if (typeof conf.compValue === 'number') {
      var compPerc = percSmallerLarger(conf.compValue, paramData);
      data.push({
        data: [[conf.compValue, 0], [conf.compValue, Infinity]],
        label: '' + (compPerc[0] * 100).toPrecision(3) + '% < ' + conf.compValue + ' < ' + (compPerc[1] * 100).toPrecision(3) + '%',
        lines: { lineWidth: 2 },
        color: 2
      });
    }

    var boundedI = function boundedI(c, x) {
      x = x.sort(function (a, b) {
        return a - b;
      });
      var nbrPoints = Math.floor(x.length * c);
      var upper = [x[x.length - 1 - nbrPoints], interval[1]];
      var lower = [interval[0], x[nbrPoints]];
      return -upper[0] < lower[1] ? upper : lower;
    };
    var HDI = function HDI(c, x) {
      x = x.sort(function (a, b) {
        return a - b;
      });
      var nbrPoints = Math.floor(x.length * c);
      var min = jStat.min(x);
      var max = jStat.max(x);

      var width = undefined;
      for (var i = 0, l = x.length - nbrPoints; i < l; i++) {
        width = x[i + nbrPoints] - x[i];
        if (width < max - min) {
          min = x[i];
          max = x[i + nbrPoints];
        }
      }
      return [min, max];
    };
    if (typeof conf.di !== 'undefined' && conf.di !== null) {
      (function () {
        var intervAdd = function intervAdd(c, _ref3) {
          var _ref32 = _slicedToArray(_ref3, 2);

          var min = _ref32[0];
          var max = _ref32[1];

          data.push({
            data: [[min, plot.getHeight(min === interval[0] ? max : min, barData)], [max, plot.getHeight(max === interval[1] ? min : max, barData)]],
            label: c.toPrecision(2).slice(2) + '% ' + conf.di + ' (' + min.toPrecision(3) + ', ' + max.toPrecision(3) + ')',
            lines: { lineWidth: 5 },
            color: 3
          });
        };
        if (conf.di === 'BI') {
          [0.95, 0.99].forEach(function (c) {
            intervAdd(c, boundedI(c, paramData));
          });
        } else {
          [0.95].forEach(function (c) {
            intervAdd(c, HDI(c, paramData));
          });
        }
      })();
    }
    if (typeof paramData.length !== 'undefined' && paramData.length !== 0) {
      var mean = jStat.mean(paramData);
      data.push({ data: [[mean, 0]],
        label: 'Mean: ' + mean.toPrecision(3),
        points: { show: true },
        color: 4
      });
    }
    return $.plot(jq, data, plotOpts);
  };

  var best = function best(ds) {
    var showResult = function showResult(chain) {
      inputGraph(mcmc.posterior_predictive_check(chain));
      progress(0.925);

      $('#stat-out').show();
      plotMcmcHist($('#mean > div'), plot.twoDArrayCol(chain, 2), { di: 'BI', comp: 0, xlims: [-1, 1] });
      progress(0.95);

      var a = plot.twoDArrayCol(chain, 0);
      var b = plot.twoDArrayCol(chain, 1);
      var xlims = plot.xHistLims([a, b]);
      var ylims = plot.yHistLims([a, b]);
      plotMcmcHist($('#alpha > div'), a, { di: 'HDI', xlims: xlims, ylims: ylims, log: true });
      progress(0.975);
      plotMcmcHist($('#beta > div'), b, { di: 'HDI', xlims: xlims, ylims: ylims, log: true });
      progress(1);
    };
    var progress = function progress(x) {
      $('progress').attr('value', x);
    };

    $('.analyze').html('Re-analyze');
    mcmc.run_BEST(ds, 20000, 20000, progress, showResult);
  };

  var freq = function freq(ds) {
    var l = ds.length;
    var dof = l - 1;
    if (l < 30) {
      alert('We need at least 30 votes for this calculation. You need at least ' + (30 - l) + ' more.');
      return;
    }

    var mean = jStat.mean(ds);
    var stdev = jStat.stdev(ds);
    var cis = plot.sampleFunc(-1, 1, function (mu) {
      return jStat.studentt.pdf((mean - mu) / stdev * Math.sqrt(l), dof);
    });
    var ci = function ci(conf) {
      var bound = jStat.studentt.inv(conf, dof) * stdev / Math.sqrt(l);
      var y = jStat.studentt.pdf(jStat.studentt.inv(conf, dof), dof);
      if (mean > 0) {
        return [y, mean - bound, Infinity];
      } else {
        return [y, -Infinity, mean + bound];
      }
    };
    var data = [{
      data: cis,
      lines: { show: true },
      color: 1
    }, {
      data: [[mean, 0]],
      label: 'Mean: ' + mean.toPrecision(3),
      points: { show: true },
      color: 4
    }];
    [0.95, 0.99].forEach(function (conf) {
      var _ci = ci(conf);

      var _ci2 = _slicedToArray(_ci, 3);

      var y = _ci2[0];
      var lb = _ci2[1];
      var ub = _ci2[2];

      data.push({
        data: [[lb, y], [ub, y]],
        label: conf.toPrecision(2).slice(2) + '% CI (' + lb.toPrecision(3) + ', ' + ub.toPrecision(3) + ')',
        lines: { lineWidth: 5 },
        color: 3
      });
    });
    $('#stat-out').show();
    $.plot($('#freq > div'), data, plotOptions);
    $('progress').attr('value', 1);
  };

  var getData = function getData(check) {
    var stringToNums = function stringToNums(s) {
      s = s.replace(/[^-1234567890.]+$/, '').replace(/^[^-1234567890.]+/, '');
      return jStat.map(s.split(/[^-1234567890.]+/), function (x) {
        var f = parseFloat(x);
        if (isNaN(f)) {
          throw 'NaN';
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
    var y1 = undefined,
        y2 = undefined;
    try {
      y1 = stringToNums($('#data1').val());
      y2 = stringToNums($('#data2').val());
    } catch (err) {
      alert('ERROR: Data not supplied for both groups or not formatted correctly.');
      return null;
    }
    var dif = y2.length - y1.length;
    if (dif !== 0 && check === true) {
      var err = ' Since we\'re supposed to pair data, that\'s bad.';
      if (dif > 0) {
        alert('You have ' + dif + ' more data points for Proposal 2 than for Proposal 1.' + err);
        return null;
      } else {
        alert('You have ' + -dif + ' more data points for Proposal 1 than for Proposal 2.' + err);
        return null;
      }
    }
    var ds = [];
    var l = Math.min(y1.length, y2.length);
    for (var i = 0; i < l; i++) {
      ds.push(y2[i] - y1[i]);
    }
    return [y1, y2, ds];
  };

  var inputGraph = function inputGraph(pred) {
    var ys = getData();
    if (typeof ys === 'undefined') {
      return;
    }
    var ylims = plot.yHistLims(ys);
    plotMcmcHist($('#preview1 > div'), ys[0], { xlims: [0, 1], ylims: ylims });
    plotMcmcHist($('#preview2 > div'), ys[1], { xlims: [0, 1], ylims: ylims });
    plotMcmcHist($('#diff > div'), ys[2], { xlims: [-1, 1] }, pred);
  };

  $(function () {

    $('.act > .analyze').click(function () {
      var d = getData(true);
      if (typeof d === 'undefined') {
        return;
      }
      if ($('#best').closest('li').hasClass('open')) {
        best(d[2]);
      } else {
        freq(d[2]);
      }
    });
    $('.preview').click(function () {
      inputGraph();
    });

    // For reasons unkown, setTimeout is required
    setTimeout(function () {
      inputGraph();
    });
    var plotOpts = {
      grid: { show: false },
      colors: [colors.value[3]]
    };
    $.plot($('#non-norm'), [{
      bars: { show: true },
      data: [[-2, 4], [-1, 1], [0, 0], [1, 1], [2, 4]]
    }], plotOpts);
    $.plot($('#sym1'), [{
      bars: { show: true },
      data: [[-2, 1], [-1, 0], [0, 0], [1, 2], [2, 0]]
    }], plotOpts);
    $.plot($('#sym2'), [{ bars: { show: true },
      data: [[-2, 0], [-1, 0], [0, 3], [1, 0], [2, 0]]
    }], plotOpts);
  });
})($, colors, plot, mcmc, jStat);

