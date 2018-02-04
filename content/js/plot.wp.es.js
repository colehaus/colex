import jStat from 'jStat'

var yHistLims = function(yss) {
    var ys = [];
    for (var i = 0, l = yss.length; i < l; i++) {
        ys = ys.concat(twoDArrayCol(histogramCounts(yss[i]), 1));
    }
    return [
        jStat.min(ys) * 0.8,
        jStat.max(ys) * 1.2
    ];
};
var xHistLims = function(xss) {
    //flatten xs_
    var xs = [].concat.apply([], xss);
    return [
        jStat.min(xs),
        jStat.max(xs)
    ];
};
var sampleFunc = function (min, max, func) {
    var step = (max - min) / 200;
    var points = [];
    for (var i = min; i < max; i += step) {
        points.push([i, func(i)]);
    }
    points.push([max, func(max)]);
    return points;
};
var getHeight = function(x, hist) {
    var i = 0;
    while (!(x > hist[i][0] && x < hist[i+1][0])) {
        i++;
    }
    return hist[i][1];
};
var histogramCounts = function (xs) {
    var l = xs.length;
    var min = jStat.min(xs);
    var max = jStat.max(xs);
    var breaks = Math.ceil(Math.sqrt(l));
    var step = (max - min) / breaks;
    if (max - min === 0) { return [[xs[0], l]]; }
    var bins = [];
    for (var i = 0; i < breaks; i++) {
        bins.push([min + i * step + step / 2, 0]);
    }
    for (i = 0; i < l; i++) {
        var bin_i = xs[i] === max ? breaks - 1 : Math.floor((xs[i] - min) / step);
        bins[bin_i][1]++;
    }
    for (i = 0; i < breaks; i++) {
        bins[i][1] = bins[i][1] / l / step;
    }
    return bins;
};
var twoDArrayCol = function (array, col_i) {
    var col = [];
    for (var i = 0, l = array.length; i < l; i++) {
        col.push(array[i][col_i]);
    }
    return col;
};

export default {
    twoDArrayCol: twoDArrayCol,
    histogramCounts: histogramCounts,
    sampleFunc: sampleFunc,
    xHistLims: xHistLims,
    yHistLims: yHistLims,
    getHeight: getHeight
};
