var post_pred = function(chain) {
    var n_post = 10;

    var l = chain.length;
    var ps = [];
    for (var i = 0; i < n_post; i++) {
        ps.push(chain[rand_int(0, l)]);
    }
    var preds1 = [];
    var preds2 = [];
    for (i = 0; i < ps.length; i++) {
        preds1.push(sample_func(0, 1, function(x) {
            return jStat.beta.pdf(x, ps[i][0], ps[i][2]);
        }));
        preds2.push(sample_func(0, 1, function(x) {
            return jStat.beta.pdf(x, ps[i][1], ps[i][3]);
        }));
    }
    return [preds1, preds2];
};
var y_hist_lims = function(ds) {
    var ys = [];
    for (var i = 0; i < ds.length; i++) {
        ys = ys.concat(param_chain(histogram_counts(ds[i]), 1));
    }
    return [ jStat.min(ys) * 0.8
           , jStat.max(ys) * 1.2
           ];
};
var x_lims = function(ds) {
    var xs = [].concat.apply([], ds);
    return [ jStat.min(xs)
           , jStat.max(xs)
           ];
};
var rand_int = function (min, max) {
    return Math.floor(Math.random() * (max - min)) + min;
};
var sample_func = function (min, max, func) {
    var step = (max - min) / 100;
    var points = [];
    for (var i = min; i < max; i += step) {
        points.push([i, func(i)]);
    }
    points.push([max, func(max)]);
    return points;
};
var string_to_num_array = function (s) {
    s = s.replace(/[^-1234567890.]+$/, '').replace(/^[^-1234567890.]+/, '');
    return jStat.map(s.split(/[^-1234567890.]+/), function(x) {
        var f = parseFloat(x);
        if (isNaN(f)) {
            throw "NaN";
        }
        // Beta function is defined on (0, 1)
        // so gross hack ensues
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

var histogram_counts = function (x) {
    var l = x.length;
    var min = jStat.min(x);
    var max = jStat.max(x);
    var breaks = Math.ceil(Math.sqrt(l));
    var step = (max - min) / breaks;
    var bins = [];
    for (var i =0; i < breaks; i++) {
        bins.push([min + i * step + step / 2, 0]);
    }
    for (i = 0; i < l; i++) {
        var bin_i = Math.floor((x[i] - min) / step);
        if(bin_i > breaks - 1) {bin_i = breaks - 1;}
        if(bin_i < 0) {bin_i = 0;}
        bins[bin_i][1]++;
    }
    for (i = 0; i < breaks; i++) {
        bins[i][1] = bins[i][1] / l / step;
    }
    return bins;
};

var HDIofMCMC = function (x) {
    x = x.sort(function(a,b){return a-b;});
    var ci_nbr_of_points = Math.floor(x.length * 0.95);
    var min_width_ci = [jStat.min(x), jStat.max(x)]; // just initializing
    for(var i = 0; i < x.length - ci_nbr_of_points; i++) {
        var ci_width = x[i + ci_nbr_of_points] - x[i];
        if(ci_width < min_width_ci[1] - min_width_ci[0]) {
            min_width_ci = [x[i], x[i + ci_nbr_of_points]];
        }
    }
    return min_width_ci;
};

var perc_larger_and_smaller_than = function (comp, data) {
    var comps = jStat.map( data, function( x ) {
        if(x >= comp) {
            return 1;
        } else {
            return 0;
        }
    });
    var mean_larger = jStat.mean(comps);
    return [1 - mean_larger, mean_larger];
};

var param_chain = function (chain, param_i) {
    var param_data = [];
    for (var i = 0; i < chain.length; i++) {
        param_data.push(chain[i][param_i]);
    }
    return param_data;
};

// Constructor for the adaptive metropolis within Gibbs
var amwg = function (start_values, posterior, data_calc) {
    var that = this;
    var n_params = start_values.length;
    var batch_count = 0;
    var batch_size = 50;
    var chain = [];
    var curr_state = start_values;
    var log_sd = [];
    var acceptance_count = [];
    this.running_asynch = false;
    for (var i = 0; i < n_params; i++) {
        log_sd[i] = 0;
        acceptance_count[i] = 0;
    }

    var next_sample = function () {
        if(data_calc != null) {
            chain.push(curr_state.concat(data_calc(curr_state)));
        } else {
            chain.push(curr_state);
        }

        for(var param_i = 0; param_i < n_params; param_i++) {
            var param_prop = jStat.normal.sample(curr_state[param_i] , Math.exp( log_sd[param_i]));
            var prop = curr_state.slice();
            prop[param_i] = param_prop;
            var accept_prob = Math.exp(posterior(prop) - posterior(curr_state));
            if(accept_prob > Math.random()) {
                acceptance_count[param_i]++;
                curr_state = prop;
            } // else do nothing
        }

        if(chain.length % batch_size == 0) {
            batch_count++;
            for(param_i = 0; param_i < n_params; param_i++) {
                if(acceptance_count[param_i] / batch_size > 0.44) {
                    log_sd[param_i] += Math.min(0.01, 1/Math.sqrt(batch_count));
                } else if(acceptance_count[param_i] / batch_size < 0.44) {
                    log_sd[param_i] -= Math.min(0.01, 1/Math.sqrt(batch_count));
                }
                acceptance_count[param_i] = 0;
            }
        }
        return curr_state;
    };

    this.next_sample = next_sample;

    this.get_chain = function() {return chain;};
    this.get_curr_state = function() {return curr_state;};

    this.burn = function(n) {
        var temp_chain = chain.slice();
        this.n_samples(n);
        chain = temp_chain;
    };

    var n_samples = function (n) {
        for(var i = 0; i < n - 1; i++) {
            next_sample();
        }
        return next_sample();
    };

    this.n_samples = n_samples;


    var n_samples_asynch = function (n, nbr_of_samples) {
        that.samples_left = n;
        if(n > 0) {
            that.running_asynch = true;
            n_samples(nbr_of_samples);
            return setTimeout(function() {n_samples_asynch(n - nbr_of_samples, nbr_of_samples);}, 0);
        } else {
            that.running_asynch = false;
        }
    };

    this.n_samples_asynch = n_samples_asynch;
};

var make_BEST_posterior_func = function (y1, y2) {
    var data = [y1, y2];

    var posterior = function(params) {
        var alpha = [params[0], params[1]];
        var beta = [params[2], params[3]];
        var log_p = 0;
        for(var group = 0; group < 2; group++) {
            log_p += Math.log(jStat.exponential.pdf(alpha[group], 1));
            log_p += Math.log(jStat.exponential.pdf(beta[group], 1));
            for(var subj_i = 0; subj_i < data[group].length; subj_i++) {
                log_p += Math.log(jStat.beta.pdf(data[group][subj_i], alpha[group], beta[group]));
            }
        }
        return log_p;
    };
    return posterior;
};

var plot_mcmc_hist = function (jq, param_data, show_hdi, comp_value, lims, log, preds) {
    var plot_options = { font: { size: 8 }
                       , shadowSize: 0
                       , yaxis: { tickLength: 5 }
                       , xaxis: { tickLength: 5 }
                       , legend: { backgroundColor: 'rgba(0, 0, 0, 0)'
                                 , color: colors.bodyText }
                       , grid: { backgroundColor: colors.bodyBack
                               , color: colors.bodyText
                               }
                       , colors: colors.chroma
                       };

    if (typeof lims === 'object' && lims !== null) {
        plot_options.xaxis.min = lims[0][0];
        plot_options.xaxis.max = lims[0][1];
        plot_options.yaxis.min = lims[1][0];
        plot_options.yaxis.max = lims[1][1];
    }
    if (log === true) {
        var logTrans = { ticks: [0, 0.01, 0.1, 1, 10, 100]
                       , transform: function (v) { return Math.log(v + 0.001); }
                       , inverseTransform: function (v) { return Math.exp(v); }
                       , tickDecimals: 2
                       };
        $.extend(plot_options.xaxis, logTrans);
        $.extend(plot_options.yaxis, logTrans);
    }
    if (typeof lims === 'object' && lims !== null && log === true) {
        plot_options.xaxis.min = plot_options.xaxis.min / 2;
        plot_options.xaxis.max = plot_options.xaxis.max * 2;
        plot_options.yaxis.min = plot_options.yaxis.min / 2;
        plot_options.yaxis.max = plot_options.yaxis.max * 2;
    }
    var data = [];
    var l = (preds || {}).length || 0;
    if (l !== 0) {
        preds.forEach(function(pred) {
            data.push({ data: pred
                        // chroma[0] with alpha
                      , color: 'rgba(215, 214, 230, 0.5)'
                      });
        });
        data[0].label = 'Posterior Prediction';
    }

    if (param_data.length !== 0) {
        var bar_data = histogram_counts(param_data);
        var bar_width = bar_data[1][0] - bar_data[0][0];
        data.push({ data: bar_data
                  , bars: { show: true
                          , align: "center"
                          , barWidth: bar_width
                          }
                  , color: 1
                  });
    }

    if (typeof comp_value === 'number') {
        var comp_data = [[comp_value, 0], [comp_value, Infinity]];
        var comp_perc = perc_larger_and_smaller_than(comp_value, param_data);
        var comp_label = "" + (comp_perc[0] * 100).toPrecision(3) + "% < " +
                         comp_value + " < " + (comp_perc[1] * 100).toPrecision(3) + "%";
        data.push({ data: comp_data
                  , label: comp_label
                  , lines: { lineWidth: 2 }
                  , color: 2
                  });
    }
    if (show_hdi === true) {
        var hdi = HDIofMCMC(param_data);
        var hdi_data = [[hdi[0], 0], [hdi[1], 0]];
        var hdi_label = "95% HDI ("+ hdi[0].toPrecision(3) + ", " + hdi[1].toPrecision(3) +")";
        data.push({ data: hdi_data
                  , label: hdi_label
                  , lines: { lineWidth: 5 }
                  , color: 3
                  });
    }
    if (typeof param_data.length !== 'undefined' && param_data.length > 0) {
        var mean = jStat.mean(param_data);
        var mean_data = [[mean, 0]];
        var mean_label = "Mean: " + mean.toPrecision(3);
        data.push({ data: mean_data
                  , label: mean_label
                  , points: { show: true }
                  , color: 4
                  });
    }
    $.plot(jq, data, plot_options);
};
