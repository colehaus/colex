import jStat from 'jStat'
import plot from 'libs/plot'

var interval = [-1, 1];
var burn_timeout_id;
var sample_timeout_id;
var plot_timeout_id;

var posterior_predictive_check = function(chain) {
    var rand_int = function (min, max) {
        return Math.floor(Math.random() * (max - min)) + min;
    };

    var n_post = 10;
    var l = chain.length;
    var preds = [];
    for (var i = 0; i < n_post; i++) {
        var params = chain[rand_int(0, l)];
        preds.push(plot.sampleFunc(-1, 1, function(x) {
            return generalized_beta(x, params[0], params[1], interval);
        }));
    }
    return preds;
};
var generalized_beta = function (x, a, b, interval) {
    var min = interval[0];
    var max = interval[1];
    var normed = (x - min)/(max - min);
    return jStat.beta.pdf(normed, a, b);
};
var run_BEST = function (ys, n_samples_, n_burnin, progress_cb, final_cb) {
    // Adaptive metropolis within Gibbs
    var amwg = function (start_values, posterior) {
        var n_params = start_values.length;
        var batch_count = 0;
        var batch_size = 50;
        var chain = [];
        var curr_state = start_values;
        var log_sd = [];
        var acceptance_count = [];
        for (var i = 0; i < n_params; i++) {
            log_sd[i] = 0;
            acceptance_count[i] = 0;
        }

        var burn = function(n) {
                   var temp_chain = chain.slice();
                   n_samples(n);
                   chain = temp_chain;
        };
        var derived_params = function(p) {
            // mean scaled to our interval
            return interval[0] + jStat.beta.mean(p[0], p[1]) * (interval[1] - interval[0]);
        };
        var next_sample = function () {
            chain.push(curr_state.concat(derived_params(curr_state)));

            for(var param_i = 0; param_i < n_params; param_i++) {
                var param_prop = jStat.normal.sample(curr_state[param_i] , Math.exp(log_sd[param_i]));
                var prop = curr_state.slice();
                prop[param_i] = param_prop;
                var accept_prob = Math.exp(posterior(prop) - posterior(curr_state));
                if(accept_prob > Math.random()) {
                    acceptance_count[param_i]++;
                    curr_state = prop;
                }
            }

            if(chain.length % batch_size === 0) {
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
        var n_samples = function (n) {
            for(var i = 0; i < n - 1; i++) {
                next_sample();
            }
            return next_sample();
        };
        var running_asynch = false;
        var samples_left = n_samples_;
        var n_samples_asynch = function (n, nbr_of_samples) {
            samples_left = n;
            if(n > 0) {
                running_asynch = true;
                n_samples(nbr_of_samples);
                return setTimeout(function() {n_samples_asynch(n - nbr_of_samples, nbr_of_samples);}, 0);
            } else {
                running_asynch = false;
                return null;
            }
        };
        return {
            chain: function() {return chain;},
            running_asynch: function() {return running_asynch;},
            burn: burn,
            n_samples: n_samples, samples_left: function () {return samples_left;},
            n_samples_asynch: n_samples_asynch
        };
    };

    var make_BEST_posterior_func = function (data) {
        return function(params) {
            var alpha = params[0];
            var beta = params[1];
            var log_p = 0;
            log_p += Math.log(jStat.exponential.pdf(alpha, 1));
            log_p += Math.log(jStat.exponential.pdf(beta, 1));
            for(var subj_i = 0, l = data.length; subj_i < l; subj_i++) {
                log_p += Math.log(generalized_beta(data[subj_i], alpha, beta, interval));
            }
            return log_p;
        };
    };

    var burn_asynch = function(n) {
        sampler.burn(500);
        if(n > 0) {
            var fracDone = 1 - 500 * n / n_burnin;
            progress_cb(0.45 * fracDone);
            burn_timeout_id = setTimeout(function() {burn_asynch(n - 1);}, 0);
        } else {
            sample_timeout_id = sampler.n_samples_asynch(n_samples_, 50);
            plot_asynch();
        }
    };
    var plot_asynch = function() {
        if(sampler.running_asynch()) {
            var fracDone =  1 - sampler.samples_left() / n_samples_;
            progress_cb(0.45 + fracDone * 0.45);
            plot_timeout_id = setTimeout(plot_asynch, 0);
        } else {
            final_cb(sampler.chain());
        }
    };

    clearTimeout(burn_timeout_id);
    clearTimeout(sample_timeout_id);
    clearTimeout(plot_timeout_id);

    var sampler = amwg([1, 1], make_BEST_posterior_func(ys)) ;
    burn_asynch(Math.ceil(n_burnin /  500));
};

export default {
        posterior_predictive_check: posterior_predictive_check,
        run_BEST: run_BEST
    };
