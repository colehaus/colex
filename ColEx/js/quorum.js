var burn_timeout_id;
var sample_timeout_id;
var plot_timeout_id;

function run_BEST(y1, y2) {
    var n_samples = 20000;
    var n_burnin = 20000;
    var plot_count = 0;
    var update_mod = 32;

    var data_calc = function(p) {
        return [jStat.beta.mean(p[0], p[2]) - jStat.beta.mean(p[1], p[3])];
    };
    var burn_asynch = function(n) {
        $('progress').attr('value', 1 - 500 * n / n_burnin );
        sampler.burn(500);

        if(n > 0) {
            burn_timeout_id = setTimeout(function() {burn_asynch(n - 1);}, 0);
        } else {
            sample_timeout_id = sampler.n_samples_asynch(n_samples, 50);
            plot_count = 0;
            $('.output').css('display', 'table-row');
            plot_asynch();
        }
    };
    var plot_asynch = function() {
        var plot_start_time = new Date();
        if (plot_count % update_mod === 0) { show_result(); }
        plot_count++;
        var plot_time = (new Date()) - plot_start_time;

        if(sampler.running_asynch) {
            $('progress').attr('value', 1 - sampler.samples_left / n_samples);
            plot_timeout_id = setTimeout(plot_asynch, plot_time);
        } else {
            $('progress').attr('value', 1);
            show_result();
            var chain = sampler.get_chain();
            input_graph.apply(this, post_pred(chain));
        }
    };
    var show_result = function() {
        var chain = sampler.get_chain();
        plot_mcmc_hist($("#diff"), param_chain(chain, 4), true, 0);

        var m1 = param_chain(chain, 0);
        var m2 = param_chain(chain, 1);
        var s1 = param_chain(chain, 2);
        var s2 = param_chain(chain, 3);
        var xlims = x_lims([m1, m2, s1, s2]);
        var ylims = y_hist_lims([m1, m2, s1, s2]);
        plot_mcmc_hist($("#mean1"), m1, true, null, [xlims, ylims], true);
        plot_mcmc_hist($("#mean2"), m2, true, null, [xlims, ylims], true);
        plot_mcmc_hist($("#sd1"), s1, true, null,  [xlims, ylims], true);
        plot_mcmc_hist($("#sd2"), s2, true, null, [xlims, ylims], true);
    };
    window.clearTimeout(burn_timeout_id);
    window.clearTimeout(sample_timeout_id);
    window.clearTimeout(plot_timeout_id);
    $("#best button").html('Restart');

    var sampler = new amwg( [1, 1, 1, 1]
                          , make_BEST_posterior_func(y1, y2)
                          , data_calc);
    burn_asynch(Math.ceil(n_burnin /  500));
};

var empty_plots = function() {
    plot_mcmc_hist($("#diff"), []);
    plot_mcmc_hist($("#mean1"), []);
    plot_mcmc_hist($("#mean2"), []);
    plot_mcmc_hist($("#sd1"), []);
    plot_mcmc_hist($("#sd2"), []);
};

var get_data = function() {
    try {
        var y1 = string_to_num_array($("#data1").val());
        var y2 = string_to_num_array($("#data2").val());
        return [y1, y2];
    } catch(err) {
        console.log("ERROR: Data not supplied for both groups or not formatted correctly.\n");
        return undefined;
    }
};

var input_graph = function(pred1_, pred2_) {
    var pred1 = pred1_ || [];
    var pred2 = pred2_ || [];
    var d = get_data();
    var ylims = y_hist_lims(d);
    plot_mcmc_hist($('#preview1'), d[0], false, null, [[0, 1], ylims], false, pred1);
    plot_mcmc_hist($('#preview2'), d[1], false, null, [[0, 1], ylims], false, pred2);
};

$(function() {
    $("#best button").click(function() {
        var d = get_data();
        if (typeof d === 'object') { run_BEST(d[0], d[1]); }
    });

    input_graph();
    $("#best textarea").on('input propertychange', input_graph);
});
