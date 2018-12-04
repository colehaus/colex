/* eslint no-undef: "off" */

import 'jquery-flot'
import $ from 'jquery'
import jStat from 'jStat'

import colors from 'libs/colors'
import plot from 'libs/plot'
import mcmc from 'libs/mcmc'

const plotOptions = {
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
}
const interval = [-1, 1]

const plotMcmcHist = (jq, paramData, conf, preds = []) => {
  let barData
  let plotOpts = $.extend(true, {}, plotOptions)
  const data = []
  if (conf.log === true) {
    const logTrans = {
      ticks: [0, 0.01, 0.1, 1, 10, 100],
      transform: v => Math.log(v + 0.001),
      inverseTransform: v => Math.exp(v),
      tickDecimals: 2
    }
    $.extend(plotOpts.xaxis, logTrans)
    $.extend(plotOpts.yaxis, logTrans)
  }

  const lims = ([min, max]) => {
    if (conf.log === true) {
      return { min: min / 2, max: max * 2 }
    } else {
      return { min, max }
    }
  }
  if (typeof conf.xlims === 'object' && conf.xlims !== null) {
    $.extend(plotOpts.xaxis, lims(conf.xlims))
  }
  if (typeof conf.ylims === 'object' && conf.ylims !== null) {
    $.extend(plotOpts.yaxis, lims(conf.ylims))
  }

  if (preds.length !== 0) {
    preds.forEach(pred => {
      data.push({
        data: pred,
        // chroma[0] with alpha
        color: 'rgba(165, 170, 204, 0.7)'
        // color: 'rgba(215, 214, 230, 0.5)'
      })
    })
    data[0].label = 'Posterior Prediction'
  }
  if (paramData.length !== 0) {
    barData = plot.histogramCounts(paramData)
    const width = barData.length > 1 ? barData[1][0] - barData[0][0] : 0.05
    data.push({
      data: barData,
      bars: {
        show: true,
        align: 'center',
        barWidth: width
      },
      color: 1
    })
  }

  const percSmallerLarger = (comp, data) => {
    const percLarger = jStat.mean(jStat.map(data, x => (x >= comp ? 1 : 0)))
    return [1 - percLarger, percLarger]
  }
  if (typeof conf.compValue === 'number') {
    const compPerc = percSmallerLarger(conf.compValue, paramData)
    data.push({
      data: [[conf.compValue, 0], [conf.compValue, Infinity]],
      label:
        '' +
        (compPerc[0] * 100).toPrecision(3) +
        '% < ' +
        conf.compValue +
        ' < ' +
        (compPerc[1] * 100).toPrecision(3) +
        '%',
      lines: { lineWidth: 2 },
      color: 2
    })
  }

  const boundedI = (c, x) => {
    x = x.sort((a, b) => a - b)
    const nbrPoints = Math.floor(x.length * c)
    const upper = [x[x.length - 1 - nbrPoints], interval[1]]
    const lower = [interval[0], x[nbrPoints]]
    return -upper[0] < lower[1] ? upper : lower
  }
  const HDI = (c, x) => {
    x = x.sort((a, b) => a - b)
    const nbrPoints = Math.floor(x.length * c)
    let [min, max] = [jStat.min(x), jStat.max(x)]
    let width
    for (let i = 0, l = x.length - nbrPoints; i < l; i++) {
      width = x[i + nbrPoints] - x[i]
      if (width < max - min) {
        ;[min, max] = [x[i], x[i + nbrPoints]]
      }
    }
    return [min, max]
  }
  if (typeof conf.di !== 'undefined' && conf.di !== null) {
    const intervAdd = (c, [min, max]) => {
      data.push({
        data: [
          [min, plot.getHeight(min === interval[0] ? max : min, barData)],
          [max, plot.getHeight(max === interval[1] ? min : max, barData)]
        ],
        label:
          c.toPrecision(2).slice(2) +
          '% ' +
          conf.di +
          ' (' +
          min.toPrecision(3) +
          ', ' +
          max.toPrecision(3) +
          ')',
        lines: { lineWidth: 5 },
        color: 3
      })
    }
    if (conf.di === 'BI') {
      ;[0.95, 0.99].forEach(c => {
        intervAdd(c, boundedI(c, paramData))
      })
    } else {
      ;[0.95].forEach(c => {
        intervAdd(c, HDI(c, paramData))
      })
    }
  }
  if (typeof paramData.length !== 'undefined' && paramData.length !== 0) {
    const mean = jStat.mean(paramData)
    data.push({
      data: [[mean, 0]],
      label: 'Mean: ' + mean.toPrecision(3),
      points: { show: true },
      color: 4
    })
  }
  return $.plot(jq, data, plotOpts)
}

const best = ds => {
  const showResult = chain => {
    inputGraph(mcmc.posterior_predictive_check(chain))
    progress(0.925)

    $('#stat-out').show()
    plotMcmcHist($('#mean > div'), plot.twoDArrayCol(chain, 2), {
      di: 'BI',
      comp: 0,
      xlims: [-1, 1]
    })
    progress(0.95)

    const a = plot.twoDArrayCol(chain, 0)
    const b = plot.twoDArrayCol(chain, 1)
    const xlims = plot.xHistLims([a, b])
    const ylims = plot.yHistLims([a, b])
    plotMcmcHist($('#alpha > div'), a, { di: 'HDI', xlims, ylims, log: true })
    progress(0.975)
    plotMcmcHist($('#beta > div'), b, { di: 'HDI', xlims, ylims, log: true })
    progress(1)
  }
  const progress = x => {
    $('progress').attr('value', x)
  }

  $('.analyze').html('Re-analyze')
  mcmc.run_BEST(ds, 20000, 20000, progress, showResult)
}

const freq = ds => {
  const l = ds.length
  const dof = l - 1
  if (l < 30) {
    alert(
      'We need at least 30 votes for this calculation. You need at least ' +
        (30 - l) +
        ' more.'
    )
    return
  }

  const mean = jStat.mean(ds)
  const stdev = jStat.stdev(ds)
  const cis = plot.sampleFunc(-1, 1, mu =>
    jStat.studentt.pdf((mean - mu) / stdev * Math.sqrt(l), dof)
  )
  const ci = conf => {
    const bound = jStat.studentt.inv(conf, dof) * stdev / Math.sqrt(l)
    const y = jStat.studentt.pdf(jStat.studentt.inv(conf, dof), dof)
    if (mean > 0) {
      return [y, mean - bound, Infinity]
    } else {
      return [y, -Infinity, mean + bound]
    }
  }
  const data = [
    {
      data: cis,
      lines: { show: true },
      color: 1
    },
    {
      data: [[mean, 0]],
      label: 'Mean: ' + mean.toPrecision(3),
      points: { show: true },
      color: 4
    }
  ]
  ;[0.95, 0.99].forEach(conf => {
    const [y, lb, ub] = ci(conf)
    data.push({
      data: [[lb, y], [ub, y]],
      label:
        conf.toPrecision(2).slice(2) +
        '% CI (' +
        lb.toPrecision(3) +
        ', ' +
        ub.toPrecision(3) +
        ')',
      lines: { lineWidth: 5 },
      color: 3
    })
  })
  $('#stat-out').show()
  $.plot($('#freq > div'), data, plotOptions)
  $('progress').attr('value', 1)
}

const getData = check => {
  const stringToNums = s => {
    s = s.replace(/[^-1234567890.]+$/, '').replace(/^[^-1234567890.]+/, '')
    return jStat.map(s.split(/[^-1234567890.]+/), x => {
      const f = parseFloat(x)
      if (isNaN(f)) {
        throw Error('NaN')
      }
      // If we let in 0 or 1, we end up taking log(0) later
      const eps = 0.001
      if (f <= 0) {
        return eps
      }
      if (f >= 1) {
        return 1 - eps
      }
      return f
    })
  }
  let y1, y2
  try {
    y1 = stringToNums($('#data1').val())
    y2 = stringToNums($('#data2').val())
  } catch (err) {
    alert(
      'ERROR: Data not supplied for both groups or not formatted correctly.'
    )
    return null
  }
  const dif = y2.length - y1.length
  if (dif !== 0 && check === true) {
    const err = " Since we're supposed to pair data, that's bad."
    if (dif > 0) {
      alert(
        'You have ' +
          dif +
          ' more data points for Proposal 2 than for Proposal 1.' +
          err
      )
      return null
    } else {
      alert(
        'You have ' +
          -dif +
          ' more data points for Proposal 1 than for Proposal 2.' +
          err
      )
      return null
    }
  }
  const ds = []
  const l = Math.min(y1.length, y2.length)
  for (let i = 0; i < l; i++) {
    ds.push(y2[i] - y1[i])
  }
  return [y1, y2, ds]
}

const inputGraph = pred => {
  const ys = getData()
  if (typeof ys === 'undefined') {
    return
  }
  const ylims = plot.yHistLims(ys)
  plotMcmcHist($('#preview1 > div'), ys[0], { xlims: [0, 1], ylims })
  plotMcmcHist($('#preview2 > div'), ys[1], { xlims: [0, 1], ylims })
  plotMcmcHist($('#diff > div'), ys[2], { xlims: [-1, 1] }, pred)
}

$(() => {
  $('.act > .analyze').click(() => {
    const d = getData(true)
    if (typeof d === 'undefined') {
      return
    }
    if (
      $('#best')
        .parent()
        .hasClass('open')
    ) {
      best(d[2])
    } else {
      freq(d[2])
    }
  })
  $('.preview').click(() => {
    inputGraph()
  })

  // For reasons unkown, setTimeout is required
  setTimeout(function () {
    inputGraph()
  })
  const plotOpts = {
    grid: { show: false },
    colors: [colors.value[3]]
  }
  $.plot(
    $('#non-norm'),
    [
      {
        bars: { show: true },
        data: [[-2, 4], [-1, 1], [0, 0], [1, 1], [2, 4]]
      }
    ],
    plotOpts
  )
  $.plot(
    $('#sym1'),
    [
      {
        bars: { show: true },
        data: [[-2, 1], [-1, 0], [0, 0], [1, 2], [2, 0]]
      }
    ],
    plotOpts
  )
  $.plot(
    $('#sym2'),
    [
      {
        bars: { show: true },
        data: [[-2, 0], [-1, 0], [0, 3], [1, 0], [2, 0]]
      }
    ],
    plotOpts
  )
})
