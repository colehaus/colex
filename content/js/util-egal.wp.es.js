// @flow
/* eslint no-undef: "off" */

import * as d3 from 'd3'
import flyd from 'flyd'
import $ from 'jquery'
import * as numbers from 'numbers'
import * as vega from 'vega-lib'
import vegaEmbed from 'vega-embed'
import { create, env } from 'sanctuary'

import { documentReadyPromise } from 'libs/util'
// eslint-disable-next-line no-unused-vars
import type { Box } from 'custom-elements/draw'
import { setHandlers } from 'custom-elements/draw'

const S = create({checkTypes: false, env})

const q = 'quantitative'
const schema = 'https://vega.github.io/schema/vega-lite/v2.json'

const percentileDomain = [0, 100]
const defaultIncomeDistribution = p => Math.pow(p, 2) * 30
const minIncome = defaultIncomeDistribution(percentileDomain[0])
const maxIncome = defaultIncomeDistribution(percentileDomain[1])
const incomeDomain = [minIncome, maxIncome]
const defaultUtilityOfMarginalDollar = d => 1 - d / maxIncome
const maxUtility = defaultUtilityOfMarginalDollar(minIncome)

const values = ([minDomain, maxDomain]: [number, number], xKey: string, yKey: string, stepSize: ?number) => (fn: number => number, series: ?string) =>
  S.range(minDomain, stepSize == null ? maxDomain : maxDomain / stepSize)
    .map(x => stepSize == null ? x : x * stepSize)
    .map(x => {
      return (
        series == null
          ? { [xKey]: x, [yKey]: fn(x) }
          : { [xKey]: x, [yKey]: fn(x), series }
      )
    })

const mountIncomeDistribution = () => {
  const id = '#income-distribution'
  const chart = {
    ...chartSpecBoilerplate($(id).width()),
    config: { mark: { orient: 'vertical' } },
    mark: 'line',
    data: { values: values(percentileDomain, 'percentile', 'income')(defaultIncomeDistribution) },
    encoding: {
      x: { field: 'percentile', type: q },
      y: {
        axis: { title: 'income, USD' },
        field: 'income',
        type: q
      }
    }
  }
  const stream = flyd.stream(defaultIncomeDistribution)
  vegaEmbed(id, chart, chartOpts).then(addDrawingHandlers(id, stream, percentileDomain[1], maxIncome))
  return stream
}

const baselineBox = (id: string): Box => {
  const svgBound = $(id + ' svg').get(0).getBoundingClientRect()
  const xAxis = $($(id + ' g.mark-rule.role-axis-grid').get(1)).children('line').get(0).getBoundingClientRect()
  const yAxis = $($(id + ' g.mark-rule.role-axis-grid').get(0)).children('line').get(0).getBoundingClientRect()
  return {
    width: xAxis.width,
    left: xAxis.left - svgBound.left,
    top: yAxis.top - svgBound.top,
    height: yAxis.height
  }
}

const addDrawingHandlers = (id: string, stream: Stream<number => number>, domainMax: number, rangeMax: number) => {
  const bound = baselineBox(id)
  const xLogicalToPixel = d3.scaleLinear().domain([0, domainMax]).range([0, bound.width])
  const yPixelToLogical = d3.scaleLinear().domain([0, bound.height]).range([0, rangeMax])
  $(id).closest('.draw').each((_, el) => {
    if (el instanceof HTMLElement) {
      // Callback will be given `fn` in pixel terms. We want the `Stream` to be in logical (e.g. income) terms.
      setHandlers(
        el,
        bound,
        fn => stream(S.pipe([xLogicalToPixel, fn, yPixelToLogical]))
      )
    }
  })
}

const chartSpecBoilerplate = (elWidth: number) => {
  return {
    '$schema': schema,
    width: elWidth - 100,
    height: elWidth * 0.6
  }
}

const chartOpts = { renderer: 'svg', actions: false }

const mountMarginalUtility = () => {
  const id = '#marginal-utility'
  const chart = {
    ...chartSpecBoilerplate($(id).width()),
    config: { mark: { orient: 'vertical' } },
    data: { values: values(incomeDomain, 'income', 'marginalUtility', 1000)(defaultUtilityOfMarginalDollar) },
    mark: 'line',
    encoding: {
      x: {
        axis: { title: 'income, USD' },
        field: 'income',
        type: q
      },
      y: {
        axis: { title: 'marginal utility' },
        field: 'marginalUtility',
        type: q }
    }
  }
  const stream = flyd.stream(defaultUtilityOfMarginalDollar)
  vegaEmbed(id, chart, chartOpts).then(() => addDrawingHandlers(id, stream, maxIncome, maxUtility))
  return stream
}

const mountUtilityDistribution = () => {
  const id = '#utility-distribution'
  const chart = {
    ...chartSpecBoilerplate($(id).width()),
    config: {
      legend: { padding: 40 },
      rule: { strokeDash: [8, 8], opacity: 0.3 }
    },
    data: { name: 'table' },
    layer: [{
      mark: { type: 'line', orient: 'vertical' },
      encoding: {
        x: { field: 'percentile', type: q },
        y: { field: 'utility', type: q, axis: { minExtent: 40 } },
        color: { field: 'series', type: 'nominal' }
      }
    }, {
      mark: 'rule',
      encoding: {
        y: {
          field: 'utility',
          type: q,
          aggregate: 'mean'
        },
        size: { value: 5 },
        color: { field: 'series', type: 'nominal' }
      }
    }]
  }
  return vegaEmbed(id, chart, chartOpts).then(res => res.view)
}

const makeUtilityDistribution = (incomeDistributionFnStream: Stream<number => number>, marginalUtilityFnStream: Stream<number => number>) => {
  const utilityOfIncomeFn = (income: number) => numbers.calculus.Riemann(marginalUtilityFnStream(), minIncome, income, 200)
  const meanIncome =
    numbers.calculus.Riemann(incomeDistributionFnStream(), percentileDomain[0], percentileDomain[1], 200) /
    (percentileDomain[1] - percentileDomain[0])
  return [(p: number) => utilityOfIncomeFn(incomeDistributionFnStream()(p)), _ => utilityOfIncomeFn(meanIncome)]
}

documentReadyPromise.then(() => {
  const incomeDistributionFnStream = mountIncomeDistribution()
  const marginalUtilityFnStream = mountMarginalUtility()
  const utilityDistributionFnStream = flyd.combine(makeUtilityDistribution, [incomeDistributionFnStream, marginalUtilityFnStream])
  mountUtilityDistribution().then(view => {
    flyd.on(([inegal, egal]) => {
      view.change(
        'table',
        vega.changeset().insert(
          S.join([
            values(percentileDomain, 'percentile', 'utility')(inegal, 'inegalitarian'),
            values(percentileDomain, 'percentile', 'utility')(egal, 'egalitarian')
          ])
        ).remove(_ => true)
      ).run()
    }, utilityDistributionFnStream)
  })
})
