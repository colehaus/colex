// @flow
/* eslint no-undef: "off" */

import * as d3 from 'd3'
import $ from 'jquery'
import vegaEmbed from 'vega-embed'
import { create, env } from 'sanctuary'

import { documentReadyPromise, claimNotNull } from 'libs/util'

const S = create({checkTypes: false, env})

const schema = 'https://vega.github.io/schema/vega-lite/v2.json'

type RawRecord = {
  D1: string,
  D2: string,
  D3: string,
  D4: string,
  D5: string,
  D6: string,
  D7: string,
  D8: string,
  D9: string,
  D10: string,
  Mean_usd: string,
  Year: string
}

type ChartRecord = {
  Country: string,
  Decile: number,
  Income: number
}

const deciles: Array<number> = S.range(1, 11)

// Vega expects one record per data point
const unroll: Array<RawRecord> => Array<ChartRecord> =
  S.chain(r_ =>
    S.map(i => {
      // Flow being silly
      const r: Object = r_
      return { Country: r.Country, Decile: i, Income: Number.parseFloat(r.Mean_usd) * Number.parseFloat(r['D' + i.toString()]) / 10 }
    })(deciles)
  )

// Can't have data at 0 due to log scale
const fixLog: ChartRecord => ChartRecord = r => r.Income === 0 ? S.insert('Income')(1)(r) : r

// Latest year per country with all required attributes defined
const filterData: Array<RawRecord> => Array<RawRecord> =
  S.pipe([
    S.filter(r =>
      deciles.every(i => r['D' + i.toString()] !== '')
      && r.Mean_usd !== ''),
    S.groupBy(S.on(S.equals)(r => r.Country)),
    S.map(
      S.pipe([
        S.sortBy(r => Number.parseInt(r.Year, 10)),
        S.last,
        S.maybeToNullable,
        claimNotNull // `groupBy` always produces non-empty lists
      ])
    ),
  ])

const prepData: Array<RawRecord> => Array<ChartRecord> =
  S.pipe([filterData, unroll, S.map(fixLog)])

const chartSpecBoilerplate = (elWidth: number) => {
  return {
    '$schema': schema,
    width: elWidth - 100,
    height: elWidth * 0.6
  }
}

const mountIncomeByCountry = (data: Array<RawRecord>) => {
  const id = '#income-by-country'
  const chart = {
    ...chartSpecBoilerplate($(id).width()),
    config: { line: { opacity: 0.2 } },
    data: { values: prepData(data) },
    mark: 'line',
    encoding: {
      x: { field: 'Decile', type: 'ordinal' },
      y: {
        field: 'Income',
        axis: { title: 'Income, USD' },
        type: 'quantitative',
        scale: { type: 'log' }
      },
      color: { field: 'Country', type: 'nominal', legend: null }
    }
  }
  const chartOpts = {
    renderer: 'svg',
    actions: false
  }
  const tooltipOpts = {
    showAllFields: false,
    fields: [ { 'field': 'Country' } ],
  }
  return vegaEmbed(id, chart, chartOpts).then(result => {
    // vegaTooltip.vegaLite(result.view, chart, tooltipOpts)
  })
}

const dataPromise: Promise<Array<RawRecord>> = d3.csv('../../data/income-distribution-across-countries/WIID3.4_19JAN2017New.csv')

Promise.all([
  dataPromise,
  documentReadyPromise
]).then(([data, _]) => {
  mountIncomeByCountry(data)
})
