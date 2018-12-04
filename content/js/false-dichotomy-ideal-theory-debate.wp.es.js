import 'jquery-flot'
import $ from 'jquery'

import colors from 'libs/colors'

$(() => {
  const plotOpts = {
    grid: { show: false },
    colors: [colors.value[3], colors.value[1]],
    legend: { show: false },
    lines: { show: true },
    yaxis: { min: -1 }
  }
  $.plot(
    $('#reliability'),
    [
      {
        data: [
          [1, 10],
          [2, 10],
          [3, 9],
          [4, 8],
          [5, 6],
          [6, 4],
          [7, 2],
          [8, 1],
          [9, 0],
          [10, 0]
        ]
      }
    ],
    plotOpts
  )
})
