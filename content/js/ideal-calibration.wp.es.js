import 'jquery-flot'
import $ from 'jquery'

import colors from 'libs/colors'

$(() => {
  const plotOpts = {
    grid: { show: false },
    colors: [colors.value[3], colors.value[1]],
    legend: {show: false},
    bars: {show: true}
  }
  $.plot($('#in-upper-spark'),
    [ { label: 'current', data: [[5, 1]] },
      { label: 'alternatives', data: [[-3, 1], [-2, 0], [-1, 4], [0, 5], [1, 4], [2, 0], [3, 0], [4, 0], [6, 0]] }
    ], plotOpts)
  $.plot($('#in-middle-spark'),
    [ { label: 'current', data: [[1, 4]] },
      { label: 'alternatives', data: [[-3, 1], [-2, 0], [-1, 4], [0, 5], [2, 0]] }
    ], plotOpts)
  $.plot($('#mediocre-spark'),
    [ { label: 'current', data: [[1, 2]] },
      { label: 'alternatives', data: [[-3, 1], [-2, 0], [-1, 2], [0, 2], [2, 0]] }
    ], plotOpts)
  $.plot($('#ideal-spark'),
    [ { label: 'current', data: [[1, 2]] },
      { label: 'alternatives', data: [[-3, 1], [-2, 0], [-1, 2], [0, 2], [2, 0], [3, 0], [4, 0], [5, 0], [6, 0], [7, 0], [8, 0], [9, 0], [10, 0], [11, 0], [12, 0], [13, 0], [14, 0], [15, 1]] }
    ], plotOpts)
})
