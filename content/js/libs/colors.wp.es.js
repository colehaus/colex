let colors = {
  hue: [
    '#b5bae6',
    '#d1abe6',
    '#e6abd0',
    '#e6bcb5',
    '#dee2b5',
    '#b5e2b5',
    '#b5e2df'
  ],
  chroma: [
    '#d7d6e6',
    '#b5bbe6',
    '#919fe6',
    '#6d84e6',
    '#4769e6',
    '#1d4de6'
  ],
  value: [
    '#b5bbe6',
    '#9090b8',
    '#6b6688',
    '#443c56',
    '#18121e'
  ]
}

colors.bodyBack = '#eee'
colors.bodyBackAlt = colors.value[0]
colors.inactive = colors.value[2]
colors.lightText = colors.value[3]
colors.bodyText = colors.value[4]

colors.noteBack = colors.hue[4]
colors.menu = colors.hue[5]

export default colors
