// @flow

export type Coordinate = {x: number, y: number}

const distance = (coord1: Coordinate, coord2: Coordinate): number =>
  Math.hypot(coord1.x - coord2.x, coord1.y - coord2.y)

const circleFromChord = (startRads: number, stopRads: number, startCoord: Coordinate, stopCoord: Coordinate): {radius: number, center: Coordinate} => {
  const radius = distance(startCoord, stopCoord) /
    Math.sin(Math.abs(stopRads - startRads) / 2) / 2
  const center = pointOnCircle(startCoord, radius, startRads + Math.PI)
  return {radius, center}
}

const pointOnCircle = ({x, y}: Coordinate, radius: number, angle: number): Coordinate => ({
  x: x + Math.cos(angle) * radius,
  y: y + Math.sin(angle) * radius
})

export { distance, circleFromChord, pointOnCircle }
