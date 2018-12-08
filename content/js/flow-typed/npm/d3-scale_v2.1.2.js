declare class ContinuousScale {
  [[call]]: number => number;
  domain(d: [number, number]): ContinuousScale;
  range(r: [number, number]): ContinuousScale;
  invert(n: number): number;
}

declare module 'd3-scale' {
  declare export function scaleLinear(): ContinuousScale;
}
