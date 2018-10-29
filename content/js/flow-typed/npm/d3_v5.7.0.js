export type ForceNode = {
  x: number,
  y: number,
  vx: number,
  vy: number,
  fx: ?number,
  fy: ?number
}
export type ForceLink = {
  target: { x: number, y: number },
  source: { x: number, y: number }
}
declare class SelectWithData<D> extends Select<SelectWithData<D>> {
  enter(): SelectWithData<D>;
  text(text: string): SelectWithData<D>;
  text(fn: D => string): SelectWithData<D>;
  attr(attr: string, value: string): SelectWithData<D>;
  attr(attr: string, value: number): SelectWithData<D>;
  attr(attr: string, value: D => string): SelectWithData<D>;
  attr(attr: string, value: (D, number) => string): SelectWithData<D>;
  on(event: string, fn: D => void): SelectWithData<D>;
  data<E>(fn: D => Array<E>): SelectWithData<E>;
  call(fn: SelectWithData<D> => void): SelectWithData<D>;
}
declare type EventType = 'mousedown' | 'mouseup' | 'mousemove'
declare class Select<Self> {
  select(selector: string): Self;
  selectAll(selector: string): Self;
  classed(klass: string, add: boolean): Self;
  append(tag: string): Self;
  id(id: string): Self;
  style(name: string, value: string): Self;
  on(eventType: EventType, handler: Function): Self;
  node(): Element;
  remove(): Self;
}
declare class SelectWithoutData extends Select<SelectWithoutData> {
  attr(attr: string, value: string): SelectWithoutData;
  attr(attr: string, value: number): SelectWithoutData;
  text(text: string): SelectWithoutData;
  data<D>(data: Array<D>): SelectWithData<D>;
  call(fn: SelectWithoutData => void): SelectWithoutData;
}

declare class ForceX {
  strength(strength: number | any => number): ForceX;
  x(x: number | any => number): ForceX;
}
declare class ForceY {
  strength(strength: number | any => number): ForceX;
  x(x: number | any => number): ForceX;
}
declare class Link<N, L> {
  strength(strength: number): Link<N, L>;
  distance(distance: number): Link<N, L>;
  links<L>(links: Array<L>): Link<N, L>;
  id(fn: N => string): Link<N, L>;
}
declare class ManyBody {
  strength(strength: number): ManyBody;
}
declare class Center {
  x(x: number): Center;
  y(y: number): Center;
}
declare type Forces<N, L> = ForceX | ForceY | Link<N, L> | ManyBody | Center | any;

declare class Simulation<N, L> {
  nodes(nodes: Array<N>): Simulation<N, L>;
  restart(): Simulation<N, L>;
  stop(): Simulation<N, L>;
  alphaDecay(rate: number): Simulation<N, L>;
  alphaTarget(target: number): Simulation<N, L>;
  on(event: 'tick', fn: () => void): Simulation<N, L>;
  force(name: string, force: Forces<N, L>): Simulation<N, L>;
  force(name: string): Forces<N, L>;
}

declare class ContinuousScale {
  [[call]]: number => number;
  domain(d: [number, number]): ContinuousScale;
  range(r: [number, number]): ContinuousScale;
  invert(n: number): number;
}

declare module 'd3' {
  declare export function line(): Array<[number, number]> => string;
  declare export function mouse(container: Element): [number, number];
  declare export function interpolateNumber(low: number, hi: number): number => number;
  declare export function csv<A>(string): Promise<A>;
  declare export function forceSimulation<N, L>(): Simulation<N, L>;
  declare export function forceCenter(width: number, height: number): Center;
  declare export function forceManyBody(): ManyBody;
  declare export function forceLink<N, L>(): Link<N, L>;
  declare export function forceX(x?: number): ForceX;
  declare export function forceY(y?: number): ForceY;
  declare export var event: { x: number, y: number };
  declare export function drag(): any;
  declare export function select(selector: string): SelectWithoutData;
  declare export function select(element: Element): SelectWithoutData;
  declare export function selectAll(selector: string): SelectWithoutData;
  declare export function scaleLinear(): ContinuousScale;
}
