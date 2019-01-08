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

declare module 'd3-force' {
  declare export function forceSimulation<N, L>(): Simulation<N, L>;
  declare export function forceCenter(width: number, height: number): Center;
  declare export function forceManyBody(): ManyBody;
  declare export function forceLink<N, L>(): Link<N, L>;
  declare export function forceX(x?: number): ForceX;
  declare export function forceY(y?: number): ForceY;
}
