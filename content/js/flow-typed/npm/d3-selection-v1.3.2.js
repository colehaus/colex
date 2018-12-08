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
declare module 'd3-selection' {
  declare export function mouse(container: Element): [number, number];
  declare export var event: { x: number, y: number };
  declare export function select(selector: string): SelectWithoutData;
  declare export function select(element: Element): SelectWithoutData;
  declare export function selectAll(selector: string): SelectWithoutData;
}
