declare class Changeset {
  insert(data: Array<Object>): Changeset;
  remove(predicate: Object => boolean): Changeset;
}

declare class Vega {
  changeset(): Changeset;
}

declare module 'vega-lib' {
  declare module.exports: Vega;
}
