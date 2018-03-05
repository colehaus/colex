import type ChangeSet from 'vega-lib';

declare class View {
  change(name: string, changeSet: ChangeSet): View;
  run(): void;
}

type EmbedResult = {
  view: View
}

declare module 'vega-lib-embed' {
  declare module.exports: (selector: string, spec: Object, opts: Object) => Promise<EmbedResult>;
}
