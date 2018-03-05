declare class Nerdamer {
  buildFunction(): number => number;
}

declare module 'nerdamer' {
  declare module.exports: string => Nerdamer;
}

declare module 'nerdamer/Calculus' {
}
