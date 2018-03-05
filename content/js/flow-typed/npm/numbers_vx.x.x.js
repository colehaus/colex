declare class Calculus {
  Riemann(fn: number => number, lo: number, hi: number, subintervals: number): number;
}

declare class Numbers {
  calculus: Calculus
}

declare module 'numbers' {
  declare module.exports: Numbers;
}
