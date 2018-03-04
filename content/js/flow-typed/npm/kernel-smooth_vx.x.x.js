declare class KernelSmooth {
  fun: {
    gaussian: number => number
  };
  regression(xs: Array<number>, ys: Array<number>, kernel: number => number, bandwidth: number): number => number;
}

declare module 'kernel-smooth' {
  declare module.exports: KernelSmooth;
}
