declare class Stream<A> {
  [[call]]: ((A) => void) & (() => A);
}

declare class Flyd {
  on<A>(handler: A => void, Stream<A>): void;
  combine<A, B, C>(handler: (Stream<A>, Stream<B>) => C, [Stream<A>, Stream<B>]): Stream<C>;
  stream<A>(initial: A): Stream<A>;
}

declare module 'flyd' {
  declare module.exports: Flyd;
}
