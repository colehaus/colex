::: {#key-terms}
Type parameter syntax
:   `class Foo[A]: ..` and `def bar[A](x: A): ..` express that `Foo` and `bar` are generic over a type `A`.

`Fin[Int]`
:   A type that represents an integer in the range `[0, Int)`.

Type alias syntax
:   `type Foo = int` is purely synctactic sugar that expresses that `Foo` is an alias for `int`.

Variadic generic syntax
:   `class Foo[Bar, *Shape, Baz]` expresses that `Foo` accepts a variable number of type arguments (which will be bound to `Shape`) in between the required arguments `Bar` and `Baz`.

ndarray
:   `ndarray[*Shape, DType]` is used to express vectors like `ndarray[Literal[10], int32]` and matrices like `ndarray[SeqLen, EmbedDim, float32]` where the number of variables corresponds to the number of dimensions and each variable specifies the length of the dimension.

`InstanceSingleton`
:    A type which signifies that every value with this type in a particular instance is identical. Typically used as a "private" type variable for annotating dimensions.
:::