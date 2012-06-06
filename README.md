[`Checking`](scala-checking/blob/master/src/main/scala/Checking.scala)
is a trait (very like
[`EitherExtras`](/robcd/scala-either-extras/blob/master/src/main/scala/EitherExtras.scala)),
that provides (yet) a(nother) biased alternative to `scala.Either`, a
'fixed' version of which is to be found
[here](/robcd/scala-either-proj-map-returns-proj/blob/master/src/main/scala/Either.scala).

This alternative is called `Checked`, whose only subclasses are `Okay`
and `Reason`.

It is for use as the return type, in place of some type, `A`, wherever
an exception might otherwise be thrown and a more 'functional' style
is preferred. The value of `A` to be returned is returned wrapped in an
instance of either `Okay` or `Reason`, depending on whether or not an
exceptional condition arose.

Note that although `Checked` provides `map` and `flatMap`, it does not
provide `filter` or `withFilter`, since it was concluded that an empty
result is not appropriate.

The same 'extras' provided for `scala.Either` by `EitherExtras` are also
provided for `Checked` by `Checking`, including the ability to check
multiple values at once using the `<*>` operator. Note that 'fast' and
'slow' have now become 'ff' (fail-fast) and 'fs' (fail-slowly), respectively.
