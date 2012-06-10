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
is preferred. Thus, a `Checked[A, R]` will contain either an `A`
wrapped in an instance of `Okay` or else an `R` wrapped in an instance
of `Reason`, depending on whether or not an exceptional condition
arose.

Note that although `Checked` provides `map` and `flatMap`, it does not
provide `filter` or `withFilter`, since it was concluded that an empty
result is not appropriate.

The same 'extras' provided for `scala.Either` by `EitherExtras` are also
provided for `Checked` by `Checking`, including the ability to check
multiple values at once using the `<*>` operator. Note that 'fast' and
'slow' have now become 'ff' (fail-fast) and 'fs' (fail-slowly), respectively.

Example code
------------

[`for` comprehension tests](scala-checking/blob/master/src/test/scala/Tests.scala)  
['lift' tests](scala-checking/blob/master/src/test/scala/LiftTests.scala)  
[`<*>` tests](scala-checking/blob/master/src/test/scala/AppFunctTests.scala)  
[`<*>` tests with multiple checks](scala-checking/blob/master/src/test/scala/AppFunctTestsWithChecks.scala)  
[case-class tests](scala-checking/blob/master/src/test/scala/CaseClassTests.scala)  
['SaturdayNight' tests](scala-checking/blob/master/src/test/scala/SaturdayNightTests.scala)  
[tests involving Option](scala-checking/blob/master/src/test/scala/TestsInvolvingOption.scala)  
[Musicans](scala-checking/blob/master/src/test/scala/Musicians.scala)  
[printMusicians](scala-checking/blob/master/src/test/scala/printMusicians.scala)

Artifacts
---------

org.lafros artifacts in [Maven central repository](http://search.maven.org/#browse%7C238533119)  
org.lafros artifacts in [Sonatype mirror](http://oss.sonatype.org/content/groups/public/org/lafros)
