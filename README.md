# scala_pure_random
Yet another functional pseudo-random generator. 

With this you can write 
```scala
val nonStandardNormal: Random[Double] = standardNormal * const(200d) + const(100d)
val cochi: Random[Double] = standardNormal / standardNormal
```

See `scalapurerandom.example.Example` for an example. 
