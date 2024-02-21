# Resources

## Talks
- [Cost-effective and easy to maintain GraphQL integration tests](https://www.youtube.com/watch?v=pYfWq4GmObY) by Jules Ivanic at [Functional Scala](https://www.functionalscala.com/) in December 2022
- [Zymposium episode on Caliban](https://youtu.be/mzqsXklbmfM) with Pierre Ricadat, Kit Langton and Adam Fraser in June 2021
- [A Tour of Caliban](https://www.youtube.com/watch?v=lgxUKsOH65k) by Pierre Ricadat at [SF Scala](https://www.meetup.com/SF-Scala/) in April 2020
- [Caliban: Designing a Functional GraphQL Library](https://www.youtube.com/watch?v=OC8PbviYUlQ) by Pierre Ricadat at [Functional Scala](https://www.functionalscala.com/) in December 2019 (slides available [here](https://www.slideshare.net/PierreRicadat/designing-a-functional-graphql-library-204680947))

## Blog Articles

- [GraphQL in Scala](https://blog.pierre-ricadat.com/series/graphql-in-scala) a blog series by Pierre Ricadat (December 2023)
- [Caliban Client: a type-safe GraphQL Client for Scala and Scala.js](https://medium.com/@ghostdogpr/caliban-client-a-type-safe-graphql-client-for-scala-and-scala-js-718aa42c5ef7) by Pierre Ricadat (February 2020)
- __*GraphQL in Scala with Caliban*__ by Pierre Ricadat (February 2020)
    - [Part 1: Turn a simple API into GraphQL](https://medium.com/@ghostdogpr/graphql-in-scala-with-caliban-part-1-8ceb6099c3c2)
    - [Part 2: Query optimization](https://medium.com/@ghostdogpr/graphql-in-scala-with-caliban-part-2-c7762110c0f9)
    - [Part 3: Customization using wrappers](https://medium.com/@ghostdogpr/graphql-in-scala-with-caliban-part-3-8962a02d5d64)    
- [Authentication in Caliban](http://fokot.github.io/post/caliban-auth.html) by František Kocun (December 2019)

## Related Projects

- [caliban-deriving](https://zio.github.io/caliban-deriving/): an alternative schema derivation implementation that supports additional use cases such as deriving fields from class methods (can be used side by side with caliban regular derivation, on the types you want).
- [caliban-extras](https://github.com/niqdev/caliban-extras): a project bringing some goodies to caliban, including [caliban-refined](https://github.com/niqdev/caliban-extras#caliban-refined), a module to automatically derive schemas for [refined](https://github.com/fthomas/refined) types.
- [zio-akka-quickstart](https://github.com/ScalaConsultants/zio-akka-quickstart.g8): a Giter8 template for a basic Scala application built using ZIO, Akka HTTP, Slick and Caliban!
- [caliban-talk](https://github.com/fokot/caliban-talk): example project using ZIO, http4s, doobie/quill, caliban-client, testcontainers and Caliban
- [caliban-test](https://github.com/swachter/caliban-test): example project using ZIO, Caliban, and jOOQ for accessing a relation database
- [caliban-zio-http-cats](https://github.com/CarlosLaraFP/Cases): real technical interview project using ZIO, Caliban, Doobie, Cats core, and domain error modeling (passed + job offer)
