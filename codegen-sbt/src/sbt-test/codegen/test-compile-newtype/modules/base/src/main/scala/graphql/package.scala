import zio.query.ZQuery

package object graphql {
  type Env         = Any
  type FID         = Int
  type MyZQuery[A] = ZQuery[Env, Throwable, A]
}
