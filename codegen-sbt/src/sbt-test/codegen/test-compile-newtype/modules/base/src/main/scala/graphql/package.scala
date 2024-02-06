import zio.query.ZQuery
import javax.sql.DataSource

package object graphql {
  type Env         = DataSource
  type FID         = Int
  type MyZQuery[A] = ZQuery[Env, Throwable, A]
}
