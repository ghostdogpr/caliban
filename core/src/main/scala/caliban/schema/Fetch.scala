package caliban.schema

import caliban.schema.Fetch.FetchData
import zio.UIO

trait DataSource[I, +A] { self =>
  def name: String
  def fetch(id: I): UIO[Option[A]]
  def batch(ids: List[I]): UIO[Map[I, A]]

  def map[B](f: A => B): DataSource[I, B] = new DataSource[I, B] {
    override def name: String                        = name + "New"
    override def fetch(id: I): UIO[Option[B]]        = self.fetch(id).map(_.map(f))
    override def batch(ids: List[I]): UIO[Map[I, B]] = self.batch(ids).map(_.map { case (k, v) => k -> f(v) })
  }
}

sealed trait Fetch[+A] { self =>
  def map[B](f: A => B): Fetch[B]
  def run: UIO[Option[A]]
}

object Fetch {
  case class FetchData[I, +A](id: I, ds: DataSource[I, A]) extends Fetch[A] {
    override def map[B](f: A => B): Fetch[B] = FetchData(id, ds.map(f))
    override def run: UIO[Option[A]]         = ds.fetch(id)
  }

  def apply[I, A](id: I, ds: DataSource[I, A]): Fetch[A] = FetchData(id, ds)
}
