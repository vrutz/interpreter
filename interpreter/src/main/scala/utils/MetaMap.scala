package utils

import scala.collection.mutable.MapBuilder
import scala.collection.MapLike
import scala.collection.generic.CanBuildFrom
import scala.meta._

/**
 * Created by rutz on 14/10/15.
 */
trait MetaMap[K <: Tree, V] extends Map[K, V] with MapLike[K, V, MetaMap[K, V]] { outer =>
  implicit def equality[K]: MetaEquality[K]

  protected val map = Map[K, V]()

  override def get(t: K): Option[V] = map.get(t)

  override def getOrElse[B1 >: V](t: K, default: => B1) = this.get(t) match {
    case Some(v) => v
    case None => default
  }
  override def contains(t: K) = map.exists(equality.isEqual(_, t))

  override def +[B1 >: V](tv: (K, B1)) = new MetaMap[K, V] {
    override def equality[K] = outer.equality
    override protected val map = outer.map + tv
  }

  override def -(t: K) = new MetaMap[K, V] {
    override def equality[K] = outer.equality
    override protected val map = outer.map - t
  }

  override def iterator = map.iterator

  override def empty = new MetaMap[K, V] {
    override def equality[K] = outer.equality
  }

}

object MetaMap {
  implicit def canBuildFrom[K, V] = new CanBuildFrom[MetaMap[K, V], K, MetaMap[K, V]] {
    def apply(from: MetaMap[K, V]): MapBuilder[K, V, MetaMap[K, V]] =
      new MapBuilder[K, V, MetaMap[K, V]](from.empty) {

        private val map = from.map

        def +(tv: (K, V)) = {
          if (!from.contains(tv._1)) map + tv
          else this
        }

        def empty = map.empty

        override def result() = new MetaMap[K, V] {
          override val map = from.map
          override def equality[K] = from.equality
        }
      }
    def apply(): MapBuilder[K, V, MetaMap[K, V]] =
      sys.error("this can't be implemented, because no equality instance is provided")
  }

  def apply[K: MetaEquality, V](tvs: (K, V)*) = {
    val metaMap  = new MetaMap[K, V] {
      override def equality[K] = implicitly[MetaEquality[K]]
    }.empty
    tvs.foldLeft(metaMap) { case (mmap, tv) => mmap + tv }
  }
}

trait MetaEquality[T] {
  def isEqual(t1: T, t2: T): Boolean
}
