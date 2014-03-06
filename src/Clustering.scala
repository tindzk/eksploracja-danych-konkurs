import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random

case class Point(var x : Double, var y: Double, var z: Double) {
  def dist(p: Point) = {
    val (dx, dy, dz) = (this.x - p.x, this.y - p.y, this.z - p.z)
    Math.sqrt(dx * dx + dy * dy + dz * dz)
  }

  def closest(pts: Iterable[Point]) = {
    pts.map(cur => (cur, this.dist(cur))).minBy(_._2)._1
  }
}

object File {
  def parse(path: String, d: Int) = {
    val file = Source.fromFile(path)
    val items = file
      .getLines()
      .map(_.trim)
      .filterNot(_.startsWith("#"))
      .filter(_.nonEmpty)
      .map(_.split(" "))
      .map(_.filter(_.nonEmpty))
      .map(_.map(_.toDouble))

    items.map(p => Point(
      p(0),
      p(1),
      if (d >= 3) p(2) else 0))
  }
}

object Clustering {
  def mean(instances: Iterable[Point]) = {
    val (sx, sy, sz) = (
      instances.map(_.x).sum,
      instances.map(_.y).sum,
      instances.map(_.z).sum)

    Point(
      sx / instances.size,
      sy / instances.size,
      sz / instances.size)
  }

  def centroid(oldCentroid: Point, instances: Iterable[Point]) = {
    if (instances.isEmpty)
      oldCentroid
    else
      this.mean(instances)
  }

  def initialClustering(instances: Seq[Point], centroids: Seq[Point]) = {
    /* List of tuples: (centroid, assigned points). */
    var clusters = Map.empty[Point, ArrayBuffer[Point]]
    centroids.foreach(c => clusters += (c -> ArrayBuffer.empty[Point]))

    /* Assign each point to its closest centroid. */
    for (p <- instances) {
      val centroid = p.closest(clusters.keySet)

      /* Get cluster the centroid is in and add the current point. */
      var xs = clusters(centroid)
      xs += p

      /* Update the cluster's centroid. */
      val newCentroid = this.centroid(centroid, xs)
      clusters -= centroid
      clusters += (newCentroid -> xs)
    }

    clusters
  }

  def randomNumber(lowerBound: Double, upperBound: Double) = {
    val rand = new Random()
    lowerBound + rand.nextDouble() * (upperBound - lowerBound)
  }

  def initialCentroids(instances: Seq[Point], k: Int) = {
    val limitsX = (instances.map(_.x).min, instances.map(_.x).max)
    val limitsY = (instances.map(_.y).min, instances.map(_.y).max)
    val limitsZ = (instances.map(_.z).min, instances.map(_.z).max)

    (1 to k).map(x => Point(
      randomNumber(limitsX._1, limitsX._2),
      randomNumber(limitsY._1, limitsY._2),
      randomNumber(limitsZ._1, limitsZ._2)))
  }

  def iterate(instances: Seq[Point], clusters: Map[Point, ArrayBuffer[Point]]) = {
    /* Reposition centroids. */
    val centroids = clusters.map(cur => this.centroid(cur._1, cur._2))

    var newClusters = Map.empty[Point, ArrayBuffer[Point]]
    centroids.foreach(c => newClusters += (c -> ArrayBuffer.empty[Point]))

    for (p <- instances) {
      val c = p.closest(centroids)
      newClusters(c) += p
    }

    newClusters
  }

  def cluster(instances: Seq[Point], k: Int): Map[Point, ArrayBuffer[Point]] = {
    val centroids = this.initialCentroids(instances, k)
    var clusters = this.initialClustering(instances, centroids)

    while (true) {
      val newClusters = this.iterate(instances, clusters)

      if (newClusters == clusters) {
        return clusters
      }

      clusters = newClusters
    }

    clusters
  }
}

object Main {
  /**
   * Parameters are: path n d k csv
   *
   * - path: Input file.
   * - n: Limit data set to n entries.
   * - k: Number of clusters.
   * - csv: If argument is non-empty, then the output is written in CSV, facilitating visualisation.
   */
  def main(args : Array[String]) {
    val path =
      if (args.length > 0) args(0)
      else "Minikonkurs01A.txt"

    val n =
      if (args.length > 1) args(1).toInt
      else 1000

    val d =
      if (args.length > 2) args(2).toInt
      else 2

    val k =
      if (args.length > 3) args(3).toInt
      else 3

    val csv =
      if (args.length > 4) true
      else false

    val pts = File.parse(path, d).toList.slice(0, n)
    val clusters = Clustering.cluster(pts, k).zipWithIndex

    for (p <- pts) {
      val clusterId = clusters.find(_._1._2.contains(p)).get._2

      if (csv) {
        println(s"${p.x},${p.y},${p.z},$clusterId")
      } else {
        println(clusterId + 1)
      }
    }
  }
}
