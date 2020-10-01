package basics

object ClassesAndTraits extends App {

  sealed trait Vector

  case class Vector2(x: Double, y: Double) extends Vector {
    def add(add: Vector2): Vector2 = Vector2(this.x + add.x, this.y + add.y)
    def add(add: Double): Vector2 = Vector2(this.x + add, this.y + add)
    def sub(sub: Vector2): Vector2 = Vector2(this.x - sub.x, this.y - sub.y)
    def sub(sub: Double): Vector2 = Vector2(this.x - sub, this.y - sub)
    def mul(mul: Vector2): Vector2 = Vector2(this.x * mul.x, this.y * mul.y)
    def mul(mul: Double): Vector2 = Vector2(this.x * mul, this.y * mul)
    def div(div: Vector2): Vector2 = if (div.x == 0 || div.y == 0) this else Vector2(this.x / div.x, this.y / div.y)
    def div(div: Double): Vector2 = if (div == 0) this else Vector2(this.x / div, this.y / div)
  }

  case class Vector3(x: Double, y: Double, z: Double) extends Vector {
    def add(add: Vector3): Vector3 = Vector3(this.x + add.x, this.y + add.y, this.z + add.z)
    def add(add: Double): Vector3 = Vector3(this.x + add, this.y + add, this.z + add)
    def sub(sub: Vector3): Vector3 = Vector3(this.x - sub.x, this.y - sub.y, this.z - sub.z)
    def sub(sub: Double): Vector3 = Vector3(this.x - sub, this.y - sub, this.z - sub)
    def mul(mul: Vector3): Vector3 = Vector3(this.x * mul.x, this.y * mul.y, this.z * mul.z)
    def mul(mul: Double): Vector3 = Vector3(this.x * mul, this.y * mul, this.z * mul)
    def div(div: Vector3): Vector3 = if (div.x == 0 || div.y == 0 || div.z == 0) this else Vector3(this.x / div.x, this.y / div.y, this.z / div.z)
    def div(div: Double): Vector3 = if (div == 0) this else Vector3(this.x / div, this.y / div, this.z / div)
  }

  sealed trait Located[A <: Vector] {
    def location: A
  }

  sealed trait Bounded[A <: Vector] {
    def bounds: (A, A)
  }

  sealed trait Movable[A, B <: Vector] {
    def move(target: B): A
  }

  sealed trait Shape2D[A] extends Located[Vector2] with Bounded[Vector2] with Movable[A, Vector2] {
    def area: Double
  }

  sealed trait Shape3D[A] extends Located[Vector3] with Bounded[Vector3] with Movable[A, Vector3] {
    def surfaceArea: Double
    def volume: Double
  }

  object Origin2D extends Located[Vector2] {
    override def location: Vector2 = Vector2(0, 0)
  }

  object Bounded {
    def minimumBoundingRectangle(objects: Set[Bounded[Vector2]]): Bounded[Vector2] = {
      new Bounded[Vector2] {
        implicit private val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering

        override def bounds: (Vector2, Vector2) = (
          Vector2(
            objects.map(_.bounds._1.x).min,
            objects.map(_.bounds._1.y).min
          ),
          Vector2(
            objects.map(_.bounds._2.x).max,
            objects.map(_.bounds._2.y).max
          )
        )
      }
    }

    def minimumBoundingBox(objects: Set[Bounded[Vector3]]): Bounded[Vector3] = {
      new Bounded[Vector3] {
        implicit private val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering

        override def bounds: (Vector3, Vector3) = (
          Vector3(
            objects.map(_.bounds._1.x).min,
            objects.map(_.bounds._1.y).min,
            objects.map(_.bounds._1.z).min
          ),
          Vector3(
            objects.map(_.bounds._2.x).max,
            objects.map(_.bounds._2.y).max,
            objects.map(_.bounds._2.z).max
          )
        )
      }
    }
  }

  final case class Point2D(location: Vector2) extends Shape2D[Point2D] {
    override def bounds: (Vector2, Vector2) = (location, location)
    override def move(target: Vector2): Point2D = Point2D(location.add(target))
    override def area: Double = 0
  }

  final case class Circle(location: Vector2, radius: Double) extends Shape2D[Circle] {
    override def bounds: (Vector2, Vector2) = (location.sub(radius), location.add(radius))
    override def move(target: Vector2): Circle = Circle(location.add(target), radius)
    override def area: Double = Math.PI * Math.pow(radius, 2)
  }

  final case class Rectangle(location: Vector2, size: Vector2) extends Shape2D[Rectangle] {
    override def bounds: (Vector2, Vector2) = (location.sub(size.div(2)), location.add(size.div(2)))
    override def move(target: Vector2): Rectangle = Rectangle(location.add(target), size)
    override def area: Double = size.x * size.y
  }

  final case class Square(location: Vector2, size: Double) extends Shape2D[Square] {
    override def bounds: (Vector2, Vector2) = (location.sub(size), location.add(size))
    override def move(target: Vector2): Square = Square(location.add(target), size)
    override def area: Double = Math.pow(size, 2)
  }

  final case class Triangle2D(vertices: (Vector2, Vector2, Vector2)) extends Shape2D[Triangle2D] {
    val verts: List[Vector2] = vertices._1 :: vertices._2 :: vertices._3 :: Nil
    override def location: Vector2 = Vector2(verts.map(_.x).sum / 3, verts.map(_.y).sum / 3)
    override def bounds: (Vector2, Vector2) = (
      Vector2(verts.map(_.x).min, verts.map(_.y).min),
      Vector2(verts.map(_.x).max, verts.map(_.y).max)
    )
    override def move(target: Vector2): Triangle2D = Triangle2D(vertices._1.add(target), vertices._2.add(target), vertices._3.add(target))
    override def area: Double = ???
  }

  object Origin3D extends Located[Vector3] {
    override def location: Vector3 = Vector3(0, 0, 0)
  }

  final case class Point3D(location: Vector3) extends Shape3D[Point3D] {
    override def bounds: (Vector3, Vector3) = (location, location)
    override def move(target: Vector3): Point3D = Point3D(location.add(target))
    override def surfaceArea: Double = 0
    override def volume: Double = 0
  }

  final case class Sphere(location: Vector3, radius: Double) extends Shape3D[Sphere] {
    override def bounds: (Vector3, Vector3) = (location.sub(radius), location.add(radius))
    override def move(target: Vector3): Sphere = Sphere(location.add(target), radius)
    override def surfaceArea: Double = 4 * Math.PI * Math.pow(radius, 2)
    override def volume: Double = (4.0 / 3.0) * Math.PI * Math.pow(radius, 3)
  }

  final case class Cube(location: Vector3, size: Double) extends Shape3D[Cube] {
    override def bounds: (Vector3, Vector3) = (location.sub(size / 2), location.add(size / 2))
    override def move(target: Vector3): Cube = Cube(location.add(target), size)
    override def surfaceArea: Double = Math.pow(size, 2) * 6
    override def volume: Double = Math.pow(size, 3)
  }

  final case class Cuboid(location: Vector3, size: Vector3) extends Shape3D[Cuboid] {
    override def bounds: (Vector3, Vector3) = (location.sub(size.div(2)), location.add(size.div(2)))
    override def move(target: Vector3): Cuboid = Cuboid(location.add(target), size)
    override def surfaceArea: Double = ((size.x * size.y) + (size.y * size.z) + (size.x * size.z)) * 2
    override def volume: Double = size.x * size.y * size.z
  }

  final case class Triangle3D(vertices: (Vector3, Vector3, Vector3)) extends Shape3D[Triangle3D] {
    val verts: List[Vector3] = vertices._1 :: vertices._2 :: vertices._3 :: Nil
    override def location: Vector3 = Vector3(verts.map(_.x).sum / 3, verts.map(_.y).sum / 3, verts.map(_.z).sum / 3)
    override def bounds: (Vector3, Vector3) = (
      Vector3(verts.map(_.x).min, verts.map(_.y).min, verts.map(_.z).min),
      Vector3(verts.map(_.x).max, verts.map(_.y).max, verts.map(_.z).max)
    )
    override def move(target: Vector3): Triangle3D = Triangle3D(vertices._1.add(target), vertices._2.add(target), vertices._3.add(target))
    override def surfaceArea: Double = ???
    override def volume: Double = 0
  }

  final case class Cylinder(location: Vector3, radius: Double, height: Double) extends Shape3D[Cylinder] {
    override def bounds: (Vector3, Vector3) = (
      Vector3(location.x - radius, location.y - radius, location.z - height / 2),
      Vector3(location.x + radius, location.y + radius, location.z + height / 2)
    )
    override def move(target: Vector3): Cylinder = Cylinder(location.add(target), radius, height)
    override def surfaceArea: Double = (2 * Math.PI * Math.pow(radius, 2)) + (2 * Math.PI * radius * height)
    override def volume: Double = Math.PI * Math.pow(radius, 2) * height
  }
}