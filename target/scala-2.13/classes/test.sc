import scala.collection.mutable.HashMap

val map:HashMap[BigInt, BigInt] = new HashMap[BigInt, BigInt]()
map += (12.toBigInt -> 33.toBigInt)
map += (12.toBigInt -> 65.toBigInt)


for (i <- 10 to 0 by -1)
  println("i = " + i)

if (map.exists(_._1 == 12.toBigInt))
  println("Value = " + map(12.toBigInt))

