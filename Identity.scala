//https://qiita.com/f81@github/items/a90c16419645be5d7008

object Identity {

  def main(args: Array[String]): Unit = {
    val domain1 = new Domain2(1, "ichi")
    val domain2 = domain1

    println(domain1 eq domain2)
    println(domain1 ne domain2)

    val domain3 = new Domain2(1, "ni")

    println(domain1 eq domain3)
    println(domain1 ne domain3)
  }

}

class Domain2(val id: Long, val name: String) {
  def canEqual(other: Any) = {
    other.isInstanceOf[Domain2]
  }

  override def equals(other: Any) = {
    other match {
      case that: Domain2 =>
        that.canEqual(Domain2.this) && id == that.id && name == that.name
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime * (prime + id.hashCode) + name.hashCode
  }
}


//$ scala Identity
//true   // domain1 eq domain2
//false  // domain1 ne domain2
//false  // domain1 eq domain3
//true   // domain1 ne domain3