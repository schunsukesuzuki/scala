object Factor {
   def main ( args: Array[String]){
    var n = 1234567890
    while (n % 2 == 0) {
      print(2); print("")
      n /= 2
    }
    var m = 3
    while (m*m <=n) {
      while (n % m == 0) {
      print(m);print("")
      n /=m
    }
    m +=2
   }
   if (n > 1) print(n)
}