def binarySearchIterative(list: Array[Int], target: Int): Int = {
  var left = 0
  var right = list.length - 1
  while (left <= right) {
    val mid = left + (right - left) / 2
    if (list(mid) == target)
      return mid
    else if (list(mid) > target)
      right = mid - 1
    else
      left = mid + 1
  }
  -1
}