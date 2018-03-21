def binarySearchRcursive(list: Array[Int], target: Int)
(start: Int = 0, end: Int = list.length - 1): Int = {
  if (start > end) return - 1
  val mid = start + (end - start + 1 ) / 2
  if (list(mid) == target)
    return mid
  else if (list(mid) > target)
    return binarySearchRecursive(list, target)(start, mid - 1)
  else
    return binarySearchRecursive(list, target)(mid + 1, end)
}