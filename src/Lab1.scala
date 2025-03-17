object Lab1 extends App {
  // This method calculates the prime factorization of a number
  // For example, 60 = 2 × 2 × 3 × 5
  def primeFactors(n: Int): List[Int] = {
    // Helper function that builds the list of factors using tail recursion
    // - n: the current number being factored
    // - divisor: the current potential prime factor we're testing
    // - acc: accumulator that collects the prime factors found so far
    @annotation.tailrec
    def factors(n: Int, divisor: Int, acc: List[Int]): List[Int] = {
      if (n == 1) acc // Base case: we've fully factored the number
      else if (n % divisor == 0) factors(n / divisor, divisor, divisor :: acc) // Found a factor, add it and continue
      else factors(n, divisor + 1, acc) // Try the next potential divisor
    }

    // Start with divisor=2 and reverse the result to get factors in ascending order
    factors(n, 2, Nil).reverse
  }

  // Example usage of prime factorization
  println(primeFactors(60)) // Output: List(2, 2, 3, 5)
  println(primeFactors(97)) // Output: List(97)
  println(primeFactors(84)) // Output: List(2, 2, 3, 7)

  // Standard binary search implementation that returns the index of an element in a sorted array
  // Returns None if the element isn't found
  def binarySearch(arr: Array[Int], elt: Int): Option[Int] = {
    // Recursive helper that searches in the array between low and high indices
    @annotation.tailrec
    def search(low: Int, high: Int): Option[Int] = {
      if (low > high) None // Element not found
      else {
        val mid = (low + high) / 2 // Calculate the middle position
        if (arr(mid) == elt) Some(mid) // Found the element!
        else if (arr(mid) > elt) search(low, mid - 1) // Search in left half
        else search(mid + 1, high) // Search in right half
      }
    }

    search(0, arr.length - 1) // Start search over the entire array
  }

  // Test our binary search
  val arr = Array(1, 3, 5, 7, 9, 11, 12)
  println(binarySearch(arr, 5)) // Output: Some(2)
  println(binarySearch(arr, 10)) // Output: None

  // Split a list of pairs into two separate lists
  def unzip[A, B](list: List[(A, B)]): (List[A], List[B]) = {
    list match {
      case Nil => (Nil, Nil) // Base case: empty list gives two empty lists
      case (a, b) :: tail =>
        val (as, bs) = unzip(tail) // Recursively unzip the rest of the list
        (a :: as, b :: bs) // Combine the head elements with the processed tail
    }
  }

  // Example of unzipping a list
  val input = List((1, 'a'), (20, 'b'), (3, 'c'))
  val result = unzip(input)
  println(result) // Output: (List(1, 20, 3), List('a', 'b', 'c'))

  // Combine two lists into a list of pairs
  def zip[A, B](list1: List[A], list2: List[B]): List[(A, B)] = {
    (list1, list2) match {
      case (Nil, _) => Nil // If first list is empty, result is empty
      case (_, Nil) => Nil // If second list is empty, result is empty
      case (head1 :: tail1, head2 :: tail2) =>
        (head1, head2) :: zip(tail1, tail2) // Create a pair and continue with the tails
    }
  }

  // Examples of zipping lists
  println(zip(List(10, 20, 30), List('a', 'b', 'c'))) // Output: List((10, 'a'), (20, 'b'), (30, 'c'))
  println(zip(List(10, 20, 30), List('a', 'b'))) // Output: List((10, 'a'), (20, 'b'))

  // Filter elements from a list based on a predicate function
  def filter[A](list: List[A], f: A => Boolean): List[A] = {
    list match {
      case Nil => Nil // Base case: empty list
      case head :: tail =>
        if (f(head)) head :: filter(tail, f) // Keep element if it matches predicate
        else filter(tail, f) // Skip element if it doesn't match
    }
  }

  // Example of filtering even numbers
  println(filter(List(1, 2, 3, 4, 5), _ % 2 == 0)) // Output: List(2, 4)

  // Apply a function to each element in a list
  def map[A, B](list: List[A], f: A => B): List[B] = {
    list match {
      case Nil => Nil // Base case: empty list
      case head :: tail => f(head) :: map(tail, f) // Apply function to head and continue
    }
  }

  // Example of doubling each number
  println(map(List(1, 2, 3, 4, 5), _ * 2)) // Output: List(2, 4, 6, 8, 10)

  // Group elements in a list by a key function
  def groupBy[A, B](list: List[A], f: A => B): Map[B, List[A]] = {
    // Use foldLeft to build the map, starting with an empty map
    list.foldLeft(Map.empty[B, List[A]]) { (acc, elem) =>
      val key = f(elem) // Calculate the key for this element
      acc + (key -> (elem :: acc.getOrElse(key, Nil))) // Add element to list for its key
    }
  }

  // Example of grouping by even/odd status
  println(groupBy(List(1, 2, 3, 4, 5), _ % 2 == 0)) // Output: Map(false -> List(5, 3, 1), true -> List(4, 2))

  // Reduce a list to a single value using a binary operation
  def reduce[A](list: List[A], f: (A, A) => A): A = {
    list match {
      case Nil => throw new UnsupportedOperationException("empty.reduce") // Can't reduce an empty list
      case head :: tail => tail.foldLeft(head)(f) // Start with head and combine with remaining elements
    }
  }

  // Example of summing a list
  println(reduce(List(1, 2, 3, 4, 5), _ + _)) // Output: 15

  // Generate all possible subsets of a set
  def subsets[A](set: Set[A]): Set[Set[A]] = {
    // Tail-recursive implementation
    @annotation.tailrec
    def loop(remaining: Set[A], acc: Set[Set[A]]): Set[Set[A]] = {
      if (remaining.isEmpty) acc // Base case: no more elements to process
      else {
        val elem = remaining.head // Take the next element
        // For each existing subset, create a new one with elem added
        val newAcc = acc ++ acc.map(_ + elem)
        loop(remaining.tail, newAcc) // Continue with the remaining elements
      }
    }

    // Start with just the empty set in our accumulator
    loop(set, Set(Set.empty[A]))
  }

  // Examples of generating subsets
  println(subsets(Set())) // Output: Set(Set())
  println(subsets(Set(1))) // Output: Set(Set(), Set(1))
  println(subsets(Set(1, 2))) // Output: Set(Set(), Set(1), Set(2), Set(1, 2))
  println(subsets(Set(1, 2, 3))) // Output: Set(Set(), Set(1), Set(2), Set(1, 2), Set(3), Set(1, 3), Set(2, 3), Set(1, 2, 3))

  // Generate all valid combinations of n pairs of parentheses
  def generateParentheses(n: Int): List[String] = {
    // Using a stack to make this tail-recursive
    @annotation.tailrec
    def loop(open: Int, close: Int, acc: List[String], stack: List[(Int, Int, String)]): List[String] = {
      stack match {
        case Nil => acc // No more states to process
        case (open, close, current) :: rest =>
          if (open == n && close == n)
            loop(open, close, current :: acc, rest) // Found a complete valid sequence
          else {
            // We can add an open parenthesis if we haven't used all n
            val withOpen = if (open < n) (open + 1, close, current + "(") :: rest else rest
            // We can add a closing parenthesis if we have unclosed opening ones
            val withClose = if (close < open) (open, close + 1, current + ")") :: withOpen else withOpen
            loop(open, close, acc, withClose) // Continue with updated stack
          }
      }
    }

    // Start with no parentheses and reverse the result to get the expected order
    loop(0, 0, Nil, List((0, 0, ""))).reverse
  }

  // Example of generating valid parentheses combinations
  println(generateParentheses(3)) // Output: List("((()))", "(()())", "(())()", "()(())", "()()()")

  // Coded with love by Alejandro Cerezo Contreras.
  // 2025-03-06 at 18:42.

  // If you are reading this, I would like to learn more about testing in Scala. ;)

}