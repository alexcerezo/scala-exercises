object exercise3 extends App{
  def isPalindrome(string: String):Boolean =
    string.reverse.replace(" ", "") == string.replace(" ", "")

  println(isPalindrome("race car"))

}