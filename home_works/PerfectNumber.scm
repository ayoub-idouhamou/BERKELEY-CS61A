
def sumOfFactors(n: Int): Int = {
  def accumulate(i: Int, acc: Int): Int = {
    if (i == n) acc
    else if (n % i == 0) accumulate(i + 1, acc + i)
    else accumulate(i + 1, acc)
  }
  accumulate(1, 0)
}

def nextPerfect(m: Int): Int = 
  if (sumOfFactors(m) == m) m 
  else nextPerfect(m + 1)

