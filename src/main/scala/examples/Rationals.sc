val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
val r = new Rational(2,0)
x.sub(y).sub(z)

y.add(y)
y.less(x)
x.max(z)

class Rational(x: Int, y: Int) {
  require(y!=0, "Denominator must be non zero")

  def this(x:Int) = this(x,1)
  private val h = hcf(x, y)

  def num = x / h

  def den = y / h

  def add(that: Rational) = {
    new Rational(x * that.den + that.num * den, den * that.den)
  }

  def neg = new Rational(-num, den)

  def sub(that: Rational) = add(that.neg)

  def less(that: Rational) = num * that.den < den * that.num

  def max(that: Rational) = if(this.less(that)) that else this
  private def hcf(a: Int, b: Int): Int = {
    if (b == 0) a
    else
      hcf(b, a % b)
  }

  override def toString() = num + "/" + den
}

