def sum(f: Int => Int,a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a+1, acc + f(a))
  }
  loop(a, 0)
}

sum(x => x*x,1,2)

def product(f: Int => Int)(a:Int, b:Int): Int = {
  if(a>b) 1
  else
    f(a)* product(f)(a+1,b)
}

product(x=>x)(1,3)

def factorial(n:Int) = product(x=>x)(1,n)

factorial(10)



def agg(g: (Int, Int) =>Int)(f: Int => Int)(a:Int, b:Int): Int = {
  if(a==b) b
  else
    g(f(a), agg(g)(f)(a+1,b))
}

agg((x,y) => x*y)(x=>x)(1,4)


def mapReduce(map: Int => Int, reduce: (Int, Int) => Int )(a:Int, b:Int): Int = {
  if(a==b) b
  else
    reduce(map(a),mapReduce(map,reduce)(a+1,b))
}

mapReduce(x=>x,(x,y)=>x*y)(1,4)