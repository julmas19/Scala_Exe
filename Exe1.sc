
object distance{
  
  def manhatan (da: Array[Double], db: Array[Double]): Double = {
    var sum = 0.0
    for (i <- 0 to (da.length-1)){
      sum += abs(da(i) - db(i))
    }
    sum
  }
  
  def euclideana  (da: Array[Double], db: Array[Double]): Double ={
    
    var sum = 0.0
    for (i <- 0 to (da.length-1)){
      //sum += ((da(i) - db(i))*(da(i) - db(i)))
      sum += potencia((da(i) - db(i)),2)
    }
    math.sqrt(sum)
  }
  
  def minkowski (da: Array[Double], db: Array[Double], exp: Double): Double = {
    var sum = 0.0
    for (i <- 0 to (da.length-1)){
      sum += abs(potencia((da(i) - db(i)),exp))
    }
    var y: Double = potencia(exp,-1)
    math.pow(sum,y)
    
  }
  
  
  
  def potencia (base: Double, exp: Double): Double = {
  if(exp == 0)
    1
  else if(exp < 0)
    potencia(base, exp+1) / base;
  else
    base * potencia(base, exp-1);
  }
  
  def abs(n: Double): Double =
      if (n < 0) -n
      else n
}
distance.manhatan(Array(4,-6,8,5),Array(5,3,-5,9))
distance.euclideana(Array(4,-6,8,5),Array(5,3,-5,9))
distance.minkowski(Array(4,-6,8,5),Array(5,3,-5,9),1)

