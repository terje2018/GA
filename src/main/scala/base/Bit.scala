package base

object Bit extends App{
  println("%08d",12345.toBinaryString)

  val reciprocal = 1.0/3.0
  val l = List.tabulate(10)(x => ((reciprocal * math.pow(2,x)).toInt % 2))
  println(l)

  val f = 173.7
  val intValue = f.intValue()
  val floatValue = f - intValue
  println(intValue+" # "+floatValue)

  var v = floatValue
  val floatList = List.tabulate(16)(x => {
    val v2 = v * 2
    v = v2 - v2.intValue()
    val result = v2.intValue() % 2
    result
  })
  println(floatList)
  var vi = intValue
  var index = 0
  var flag = true
  var intList = List.tabulate(16)(x => {
    println("#" + x)
    val result = vi % 2
    vi = vi / 2
    if (vi == 0 && flag){
      index = x
      flag = false
    }
    result
  })
  println(intList)
  println(intList.reverse)
  println(index)

  println(intList.slice(0,index+1))
  println(intList.slice(index+1,16))

  val list = value2bits(173.625,24)
  println(list)
  val dec = value2decimal(list)
  println(dec)


  def value2bits(value:Double,length:Int=32):List[Int] = {
    val intValue = value.toInt
    val decimalValue = value - intValue
    var index = 0
    var flag = true
    var vi = intValue
    var intList = List.tabulate(length/2)(x => {
      val result = vi % 2
      vi = vi / 2
      if (vi == 0 && flag){
        index = x
        flag = false
      }
      result
    })
    intList = intList.slice(index+1,16) ++: intList.slice(0,index+1).reverse

    var vd = decimalValue
    val floatList = List.tabulate(length/2)(_ => {
      val v2 = vd * 2
      vd = v2 - v2.intValue()
      val result = v2.intValue() % 2
      result
    })
    val result = intList ++: floatList
    result
  }

  def value2decimal(list:List[Int]):Double = {
    val length = list.size
    val intList = list.slice(0,length/2)
    val decList = list.slice(length/2,length)
    val index = intList.indexOf(1)
    val intList_ = intList.slice(index,length/2).reverse
    val length_ = intList_.size
    var intValue = 0d
    for (index <- 0 until length_) {
      intValue = intValue + intList_(index) * math.pow(2,index)
    }
    for (index <- 0 until length/2) {
      intValue = intValue + decList(index) * math.pow(2,-(index+1))
    }
    intValue
  }

}


