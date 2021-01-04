package base

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object BinaryEncoding extends App{

  var popu = initPopulation(10,7)
  for(_ <- 0 to 10){
    var result = truncateSelect4Packs(popu,0.5f)
    println(result._1)
    println(result._2)
    popu = result._2
    println("----------")
  }

  def encodingGens(length:Int):ArrayBuffer[Boolean]={
    val buffer = ArrayBuffer[Boolean]()
    for (_ <- 1 to length) {
      buffer += Random.nextBoolean()
    }
    buffer
  }

  def decodingGens4Packs(gens:ArrayBuffer[Boolean],Wlimit:Int=100):ArrayBuffer[Boolean]={
    val W = ArrayBuffer[Float](40,50,30,10,10,40,30)
    val P = ArrayBuffer[Float](40,60,10,10,3,20,60)
    val length = W.length
    val map = mutable.ListMap[Int,Float]()
    for (index <- 0 until length){
      map.+=((index,P(index)/W(index)))
    }

    var w = 0.0f
    val listMap = scala.collection.immutable.ListMap[Int,Float](map.toSeq.sortWith(_._2 > _._2):_*)
    for(key <- listMap.keys){
      val n = {if(gens(key)) 1 else 0}
      if ((w + n * W(key)) > Wlimit){
        gens.update(key,false)
      } else {
        w = w + n * W(key)
      }
    }
    gens
  }

  def fitnessPacks(gens:ArrayBuffer[Boolean]):Int={
    var F = 0
    val P = ArrayBuffer[Int](40,60,10,10,3,20,60)
    val length = P.length
    for(index <- 0 until length){
      val n = {if(gens(index)) 1 else 0}
      F = F + n*P(index)
    }
    F
  }

  def singleAcross(gens:(ArrayBuffer[Boolean],ArrayBuffer[Boolean]),index:Int):(ArrayBuffer[Boolean],ArrayBuffer[Boolean])={
    val one = gens._1
    val two = gens._2
    val gensLength = two.length
    if (index >= gensLength || index <=0){
      gens
    } else {
      for (in <- index until gensLength){
        val swapTemp1 = one(in)
        one.update(in,two(in))
        two.update(in,swapTemp1)
      }
      gens
    }
  }

  def doubleAcross(gens:(ArrayBuffer[Boolean],ArrayBuffer[Boolean]),index:Int,step:Int):(ArrayBuffer[Boolean],ArrayBuffer[Boolean])={
    val one = gens._1
    val two = gens._2
    val gensLength = two.length
    if ((index + step) >= gensLength || index <=0){
      gens
    } else {
      for (in <- index until (index + step)){
        val swapTemp1 = one(in)
        one.update(in,two(in))
        two.update(in,swapTemp1)
      }
      gens
    }
  }

  def mutate(gen:ArrayBuffer[Boolean],index:Int):ArrayBuffer[Boolean]={
    if (index >= gen.length) {
      gen
    } else {
      gen.update(index,!gen(index))
      gen
    }
  }

  def initPopulation(groupNum:Int,gensLength:Int):ArrayBuffer[ArrayBuffer[Boolean]]={
    val population = ArrayBuffer[ArrayBuffer[Boolean]]()
    for (_ <- 0 until groupNum){
      population += encodingGens(gensLength)
    }
    population
  }

  def truncateSelect4Packs(population:ArrayBuffer[ArrayBuffer[Boolean]],p:Float):(Float,ArrayBuffer[ArrayBuffer[Boolean]])={
    val length = population.length
    var popuArray = ArrayBuffer[(Float,ArrayBuffer[Boolean])]()
    for(i <- 0 until length){
      val gen = decodingGens4Packs(population(i))
      val fitness = fitnessPacks(population(i))
      popuArray.+=((fitness,gen))
    }
    popuArray = popuArray.sortWith((_._1 > _._1))
    val topNum = (p*length).toInt
    for(_ <- 0 until length/2){

      val one = popuArray(Random.nextInt(topNum))._2
      val oneGen = ArrayBuffer[Boolean]().++=(one)
      val two = popuArray(Random.nextInt(length))._2
      val twoGen = ArrayBuffer[Boolean]().++=(two)

      val genLength = one.length
      val acrossIndex = Random.nextInt(genLength)
      val acrossedGens=singleAcross((oneGen,twoGen),acrossIndex)

      population.+=(acrossedGens._1)
      population.+=(acrossedGens._2)
    }

    for(i <- 0 until 2*length){
      val gen = population(i)
      val _gen = decodingGens4Packs(gen)
      val fitness = fitnessPacks(population(i))
      popuArray += ((fitness,_gen))
    }

    popuArray = popuArray.sortWith((_._1 > _._1))
    for(i <- 0 until length){
      val gen = popuArray(i)._2
      val genLength = gen.length
      val rNum = Random.nextInt(100)
      if(rNum<5){
        mutate(gen,Random.nextInt(genLength))
      }
    }
    val slice = popuArray.slice(0,length)
    val avg = slice.map(_._1).sum / length
    val max = slice.map(_._1).max
    val array = popuArray.slice(0,length).map(_._2)
    (avg/max,array)
  }

}


