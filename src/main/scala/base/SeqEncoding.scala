package base

import base.BinaryEncoding.encodingGens

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, SortedSet}
import scala.util.Random

object SeqEncoding extends App{

//  val set = encodingGens(7)
//  println(set)
//  val set1 = encodingGens(7)
//  println(set1)
//  val dset = decodingGens4Packs(set)
//  println(dset)
//  val dset1 = decodingGens4Packs(set1)
//  println(dset1)
//  val result = across(dset,dset1,2,3,1)
//  println(result)
//
//  println(Random.nextFloat())

  var population = initPopulation(20,7)
  for (_ <- 0 until 20) {
    val length = population.length
    for (index <- 0 until length) {
      decodingGens4Packs(population(index))
    }
    val buffer = ArrayBuffer[SortedSet[Int]]()
    for (index <- 0 until length) {
      val index1 = Random.nextInt(length)
      val gen1 = population(index1)
      val index2 = Random.nextInt(length)
      val gen2 = population(index2)

      val oneIndex = Random.nextInt(7)
      val twoIndex = Random.nextInt(7)
      val twoStep = Random.nextInt(7/2)

      val acrossGen = across(gen1,gen2,oneIndex,twoIndex,twoStep)
      population += decodingGens4Packs(acrossGen)
    }

    val length2 = population.length
    for (index <- 0 until length2) {
      val gen = population(index)
      if(Random.nextInt(100)<5){
        val _gen = decodingGens4Packs(mutate(gen,2,7))
        population.update(index,_gen)
      }
    }

    val result = rotateSelect(population)
    val f = result._1
    population = result._2

    println(f)
    println(population)
    println("--------")
  }

  def encodingGens(length:Int):SortedSet[Int]={
    val buffer = SortedSet[Int]()
    for (_ <- 1 to length) {
      buffer += Random.nextInt(length)
    }
    buffer
  }

  def decodingGens4Packs(gens:SortedSet[Int],Wlimit:Int=100):SortedSet[Int]={
    val W = ArrayBuffer[Float](40,50,30,10,10,40,30)
    val P = ArrayBuffer[Float](40,60,10,10,3,20,60)
    val map = mutable.ListMap[Int,Float]()
    gens.foreach(index => {
      map.+=((index,P(index)/W(index)))
    })

    var w = 0.0
    val listMap = scala.collection.immutable.ListMap[Int,Float](map.toSeq.sortWith(_._2 > _._2):_*)
    for(key <- listMap.keys){
//      val n = {if(gens(key)) 1 else 0}
      if ((w + W(key)) > Wlimit){
        gens.remove(key)
      } else {
        w = w + W(key)
      }
    }
    gens
  }

  def across(one:SortedSet[Int],two:SortedSet[Int],oneIndex:Int,twoIndex:Int,twoStep:Int):SortedSet[Int]={
    val twoLength = two.size
    val oneLength = one.size
    var step = twoStep
    if (oneIndex >= oneLength || twoIndex >= twoLength){
      one
    } else {
      if ((twoIndex + twoStep) > twoLength){
        step = twoLength - twoIndex
      }
      val array = two.toArray
      for (index <- twoIndex until (twoIndex + step)) {
        one += array(index)
      }
      one
    }
  }

  def fitness(gen:SortedSet[Int]):Int = {
    val P = ArrayBuffer[Int](40,60,10,10,3,20,60)
    val length = gen.size
    var F =0
    val list = gen.toList
    for (index <- 0 until length) {
      F = F + P(list(index))
    }
    F
  }

  def mutate(gen:SortedSet[Int],option:Int,genLength:Int):SortedSet[Int]={
    option match {
      case 1 => {
        val length = gen.size
        val index = Random.nextInt(length)
        val value = gen.toList(index)
        gen.remove(value)
        gen
      }
      case 2 => {
        var flag = true
        while(flag){
          val value = Random.nextInt(genLength)
          if (!gen.contains(value)){
            gen.+=(value)
            flag = false
          }
        }
        gen
      }
      case _ => {
        val length = gen.size
        val index = Random.nextInt(length)
        val value = gen.toList(index)
        gen.remove(value)
        gen
      }
    }
  }

  def initPopulation(groupNum:Int,gensLength:Int):ArrayBuffer[SortedSet[Int]]={
    val population = ArrayBuffer[SortedSet[Int]]()
    for (_ <- 0 until groupNum){
      population += encodingGens(gensLength)
    }
    population
  }

  def rotateSelect(gens:ArrayBuffer[SortedSet[Int]]):(Float,ArrayBuffer[SortedSet[Int]])={
    val length = gens.length
    var totalFitness = 0.0f
//    val min = fitness(gens.minBy(gen => {
//      fitness(gen)
//    }))
    val gensArray = gens.toSet.toArray
    gensArray.map(gen => {
      totalFitness = totalFitness + fitness(gen)
    })
    var addFitness =0.0f
    val rotateBuffer = ArrayBuffer[Float]()
    gensArray.map(gen => {
      addFitness = addFitness + fitness(gen)
      rotateBuffer += (addFitness/totalFitness)
    })

    val buffer = ArrayBuffer[SortedSet[Int]]()
    for (i <- 0 until length/2){
      val point = Random.nextFloat()
      val index = rotateBuffer.indexWhere(f => f>point)
      buffer += gensArray(index)
    }

    val max = buffer.maxBy(gen => {
      val value = fitness(gen)
      value
    })
    var _totalFitness = 0.0f
    buffer.map(gen => {
      _totalFitness = _totalFitness + fitness(gen)
    })

    val avg = _totalFitness/(length/2)
    (avg / fitness(max),buffer)
  }

}
