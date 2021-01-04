package base

import scala.collection.immutable.HashSet
import scala.util.Random
import scalax.collection.{Graph}
import scalax.collection.GraphPredef._
import scalax.collection.edge.{WUnDiEdge}
import scalax.collection.edge.Implicits._
import scala.util.control._


object GraphEncoding extends App{

  val g = getGraphX
  val spanG = spanTree(g)
  println(spanG)
  val spanG1 = spanTree(g)
  println(spanG1)
  val primG = prim(g)
  println(primG)
  println("------------")

  val (pgg1,ws) = pruferEncoding(primG)
  println(pgg1)
  val pdg = pruferDecoding(pgg1,ws)
  println(pdg)
  println(primG.get(1~2 % -1))
  println("------------")

  val (one,two) = acrossover(spanG,spanG1)
  println(one + s"${fitness(one)}")
  println(two + s"${fitness(two)}")

  val graphM = mutate(spanG,g)
  println(graphM+ s"${fitness(graphM)}")
  println("===============")

  main(20,20)

  def getGraphX():Graph[Int, WUnDiEdge]={
    Graph(1~2 % 1, 2~3 % 2, 3~4 % 3, 1~5  % 8,
      5~6 % 9, 6~4 % 10, 3~6 % 7, 2~5 % 4,
      2~6 % 5, 3~5 % 6,3~8 % 6,4~8 % 5,
      4~7 % 4,6~7 % 3,1~9 % 6,5~9 % 4,6~9 % 7,9~7 % 8,
      9~10 % 4,7~10 % 6,10~11 % 3,11~7 % 5)
  }

  //不太适合做mst，随机性不够。
  def kruskal(graph:Graph[Int, WUnDiEdge])={
  }

  //从一个随机起始点开始，沿着点不断的向邻居的点所相关的边扩散。
  def spanTree(graph:Graph[Int, WUnDiEdge]):Graph[Int, WUnDiEdge]={
    val list = graph.nodes.toList
    val gLength = list.length
    val rd = Random.nextInt(gLength)
    val node = list(rd)

    var g = Graph[Int, WUnDiEdge]()
    val neighbors = Random.shuffle(node.neighbors)
    for(n <- neighbors){
      g = span(n,g)
    }

    def span(node:graph.NodeT,g:Graph[Int, WUnDiEdge]):Graph[Int, WUnDiEdge]={
      if ((list.length - g.edges.size)==1){
        return g
      }
      var graph = g
      val neighbors = Random.shuffle(node.neighbors)
      for(n <- neighbors if !graph.nodes.contains(n)){
        val edge = node.findOutgoingTo(n)
        val _g = g.+(edge.get)
        if (_g.isAcyclic){
          graph = _g
          graph = span(n,graph)
        }
      }
      graph
    }
    g
  }

  //prim算法生成mst
  def prim(graph:Graph[Int, WUnDiEdge]):Graph[Int, WUnDiEdge]={
    val list = graph.nodes.toList
    val nodeSize = list.length
    val rd = Random.nextInt(nodeSize)
    val node = list(rd)

    var g = Graph[Int, WUnDiEdge]()
    val edges = node.edges.toList
    val weightEdges = edges.sortWith((p1,p2) => {
      p1.weight < p2.weight
    })
    val edge = weightEdges.head
    g = g.+(edge)
    var edgeSize = g.edges.size
    while((nodeSize-edgeSize)!=1){
      g = span(g)
      edgeSize = g.edges.size
    }

    def span(g:Graph[Int, WUnDiEdge]):Graph[Int, WUnDiEdge]={
      val nodeList = g.nodes.toList
      var edgeSet = HashSet[Graph[Int, WUnDiEdge]#EdgeT]()
      for (n <- nodeList) {
        val edgeList = graph.get(n.value).edges.toList //需要从最初的图里面去选取
        edgeSet = edgeSet.++(edgeList)
      }
      edgeSet = edgeSet.filter(ele => {!g.edges.contains(ele)})
      val edgeList = edgeSet.toList.sortWith((p1,p2)=>{
        p1.weight < p2.weight
      })
      var _g = g
      val loop = new Breaks
      loop.breakable{
        for (edge <- edgeList) {
          val tg = g.+(edge)
          if (tg.isAcyclic) {
            _g = tg
            loop.break()
          }
        }
      }
      _g
    }
    g
  }

  //便于分布式环境下，在网络上传输。
  def pruferEncoding(graph:Graph[Int, WUnDiEdge]):(List[Int],List[Double])={

    var seq = List[Int]()
    var weights = List[Double]()
    var g = graph
    while (g.edges.size > 1) {
      val nodes = g.nodes.toList.sortWith((p1,p2)=>{
        p1.value < p2.value
      })
      val loop = new Breaks
      loop.breakable{
        for (node <- nodes) {
          if (node.isLeaf) {
            val value = node.value
            val edges = node.edges.toList
            edges.foreach(e => {
              e.nodes.foreach(n => {
                if (n.value !=  value) {
                  seq = seq.:+(n.value)
                  weights = weights.:+(e.weight)
                  g = g.-(e)
                  loop.break() //一次搜索一个leaf
                }
              })
            })
          }
        }
      }
    }
    weights = weights.:+(g.edges.head.weight)
    (seq,weights)
  }

  def pruferDecoding(seq:List[Int],weights:List[Double]):Graph[Int, WUnDiEdge]={
    var g = Graph[Int, WUnDiEdge]()
    val length = seq.size + 2
    var seq1 = seq
    var seq2 = (1 to length).toList
    var _weights = weights

    seq2 = seq2.filter(p => {
      ! seq.contains(p)
    })
    while (seq1.size >0) {
      val head1 = seq1.head
      val head2 = seq2.head
      val weight = _weights.head
      g = g.+(head1~head2 % weight)

      seq1 = seq1.drop(1)
      seq2 = seq2.drop(1)
      _weights = _weights.drop(1)
      if (!seq1.contains(head1)) {
        seq2 = seq2.+:(head1)
      }
    }
    val head = seq2.head
    val last = seq2.last
    val weight = _weights.head
    g = g.+(head~last % weight)
    g

  }

  def acrossover(graph1:Graph[Int, WUnDiEdge],graph2:Graph[Int, WUnDiEdge]):(Graph[Int, WUnDiEdge],Graph[Int, WUnDiEdge])={
    val graph = graph1.++(graph2)
    (prim(graph),prim(graph))
  }

  //将tree随机删除一条边，使其变得不联通，然后随机加上一条边，变得联通。
  def mutate(tree:Graph[Int, WUnDiEdge],graph:Graph[Int, WUnDiEdge]):Graph[Int, WUnDiEdge] = {
    val loop = new Breaks
    var resultTree = tree
    loop.breakable{
      while (true) {
        val leafs = tree.edges.filter(p => {
          val l = p.toList
          !l.head.isLeaf && !l.last.isLeaf
        })
        val edgeRandom = Random.shuffle(leafs).head
        val graph_1 = tree.-(edgeRandom)
        if (!graph_1.isConnected) {
          val graph_random = graph.-(edgeRandom)
          val edges = Random.shuffle(graph_random.edges.toList)
          for (edge <- edges) {
            val graphPlus1 = graph_1.+(edge)
            if (graphPlus1.isConnected) {
              resultTree = graphPlus1
              loop.break()
            }
          }
        }
      }
    }
    resultTree
  }

  def fitness(graph:Graph[Int, WUnDiEdge]):Double = {
    var _weight:Double = 0
    graph.edges.foreach(e => {
      _weight = _weight + e.weight
    })
    _weight
  }

  def dynamicLinearScalingMin(max:Double,graph:Graph[Int, WUnDiEdge],m:Double,k:Float=0.95f):Double={
    val e = m * k
    val result = max - fitness(graph) + e
    result
  }

  def initPopulation(num:Int):List[Graph[Int, WUnDiEdge]] = {
    var popu = List[Graph[Int, WUnDiEdge]]()
    val graph = getGraphX()
    for (_ <- 0 until num) {
      popu = popu.+:(prim(graph))
//      popu = popu.+:(spanTree(graph))
    }
    popu
  }

  def select(popu:List[Graph[Int, WUnDiEdge]],m:Double,num:Int):List[Graph[Int, WUnDiEdge]] = {
    val size = popu.length
    val maxGraph = popu.maxBy(g => {
      fitness(g)
    })
    val max = fitness(maxGraph)
    val totalFitness = popu.map(p => {
      dynamicLinearScalingMin(max,p,m)
    }).reduce((p1,p2)=>{
      p1 + p2
    })
    val orderPopu = popu.sortBy(g => {
      val value = dynamicLinearScalingMin(max,g,m)
      -value
    })
    var value =0.0
    val orderKeyPopu = orderPopu.map(f => {
      val key = dynamicLinearScalingMin(max,f,m)
      value = value + key/totalFitness
      (value,f)
    })
    var result = List[Graph[Int, WUnDiEdge]]()
    for (_ <- 0 until num) {
      val point = Random.nextDouble()
      val index = orderKeyPopu.indexWhere(p => p._1 > point)
      val individual = orderKeyPopu(index)._2
      result = result.:+(individual)
    }
    result
  }

  def stopCondition(popu:List[Graph[Int, WUnDiEdge]],rate:Double):Boolean={
    val maxGraph = popu.map(p => {
      fitness(p)
    }).max
    val sum = popu.map(p => {
      fitness(p)
    }).reduce((p1,p2) => {
      p1+p2
    })
    val mean = sum / (popu.size)

    println(s"mean/max ${mean / maxGraph}")
    if (mean / maxGraph > rate) {
      true
    } else {
      false
    }
  }

  def main(num:Int,genNum:Int)={
    var popu = initPopulation(num)
    var flag = false
    var rate = 30.0
    for (_ <- 0 until genNum if !flag) {
      rate = 0.95 * rate
      for (index <- 0 until num/2 if !flag) {
        val individual1 = popu(index)
        val individual2 = popu(index + num/2)
        val (xIndividual1,xIndividual2) = acrossover(individual1,individual2)
        popu = popu.:+(xIndividual1)
        popu = popu.:+(xIndividual2)

        val random = Random.nextInt(100)
        if (random <5) {
          val individual = popu(Random.nextInt(num))
          val mIndividual = mutate(individual,getGraphX())
          popu = popu.:+(mIndividual)
        }
      }
      popu = select(popu,rate,num)
//      println(popu)
      println(popu.head)
      popu.foreach(p => {
        print(fitness(p) + " . ")
      })
      flag = stopCondition(popu,0.975)
    }
  }

}
