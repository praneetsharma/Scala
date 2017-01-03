

object MixinClassExample {
  def main(args: Array[String]): Unit = {
    val genTrain = new GeneralTrain
    genTrain.printTrainDetail
    
    println("---")
    
    val slowTrain = new SlowTrain
    slowTrain.printTrainDetail
    println(slowTrain.isSpecialTrain)
    
    println("---")
    
    val fastTrain = new FastTrain
    fastTrain.printTrainDetail
    println(fastTrain.isSpecialTrain)
    fastTrain.printInfo
  }  
}

abstract class AbsTrain {
  def isSpecialTrain: Boolean
}

trait MyTrain {
  def printInfo: Unit = {
    println("TRAIN 12345: FAST EXPRESS")
  }
}

class GeneralTrain {
  def printTrainDetail: Unit = {
    println("General Train")
  }
}

class SlowTrain extends AbsTrain {
  def isSpecialTrain: Boolean = {
    false
  }
  
  def printTrainDetail: Unit = println("Slow Train")
}

class FastTrain extends AbsTrain with MyTrain {
  def isSpecialTrain: Boolean = true
  
  def printTrainDetail: Unit = println("Fast Train")
}