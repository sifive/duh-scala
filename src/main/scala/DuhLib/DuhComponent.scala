package sifive.duhlib.component

import scala.collection.mutable.{LinkedHashMap, ListBuffer}
import sifive.enterprise.util.JsonUtil
import java.io._

import scala.collection.mutable

class DuhComponent (vendor:String, library:String, name:String, version:String) {

  val vlnv = LinkedHashMap [String, String] ("vendor" -> vendor, "library" -> library, "name" ->  name, "version" -> version)
  var ports = LinkedHashMap[String,String]()
  val interfaceList = new ListBuffer[LinkedHashMap[String,Any]] ()



  def addPorts (ports: LinkedHashMap[String,String]) = {
    //we want addPorts to be able to be called multuple times - hence we use the ++ apend operator
    this.ports ++= ports
  }

  def addInterface (vendor:String,
                    library:String,
                    name:String,
                    version:String,
                    instanceName: String,
                    isMaster:Boolean,
                    portMap:LinkedHashMap[String,String],
                    associations:LinkedHashMap[String,String]) = {

    //add an interface instance to our list of interface instances.
    //goal is to abstact away all of the scala casting and dictionary manipulation + duh formatting into something far easier to use
    val imode = if (isMaster) "master" else "slave"

    val busType = LinkedHashMap ("vendor" -> vendor, "library" -> library, "name" -> name, "version" -> version)


    val abstraction = Map("viewRef" -> "RTLview", "portMaps" -> portMap)
    val abstractions = List (abstraction)
    //return the linkedhashmap

    interfaceList += LinkedHashMap ("name"-> instanceName, "interfaceMode" -> imode, "busType"->busType, "associations" -> associations, "abstractionTypes" -> abstractions)

  }

  def getComponent() : Map[String,Any] = {

    val cMap = Map("component" -> (vlnv ++ LinkedHashMap ("test" -> "test", "model" -> Map("ports"-> ports), "busInterfaces" -> interfaceList)))
    //return it
    cMap.asInstanceOf[Map[String,Any]]
  }

  def toJsonString(): String = {

    JsonUtil.toJson(getComponent(),true)
  }

  def toJson (): Unit = {
    println(toJsonString())
  }

  def toJsonFile(filename:String): Unit = {

    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(toJsonString())
    bw.close()

  }

}
