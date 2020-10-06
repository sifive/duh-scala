package sifive.duhlib.design

import scala.collection.mutable.{LinkedHashMap, ListBuffer}
import sifive.enterprise.util.JsonUtil
import java.io._

import scala.collection.mutable

class DuhDesign (vendor:String, library:String, name:String, version:String) {

  var componentMap= scala.collection.mutable.Map[String,Map[String, Any]]()
  var designMap= scala.collection.mutable.Map[String,scala.collection.mutable.Map[String, Any]]()
  designMap("design")= scala.collection.mutable.LinkedHashMap[String, Any]("vendor" -> vendor, "library" -> library, "name" -> name, "version" -> version)


  designMap("design")("connections") = new ListBuffer[LinkedHashMap[String,Any]] ()
  designMap("design")("instances") = new ListBuffer[LinkedHashMap[String,Any]] ()
  designMap("refs") = new LinkedHashMap[String,Any] ()

  def addConnection(src: String, src_intf:String, tgt:String, tgt_intf:String): Unit = {

    //TODO: check that src/tgt have been added to the design and that they have interface src_intf/tgt_intf

    val source = Seq(src, src_intf)
    val target = Seq(tgt, tgt_intf)

    val connectMap = LinkedHashMap("source"->source, "target"->target).asInstanceOf[LinkedHashMap[String,Any]]
    val myConns  = designMap("design")("connections").asInstanceOf[ListBuffer[LinkedHashMap[String,Any]]]
    myConns += connectMap

  }

  def addComponent (ctype:String, vendor:String, library:String, name:String, version:String) = {
    val vlnv = LinkedHashMap ("vendor" -> vendor, "library" -> library, "name" ->  name, "version" -> version)

    //rMap is the map that goes into the reference section of the design doc
    val rMap = LinkedHashMap(ctype -> vlnv)

    val refsMap = designMap("refs").asInstanceOf[LinkedHashMap[String,Any]]
    refsMap ++= rMap

  }


  def addInstance(instance:String, ctype:String) = {

    //need to store the component instance info and the reference to the component type that we're using
    //need to store the connection info
    val compMap = LinkedHashMap("name" -> instance, "ref" -> Map ("$ref" -> "#/refs/".concat(ctype))).asInstanceOf[LinkedHashMap[String,Any]]

    val myInstances = designMap("design")("instances").asInstanceOf[ListBuffer[LinkedHashMap[String,Any]]]
    myInstances += compMap

  }

  def toJsonString(): String = {

    JsonUtil.toJson(designMap,true)
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
