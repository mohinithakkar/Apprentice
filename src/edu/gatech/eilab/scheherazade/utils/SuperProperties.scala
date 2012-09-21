package edu.gatech.eilab.scheherazade.utils

import java.util.Properties
import java.io._

/**
 * This is a single property
 *
 */
class SingleProperty extends Properties {

  def printParameterValues(pw: PrintWriter) {

    val it = this.entrySet().iterator()
    while (it.hasNext) {
      val entry = it.next()
      val value = entry.getValue().toString
      pw.print(value + ", ")
    }
  }

  def doubleParam(name: String) = paramOrFail(name, x => x.toDouble)
  def intParam(name: String) = paramOrFail(name, x => x.toInt)
  def param(name: String): String = {
    if (containsKey(name)) {
      val s = getProperty(name)
      s
    } else throw new RuntimeException("FATAL: cannot retrieve parameter " + name)
  }

  def param[T](name: String, conversionFunc: String => T): Option[T] =
    {
      if (containsKey(name)) {
        val s = getProperty(name)
        try {
          Some(conversionFunc(s))
        } catch {
          case e: Exception =>
            System.err.println("illegal parameter value for " + name + " " + s)
            e.printStackTrace()
            None
        }
      } else
        None
    }

  def paramOrFail[T](name: String, conversionFunc: String => T): T =
    {
      val option = param(name, conversionFunc)
      option match {
        case Some(t) => t
        case None => throw new RuntimeException("FATAL: cannot retrieve parameter " + name)
      }
    }
}

/**
 * This class is a property that specifies a range of properties
 * Calling allParameters yields all the properties it specifies
 */
class SuperProperties extends Properties {

  def allParameters(): Array[SingleProperty] =
    {
      val paraNames = getProperty("parameters").split(",")
      var params: Array[SingleProperty] = new Array[SingleProperty](0)
      for (n <- paraNames) {
        val name = n.trim
        // for each parameter listed, we get a list of values
        val values = getProperty(name).split(",")
        if (params.isEmpty) {
          params = values.map { v =>
            val p = new SingleProperty()
            p.setProperty(name, v)
            p
          }
        } else {
          // for each existing Properties object, we append this parameter
          params = params flatMap { param =>
            values.map { v =>
              val p = param.clone().asInstanceOf[SingleProperty]
              p.setProperty(name, v)
              p
            }
          }
        }
      }

      params
    }

  def printParameterNames(pw: PrintWriter) {
    val paraNames = getProperty("parameters").split(",")
    paraNames foreach { name =>
      pw.print(name.trim + ", ")
    }
    pw.println()
  }
}