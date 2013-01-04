package edu.gatech.eilab.scheherazade

import main._

package utils {

  /** find unused sentences, which are sentences that exist 
   * in the stories but not included in the gold standard
   * 
   */
  object UnusedSentences {

    def main(args: Array[String]) {
      val reader = new ConfigReader("configAir.txt")
      val (stories, clusters) = reader.initData
      
      val sent1 = stories.flatMap(_.members)
      val sent2 = clusters.flatMap(_.members)
      
      val unused = sent1.filterNot(sent2 contains)
      
      println(unused.map(_.toSimpleString()).mkString("\n"))
    }
  }
}