package edu.gatech.eilab.scheherazade.utils

import collection.mutable.ArrayBuffer

class Calculator(){
  val numbers = new ArrayBuffer[Double]();
   
  def add(number:Seq[Double]):Unit = {
    this.numbers ++= number
  }
  
  def add(number:Double):Unit = {
    numbers += number;
  }
  def avg():Double = {
      var sum:Double = 0.0;
      for(x:Double <- numbers){
        sum = sum + x;
      }
      sum/numbers.length;
  }
   // Thanks to Christopher Martin
  def avg_short:Double = numbers.reduceLeft(_ + _) / numbers.length

  def stdDev():Double = {
      var sum:Double    = 0.0;
      if(numbers.length>=2){
        val mean = avg();
        val factor:Double = 1.0/(numbers.length.toDouble-1);
        for(x:Double <- numbers){
          sum = sum + ((x-mean)*(x-mean));
        }
        sum = sum * factor;
      }
      return math.sqrt(sum);
  }
  def print():String = {
    var result:String = "";
    result = result + "Calculator -> ";
    for(x <- numbers) result = result + x +" ";
    return result;
  }
  def reset():Unit = {
    numbers.clear();
  }
}
