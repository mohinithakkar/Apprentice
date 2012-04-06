package cluster.gui
import swing._
import event._
import Swing._
import ListView._
import java.awt.Color
import cluster.algo.OPTICS.Point

class ReachPlot(points: Array[Point]) {

  val colors = Array(Color.BLUE, Color.CYAN, Color.RED, Color.GREEN, Color.MAGENTA, Color.ORANGE)
  val array = points.map(p => if (p.reachability == Double.PositiveInfinity) 0 else p.reachability)
  println(array.mkString(" "))
  val drawPanel = new BarChartPanel(array)

  val textField = new TextField(" " * 40)
  val confirmCheck = new CheckBox("Don't Bother")
  val fastCheck = new CheckBox("Show final")

  var ack = false
  var waitForUser = true
  var top: MainFrame = null

  def show() {

    var mainPanel = new BorderPanel() {
      import BorderPanel.Position._

      add(drawPanel, Center)

      val bottom = new FlowPanel() {

        val button = new Button("Got it")

        textField.enabled = false
        textField.preferredSize = new Dimension(200, 20)

        contents += textField
        contents += button
        contents += confirmCheck
        contents += fastCheck

        listenTo(button)

        reactions += {
          case ButtonClicked(b) =>
            if (b == button)
              ack = true
        }
      }

      add(bottom, South)
    }

    //drawPanel.size = (new Dimension(600, 600))
    top = new MainFrame {
      title = "Reachability Plot"
      contents = mainPanel
    }
    top.minimumSize = new Dimension(600, 600)
    //top.visible = true
    drawPanel.highlight(3)

    top.visible = true
  }

  def markPoints(regions: List[List[Point]]) {

    for (i <- 0 until regions.length) {
      val color = colors(i % colors.length)
      for (p <- regions(i)) {
        drawPanel.mark(points.indexOf(p), color)
      }
    }
    top.repaint()
  }

  def sendMessage(text: String) {
    textField.text = text
    if (!confirmCheck.selected) {
      ack = false
      while (!ack) { Thread.sleep(500) }
    } else if (!fastCheck.selected) {
      Thread.sleep(500)
    }
  }

  def markRegions(regions: List[(Int, Int)]) {
    var i = 0
    for (r <- regions) {
      val color = colors(i % colors.length)
      drawPanel.markRegion(r._1, r._2, color)
      i += 1
    }
    top.repaint()
  }

  def markRegionalPoints(regions: List[List[Int]]) {
    var i = 0
    for (r <- regions) {
      val color = colors(i % colors.length)
      for (i <- r)
        drawPanel.mark(i, color)
      i += 1
    }
    top.repaint()
  }

  def disableRegions(regions: List[(Int, Int)]) {
    for (r <- regions) {
      drawPanel.markRegion(r._1, r._2, Color.GRAY)
    }
    top.repaint()
  }

  def unmarkRegions(regions: List[(Int, Int)]) {
    for (r <- regions) {
      drawPanel.markRegion(r._1, r._2, Color.black)
    }
    top.repaint()
  }

  def unmarkAll() {
    for (i <- 0 until colors.length)
      colors(i) = Color.BLACK

    top.repaint()
  }
}

class BarChartPanel(var values: Array[Double], var colors: Array[Color]) extends Panel {

  val initDimension = new Dimension(900, 600)

  minimumSize = initDimension
  preferredSize = initDimension

  def this(values: Array[Double]) = this(values, Array.fill(values.length)(java.awt.Color.black))

  override def paint(g: Graphics2D): Unit = {
    val d = this.size
    val clientWidth = d.width
    val clientHeight = d.height
    val barWidth = clientWidth / values.length

    //println(clientHeight + ", " + clientWidth)
    val max = values.fold[Double](0) { (a: Double, b: Double) => if (a > b) a else b }
    val min = values.fold[Double](Double.PositiveInfinity) { (a: Double, b: Double) => if (a < b) a else b }
    val top = 30
    val scale = clientHeight * 0.9 / (max - min)

    //println("scale = " + scale)
    for (i <- 0 until values.length) {
      var valueX = i * barWidth + 1;
      var valueY = top;
      var height = (values(i) * scale).toInt;
      if (values(i) >= 0)
        valueY += ((max - values(i)) * scale).toInt
      else {
        valueY += (max * scale).toInt
        height = -height;
      }

      //g.setColor(colors(i));
      g.setColor(colors(i));
      g.fillRect(valueX, valueY, barWidth - 2, height);
      g.setColor(colors(i));
      g.drawRect(valueX, valueY, barWidth - 2, height);
    }
  }

  def highlight(i: Int) {
    colors(i) = Color.red
  }

  def mark(i: Int, color: Color) {
    colors(i) = color
  }

  def dehighlight(i: Int) {
    colors(i) = Color.black
  }

  def markRegion(start: Int, end: Int) {
    for (i <- start to end)
      highlight(i)
  }

  def demarkRegion(start: Int, end: Int) {
    for (i <- start to end)
      dehighlight(i)
  }

  def markRegion(start: Int, end: Int, color: Color) {
    for (i <- start to end)
      colors(i) = color
  }

}







