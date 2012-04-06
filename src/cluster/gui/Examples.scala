package cluster.gui

import swing._
import event._
import Swing._
import ListView._

object UIDemo extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Scala Swing Demo"
    
    /*
     * Create a menu bar with a couple of menus and menu items and 
     * set the result as this frame's menu bar.
     */
    menuBar = new MenuBar {
      contents += new Menu("A Menu") {
        contents += new MenuItem("An item")
        contents += new MenuItem(Action("An action item") {
          println("Action '"+ title +"' invoked")
        })
        contents += new Separator
        contents += new CheckMenuItem("Check me")
        contents += new CheckMenuItem("Me too!")
        contents += new Separator
        val a = new RadioMenuItem("a")
        val b = new RadioMenuItem("b")
        val c = new RadioMenuItem("c")
        val mutex = new ButtonGroup(a,b,c)
        contents ++= mutex.buttons
      }
      contents += new Menu("Empty Menu")
    }
    
    /*
     * The root component in this frame is a panel with a border layout. 
     */
    contents = new BorderPanel {
      import BorderPanel.Position._
      
      var reactLive = false
      
      val tabs = new TabbedPane {
        import TabbedPane._
        val buttons = new FlowPanel {
          border = Swing.EmptyBorder(5,5,5,5)
          
          contents += new BoxPanel(Orientation.Vertical) {
            border = CompoundBorder(TitledBorder(EtchedBorder, "Radio Buttons"), EmptyBorder(5,5,5,10))
            val a = new RadioButton("Green Vegetables")
            val b = new RadioButton("Red Meat")
            val c = new RadioButton("White Tofu")
            val mutex = new ButtonGroup(a,b,c)
            contents ++= mutex.buttons
          }
          contents += new BoxPanel(Orientation.Vertical) {
            border = CompoundBorder(TitledBorder(EtchedBorder, "Check Boxes"), EmptyBorder(5,5,5,10))
            val paintLabels = new CheckBox("Paint Labels")
            val paintTicks = new CheckBox("Paint Ticks")
            val snapTicks = new CheckBox("Snap To Ticks")
            val live = new CheckBox("Live")
            contents.append(paintLabels, paintTicks, snapTicks, live)
            listenTo(paintLabels, paintTicks, snapTicks, live)
            reactions += {
              case ButtonClicked(`paintLabels`) => 
                slider.paintLabels = paintLabels.selected
              case ButtonClicked(`paintTicks`) => 
                slider.paintTicks = paintTicks.selected
              case ButtonClicked(`snapTicks`) => 
                slider.snapToTicks = snapTicks.selected
              case ButtonClicked(`live`) => 
                reactLive = live.selected
            }
          }
          contents += new Button(Action("Center Frame") { centerOnScreen() })
        }
        pages += new Page("Buttons", buttons) 
        pages += new Page("GridBag", GridBagDemo.ui)
        pages += new Page("Converter", CelsiusConverter2.ui)
        //pages += new Page("Tables", TableSelection.ui)
        //pages += new Page("Dialogs", Dialogs.ui)
        //pages += new Page("Combo Boxes", ComboBoxes.ui)
        pages += new Page("Split Panes", 
          new SplitPane(Orientation.Vertical, new Button("Hello"), new Button("World")) {
            continuousLayout = true
          })
        
        val password = new FlowPanel {
          contents += new Label("Enter your secret password here ")
          val field = new PasswordField(10)
          contents += field
          val label = new Label(field.text)
          contents += label
          listenTo(field)
          reactions += {
            case EditDone(`field`) => label.text = field.password.mkString
          }
        }
        
        pages += new Page("Password", password)
        //pages += new Page("Painting", LinePainting.ui)
        //pages += new Page("Text Editor", TextEditor.ui)
      }
            
      val list = new ListView(tabs.pages) {
        selectIndices(0)
        selection.intervalMode = ListView.IntervalMode.Single
        renderer = ListView.Renderer(_.title)
      }
      val center = new SplitPane(Orientation.Vertical, new ScrollPane(list), tabs) {
        oneTouchExpandable = true
        continuousLayout = true
      }      
      layout(center) = Center 
      
      /*
       * This slider is used above, so we need lazy initialization semantics.
       * Objects or lazy vals are the way to go, but objects give us better 
       * type inference at times.
       */
      object slider extends Slider {
        min = 0
        value = tabs.selection.index
        max = tabs.pages.size-1
        majorTickSpacing = 1
      }
      layout(slider) = South

      /*
       * Establish connection between the tab pane, slider, and list view.
       */
      listenTo(slider)
      listenTo(tabs.selection)
      listenTo(list.selection)
      reactions += {
        case ValueChanged(`slider`) => 
          if(!slider.adjusting || reactLive) tabs.selection.index = slider.value
        case SelectionChanged(`tabs`) => 
          slider.value = tabs.selection.index
          list.selectIndices(tabs.selection.index)
        case SelectionChanged(`list`) =>
          if (list.selection.items.length == 1)
            tabs.selection.page = list.selection.items(0)
      }
    }
  }
}

import swing._
import swing.event._
import GridBagPanel._
import java.awt.Insets

object GridBagDemo extends SimpleSwingApplication {
  lazy val ui = new GridBagPanel {
    val c = new Constraints
    val shouldFill = true
    if (shouldFill) {
      c.fill = Fill.Horizontal
    }

    val button1 = new Button("Button 1") 
    
    c.weightx = 0.5

    c.fill = Fill.Horizontal
    c.gridx = 0;
    c.gridy = 0;
    layout(button1) = c

    val button2 = new Button("Button 2")
    c.fill = Fill.Horizontal
    c.weightx = 0.5;
    c.gridx = 1;
    c.gridy = 0;
    layout(button2) = c

    val button3 = new Button("Button 3")
    c.fill = Fill.Horizontal
    c.weightx = 0.5;
    c.gridx = 2;
    c.gridy = 0;
    layout(button3) = c

    val button4 = new Button("Long-Named Button 4")
    c.fill = Fill.Horizontal
    c.ipady = 40;      //make this component tall
    c.weightx = 0.0;
    c.gridwidth = 3;
    c.gridx = 0;
    c.gridy = 1;
    layout(button4) = c

    val button5 = new Button("5")
    c.fill = Fill.Horizontal
    c.ipady = 0;       //reset to default
    c.weighty = 1.0;   //request any extra vertical space
    c.anchor = Anchor.PageEnd
    c.insets = new Insets(10,0,0,0);  //top padding
    c.gridx = 1;       //aligned with button 2
    c.gridwidth = 2;   //2 columns wide
    c.gridy = 2;       //third row
    layout(button5) = c
  }
  
  def top = new MainFrame {
    title = "GridBag Demo"
    contents = ui
  }
}

object CelsiusConverter2 extends SimpleSwingApplication {
  def newField = new TextField { 
    text = "0"
    columns = 5
    horizontalAlignment = Alignment.Right
  }
  val celsius = newField
  val fahrenheit = newField
  
  listenTo(fahrenheit, celsius)
  reactions += {
    case EditDone(`fahrenheit`) =>
      val f = Integer.parseInt(fahrenheit.text)
      val c = (f - 32) * 5 / 9
      celsius.text = c.toString
    case EditDone(`celsius`) =>
      val c = Integer.parseInt(celsius.text)
      val f = c * 9 / 5 + 32
      fahrenheit.text = f.toString
  }
  
  lazy val ui = new FlowPanel(celsius, new Label(" Celsius  =  "), 
                              fahrenheit, new Label(" Fahrenheit")) {
    border = Swing.EmptyBorder(15, 10, 10, 10)
  }
  def top = new MainFrame {
    title = "Convert Celsius / Fahrenheit"
   	contents = ui
  }
}