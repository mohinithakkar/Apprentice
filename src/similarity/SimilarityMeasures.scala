package similarity

abstract class SimilarityMeasure {

  //def init()
  def similarity(word1: String, word2: String): Double
}

object DISCO extends SimilarityMeasure {
  import de.linguatools.disco.DISCO
  import de.linguatools.disco.ReturnDataBN
  import de.linguatools.disco.ReturnDataCol

  val discoDir = "../../en-wikipedia-20080101/"
  var disco = new DISCO(discoDir, false)

  def similarity(word1: String, word2: String): Double = {
    disco.firstOrderSimilarity(word1, word2) * math.sqrt(8)
  }

  private def existsCollocation(mainWord: String, word2: String): Option[Double] = {
    val collocation1 = disco.collocations(discoDir, mainWord)
    if (collocation1 != null)
      collocation1.find { _.word == word2 }.map { _.value.toDouble }
    else
      None
  }
}

object Lin extends SimilarityMeasure {
  import edu.cmu.lti.lexical_db.NictWordNet
  import edu.cmu.lti.ws4j.util.WS4JConfiguration;
  import edu.cmu.lti.ws4j.impl._

  val db = new NictWordNet();
  WS4JConfiguration.getInstance().setMFS(true)
  val lin = new Lin(db)

  def similarity(word1: String, word2: String): Double = {
    lin.calcRelatednessOfWords(word1, word2)
  }
}

object Resnik extends SimilarityMeasure {
  import edu.cmu.lti.lexical_db.NictWordNet
  import edu.cmu.lti.ws4j.util.WS4JConfiguration;
  import edu.cmu.lti.ws4j.impl.Resnik

  val db = new NictWordNet();
  WS4JConfiguration.getInstance().setMFS(true)
  val lin = new Resnik(db)

  def similarity(word1: String, word2: String): Double = {
    lin.calcRelatednessOfWords(word1, word2)
  }
}

object Vector extends SimilarityMeasure {
  import edu.cmu.lti.lexical_db.NictWordNet
  import edu.cmu.lti.ws4j.util.WS4JConfiguration;
  import edu.cmu.lti.ws4j.impl._

  val db = new NictWordNet();
  WS4JConfiguration.getInstance().setMFS(true)
  val vec = new WuPalmer(db)

  def similarity(word1: String, word2: String): Double = {
    vec.calcRelatednessOfWords(word1, word2)
  }
}