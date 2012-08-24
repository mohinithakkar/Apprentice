//import java.util.Properties
//import edu.stanford.nlp.pipeline._
//import edu.stanford.nlp.ling.CoreAnnotations._
//import edu.stanford.nlp.ling._
//
//object Test {
//
//  def main(args: Array[String]) {
//	  val text = "John ordered a hamburger."
//	      // creates a StanfordCoreNLP object, with POS tagging, lemmatization, NER, parsing, and coreference resolution 
//    val props = new Properties();
//    props.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse");
//    val pipeline = new StanfordCoreNLP(props)
//    
//    // create an empty Annotation just with the given text
//    val document = new Annotation(text)
//    
//    // run all Annotators on this text
//    pipeline.annotate(document)
//    
//    // these are all the sentences in this document
//    // a CoreMap is essentially a Map that uses class objects as keys and has values with custom types
//    val sentences = document.get(classOf[SentencesAnnotation])
//    
//    sentences foreach { sentence =>
//      // traversing the words in the current sentence
//      // a CoreLabel is a CoreMap with additional token-specific methods
//      sentence.get(TokensAnnotation.`class`) foreach { token =>
//        // this is the text of the token
//        val word = token.get(TextAnnotation.`class`)
//        // this is the POS tag of the token
//        val pos = token.get(PartOfSpeechAnnotation.`class`)
//        // this is the NER label of the token
//        val ne = token.get(NamedEntityTagAnnotation.`class`)     
//      }
//
//      // this is the parse tree of the current sentence
//      Tree tree = sentence.get(TreeAnnotation.`class`)
//
//      // this is the Stanford dependency graph of the current sentence
//      SemanticGraph dependencies = sentence.get(CollapsedCCProcessedDependenciesAnnotation.`class`)
//    }
//  }
//}