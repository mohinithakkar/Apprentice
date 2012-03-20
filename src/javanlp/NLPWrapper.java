package javanlp;

import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import edu.stanford.nlp.pipeline.*;
import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation;
import edu.stanford.nlp.trees.semgraph.SemanticGraph;
import edu.stanford.nlp.trees.semgraph.SemanticGraphCoreAnnotations.CollapsedCCProcessedDependenciesAnnotation;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.ling.CoreAnnotations.*;
import edu.stanford.nlp.ling.*;

public class NLPWrapper {

	Properties props = null;
	StanfordCoreNLP pipeline;
	List<CoreMap> sentences;
	Iterator<CoreMap> sentIt;
	CoreMap curSentence;

	public NLPWrapper() {
		// creates a StanfordCoreNLP object, with POS tagging, lemmatization,
		// NER, parsing.
		props = new Properties();
		props.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse");
		pipeline = new StanfordCoreNLP(props);
	}

	public void getParsed(String text) {
		
		// create an empty Annotation just with the given text
		Annotation document = new Annotation(text);

		// run all Annotators on this text
		pipeline.annotate(document);
		sentences = document.get(SentencesAnnotation.class);
		sentIt = sentences.iterator();
	}

	public boolean hasNextSentence() {
		return sentIt.hasNext();
	}

	public void processNextSentence() {
		curSentence = sentIt.next();
	}

	public String[][] getTokens() {
		List<CoreLabel> tokenList = curSentence.get(TokensAnnotation.class);
		String[][] ans = new String[tokenList.size()][4];
		int i = 0;
		Iterator<CoreLabel> tokit = tokenList.iterator();
		while (tokit.hasNext()) {
			CoreLabel token = tokit.next();
			// this is the text of the token
			String word = token.get(TextAnnotation.class);
			// this is the POS tag of the token
			String pos = token.get(PartOfSpeechAnnotation.class);
			String lemma = token.get(LemmaAnnotation.class);
			// this is the NER label of the token
			String ne = token.get(NamedEntityTagAnnotation.class);
			ans[i][0] = word;
			ans[i][1] = pos;
			ans[i][2] = lemma;
			ans[i][3] = ne;
			i++;
		}
		return ans;
	}

	public Tree getParseTree() {
		// this is the parse tree of the current sentence
		return curSentence.get(TreeAnnotation.class);
	}

	public SemanticGraph getSemanticGraph() {
		// SemanticGraph dependencies =
		// sentence.get(CollapsedCCProcessedDependenciesAnnotation.class);
		return curSentence
				.get(CollapsedCCProcessedDependenciesAnnotation.class);
	}
	
//	public void main(String[] args)
//	{
//		init();
//		getParsed("This is a sentence. \n Tom orders a cake.\n Jerry ordered a cake. \n");
//		while(hasNextSentence())
//		{
//			processNextSentence();
//			String[][] t = getTokens();
//			for(int i = 0; i < t.length; i++)
//			{
//				for(int j = 0; j < 4; j++)
//					System.out.print(t[i][j] + " ");
//				
//				System.out.println();
//			}
//			
//		}
//	}
}
