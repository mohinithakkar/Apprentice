package edu.gatech.eilab.scheherazade.generation;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Scanner;

import edu.stanford.nlp.pipeline.*;
import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation;
import edu.stanford.nlp.trees.semgraph.SemanticGraph;
import edu.stanford.nlp.trees.semgraph.SemanticGraphCoreAnnotations.CollapsedCCProcessedDependenciesAnnotation;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.ling.CoreAnnotations.*;
import edu.stanford.nlp.ling.*;

public class Lemmatizer {

	Properties props = null;
	StanfordCoreNLP pipeline;
	List<CoreMap> sentences;
	Iterator<CoreMap> sentIt;
	CoreMap curSentence;

	public Lemmatizer() {
		// creates a StanfordCoreNLP object, with POS tagging, lemmatization,
		// NER, parsing.
		props = new Properties();
		props.put("annotators", "tokenize, ssplit, pos, lemma");
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

	public String[] getLemma() {
		List<CoreLabel> tokenList = curSentence.get(TokensAnnotation.class);
		String[] ans = new String[tokenList.size()];
		int i = 0;
		Iterator<CoreLabel> tokit = tokenList.iterator();
		while (tokit.hasNext()) {
			CoreLabel token = tokit.next();
			String lemma = token.get(LemmaAnnotation.class);
			ans[i]= lemma;
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
	
	public void storeWordLemmas(int fno,Lemmatizer lm)
	{
			String fcontents=readFile(fno);
			lm.getParsed(fcontents);
			while(lm.hasNextSentence())
			{	lm.processNextSentence();
				String[] data = lm.getLemma();
				writeFile(fno,data);
			}
	}
	
	
	
	public void writeFile(int fno,String data[])
	{
		try {
	        String fname="lemmatizedBooks/"+fno+".txt";
	        FileWriter fw=new FileWriter(fname);
	    	PrintWriter writer=new PrintWriter(new BufferedWriter(fw));
		    	for(int i = 0; i < data.length; i++)
				{
			    	writer.print(data[i] + " ");
				}
	    	System.out.println("File "+fno+" created");
	    	writer.close();
		} 
		catch (Exception e) 
		{
			e.printStackTrace();
		}
	}
	
	public String readFile(int fno)
	{
		try {
			String fname="gbooks/"+fno+".txt";
			FileInputStream inputStream=new FileInputStream(fname);
	        String inputStreamString = new Scanner(inputStream,"UTF-8").useDelimiter("\\A").next();
	        inputStreamString=inputStreamString.replace(".", "").replace(",", "").replace("?", "").replace("!","").replace(";","").replace("\"", "").toLowerCase();
	        //System.out.println(inputStreamString);
	        return inputStreamString;
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		return null;
	}
	
	public static void main(String[] args)
	{
		Lemmatizer lm=new Lemmatizer();
		for(int i=11049;i< 11540; i++)
		{	File f=new File("gbooks/"+i+".txt");
			if(f.exists())
			{lm.storeWordLemmas(i,lm);
			}
		}
		
	}
}
