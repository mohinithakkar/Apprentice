package edu.gatech.eilab.scheherazade.generation;

import java.util.HashMap;
import java.util.Map;

import edu.stanford.nlp.tagger.maxent.MaxentTagger;

import simplenlg.framework.*;
import simplenlg.lexicon.*;
import simplenlg.realiser.english.*;
import simplenlg.phrasespec.*;
import simplenlg.features.*;

public class POSMain {

	public static void main(String[] args) 
	{
		
		
		// The sample string
		 
		String sample[] = new String[15];
		sample[0]="Sally greeted John";
		sample[1]="John opens bank door"; 
		sample[2]="John sees Sally";
		sample[3]="John gives Sally bag"; 
		sample[4]="John demanded money";
		sample[5]="Sally puts money in bag";
		sample[6]="John takes the bag";
		sample[7]="John leaves bank";
		sample[8]="Police arrests John";
		sample[9]="John drives to bank";
		sample[10]="The note demands money"; 
		sample[11]="John points gun at Sally";
		sample[12]="Sally scared";
		sample[13]="John and Sally leave";
		sample[14]="Jim and Sally leave";
		
		POSMain pos=new POSMain();
		pos.convertToFirstPerson(sample);
        
	}
	
	
	public String[] convertToFirstPerson(String[] input) 
	{String output[]=new String[input.length];
		try
		{
		Map<String,String> exceptionwords=new HashMap<String,String>();
		exceptionwords.put("scareded", "scared");
		
		Lexicon lexicon = Lexicon.getDefaultLexicon();
        NLGFactory nlgFactory = new NLGFactory(lexicon);
        Realiser realiser = new Realiser(lexicon);
		MaxentTagger tagger = new MaxentTagger("taggers/wsj-0-18-bidirectional-distsim.tagger");
		
			for(int k=0;k<input.length;k++)
			{
			String tagged = tagger.tagString(input[k]);
			
			//System.out.print("Tagged :"+tagged);
			
	        SPhraseSpec p = nlgFactory.createClause();
	        
			String[] splits = tagged.split(" ");
			String m="the ";
			String firstsubj="";
			boolean conjunction=false;
			boolean theobj=false;
			int n=0;
			String prevobj="";
				for(String x: splits)
				{n++;
					if(x.contains("_NNP") && n<4)
					{ int i=x.indexOf("_");
						if(n==1)
						{String subj=x.substring(0, i);
							if(subj.equals("John"))
					        p.setSubject("I");
							else
							p.setSubject(subj);	
						firstsubj=subj;
						}
						else if(conjunction && n==3)
						{String subj2=x.substring(0, i);
							if(firstsubj.equals("John"))
							p.setSubject("We");
							else
							{	
							NPPhraseSpec subject1 = nlgFactory.createNounPhrase(firstsubj);
					        NPPhraseSpec subject2 = nlgFactory.createNounPhrase(subj2);
			
						    CoordinatedPhraseElement sub = nlgFactory.createCoordinatedPhrase(subject1, subject2);
						    p.setSubject(sub);	
							}
						}
						else if(n==3 && splits.length==3)
						{String subj=x.substring(0, i);
							if(subj.equals("John"))
					        p.setObject("me");
							else
							p.setObject(subj);	
						}
						else if(n==3)
						{String subj="to "+x.substring(0, i);
							if(subj.equals("to John"))
					        subj="to me";
						p.addComplement(subj);	
						}
					}
					else if(x.contains("_CC") && n==2)
					{conjunction=true;
					}
					else if(x.contains("_DT") && n==1)
					{theobj=true;
					}
					else if(x.contains("_VB") || x.contains("_NNS"))
					{ int i=x.indexOf("_");
					String verb=x.substring(0, i);
					p.setVerb(verb);
					}
					else if(x.contains("_IN") || x.contains("_TO") )
					{ int i=x.indexOf("_");
					String preposition=x.substring(0, i);
						if(preposition.equals("at"))
							m=" "+preposition+" ";
						else
							m=" "+preposition+" the ";
					}
					else if(x.contains("_NN"))
					{ int i=x.indexOf("_");
					String obj=x.substring(0, i);
						if(theobj)
						{p.setSubject("The "+obj);
						theobj=false;
						}
						else if(n==1)
							p.setSubject(obj);
						else if(prevobj=="" || !m.equals("the "))
							prevobj+= m + obj;
						else	
							prevobj+= " " +obj;
							
					p.setObject(prevobj);
					}
					else if(x.contains("_RB"))
					{ int i=x.indexOf("_");
					String a=x.substring(0, i);
					p.addComplement(a);
					}
				}
			
	        
	        p.setFeature(Feature.TENSE, Tense.PAST);
	        
	        output[k] = realiser.realiseSentence(p); 
	        
	        	for(Map.Entry<String,String> entry:exceptionwords.entrySet())
	        	{
		        	if(output[k].contains(entry.getKey()))
		        	{output[k]=output[k].replace(entry.getKey(), entry.getValue());
		        	break;
		        	}
	        	}
	        System.out.println(output[k]);
			}
	        
        return output;
		}
		catch(Exception e)
		{System.err.println(e);
		}
		
	return output;	
	}
}
