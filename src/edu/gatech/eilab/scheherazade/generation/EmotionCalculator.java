package edu.gatech.eilab.scheherazade.generation;

import java.io.*;
import java.util.*;
import org.apache.commons.math3.distribution.NormalDistribution;

public class EmotionCalculator {

	/**
	 * @param args
	 */
	public static void main(String[] args) 
	{	SWN3 sw=new SWN3();
		EmotionCalculator ec=new EmotionCalculator();
		System.out.println("Starting:");
		ec.getEmotion("love", sw);    
		ec.getEmotion("hate", sw);
	}
	
	public Double getEmotion(String word,SWN3 sw)
	{	long startTime = System.currentTimeMillis(); 
		Double pos=getEmotionValue(word,sw,true);
		Double neg=getEmotionValue(word,sw,false);
		System.out.println(word+":"+(pos-neg));
		long estimatedTime = System.currentTimeMillis()- startTime;
		System.out.println("Took:"+ estimatedTime/1000 + "seconds");
		return (pos-neg);
	}
	static Double getEmotionValue(String word,SWN3 sw,boolean positive)
	{
		Double grandtotal=0.0;
		int count=0;
		for(int i=10001;i<11048;i++)
		{
		String bname="lemmatizedBooks/"+i+".txt";
		//System.out.println(bname);
		File f=new File(bname);
			if(f.exists() && f.length() > 0.0)
			{String[] fileWords=readFile(bname);
			Double[] val=computeValue(word,fileWords,sw,positive);
				if(val[1]!=0)
				{grandtotal+=val[0];
				count+=val[1].intValue();
				}
			}
		}

	//System.out.println("total"+grandtotal+",count"+count);		
		if(count!=0)
		grandtotal/=count;
	//System.out.println(word+","+positive+":"+grandtotal);	
	return grandtotal;
	}

	static String[] readFile(String fname)
	{
		try {
			FileInputStream inputStream=new FileInputStream(fname);
	        String inputStreamString = new Scanner(inputStream,"UTF-8").useDelimiter("\\A").next();
	        //System.out.println(inputStreamString);
	        String[] words=inputStreamString.split("\\s+");
	        //System.out.println("Total Words:"+words.length);
	        return words;
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		return null;
	}
	
	/*
	static Double[] computeValue(String word,String[] fileWords, SWN3 sw,boolean positive)
	{Double total=0.0;
	int count=0;
	StopWords stopWordsClass=new StopWords();
	Set<String> stopWordsSet=stopWordsClass.getStopWords();
		for(int i=0;i<fileWords.length;i++)
		{
			if(fileWords[i].equals(word))
			{//System.out.println(word+" found at:"+ i);
			Double val=sw.extract(word,positive);
			total+=val;
			//System.out.println("SentiWordnet Value"+val);
			//System.out.println("--Before Words--");
				for(int j=i-1,k=49;j>=0 && j >= i-50;j--,k--)
				{
					if(!stopWordsSet.contains(fileWords[j]))
					{val=sw.extract(fileWords[j],positive);
					//System.out.println(k+":"+fileWords[j]+":"+val);
					val/=Math.log10(Math.abs(k-50)+1);
					total+=val;
					}
				}
			//System.out.println("total:"+total);
			//System.out.println("--After Words--");
			
				for(int j=i+1,k=51;j<fileWords.length && j <= i+50;j++,k++)
				{
					if(!stopWordsSet.contains(fileWords[j]))
					{val=sw.extract(fileWords[j],positive);
					//System.out.println(k+":"+fileWords[j]+":"+val);
					val/=Math.log10(Math.abs(k-50)+1);
					total+=val;
					}
				}
			//System.out.println("Sub total:"+total);
			count++;
			}
		}
		
		
	//System.out.println("total="+total+",count="+count);	
	Double[] ret={total,(double)count};
	return ret;
	}
	*/
	
	static Double[] computeValue(String word,String[] fileWords, SWN3 sw,boolean positive)
	{Double total=0.0;
	int count=0;
	StopWords stopWordsClass=new StopWords();
	Set<String> stopWordsSet=stopWordsClass.getStopWords();
	NormalDistribution nm=new NormalDistribution(0,4);
		for(int i=0;i<fileWords.length;i++)
		{
			if(fileWords[i].equals(word))
			{//System.out.println(word+" found at:"+ i);
			Double val=sw.extract(word,positive);
			total+=val;
			//System.out.println("SentiWordnet Value"+val);
			//System.out.println("--Before Words--");
				double k=0.2;
				for(int j=i-1;j>=0 && j >= i-51;j--)
				{
					if(!stopWordsSet.contains(fileWords[j]))
					{val=sw.extract(fileWords[j],positive);
					double weight=nm.density(k)*10;
					//System.out.println(fileWords[j]+" : "+val+" , "+weight);
					total+=(val*weight);
					}
				k+=0.2;	
				}
			//System.out.println("sub total:"+total);
			//System.out.println("--After Words--");
				k=0.2;	
				for(int j=i+1;j<fileWords.length && j <= i+51;j++)
				{
					if(!stopWordsSet.contains(fileWords[j]))
					{val=sw.extract(fileWords[j],positive);
					double weight=nm.density(k)*10;
					//System.out.println(fileWords[j]+" : "+val+" , "+weight);
					total+=(val*weight);
					}
				k+=0.2;	
				}
			//System.out.println("Sub total:"+total);
			count++;
			}
		}
		
	//System.out.println("total="+total+",count="+count);	
	Double[] ret={total,(double)count};
	return ret;
	}

}
