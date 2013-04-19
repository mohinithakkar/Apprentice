package edu.gatech.eilab.scheherazade.generation;

import java.io.*;

public class EngFreq {

	/**
	 * @param args
	 */
	public static void main(String[] args)
	{
		EngFreq ef=new EngFreq();
		ef.getEngWordFreq("$1000000"); 
	}
	
	public Double getEngWordFreq(String word) 
	{	try
		{
		String cmd[]={"C:\\Users\\Mohini\\Desktop\\msproj\\getNgrams\\getNgrams.exe",word,"-smoothing=0","-startYear=1999","-nosave","-quit"};
		
		Process process = new ProcessBuilder(cmd).start();
	       InputStream is = process.getInputStream();
	       InputStreamReader isr = new InputStreamReader(is);
	       BufferedReader br = new BufferedReader(isr);
	       String line;
	       int count=0;

	       while ((line = br.readLine()) != null) {
	    	   if(count==3)
	    		   break;
	    	   count++;
	       }
	       //System.out.println(line);
	       Double prob=Double.parseDouble(line.substring(line.indexOf(",")+1));
	       //System.out.println(prob);
	       return prob;
		}
		catch(Exception e)
		{System.err.println(e);
		}
		return 0.0;
	
	}
}
