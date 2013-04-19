package edu.gatech.eilab.scheherazade.generation;

import java.io.*;
import java.net.*;

public class WebFileReader {

	/**
	 * @param args
	 */
	public static void main(String[] args) throws InterruptedException
	{	
		for(int i=12900;i< 12910; i++)
		{
			readFile(i);	
			Thread.sleep(10000);  
		}
		
	}
	

	static void readFile(int no)
	{
		try {
			String name="http://www.gutenberg.org/ebooks/"+no+".txt.utf-8";
			URL u=new URL(name);
			InputStream in=u.openStream();
	    	BufferedReader br=new BufferedReader(new InputStreamReader(in));
	    	
	        String fname="gbooks/"+no+".txt";
	        FileWriter fw=new FileWriter(fname);
	    	PrintWriter writer=new PrintWriter(new BufferedWriter(fw));
	    	
	    	String line=null;
	    	boolean w=false;
	    		while((line=br.readLine())!=null)
	    		{	if(line.startsWith("*** END OF THIS PROJECT GUTENBERG EBOOK "))
	    			{w=false;
	    			break;
	    			}
	    			
		    		if(line.startsWith("*** START OF THIS PROJECT GUTENBERG EBOOK "))
	    			{w=true;}
		    		else if(w)
	    			{writer.println(line);}
	    			
	    			
	    		}
	    	

	    	System.out.println("File "+no+" created");
	    	writer.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
}
