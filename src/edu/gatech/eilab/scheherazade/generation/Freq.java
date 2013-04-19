package edu.gatech.eilab.scheherazade.generation;

import net.jeremybrooks.knicker.AccountApi;
import net.jeremybrooks.knicker.WordApi;
import net.jeremybrooks.knicker.dto.FrequencySummary;
import net.jeremybrooks.knicker.dto.TokenStatus;

public class Freq {

	/**
	 * @param args
	 */
	public static void main(String[] args)
	{
		
		
        
	}
	
	public int getGlobalWordFreq(String word)
	{int freq=0;
		try
		{System.setProperty("WORDNIK_API_KEY", "b6633a28d8c34cfbf52590ce6ef007e6f65ff4b0079765cf0");


		// check the status of the API key
		TokenStatus status = AccountApi.apiTokenStatus();
			if (status.isValid()) {
			    //System.out.println("API key is valid.");
			} else {
			    System.out.println("API key is invalid!");
			    System.exit(1);
			}


		// get a list of definitions for a word
		FrequencySummary fs= WordApi.frequency(word);
		freq=fs.getTotalCount();
		//System.out.println(word+":"+freq);
		}
		catch(Exception e)
		{
		System.err.println(e);
		}
	return freq;
	
	}
}
