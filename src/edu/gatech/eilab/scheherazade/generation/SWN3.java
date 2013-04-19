package edu.gatech.eilab.scheherazade.generation;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

public class SWN3 {
	private String pathToSWN = "sentiwordnet/SentiWordNet_3.0.0.txt";
	private HashMap<String, Double> _dict;

	public SWN3(){

		_dict = new HashMap<String, Double>();
		HashMap<String, Vector<Double>> _temp = new HashMap<String, Vector<Double>>();
		try{
			BufferedReader csv =  new BufferedReader(new FileReader(pathToSWN));
			String line = "";			
			while((line = csv.readLine()) != null)
			{
				String[] data = line.split("\t");
				//Double score = Double.parseDouble(data[2])-Double.parseDouble(data[3]);
				Double pos= Double.parseDouble(data[2]);
				Double neg= Double.parseDouble(data[3]);
				String[] words = data[4].split(" ");
				for(String w:words)
				{
					String[] w_n = w.split("#");
					
					/*w_n[0] += "#"+data[0];
					int index = Integer.parseInt(w_n[1])-1;
					
					if(_temp.containsKey(w_n[0]))
					{
						Vector<Double> v = _temp.get(w_n[0]);
						if(index>v.size())
							for(int i = v.size();i<index; i++)
								v.add(0.0);
						v.add(index, score);
						_temp.put(w_n[0], v);
					}
					else
					{
						Vector<Double> v = new Vector<Double>();
						for(int i = 0;i<index; i++)
							v.add(0.0);
						v.add(index, score);
						_temp.put(w_n[0], v);
					}*/
					
					
					String pw= w_n[0] +"#"+data[0]+"#p";
					String nw= w_n[0] +"#"+data[0]+"#n";
					int index = Integer.parseInt(w_n[1])-1;
					if(_temp.containsKey(pw))
					{
						Vector<Double> v = _temp.get(pw);
						if(index>v.size())
							for(int i = v.size();i<index; i++)
								v.add(0.0);
						v.add(index, pos);
						_temp.put(pw, v);
						
						Vector<Double> v1 = _temp.get(nw);
						if(index>v1.size())
							for(int i = v1.size();i<index; i++)
								v1.add(0.0);
						v1.add(index, neg);
						_temp.put(nw, v1);
					}
					else
					{
						Vector<Double> v = new Vector<Double>();
						for(int i = 0;i<index; i++)
							v.add(0.0);
						v.add(index, pos);
						_temp.put(pw, v);
						
						Vector<Double> v1 = new Vector<Double>();
						for(int i = 0;i<index; i++)
							v1.add(0.0);
						v1.add(index,neg);
						_temp.put(nw, v1);
					} 
				}
			}
			Set<String> temp = _temp.keySet();
			for (Iterator<String> iterator = temp.iterator(); iterator.hasNext();) {
				String word = (String) iterator.next();
				Vector<Double> v = _temp.get(word);
				double score = 0.0;
				double sum = 0.0;
				for(int i = 0; i < v.size(); i++)
					score += ((double)1/(double)(i+1))*v.get(i);
				for(int i = 1; i<=v.size(); i++)
					sum += (double)1/(double)i;
				score /= sum;
				_dict.put(word, score);
			}
		}
		catch(Exception e){e.printStackTrace();}		
	}

	public Double extract(String word,boolean positive)
	{

	   Double total = new Double(0);
	   int count=0;
	   		if(positive)
	   		{	if(_dict.get(word+"#n#p") != null)
		        {total += _dict.get(word+"#n#p");
		        //System.out.println("n:p"+_dict.get(word+"#n#p") );
		        count++;
		        }
	   			if(_dict.get(word+"#a#p") != null)
		        {total += _dict.get(word+"#a#p");
		        //System.out.println("a:p"+_dict.get(word+"#a#p") );
		        count++;
		        }
	   			if(_dict.get(word+"#r#p") != null)
		        {total += _dict.get(word+"#r#p");
		        //System.out.println("r:p"+_dict.get(word+"#r#p") );
		        count++;
		        }
	   			if(_dict.get(word+"#v#p") != null)
		        {total += _dict.get(word+"#v#p");
		        //System.out.println("v:p"+_dict.get(word+"#v#p") );
		        count++;
		        }
	   		}
	   		else
	   		{	if(_dict.get(word+"#n#n") != null)
		        {total += _dict.get(word+"#n#n");
		        count++;
		        }
	   			if(_dict.get(word+"#a#n") != null)
		        {total += _dict.get(word+"#a#n");
		        count++;
		        }
	   			if(_dict.get(word+"#r#n") != null)
		        {total += _dict.get(word+"#r#n");
		        count++;
		        }
	   			if(_dict.get(word+"#v#n") != null)
		        {total += _dict.get(word+"#v#n");
		        count++;
		        }
	   		}
	  
	   		if(count!=0)
	   			return (total/count);
	   		else
	   			return total;
	}
	
	public String extractType(String word)
	{ String emotType="";
	  Double[] total = {0.0,0.0,0.0,0.0};
	   			if(_dict.get(word+"#n") != null)
		        {total[0] = _dict.get(word+"#n");
		        //System.out.println("n:"+total[0] );
		        }
	   			if(_dict.get(word+"#a") != null)
		        {total[1]  = _dict.get(word+"#a");
		        //System.out.println("a:"+total[1] );
		        }
	   			if(_dict.get(word+"#r") != null)
		        {total[2]  = _dict.get(word+"#r");
		        //System.out.println("r:"+total[2] );
		        }
	   			if(_dict.get(word+"#v") != null)
		        {total[3] = _dict.get(word+"#v");
		        //System.out.println("v:"+total[3] );
		        }
	   			
	   	Double ftotal= 0.0;
	   	for(Double x:total)
	   	{
	   		if(Math.abs(x)>ftotal)
	   			ftotal=x;
	   	}
	   			
	   //	System.out.println("final:"+ftotal);
	   	
		if(ftotal == 0)
			emotType = "neutral";
		else if(ftotal >=0.75)
			emotType = "strong_positive";
		else if(ftotal> 0.25 && ftotal<=0.5)
			emotType = "positive";
		else if(ftotal > 0 && ftotal<=0.25)
			emotType = "weak_positive";
		else if(ftotal < 0 && ftotal>=-0.25)
			emotType = "weak_negative";
		else if(ftotal< -0.25 && ftotal>=-0.5)
			emotType = "negative";
		else if(ftotal<=-0.75)
			emotType = "strong_negative";
		
	System.out.println(word+":"+emotType);	
	return emotType;	
	}
	
	public static void main(String[] args) 
	{	SWN3 sw=new SWN3();
		System.out.println(sw.extract("hate",true));
	}
}
