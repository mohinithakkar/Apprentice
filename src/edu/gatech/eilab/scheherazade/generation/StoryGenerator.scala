package edu.gatech.eilab.scheherazade
import main._
import data._
import graph._
import analysis._
import scala.collection.mutable.ListBuffer
import java.io._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

package generation {
  
  object StoryGenerator {

    def main(args: Array[String]) {
      val reader = new ConfigReader("configRobBest.txt")
      var (stories, clusters) = reader.initDataFiltered()

      var clusterProbabilities=getClusterProb(stories,clusters)
      
      val para = reader.properties.allParameters()(0)

      val minimumSize = para.intParam("minClusterSize")
      val insideClusters = clusters.filterNot(c => c.members.size < minimumSize)
      val insideStories = reader.filterUnused(stories, insideClusters)

      val gen = new GraphGenerator(insideStories, insideClusters)
      var graph: Graph = gen.generate(para)("mutualExcl")._1

      var walk = Walk(graph)
      //randomWalk(walk)
      //mostProbableWalk(walk,clusters,clusterProbabilities)
      //leastProbableWalk(walk,clusters,clusterProbabilities)
      
      /*
      var agentstories=new ListBuffer[AgentStory]
      var storyscores=new ListBuffer[Double]
      var runsize=1000
	      for(i<-0 until runsize)
	      {agentstories+=randomlyDistributedWalk(walk,clusters,clusterProbabilities)
	        //agentstories+=hybridWalk(walk,clusters,clusterProbabilities)
	       //agentstories(i).printStory()
	       //println(agentstories(i).getScoreasSum())
	       storyscores+=agentstories(i).getScoreasSum()
	       //println(agentstories(i).getScoreasAvg())
	       //storyscores+=agentstories(i).getScoreasAvg()
	      }
      println("*******HIGHEST SCORE STORY*********")
      var m = storyscores.zipWithIndex.max._2
      agentstories(m).printStory()
      println("*******LOWEST SCORE STORY*********")
      m = storyscores.zipWithIndex.min._2
      agentstories(m).printStory()
      */
      
      //current domain
      
      //var wordFreq=scala.collection.mutable.Map[String,Double]()
      //getWordFreq(stories, wordFreq)
      //wordFreq.keys.foreach(i=>println(i+":"+wordFreq(i)))
      //leastProbableWordsWalk(walk,clusters,wordFreq) 
      //mostProbableWordsWalk(walk,clusters,wordFreq) 
      //leastProbableWalkWithLowWordFreq(walk,clusters,clusterProbabilities,wordFreq)
      //leastProbableWalkWithHighWordFreq(walk,clusters,clusterProbabilities,wordFreq)
     
      
      //english database
      //var globalWordFreq=scala.collection.mutable.Map[String,Double]()
      //getGlobalWordFreq(stories,globalWordFreq)
      //globalWordFreq.keys.foreach(i=>println(i+":"+globalWordFreq(i)))
      //leastProbableWordsWalk(walk,clusters,globalWordFreq) 
      //mostProbableWordsWalk(walk,clusters,globalWordFreq) 
      
       
      //var clu=List(clusters(0))
      //localClusterDesc(clusters,globalWordFreq)
      //localClusterAsBagDesc(clusters,globalWordFreq)
      //localClusterDescUsingMultDist(clusters,globalWordFreq)
      //localClusterAsBagDescUsingMultDist(clusters,globalWordFreq)
      
      var emotionalWordValues=scala.collection.mutable.Map[String,Double]()
      //var sto=List(stories(59)) // upto 59
      //getSWNEmotionalWordValues(sto,emotionalWordValues)
      readSWNEmotionalWordValues(emotionalWordValues)
      //emotionalClusterDesc(clusters,emotionalWordValues)
      //emotionalClusterAsBagDesc(clusters,emotionalWordValues)
      //negEmotionalWalk(walk,clusters,emotionalWordValues) 
      posEmotionalWalk(walk,clusters,emotionalWordValues) 
    }

    def randomWalk(firstWalk: Walk) {
      var walk = firstWalk
      var storycontent=new ListBuffer[String]()
      println("*******RANDOM STORY*********")
      
      while (walk.hasMoreSteps) {
        val fringe = walk.fringe
        val i = math.floor(math.random * fringe.size).toInt
        val step = fringe(i)
        storycontent+=step.name
        walk = walk.forward(step)
      }
      
      var POS=new POSMain();
      POS.convertToFirstPerson(storycontent.toArray);
    }
    
     def mostProbableWalk(firstWalk: Walk,clusters: List[Cluster],clusterProbabilities:Array[Double]) {
      var walk = firstWalk
      var storycontent=new ListBuffer[String]()
      println("*******MOST PROBABLE STORY*********")
      
      while (walk.hasMoreSteps) {
        val fringe = walk.fringe
        
        var fringeProbs=fringe.map(f=>findProb(f,clusters,clusterProbabilities) )
    	//println("fringe data:"+fringe.mkString(","))
    	//println("fringe prob:"+fringeProbs.mkString(","))
    	
    	val i = fringeProbs.zipWithIndex.max._2
    	//println("max"+i)
        val step = fringe(i)
        storycontent+=step.name
        //println(step.name)
        walk = walk.forward(step)
      }
      
      var POS=new POSMain();
      POS.convertToFirstPerson(storycontent.toArray);
    }
    
     def leastProbableWalk(firstWalk: Walk,clusters: List[Cluster],clusterProbabilities:Array[Double]) {
      var walk = firstWalk
      var storycontent=new ListBuffer[String]()
      println("*******LEAST PROBABLE STORY*********")
      
      while (walk.hasMoreSteps) {
        val fringe = walk.fringe
        
        var fringeProbs=fringe.map(f=>findProb(f,clusters,clusterProbabilities) )
    	//println("fringe data:"+fringe.mkString(","))
    	//println("fringe prob:"+fringeProbs.mkString(","))
    	
    	val i = fringeProbs.zipWithIndex.min._2
    	//println("min"+i)
        val step = fringe(i)
        //storycontent+=step.name
        val j = math.floor(math.random * step.members.size).toInt
        println(step.members(j).getOrigString().replaceAll("John","I").replaceAll("his","my").replaceAll("him","me"))
        walk = walk.forward(step)
      }
      
      //var POS=new POSMain();
      //POS.convertToFirstPerson(storycontent.toArray);
    }
     
     def leastProbableWalkWithLowWordFreq(firstWalk: Walk,clusters: List[Cluster],clusterProbabilities:Array[Double],wordFreq:scala.collection.mutable.Map[String,Double]) {
      var walk = firstWalk
      var storycontent=new ListBuffer[String]()
      println("*******LEAST PROBABLE STORY with Low Word Freq*********")
      
      while (walk.hasMoreSteps) {
        val fringe = walk.fringe
        
        var fringeProbs=fringe.map(f=>findProb(f,clusters,clusterProbabilities) )
    	//println("fringe data:"+fringe.mkString(","))
    	//println("fringe prob:"+fringeProbs.mkString(","))
    	
    	val i = fringeProbs.zipWithIndex.min._2
    	//println("min"+i)
        val step = fringe(i)
        //storycontent+=step.name
        var sent=selectMinSent(step,wordFreq,false)
        var sentText=sent.getOrigString().replaceAll("his","my").replaceAll("him","me")
        	if(sentText.indexOf("John")< sentText.length()-6)
        	  sentText=sentText.replace("John","I")
        	else
        	  sentText=sentText.replace("John","me")
        println(sentText)
        walk = walk.forward(step)
      }
      
    }
     
     def leastProbableWalkWithHighWordFreq(firstWalk: Walk,clusters: List[Cluster],clusterProbabilities:Array[Double],wordFreq:scala.collection.mutable.Map[String,Double]) {
      var walk = firstWalk
      var storycontent=new ListBuffer[String]()
      println("*******LEAST PROBABLE STORY with High Word Freq*********")
      
      while (walk.hasMoreSteps) {
        val fringe = walk.fringe
        
        var fringeProbs=fringe.map(f=>findProb(f,clusters,clusterProbabilities) )
    	//println("fringe data:"+fringe.mkString(","))
    	//println("fringe prob:"+fringeProbs.mkString(","))
    	
    	val i = fringeProbs.zipWithIndex.min._2
    	//println("min"+i)
        val step = fringe(i)
        //storycontent+=step.name
        var sent=selectMaxSent(step,wordFreq,false)
        var sentText=sent.getOrigString().replaceAll("his","my").replaceAll("him","me")
        	if(sentText.indexOf("John")< sentText.length()-6)
        	  sentText=sentText.replace("John","I")
        	else
        	  sentText=sentText.replace("John","me")
        println(sentText)
        walk = walk.forward(step)
      }
      
    }
     
     def leastProbableWordsWalk(firstWalk: Walk,clusters: List[Cluster],wordFreq:scala.collection.mutable.Map[String,Double]) {
      var walk = firstWalk
      var storycontent=new ListBuffer[String]()
      println("*******Least Frequent First Story based on Word Frequency*********")
      
      while (walk.hasMoreSteps) {
        val fringe = walk.fringe
        
        var fringeSents = fringe.map(f=>selectMinSent(f,wordFreq,false) )
        var fringeWordProbs=fringeSents.map(f=>findSentProb(f,wordFreq) )
    	//println("fringe sents:"+fringeSents.mkString(","))
    	//println("fringe word prob:"+fringeWordProbs.mkString(","))
    	
    	val i = fringeWordProbs.zipWithIndex.min._2
    	//println("min"+i)
        var sent=fringeSents(i).getOrigString().replaceAll("his","my").replaceAll("him","me")
        	if(sent.indexOf("John")< sent.length()-6)
        	  sent=sent.replace("John","I")
        	else
        	  sent=sent.replace("John","me")
        println(sent)	  
        walk = walk.forward(fringe(i))
      }
    }
     
      def mostProbableWordsWalk(firstWalk: Walk,clusters: List[Cluster],wordFreq:scala.collection.mutable.Map[String,Double]) {
      var walk = firstWalk
      var storycontent=new ListBuffer[String]()
      println("*******Most Frequent First Story based on Word Frequency*********")
      
      while (walk.hasMoreSteps) {
        val fringe = walk.fringe
        
        var fringeSents = fringe.map(f=>selectMaxSent(f,wordFreq,false) )
        var fringeWordProbs=fringeSents.map(f=>findSentProb(f,wordFreq) )
    	//println("fringe sents:"+fringeSents.mkString(","))
    	//println("fringe word prob:"+fringeWordProbs.mkString(","))
    	
    	val i = fringeWordProbs.zipWithIndex.max._2
    	//println("max"+i)
        var sent=fringeSents(i).getOrigString().replaceAll("his","my").replaceAll("him","me")
        	if(sent.indexOf("John")==0)
        	  sent=sent.replace("John","I")
        	else
        	  sent=sent.replace("John","me")
        println(sent)	  
        walk = walk.forward(fringe(i))
      }
      
      //var POS=new POSMain();
      //POS.convertToFirstPerson(storycontent.toArray);
    }
     
     def negEmotionalWalk(firstWalk: Walk,clusters: List[Cluster],wordFreq:scala.collection.mutable.Map[String,Double]) {
      var walk = firstWalk
      var storycontent=new ListBuffer[String]()
      var lm=new Lemmatizer()
      println("******Greedy walk selecting negative emotional sentences*********")
      
      while (walk.hasMoreSteps) {
        val fringe = walk.fringe
       
        var fringeSents = fringe.map(f=>selectMinSentLemma(f,wordFreq,lm) )
        var fringeVal=fringeSents.map(f=>findSentProbWithLemma(f,wordFreq,lm) )
    	println("fringe sents:"+fringeSents.mkString(","))
        println("fringe word prob:"+fringeVal.mkString(","))
    	
    	val i = fringeVal.zipWithIndex.min._2
    	println("---"+fringeSents(i).getOrigString()+"---")
        walk = walk.forward(fringe(i))
      }
    }
          
    def posEmotionalWalk(firstWalk: Walk,clusters: List[Cluster],wordFreq:scala.collection.mutable.Map[String,Double]) {
      var walk = firstWalk
      var storycontent=new ListBuffer[String]()
      var lm=new Lemmatizer()
      println("******Greedy walk selecting positive emotional sentences*********")
      
      while (walk.hasMoreSteps) {
        val fringe = walk.fringe
       
        var fringeSents = fringe.map(f=>selectMaxSentLemma(f,wordFreq,lm) )
        var fringeVal=fringeSents.map(f=>findSentProbWithLemma(f,wordFreq,lm) )
    	println("fringe sents:"+fringeSents.mkString(","))
        println("fringe word prob:"+fringeVal.mkString(","))
    	
    	val i = fringeVal.zipWithIndex.max._2
    	println("---"+fringeSents(i).getOrigString()+"---")
        walk = walk.forward(fringe(i))
      }
    }
          
     def selectMinSent(cluster:Cluster,wordFreq:scala.collection.mutable.Map[String,Double],multi:Boolean):Sentence=
     {
       var lSent:Sentence=null
       var lprob=Math.MAX_DOUBLE
       cluster.members.foreach{s=>
         var p=0.0
         	if(multi)
         	 p=findSentProbUsingMultiDist(s,wordFreq)
         	else
         	  p=findSentProb(s,wordFreq)
        //println(s.getOrigString())
        //("Sent Prob:"+p)
	        if(p<lprob)
	        {lprob=p
	         lSent=s
	        }
       }
       
       print("Least Sent:  ")
       println(lSent.getOrigString() +"   "+lprob)
       return lSent
     }
     
      def selectMinSentLemma(cluster:Cluster,wordFreq:scala.collection.mutable.Map[String,Double],lm:Lemmatizer):Sentence=
     {
       var lSent:Sentence=null
       var lprob=Math.MAX_DOUBLE
       cluster.members.foreach{s=>
         var p=findSentProbWithLemma(s,wordFreq,lm)
        //println(s.getOrigString())
        //("Sent Prob:"+p)
	        if(p<lprob)
	        {lprob=p
	         lSent=s
	        }
       }
       
       //print("Least Sent:  ")
       //println(lSent.getOrigString() +"   "+lprob)
       return lSent
     }
     
     def selectMaxSent(cluster:Cluster,wordFreq:scala.collection.mutable.Map[String,Double],multi:Boolean):Sentence=
     {
       var mSent:Sentence=null
       var mprob=Math.MIN_DOUBLE
       cluster.members.foreach{s=>
        var p=0.0
         	if(multi)
         	  p=findSentProbUsingMultiDist(s,wordFreq)
         	else
         	  p=findSentProb(s,wordFreq)
        //println(s.getOrigString())
        //println("Sent Prob:"+p)
	        if(p>mprob)
	        {mprob=p
	         mSent=s
	        }
       }
       
       print("Most Sent:  ")
       println(mSent.getOrigString()+ "  "+mprob)
       return mSent
     }
     
     def selectMaxSentLemma(cluster:Cluster,wordFreq:scala.collection.mutable.Map[String,Double],lm:Lemmatizer):Sentence=
     {
       var mSent:Sentence=null
       var mprob=Math.MIN_DOUBLE
       cluster.members.foreach{s=>
        var p=findSentProbWithLemma(s,wordFreq,lm)
        //println(s.getOrigString())
        //println("Sent Prob:"+p)
	        if(p>mprob)
	        {mprob=p
	         mSent=s
	        }
       }
       
       //print("Most Sent:  ")
       //println(mSent.getOrigString()+ "  "+mprob)
       return mSent
     }
          
     def localClusterDesc(clusters: List[Cluster],wordFreq:scala.collection.mutable.Map[String,Double]) {
      
      println("***Cluster Description based on word frequencies over all stories using sentence independently (log)****")
      
      	clusters.foreach
      	{c=>
      	println("Cluster:  "+c.name)
        var minProb=findSentProb(selectMinSent(c,wordFreq,false),wordFreq)
        var maxProb=findSentProb(selectMaxSent(c,wordFreq,false),wordFreq)
        //println("Min:"+ minProb + " Max:"+maxProb)
      	}
        
    }
     
     def localClusterAsBagDesc(clusters: List[Cluster],wordFreq:scala.collection.mutable.Map[String,Double]) {
      
      println("***Cluster Description based on word frequencies over all stories treating cluster as a bag of words (log)***")
      
      	clusters.foreach{c=>
      	var p=0.0
      		c.members.foreach{s=>
      		p+=findSentProb(s,wordFreq)
      		}
      	println("Cluster:  "+c.name +"  " + p)	   
      	}
    }
     
    def emotionalClusterAsBagDesc(clusters: List[Cluster],wordFreq:scala.collection.mutable.Map[String,Double]) {
      
      println("***Emotional Cluster Description treating cluster as a bag of words ***")
      var lm=new Lemmatizer()
      	clusters.foreach{c=>
      	var p=0.0
      		c.members.foreach{s=>
      		p+=findSentProbWithLemma(s,wordFreq,lm)
      		}
      	println("Cluster:  "+c.name +"  " + p)	   
      	}
    }
     
     
     def localClusterDescUsingMultDist(clusters: List[Cluster],wordFreq:scala.collection.mutable.Map[String,Double]) {
      
      println("***Cluster Description based on word frequencies over all stories using sentence independently (multinomial distribution)****")
      
      	clusters.foreach
      	{c=>
      	println("Cluster:  "+c.name)
        var minProb=findSentProbUsingMultiDist(selectMinSent(c,wordFreq,true),wordFreq)
        var maxProb=findSentProbUsingMultiDist(selectMaxSent(c,wordFreq,true),wordFreq)
        //println("Min:"+ minProb + " Max:"+maxProb)
      	}
        
    }
     
     def localClusterAsBagDescUsingMultDist(clusters: List[Cluster],wordFreq:scala.collection.mutable.Map[String,Double]) {
      
      println("***Cluster Description based on word frequencies over all stories treating cluster as a bag of words (multinomial distribution)****")
      
      var stopWordsClass=new StopWords()
      var stopWordsSet=stopWordsClass.getStopWords()
      
      	clusters.foreach
      	{c=>
      	var p:Double=0
      	var frequencyTable=scala.collection.mutable.Map[String,Int]()
	      	c.members.foreach{s=>
	      		s.tokens.foreach{t=>
	      		  var w=t.word.replace(".","").toLowerCase()
	      		  //print(w+" ")
		          if(!stopWordsSet.contains(w) && w!="" && w!=null && w!=" " && w!="\n")
		          {	if(frequencyTable.contains(w))
		          		frequencyTable(w)= frequencyTable(w)+1
		    		else
		    			frequencyTable(w)=1
		          }
	      		}
	      	}
      	//println("Cluster Bag:  ")
      	//frequencyTable.keys.foreach(i=>println(i+":"+frequencyTable(i)+" "+wordFreq(i)))
      	val n=frequencyTable.values.sum
      	//println("n "+n);
      	//println("n! "+factorial(n));
      	//println("Log of n! "+java.lang.Math.log10(factorial(n)))
      	p+=java.lang.Math.log10(factorial(n))
      	frequencyTable.keys.foreach{i=>
      	//println("log(x!) "+java.lang.Math.log10(factorial(frequencyTable(i))))
      	p-=java.lang.Math.log10(factorial(frequencyTable(i)))
      	//println("x*log(p)"+(frequencyTable(i)*java.lang.Math.log10(wordFreq(i))))
      	p+=(frequencyTable(i)*java.lang.Math.log10(wordFreq(i)))
      	}
      	println(c.name+" : " +p)
      	}
    }
    
    def emotionalClusterDesc(clusters: List[Cluster],emotionalWordValues:scala.collection.mutable.Map[String,Double]) 
    {
      
      var lm=new Lemmatizer()
      println("***Cluster Description based on emotional words using sentence independently ****")
      
      	clusters.foreach
      	{c=>
      	println("Cluster:  "+c.name)
        var minProb=findSentProbWithLemma(selectMinSentLemma(c,emotionalWordValues,lm),emotionalWordValues,lm)
        var maxProb=findSentProbWithLemma(selectMaxSentLemma(c,emotionalWordValues,lm),emotionalWordValues,lm)
        //println("Min:"+ minProb + " Max:"+maxProb)
      	}
        
    }
     
    def randomlyDistributedWalk(firstWalk: Walk,clusters: List[Cluster],clusterProbabilities:Array[Double]):AgentStory= {
      var walk = firstWalk
      var storycontent=new ListBuffer[String]()
      var score=0.0
      //println("*******RANDOMLY DISTRIBUTED STORY*********")
      var randomGen = new scala.util.Random
      while (walk.hasMoreSteps) {
        val fringe = walk.fringe
        var fringeProbs=fringe.map(f=>findProb(f,clusters,clusterProbabilities) )
    	//println("fringe data:"+fringe.mkString(","))
    	//println("fringe prob:"+fringeProbs.mkString(","))
    	
    	val sum = fringeProbs.sum
    	var relfringeProbs=fringeProbs.map(f=>f/sum)
    	var cumufringeProbs=new ArrayBuffer[Double]()
    	cumufringeProbs+=relfringeProbs(0)
    	for(i<-1 until relfringeProbs.size)
    	{ cumufringeProbs+= relfringeProbs(i)+cumufringeProbs(i-1) }
    	//println("cumu fringe probs"+cumufringeProbs.mkString(","))
    	var k=randomGen.nextFloat() 
    	//println("random no"+k)
	     for(i<-0 until cumufringeProbs.size)
	     { 
	       if(k <= cumufringeProbs(i) && i==0 || k <= cumufringeProbs(i) && k>cumufringeProbs(i-1))
	       {
	         val step = fringe(i)
	         //score+=fringeProbs(i)
	         score+=java.lang.Math.log10(fringeProbs(i))
	         val j = math.floor(math.random * step.members.size).toInt
	         var sent=step.members(j).getOrigString()
	         storycontent+=toFirstPerson(sent);
	         walk = walk.forward(step)
	       }
	     }
      }
      //println("Story Score:"+score)
          storycontent+=score+""; 
          return new AgentStory(storycontent.toList)
      }
      
      def hybridWalk(firstWalk: Walk,clusters: List[Cluster],clusterProbabilities:Array[Double]):AgentStory= {
      var walk = firstWalk
      var storycontent=new ListBuffer[String]()
      var score=0.0
      val me=walk.graph.mutualExcls
      //println(me)
      //println("*******HYBRID STORY*********")
      var randomGen = new scala.util.Random
      while (walk.hasMoreSteps) {
        val fringe = walk.fringe
        var fringeProbs=fringe.map(f=>findProb(f,clusters,clusterProbabilities) )
    	//println("fringe: "+fringe.map(f=>f.name))
    	//println("fringe prob:"+fringeProbs.mkString(","))
        val mepresent=checkForME(fringe,me) 
        //println("me present: "+mepresent)
        var sent=""
        var step=fringe(0)
        	if(mepresent)
        	{
        		val sum = fringeProbs.sum
		    	var relfringeProbs=fringeProbs.map(f=>f/sum)
		    	var cumufringeProbs=new ArrayBuffer[Double]()
		    	cumufringeProbs+=relfringeProbs(0)
		    	for(i<-1 until relfringeProbs.size)
		    	{ cumufringeProbs+= relfringeProbs(i)+cumufringeProbs(i-1) }
		    	//println("cumu fringe probs"+cumufringeProbs.mkString(","))
		    	var k=randomGen.nextFloat() 
		    	//println("random no"+k)
			     for(i<-0 until cumufringeProbs.size)
			     { 
			       if(k <= cumufringeProbs(i) && i==0 || k <= cumufringeProbs(i) && k>cumufringeProbs(i-1))
			       {
			         step = fringe(i)
			         //score+=fringeProbs(i)
			         score+=java.lang.Math.log10(fringeProbs(i))
			       }
			     }
		      }
        	else
        	{
		    	val i = fringeProbs.zipWithIndex.min._2
		    	//println("min"+i)
		        step = fringe(i)
			    //score+=fringeProbs(i)
		        score+=java.lang.Math.log10(fringeProbs(i))
        	}
        	
        //println("step:"+step.name)
        val j = math.floor(math.random * step.members.size).toInt
		sent=step.members(j).getOrigString()
        storycontent+=toFirstPerson(sent);
		walk = walk.forward(step)
        }
    	
      //println("Story Score:"+score)
          storycontent+=score+""; 
          return new AgentStory(storycontent.toList)
      }
      
      def toFirstPerson(sent:String):String=
      {
        var fpsent=sent.replaceAll("his","my").replaceAll("him","me")
        	if(fpsent.indexOf("John") < 4)
			fpsent=fpsent.replaceAll("John","I")
			else
			fpsent=fpsent.replaceAll("John","me")
		//println(fpsent);	  
        fpsent
      }
         
     def getClusterProb(stories:List[Story],clusters:List[Cluster]):Array[Double]=
    {
      var clusterProbabilities=new Array[Double](clusters.length)
      for(i<-0 until clusters.length )
      {
      clusterProbabilities(i)=stories.filter((z)=> z.members.filter((x)=>clusters(i).members.exists(z.members contains)).length>0).length/stories.length.toDouble
      //println("Cluster prob:["+i+"]"+clusterProbabilities(i))
      }
      clusterProbabilities
    }
     
     
    def getWordFreq(stories:List[Story],wordFreq:scala.collection.mutable.Map[String,Double])=
    {
    	var totalWords:Int=0
    	for(j<-stories)
    	{
    			for(k<-j.members)
    			{
    			  totalWords+=k.tokens.length
    					  for(l<-k.tokens)
    					  {
    					    var m=l.word.replace(".","").toLowerCase()
    					    //print(m+" ")
    					    if(wordFreq.contains(m))
    					     wordFreq(m)= wordFreq(m)+1
    					    else
    					      wordFreq(m)=1
    					  }
    			  //println()		    
    			}
    	  //println()
    	}
    	println("total words:"+totalWords)
    	//wordFreq.keys.foreach(i=>println(i+":"+wordFreq(i)))
    	wordFreq.keys.foreach(i=>(wordFreq(i)=wordFreq(i)/totalWords))
    }
        
    def getGlobalWordFreq(stories:List[Story],globalWordFreq:scala.collection.mutable.Map[String,Double])=
    {	var ef=new EngFreq()
    	println("Running..Please Wait!")
    	var f=new File("robberyWordFreq.txt")
    	if(!f.exists())
    	{
    	  var fw=new FileWriter("robberyWordFreq.txt",true)
    	  System.out.println("File created")
    	  val writer=new PrintWriter(new BufferedWriter(fw))
    	  for(j<-stories)
    	  {
    			for(k<-j.members)
    			{
    					  for(l<-k.tokens)
    					  {
    					    var m=(l.word.replace(".","")).toLowerCase()
    					    //print(m+" ")
    					    if(globalWordFreq.contains(m))
    					    {
    					    
    					    }
    					    else
    					    { globalWordFreq(m)=ef.getEngWordFreq(m)
    					      writer.println(m + " " + globalWordFreq(m))
    					      println("retrieved "+m+ " : "+globalWordFreq(m))
    					    }
    					  }
    			  //println()		    
    			}
    	  //println()
    	  }
    	  
    	  
    	  writer.close()
    	}
    	else
    	{
	    	System.out.println("File exists")
	    	for(line<-Source.fromFile("robberyWordFreq.txt").getLines())
	    	{
	    	  var spaceIndex=line.indexOf(" ")
	    	  var word=line.substring(0,spaceIndex)
	    	  var prob=line.substring(spaceIndex+1)
	    	  //println(word+":"+prob)
	    	  globalWordFreq(word)=(prob.toDouble)/100.0
	    	}
	    	  
    	}
    	
    	//globalWordFreq.keys.foreach(i=>println(i+":"+globalWordFreq(i)))
    println("global frequencies computed")
    	
    }
    
    def readSWNEmotionalWordValues(emotionalWordValues:scala.collection.mutable.Map[String,Double])=
    {  println("Running..Please Wait!")
    
    	for(line<-Source.fromFile("robberyEmotVal.txt").getLines())
	    	{
	    	  var spaceIndex=line.indexOf(" ")
	    	  var word=line.substring(0,spaceIndex)
	    	  var prob=line.substring(spaceIndex+1)
	    	  //println(word+":"+prob)
	    	  emotionalWordValues(word)=(prob.toDouble)
	    	}
    //emotionalWordValues.keys.foreach(i=>println(i+":"+emotionalWordValues(i)))
    println("emotional values read")
    
    }
        
    def getSWNEmotionalWordValues(stories:List[Story],emotionalWordValues:scala.collection.mutable.Map[String,Double])=
    {  println("Running..Please Wait!")
       var stopWordsClass=new StopWords()
       var stopWordsSet=stopWordsClass.getStopWords()  
       var ec=new EmotionCalculator()
       var sw=new SWN3()
       var lm=new Lemmatizer()
    
    	for(line<-Source.fromFile("robberyEmotVal.txt").getLines())
	    	{
	    	  var spaceIndex=line.indexOf(" ")
	    	  var word=line.substring(0,spaceIndex)
	    	  var prob=line.substring(spaceIndex+1)
	    	  println(word+":"+prob)
	    	  emotionalWordValues(word)=(prob.toDouble)
	    	}
    
    	var fname="robberyEmotVal.txt"
		var fw=new FileWriter(fname,true)
		var writer=new PrintWriter(new BufferedWriter(fw))
    	println("------adding words------------")
    	  for(j<-stories)
    	  {
    			for(k<-j.members)
    			{
    					  for(l<-k.tokens)
    					  {
    					    var m=(l.word.replace(".","").replace(",", "").replace("?", "").replace("!","").replace(";","").replace("\"", "")).toLowerCase()
    					    //print(m+" ")
	    					    if(!stopWordsSet.contains(m))
		    					{ lm.getParsed(m)
								  var lemma=""
										while(lm.hasNextSentence())
										{	lm.processNextSentence()
											var data = lm.getLemma()
											lemma=data(0)
										}
								  println(m+","+lemma)	
		    					    	if(!emotionalWordValues.contains(lemma))
			    					    {
			    						emotionalWordValues(lemma)=ec.getEmotion(lemma, sw)
			    						writer.println(lemma+" "+emotionalWordValues(lemma))
			    						//print(lemma+ ":"+emotionalWordValues(lemma)+"  ")
			    					    }
	    					    }
    					  }
    			 //println()		    
    			}
    	  //println()
    	  }
       
    //emotionalWordValues.keys.foreach(i=>println(i+":"+emotionalWordValues(i)))	 
    println("emotional word values computed")
    System.out.println("File created")
	writer.close()
    
    }
      
     def findProb(f:Cluster,clusters:List[Cluster],clusterProbabilities:Array[Double]):Double=
      {var p:Double=0
        for(j<-0 until clusters.length)
        {
          if(clusters(j).equals(f)) p=clusterProbabilities(j)
        }
        p
      }
     
     
     def findSentProb(s:Sentence,wordFreq:scala.collection.mutable.Map[String,Double]):Double=
      {var p:Double=0
      
       var stopWordsClass=new StopWords()
       var stopWordsSet=stopWordsClass.getStopWords()
       
        s.tokens.foreach{t=>
          var w=t.word.replace(".","").toLowerCase()
          //print(w+" ")
          	if(!stopWordsSet.contains(w))
          	{	if(wordFreq.contains(w))
          		p+=java.lang.Math.log10(wordFreq(w))
          	//print(p+" ")  
          	}
      	  }
        return p
      }
     
     def findSentProbUsingMultiDist(s:Sentence,wordFreq:scala.collection.mutable.Map[String,Double]):Double=
      {var p:Double=0
      
       var stopWordsClass=new StopWords()
       var stopWordsSet=stopWordsClass.getStopWords()
       
       var frequencyTable=scala.collection.mutable.Map[String,Int]()
       
        s.tokens.foreach{t=>
          var w=t.word.replace(".","").replace(",", "").replace("?", "").replace("!","").replace(";","").replace("\"", "").toLowerCase()
          //print(w+" ")
          	if(!stopWordsSet.contains(w) && w!="" && w!=null && w!=" " && w!="\n" && wordFreq(w)!=0)
          	{	if(frequencyTable.contains(w))
          			frequencyTable(w)= frequencyTable(w)+1
    			else
    				frequencyTable(w)=1
          	}
      	  }
      
      	//frequencyTable.keys.foreach(i=>println(i+":"+frequencyTable(i)+" "+wordFreq(i)))
      	val n=frequencyTable.values.sum
      	p+=java.lang.Math.log10(factorial(n))
      	frequencyTable.keys.foreach{i=>
      		  p-=java.lang.Math.log10(factorial(frequencyTable(i)))
      		  p+=(frequencyTable(i)*java.lang.Math.log10(wordFreq(i)))
      	}
      	//println(p)
      	
        return p
      }
     
      def findSentProbWithLemma(s:Sentence,wordFreq:scala.collection.mutable.Map[String,Double],lm:Lemmatizer):Double=
      {var p:Double=0
       var count=0
       var stopWordsClass=new StopWords()
       var stopWordsSet=stopWordsClass.getStopWords()
       
       var frequencyTable=scala.collection.mutable.Map[String,Int]()
       
        s.tokens.foreach{t=>
          var w=t.word.replace(".","").replace(",", "").replace("?", "").replace("!","").replace(";","").replace("\"", "").toLowerCase()
          //print(w+" ")
          	if(!stopWordsSet.contains(w) && w!="" && w!=null && w!=" " && w!="\n")
          	{	lm.getParsed(w)
				var lemma=""
					while(lm.hasNextSentence())
					{	lm.processNextSentence()
						var data = lm.getLemma()
						lemma=data(0)
					}
          		if(wordFreq(lemma)!=0)
          		{ p+=wordFreq(lemma)
          		  count+=1
          		  //println(lemma+":"+wordFreq(lemma))
          		}
          	}
      	  }
      	//println(p/count)
        return (p/count)
      }
     
     def factorial(n:Int):Double = 
       {
       if(n==0) return 1 
       else return n*factorial(n-1)
       }
     
     def checkForME(fringe:List[Cluster],me:List[MutualExcl]):Boolean=
     {	
       val c1=me.map(m=>m.c1)
       val c2=me.map(m=>m.c2)
        for(i<-0 until c1.length)
        {
         if(fringe.contains(c1(i)) && fringe.contains(c2(i)))
           {
           //println("c1:"+c1(i).name)
           //println("c2:"+c2(i).name)
           return true
           }
        }
       return false
     }
      

    def bruteSearch(firstWalk: Walk) {
      val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("valid stories.txt")))

      var q = scala.collection.mutable.Queue[Walk]()
      var good: Long = 0
      var result = scala.collection.mutable.Set[Walk]()
      q.enqueue(firstWalk)

      while (!q.isEmpty) {
        var n = q.dequeue()

        //println(n)
        //println("fringe: " + n.fringe.map(_.name).mkString("(", ", ", ")"))
        //println("\n\n")    

        //println("story length = " + n.history.size)
        if (!n.hasMoreSteps()) {
          // we have reached the end.
          result += n
        } else {
          n.fringe foreach { step =>
            q += n.forward(step)
          }
        }

        n = null
      }

      //for (story <- result)
      //pw.println("GOOD STORY: \n" + story)

      pw.close()

      println("found " + result.size + " stories.")
      println("Considered " + Walk.totalGenerated + " search nodes. ")
    }
  }
}

class AgentStory(val story:List[String])
{
  var score=0.0
  
  def getScoreasSum():Double=
    {
    score=story.last.toDouble
    score
    }
  
  def getScoreasAvg():Double=
    {
    score=(story.last.toDouble)/(story.length-1)
    score
    }
 
  def printStory()
  {
    for(i<-0 until story.length-1)
      println(story(i))
  }
 
}
//  class Walk(val id: Int, val history: List[Cluster], val fringe: List[Cluster], val exclList: List[Cluster], val selfGraph: Graph) {
//
//    val debug = false
//
//    def nextFringe(step: Cluster, melinks:List[MutualExcl], optionals:List[Cluster]): Walk = {
//      if (!fringe.contains(step)) throw new RuntimeException("Illegal Step: " + step)
//
//      val newHistory = step :: history
//      var excluded = Walk.excluded(List(step), melinks).filter(selfGraph.nodes contains)
//      //println(excluded.map(_.name).mkString("directly mutex: ", ", ", ""))
//      //println(exclList.map(_.name).mkString("old mutex: ", ", ", ""))
//      var excl = excluded ::: exclList
//      excl = findTransitiveClosure(selfGraph, excl)
//      //println(excl.map(_.name).mkString("closure mutex: ", ", ", ""))
//      excluded = excl -- exclList
//      val expired = selfGraph.links.filter(l => l.target == step).map(_.source)
//      val newGraph = selfGraph.addSkipLinks(excluded).removeNodes(excluded ::: expired)
//
//      var newFringe = Walk.maxFringe(newHistory, newGraph, optionals)
//      // delete those already executed
//      newFringe = newFringe filterNot (newHistory contains)
//      
//      new Walk(id, newHistory, newFringe, excl, newGraph)	
//    }
//
//    /**
//     * Takes one step in the graph
//     *
//     */
//    def oneStep(melinks: List[MutualExcl], optionals: List[Cluster]): List[Walk] =
//      {
//        fringe map { step =>
//
//          val newHistory = step :: history
//          var excluded = Walk.excluded(List(step), melinks).filter(selfGraph.nodes contains)
//          //println(excluded.map(_.name).mkString("directly mutex: ", ", ", ""))
//          //println(exclList.map(_.name).mkString("old mutex: ", ", ", ""))
//          var excl = excluded ::: exclList
//          excl = findTransitiveClosure(selfGraph, excl)
//          //println(excl.map(_.name).mkString("closure mutex: ", ", ", ""))
//          excluded = excl -- exclList
//          val expired = selfGraph.links.filter(l => l.target == step).map(_.source)
//          val newGraph = selfGraph.addSkipLinks(excluded).removeNodes(excluded ::: expired)
//
//          var newFringe = Walk.maxFringe(newHistory, newGraph, optionals)
//          // delete those already executed
//          newFringe = newFringe filterNot (newHistory contains)
//          // all steps preceding the step is prohibited
//
//          // enforce an ordering for parallel events
//          val parallel = selfGraph.nodes.filterNot(c => selfGraph.ordered(step, c)).filter(c => c.name > step.name)
//          // newly exclueded by mutual exclusions
//
//          if (debug) {
//            println("*******************************")
//            println(this)
//            println("taking step: " + step.name)
//            println("excluded because of ME: " + excluded.map(_.name).mkString("(", ", ", ")"))
//            println("excluded because of symmetry: " + parallel.map(_.name).mkString("(", ", ", ")"))
//            println("excluded because of temporal ordering: " + expired.map(_.name).mkString("(", ", ", ")"))
//            println("excluded by parent: " + exclList.map(_.name).mkString("(", ", ", ")"))
//            println("is temporal ordering removal necessary: " + (newFringe filter (expired contains)).map(_.name).mkString)
//
//            newGraph.draw("valid")
//
//          }
//
//          newFringe =
//            if ((newFringe -- parallel).isEmpty) newFringe
//            else (newFringe -- parallel)
//
//          val id = Walk.nextId()
//          if (debug) {
//            println("final fringe: " + newFringe.map(_.name).mkString("(", ", ", ")"))
//
//            println("next story : " + id + "\n*******************************")
//            readLine()
//          }
//          new Walk(id, newHistory, newFringe, excl, newGraph)
//
//        }
//      }
//
//    /**
//     * if all direct predecessors of an event is in the event list, add that event to the event list
//     * continue adding events until no such event exists
//     */
//    def findTransitiveClosure(graph: Graph, events: List[Cluster]): List[Cluster] =
//      {
//
//        var all = ListBuffer[Cluster]() ++ events
//        var newFound: ListBuffer[Cluster] = null
//        var remainder = graph.nodes filterNot (all contains)
//        do {
//          newFound = ListBuffer[Cluster]()
//          for (e <- remainder) {
//            val pred = graph.predecessorsOf(e)
//            if ((!pred.isEmpty) &&
//              pred.forall(all contains))
//              newFound += e
//          }
//          all ++= newFound
//          remainder = remainder filterNot (newFound contains)
//        } while (!newFound.isEmpty)
//
//        all.toList
//      }
//
//    def hasMoreSteps() = !fringe.isEmpty
//
//    override def toString(): String = {
//      history.reverse.map(_.name).mkString("Story: " + id + "\n", "\n", "\n***\n")
//    }
//
//    override def equals(o: Any): Boolean = o match {
//      case that: Walk => {
//        if (this.history.size != that.history.length) return false
//        else {
//          val size = this.history.size
//          for (i <- 0 until size) {
//            if (this.history(i) != that.history(i))
//              return false
//          }
//          return true
//        }
//      }
//      case _ => false
//    }
//
//    override def hashCode(): Int = {
//      history.map(_.hashCode).sum * 389 / 311
//    }
//  }
//
//  object Walk {
//
//    var id = 0
//
//    private def nextId() = {
//      id += 1
//      id
//    }
//    private def maxFringe(history: List[Cluster], graph: Graph, optionals: List[Cluster]) =
//      {
//        // if all of its parents are either included in the history or optionals, it is on the fringe
//        val parents = optionals ::: history
//        var possible = graph.nodes.filter { node => graph.predecessorsOf(node).forall(parents.contains) }
//        possible
//      }
//
//    /**
//     * nodes excluded with mutual exclusion links
//     *
//     */
//    private def excluded(history: List[Cluster], melinks: List[MutualExcl]): List[Cluster] =
//      {
//        history.flatMap(s => melinks.filter(m => m.c1 == s).map(m => m.c2) :::
//          melinks.filter(m => m.c2 == s).map(m => m.c1))
//      }
//
//    def fromInits(inits: List[Cluster], graph: Graph, melinks: List[MutualExcl], optionals: List[Cluster]): Walk = {
//      val fringe = inits
//      new Walk(nextId(), Nil, fringe, Nil, graph)
//    }
//  }

