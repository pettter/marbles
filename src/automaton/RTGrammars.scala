package marbles.automaton
import marbles.util._

object RTGrammar {

  /* Simple method to extract the nonterminal symbols of the rules in a
   * ruleset */
  def simplifyRules[T](rules:Map[String, Set[Tree[Either[String,T]]]]):Map[String,Set[Seq[String]]] = {
    for((nt,ts) <- rules) yield 
      (nt, (ts map (_.leaves collect (_.root match { case
				      Left(y) => y} ))))
  }

  /* Parse a regular tree grammar */

  implicit def RTGrammarParsers[T](implicit tps:ElementParsers[T]) =
    new ElementParsers[RTGrammar[T]] {
      val tp:Parser[T] = tps

      def begin:Parser[String] = "RTG{"~nonTreeParser~"," ^^ { case _~nt~_ => nt }

      def rule = nonTreeParser ~ "|" ~
	Tree.treeParser[Either[String,T]](ElementParsers.either(new
	      ElementParsers[String] {
	      def nonterminal:Parser[String] = "|"~nonTreeParser ^^ { case "|"~nt => nt}
	      def start = nonterminal
	      }, tps))~"," ^^ {
	  case nt~"|"~t~"," => (nt,t)
	}

      def rules = rep(rule) ^^ (rs => {
	  for(nt:String <- rs map (_._1) toSet) yield {
	    (nt, (rs filter (_._1 == nt) map (_._2)) toSet)
	  }}.toMap)




      def alpha:Parser[RankedAlphabet[T]] =
	"{"~>RankedAlphabet.alphaParsers[T](tps)<~"},"


      def rtg = begin~
	alpha~"{"~
	rules~"}"~"}" ^^ {
	  case ss~alpha~_~rset~_~_ => new RTGrammar(alpha, rset.keySet, rset, ss)
	}


      def start = rtg
    }


}

class RTGrammar[T](val sigma:RankedAlphabet[T],
    val nonterminals:Set[String],
    val rules:Map[String, Set[Tree[Either[String,T]]]],
    val start:String) extends Iterator[Tree[T]]{


  /* Map how many nonterminals need to be traversed to result in a "ground
   * term" from each */
  val ntDepthMap:Map[String,Int] = NTDepthCalc(RTGrammar.simplifyRules(rules),Map(),0)

  /* Simple map, from nonterminal to a sorted list of resulting
   * nonterminals from each rule */
  def simpleRules:Map[String,List[Seq[String]]] = {
    for((nt,ts) <- internRules) yield 
      (nt, (ts map (_.leaves collect (_.root match { case
				      Left(y) => y} ))))
  }

  /* We prefer to keep the rules sorted from "smaller" to "larger",
   * considering the number of nonterminals required to be passed to a
   * ground term as the "size" */
  val internRules = rules map { case (x,y) => (x,y.toList.sortBy (
	_.leaves.collect ( _.root match { case Left(x) => ntDepthMap(x) })
	match {
	  case List() => 0 //Empty does not like max
	  case x => x max
	}
	))} 

  /* Calculate the "NT-size" of each nonterminal */
  def NTDepthCalc(ruleMap :Map[String, Set[Seq[String]]],
      buildMap:Map[String, Int],
      depth:Int
      ):Map[String, Int] = {
    if(buildMap.keySet == nonterminals)
      buildMap
    else {
      //This is going to be slightly tricky...
      val nextRuleMap = for((nt,set) <- ruleMap) yield (nt, //Nonterminals remain the same
	  // for each member of the set, filter out any nonterminals
	  // which has received its depth
	  set map ( _.filter(!buildMap.isDefinedAt(_)))
	  ) 
	NTDepthCalc(nextRuleMap,
	    buildMap ++ // Add any new nonterminals
	    nextRuleMap.filter(x => !buildMap.isDefinedAt(x._1)).
	    // Which has a rule with no nonterminals left
	    filter { case (x,y) => y.contains(Nil) }.
	    // That is, the last rule with nonterminals left
	    // was from last iteration
	    map { case (x,y) => (x,depth)},
	    depth + 1)
    }


  }

  /* Simple class for keeping track of some generator-related stuff */
  class MemoryTree(r:Int,ss:Seq[Tree[Int]], val d:Int) extends Tree[Int](r,ss) {
  }
  /* Current "position" in the iteration of trees */
  var posTree:MemoryTree = new MemoryTree(0,Nil,0)
  /* Current tree in the iteration */
  var current:Tree[T] = computeCurrent()

  /* Create a new position tree with the specified nonterminal, rule index
   * and target depth */
  def fillMemoryTree(nt:String, ix:Int, depth:Int):Tree[Int] = {
    depth match {
      case 0 => Tree(ix,Nil)

      case x => {
	Tree(ix,
	  for(snt <- simpleRules(nt).toList(ix)) yield
	    fillMemoryTree(snt, 0 , depth - 1)
	)
      }
    }
  }


  /* Recursion step for updating the position tree */
  def stepRecurse(ixt:Tree[Int],
      nt:String,
      depth:Int
      ):Tuple2[Boolean,Tree[Int]] = depth match {
    /* We are at the target depth */
    case 0 => {
      val ix = ixt.root + 1
      if(ix < simpleRules(nt).size)
      /* So we either move forward in the iteration */
	(true,Tree(ix,Nil))
      else
      /* Or we reset to 0 and return failure */
	(false,Tree(0,Nil))
    }
    /* We are at a depth some levels above the target */
    case x => {
      /* What are the nonterminals at the level below? */
      val nts = simpleRules(nt)(ixt.root)
      /* Do the recursion for all subtrees */
      val tss = for((st, snt) <- ixt.subtrees zip nts) yield
	stepRecurse(st, snt, depth - 1) 
   
	val rootInc = ! tss.map(_._1).foldLeft(false)(_ || _)
	if(rootInc) {
	/* If everyone below failed, we attempt an update here */
	  val ix = ixt.root + 1
	  if(ix < simpleRules(nt).size)
	    /* Which we either do */
	    (true,fillMemoryTree(nt,ix,depth - 1))
	  else
	    /* Or fail to do */
	    (false,Tree(0,Nil))
	}else
	  /* One of our subtrees managed to update, now to choose which
	   * ones have changed  */
	  (true, Tree(ixt.root, {
		      var changed = false
		      /* Loop through the former subtrees and the results
		       * from the recursion */
		      for(((inc,t1),t2) <- tss zip ixt.subtrees) yield {
			if(changed)
			  /* If we already passed the updated tree, choose
			   * the old one */
			  t2
			else if(inc) {
			  /* Otherwise, if this is the updated tree, bring
			   * it */
			  changed = true
			  t1
			} else
			  /* This tree failed to iterate, but was before
			   * the iterated tree, so it has been reset  */
			  t1
			}
		      }))
    }
  }

  /* Some internal housekeeping for the position update recursion */
  def step2(sTree:MemoryTree):MemoryTree = {
    val (b, t) = stepRecurse(sTree,
	start,
	sTree.d)
    /* if b is true, the memory tree was updated */
    if(b)
      new MemoryTree(t.root,t.subtrees,sTree.d)
    else{
      /* Otherwise, we need to try again with a deeper depth. */
      val (b2 , t2) = stepRecurse(fillMemoryTree(start,0,sTree.d + 1),
	  start,
	  sTree.d + 1)
	new MemoryTree(t2.root, t2.subtrees, sTree.d+1)
    }
  }

  /* Update the current tree and position, but do not return anything */
  def step(){
    posTree = step2(posTree)
    current = computeCurrent()
  }

  /* Update the current tree and position, and get the new tree */
  def next():Tree[T] = {
    step()
    current
  }

  /* Currently, we will never run out of trees. */
  def hasNext = true

  /* Recurion step for the derivation from position to actual tree */
  def currentRecurse(t   : Tree[Either[String,T]],
		     ixts: List[Tree[Int]]
			): Tuple2[Tree[T], List[Tree[Int]]] = {
    t match {
      case Tree(Left(nt),Nil) => {
      /* We have a nonterminal */
	ixts match {
	  case ixt :: ixttail => {
	    /* And we have a designated rule to choose */
	    val tt = internRules(nt).toList(ixt.root)
	    /* So we take the right-hand tree, and recurse with any more
	     * positions. */
	    val (ret, leftixs) = currentRecurse(tt, ixt.subtrees.toList)
	      //TODO: Warn if leftixs has members
	    (ret, ixttail)
	  }
	  case Nil => {
	    /* We have no designated rule, so we go to ground as fast as
	     * possible by choosing the first rule */
	    val tt = internRules(nt).head
	    val (ret, leftixs) = currentRecurse(tt, Nil)
	    (ret, Nil)
	  }
	}
      }
      case Tree(Right(rt),ss) => {
	/* We are in the middle of the tree, so we need to keep track of
	 * what rules have been chosen and which have not */
	var subixts = ixts
	(Tree(rt, //We move the root from Either[String, T] to T
	      {
		for(st <- ss) yield { // try each subtree in turn
		  val result = currentRecurse(st, subixts)
		  /* Update what rules have been used */
		  subixts = result._2
		  /* And add the resulting Tree[T] to the subtrees */
		  result._1
		}
	      }
	     )
	 /* Finally, return what rules have not been used. */
	 ,subixts)
      }
    }
  }

  /* Compute the current Tree[T] from the position tree */
  def computeCurrent():Tree[T] = {
    val (result, ixls) = currentRecurse(Tree(Left(start),Nil),List(posTree))
      //TODO: warn if ixls != Nil
      result
  }

}


object WRTGrammar {
}


