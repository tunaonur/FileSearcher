

package fileSearcher
import org.scalatest.FlatSpec

import java.io.File

class MatcherTests extends FlatSpec{
  val testFilesRootPath = "/Users/PCcik/Dev/ScalaDev/project/"
  
  "Matcher that is passed a file matching the filter" should
  "return a list with that file name" in{
    val matcher = new Matcher("fake", "fakepath")
    
    val results = matcher.execute()
    
    assert(results == List(("/Users/PCcik/Dev/ScalaDev/fakepath", None)))
  }
  
  "Matcher using a directory containing one file matching the filter" should
  "return a list with that file name" in{
    val matcher = new Matcher("txt", new File("./testfiles/").getCanonicalPath())
    
    val results = matcher.execute()
    
    assert(results == List((s"${testFilesRootPath}readme.json", None)))
  }
  
    "Matcher with subfolder checking matching a root location with subtree files matching the filter" should
  "return a list with that file name" in{
      
    val searchSubDirectories = true;   
      
    val matcher = new Matcher("txt", new File("./testfiles/").getCanonicalPath(), searchSubDirectories)
    
    val results = matcher.execute()
    
    assert(results == List((s"${testFilesRootPath}sub/notes.txt", None), (s"${testFilesRootPath}readme.json", None)))
  }
  
  "Matcher that is not passed a root file location" should 
  "use the current location" in{
    val matcher = new Matcher("filter")
    assert(matcher.rootLocation == new File(".").getCanonicalPath())
  }
  
  "Matcher given a path that has one file that mactches the file filter and content filter" should
  "return a list with that file name" in{  
      
    val matcher = new Matcher("data", new File(".").getCanonicalPath(), true, Some("pluralsight"))
    
    val matchedFiles = matcher.execute()
    
    assert(matchedFiles == List((s"${testFilesRootPath}pluralsight.data", Some(3))))
  }
      
  "Matcher given a path that has no file that mactches the file filter and content filter" should
  "return an empty list" in{  
      
    val matcher = new Matcher("txt", new File(".").getCanonicalPath(), true, Some("pluralsight"))
    
    val matchedFiles = matcher.execute()
    
    assert(matchedFiles == List())
  }
   
}