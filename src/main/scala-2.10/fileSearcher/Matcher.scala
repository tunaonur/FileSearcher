package fileSearcher
import java.io.File
import scala.annotation.tailrec

class Matcher(filter:String, val rootLocation:String = new File(".").getCanonicalPath(),
    checkSubFolders :Boolean = false, contentFilter : Option[String] = None ) {
  val rootIOObject = FileConverter.convertToIOObject(new File(rootLocation))
  
  def execute() = {
    @tailrec
    def recursiveMatch(files: List[IOObject], currentlist : List[FileObject]) : List[FileObject] = files match{
      case List() => currentlist
      case iOObject :: rest => iOObject match {
            case file: FileObject if FilterChecker(filter) matches file.name => 
              recursiveMatch(rest, file:: currentlist)
            case directory: DirectoryObject => 
              recursiveMatch(rest ::: directory.children, currentlist)
            case _ => recursiveMatch(rest, currentlist)
        
      }
    }
    val matchedFiles = rootIOObject match{
      case file: FileObject if FilterChecker(filter) matches file.name => List(file)
      case directory: DirectoryObject => 
        if(checkSubFolders)recursiveMatch(directory.children(), List())
        else FilterChecker(filter) findMatchedFiles directory.children()
      case _ => List()
    }
    
    val contentFilteredFiles = contentFilter match{
      case Some(dataFilter) => matchedFiles.map(iOObject => (iOObject, Some(FilterChecker(dataFilter).findMatchedContentCount(iOObject.file))))
                                           .filter(matchedTuple=>matchedTuple._2.get>0)
      case None => matchedFiles map(iOObject=>(iOObject, None))
    }
    
    contentFilteredFiles map{case (iOObject, count)=> (iOObject.fullName, count)}
  }
}