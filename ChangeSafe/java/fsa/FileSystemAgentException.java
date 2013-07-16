/**
  <code>FileSystemAgentException</code> objects are a catchall class
  used by FileSystemAgent subtypes to report
  exception conditions arising from routine operation.  They are not used to mask other exception types.
  
  @see FileSystemAgent
  @author  Dave Tenny
  **/

public class FileSystemAgentException extends Exception
{
  private String _reason = null ;

  /**
    Construct a <code>FileSystemAgentException</code> with the indicated message.
    
    @param  s  some string describing the nature of the exception.
    **/

  public FileSystemAgentException(String s)
  {
  super(s) ;
  }
}
