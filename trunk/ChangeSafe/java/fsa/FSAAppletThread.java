/*****************************************************************************
 **	      Copyright (c) 1999 Content Integrity, Inc.
 **	      ALL RIGHTS RESERVED.
 **
 **	      Content Integrity, Inc. CONFIDENTIAL and PROPRIETARY material.
 **
 **           Content Integrity, Inc
 **           Braintree Executive Office Park
 **           P.O. Box 850942
 **           Braintree, MA 02185-0942
 **
 **  This software and information comprise valuable intellectual property
 **  and trade secrets of Content Integrity, Inc., developed at substantial
 **  expense by Content Integrity, which Content Integrity intends to
 **  preserve as trade secrets.  This software is furnished pursuant to a
 **  written license agreement and may be used, copied, transmitted, and
 **  stored only in accordance with the terms of such license and with the
 **  inclusion of the above copyright notice.  This software and
 **  information or any other copies thereof may not be provided or
 **  otherwise made available to any other person.  NO title to or
 **  ownership of this software and information is hereby transferred.
 **  Content Integrity assumes no responsibility for the use or reliability
 **  of this software.
 **
 *****************************************************************************
 **
 ** File Name:	   FSTestApplet.java
 ** Author:       Dave Tenny
 ** 
 ** Module Description:
 **
 ** Do the main work of creating a FileSystemAgent, connecting to
 ** a server, and invoking the FSA serve() interface.  Do all this
 ** in a separate thread so that the applet which spawned us
 ** can independently repaint() and perform other operations in
 ** its thread while we go about the merry business of operating the
 ** FSA.
 ****************************************************************************/

import java.awt.*;
import java.applet.*;
// import FSTestApplet ;
// import FSAProgressIndicatorApplet ;
// import HttpFileSystemAgent ;
import java.util.Properties ;

public class FSAAppletThread extends Thread
{
  private FSTestApplet _applet ;	// applet which spawned us
  private String _url ;			// url we use for FSA connect
  private boolean _debug ;		// if present, activate serve() diagnostic traces
  private FileSystemAgent _agent = null ; // the agent which will do the work.
  private FSAProgressIndicator _indicator = null ; // process indicator we'll use in agent 
					// it is a shared object between the applet and thread, caution.
  private boolean _successful = false ;	// set to true when overall thread execution is done if sucessful
  private Exception _failureException = null ; // set to the exception which caused failure,
					// if _successful is false and we didn't run. May be null even if
					// _successful is false.

  /**
     Create a thread which runs the creates a FileSystemAgent, connects it to an HTTP server,
     and invokes its serve() method.

     APPLET is the FSTestApplet which spawned us.  This is a hack.  We basically need to
     use the URL redirection logic when we're done.  However I was too lazy to move the
     code into this class or (better) a helper class.  

     URL is the url to use in connecting to the HTTP server.

     FSAProgressIndicator is the indicator used by the agent to communicate progress, it may be null
     but is typically somthing which works as an intermediary between the applet which spawned this thread
     and the server executing in this thread.

     DEBUG is true if we want low-level FSA diagnostic tracebacks, false otherwise.
     Note that tracebacks when written to the Netscape java console will fill the console, with
     much output going to the bit bucket until you click on the console's CLEAR button.
  **/

  public FSAAppletThread(FSTestApplet applet, String url, FSAProgressIndicator indicator, boolean debug)
  {
    _applet = applet ;
    _url = url ;
    _indicator = indicator ;
    _debug = debug ;
  }					// FSAAppletThread

  // This is the primary thread interface, started by invoking the start() method on the thread.
  // It should NOT be called directly.

  public void run()
  {
    _successful = true ;

    connect() ;				// attempt security calls, and connect to server

    if (!_successful)
      {					// we weren't successful, end it here...
	close() ;			// exception proof
	return ;
      }					// we weren't successful, end it here...

    // Try to invoke the serve() interface
    serve() ;				// exception proof
    close() ;				// exception proof

    // If we are unsuccessful but there is no exception, it often means the FSA serve()
    // method is returning to us the fact that from the lisp server standpoing, the operation
    // for which the serve() method was being used has failed.  In otherwords, the server is telling
    // us to return a failed status code, so that someone will redirect the user browser to a URL
    // with information about the failure of the operation.

    if (_debug)
      if (_successful)
	System.out.println("FSAAppletThread succeeded.") ;
      else
	System.out.println("FSAAppletThread failed, reason: "
			   + ((_failureException != null) ? _failureException.toString() : "<unknown>")) ;

    _applet.maybeRedirectBrowser(true) ; // we're done.  Signal the applet to update the browser page.
  }					// run()

  // Return true if FSA thread was successful in its mission, false otherwise.

  public boolean wasSuccessful()
  {
    return _successful ;
  }

  // Return the exception which caused wasSuccessful() to return false, or NULL if there
  // was no exception (in which case the failure mode is unknown, and likely due to the fact
  // that the thread never ran, or some other high level control problem exists).
  // Missing exception information when the thread failed may also indicate a failure return
  // code from the serve() interface, in which case the server is telling us to redirect to a failure
  // URL for more details.

  public Exception getException()
  {
    return _failureException ;
  }

  /**
     Some error occurred in running this thread, note it for later query
     with the wasSuccessful() and getException() interfaces.
     If we're noting a failure and once was already recorded, we don't overwrite it so that the
     caller may see the reason of the original point of failure.
  **/

  private void noteFailure(Exception e)
  {
    System.out.println(e.toString()) ;
    _successful = false ;
    if (_failureException == null)
      _failureException = e ;
  }					// noteFailure

  // Attempt to close the server.  If the connect failed, the close will likely fail too.
  // In fact if just about any condition arose during connect()/serve(), close will probably
  // fail. 

  private void close()
  {					// close the FSA connection
    if (_agent != null)
      {					// agent != null
	try
	  {
	    _agent.close() ;
	  }
	catch (Exception e)
	  {
	    noteFailure(e) ;
	  }
      }					// agent != null
  }					// close the FSA connection

  // Allocate the FSA and establish an HTTP connection to the url specified in our CTOR.
  // We don't throw the security exceptions out of this routine, because this routine
  // encapsulates the browser-specific security calls (not that it's a good reason for not throwing,
  // but this is as good a place as any to deal with all browser/exception specific stuff
  // related to connection open logic.

  public void connect()
  {
    // Open the HTTP server connection
    try
      {					// TRY: request security privs and open FSA
	// Ask for added priviledges
        try
	  { 
	      netscape.security.PrivilegeManager.enablePrivilege("UniversalConnect"); 
	      netscape.security.PrivilegeManager.enablePrivilege("UniversalPropertyRead");
	  } 
        catch(netscape.security.ForbiddenTargetException e)
	  { 
	    noteFailure(e) ;
	    return ;
	  }
        
	_agent = new HttpFileSystemAgent(System.getProperties(), _url, _applet._user_name) ;
      }					// TRY: request security privs and open FSA
    catch (Exception e)
      {					// CATCH: security and agent creation problems
	noteFailure(e) ;		// connect() caller will close()
      }					// CATCH: security and agent creation problems
  }					// connect

  // Attempt to invoke the agent serve() method.  If we're called we assume that the
  // initialization of the connection has proceeded without error.

  public void serve()
  {
    try
      {					// TRY: request privs required for serve
	// Ask for added priviledges
  	try
  	  {
	      netscape.security.PrivilegeManager.enablePrivilege("UniversalFileAccess" );
	      netscape.security.PrivilegeManager.enablePrivilege("UniversalPropertyRead" );
	  }
	catch(netscape.security.ForbiddenTargetException e)
	  {
	    noteFailure(e) ;		// we deliberately continue here, see next comment
	  }

	// Note, if we don't invoke serve(), the lisp server will send requests to an FSA which isn't
	// there.  We have to handle this anyway, but we might get cleaner and more useful error
	// messages by letting file system agent operations fail if 'enablePrivilege' didn't really
	// give us the priv.  We could also detect priv failure and not call serve(), letting the
	// server fend for itself.  As coded here, the FSA detects failure conditions, traps them,
	// reports them to the server, and these conditions are then reported in the showDocument call.
	// Can we do better?  Sure.  But we haven't yet.

	_successful = _agent.serve(_debug, _indicator) ; // debug true => turn on debugging
      }					// TRY: request prives required by serve()
    catch (ThreadDeath td) {
	noteFailure (new FileSystemAgentException ("Action Cancelled."));
	// throw td; //shouldn't do this!
    }
    catch (Exception e)
      {					// CATCH: priv or agent operation exception
	noteFailure(e) ;		// caller will invoke close()
      }					// CATCH: priv or agent operation exception
  }					// serve()
}					// class FSAAppletThread

