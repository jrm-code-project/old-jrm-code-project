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
 ** Authors:       Dave Tenny
 ** 
 ** Module Description:
 ** 
 ** FSTestApplet is a test driver for the FileSystemAgent class from
 ** an applet.  It contains key enabling calls to the security manager
 ** which are necessary to allow applets to work outside the "sand box".
 ** 
 ** Current support is for NS4 (and maybe NS3, unsure).
 ** Intended near-term support is for NS4 and IE4.
 ** Intended medium-term support is for NS3/4 and IE3/4.
 **
 ** This code is typically invoked from an html file.
 ** though it is also useful as a debugging aid when used by hand.
 ** FSTestApplet.html provides a sample use of the applet.
 **
 ** For it to work properly requires that the lisp HTTP server be
 ** running, and that it is prepared to service the URL specified as
 ** an argument to this module.
 **
 ** PARAMETERS: URL (required), SUCCESSURL (optional), FAILURL (optional),
 ** and NODEBUG (optional).
 ** Applet parameters are specified with the HTML <PARAM> element.
 **
 ** URL identifies the HTTP server and GET URL which will be contacted
 ** in order for this FSA process to work with in slave mode. (The HTTP
 ** server is the master).
 **
 ** SUCCESSURL will be used via the AppletContext.showDocument
 ** interface, causing the browser to display the new URL when when
 ** the operation completes, but only if it completes successfully.
 ** 
 ** FAILURL is used to redirect the browser to an URL which is displayed
 ** when something goes wrong during activity which is aided by the FSA.
 **
 ** FRAME is used to indicate the name of the frame in which SUCCESSURL
 ** or FAILURL (as appropriate) are to rendered by the browser.
 **
 ** DEBUG, if specified, enables debugging diagnostics from the FSA
 ** serve() method.  Debugging traces are printed to
 ** the Java console.  The default is no debugging traces.
 **
 ** Sample usage within .html file: <param name="URL"
 ** value="http://harrison.ne.mediaone.net:7111/test-1a">
 **
 ** The http server must be prepared to do something with the passed
 ** URL. All this module does is initiate the request by contacting
 ** the HTTP server to process the request, and then enter slave mode
 ** to perform FileSystemAgent activities on behalf of the lisp
 ** server.
 **
 ** Implementation note:
 ** We run the FSA as a separate thread, because the applet MUST be on its
 ** own and relatively non-blocking thread in order to process any updates
 ** based on repaint() operations and other browser events sent to the applet.
 ** Techically you can run the FSA stuff from the start() or init() methods,
 ** but then you can't repaint the browser real-estate for the applet at all.
 ** (repaints get queued, and done all at once when you're done blocking
 ** init/start).
 ****************************************************************************/

import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.net.URL ;
// import FSAProgressIndicatorApplet ;
// import FSAAppletThread ;

public class FSTestApplet extends Applet implements ActionListener
{
  private boolean _browserRedirected = false ; // true if we've already told the browser where to go
  private boolean _initialized = false ; // true if we have initialized self and started agent thread
  private String _url = null ;		// url to call for the checkin operation
  private String _successURL = null ;	// url to use for successful agent operation
  private String _failURL = null ;	// url to use for unsuccessful agent operation
  private String _frame = null ;	// frame in which to render urls
  private String _textForButtonCancel = null ; // text to use in cancel button
  private boolean _debug = true ;	// if true, activate serve() diagnostic traces
  private FSAAppletThread _agentThread = null ; // the agent and its thread which will do the work.
  private FSAProgressIndicator _indicator = null ; // process indicator we'll use in agent 
  String _user_name = null ;            // for receiving USERNAME parameter

    public String getAppletInfo()
    {
        return "FileSystemAgent Applet  ChangeSafe,LLC";
    }

  // Applet protocol tells us to begin work.  Guard against buggy browsers which invoke init redundantly.

  public void init()
  {
    if (!_initialized)
      myInitialize() ;
    else
      note("Redundant init() calls") ;
  }					// init()

  // Utility routine to note anomalous but not necessarily failure situations to the java console
  // and applet status area.

  private void note(String s)
  {
    System.out.println(s) ;
    showStatus(s) ;
  }					// note()

  // Do the one-time work of starting FSAAppletThread 

  private void myInitialize()
  {
    System.out.println ( "FSTestApplet: myInitialize()" ) ;
    if (_initialized)
      {
	note("Erroneous redundant call to myInitialize()") ;
	return ;
      }

    _initialized = true ;

    System.out.println ( "FSTestApplet: myInitialize(): fetching URL param.." ) ;
    if ((_url = getParameter("URL") )== null)
      {					// URL parameter is required
	note("Missing <URL> parameter.") ;
	return ;
      }					// URL parameter is required

    // The following applet parameters are all optional
    System.out.println ( "FSTestApplet: myInitialize(): fetching SUCCESSURL param.." ) ;
    _successURL = getParameter("SUCCESSURL") ; // url to use for successful agent operation
    _failURL = getParameter("FAILURL") ; // url to use for unsuccessful agent operation
    _frame = getParameter("FRAME") ; // frame in which to render urls
    _debug = (getParameter("DEBUG") != null) ; // if present, activate serve() diagnostic traces
    _user_name = getParameter("USERNAME");     // name of the user, because we can't figure
                                               // it out for ourselves in Netscape.
    _textForButtonCancel = getParameter("CANCEL_BUTTON_TEXT");

    if (_frame == null)
      _frame = "_self" ;

    // Start a thread to do all the interesting work, at which point this (preexisting) applet
    // thread just becomes a vehicle for noting the progress of the FSA.  The FSA and this
    // applet communicate via a shared FSAProgressIndicator object.
    System.out.println ( "FSTestApplet: myInitialize(): setting BorderLayout().." ) ;
    setLayout ( new BorderLayout() ) ;
    _indicator = new FSAProgressIndicatorApplet(this, "Cancel", _failURL, _frame ) ;
    System.out.println ( "FSTestApplet: myInitialize(): adding progress inficator: " + _indicator ) ; 
    add ( "Center", _indicator ) ;
    _agentThread = new FSAAppletThread(this, _url, _indicator, _debug) ;
    System.out.println ( "FSTestApplet: myInitialize(): starting agent thread: " + _agentThread ) ;
    _agentThread.start() ;		// thread start() method ultimately invokes thread's run() method.
  }					// myInitialize

  /**
     Decide what to do in response to various browser/applet protocol invocations.

     At this point, our agent thread may be in one of several states:

     1) not started
     2) started but not finished
     3) finished
  
     Ignore cases 1 and 2, but if case 3 is the situation, and we haven't already redirected the
     browser, do so now.

     If DEFINITIVELY is specified, we know the server applet is done.  Otherwise we don't,
     and try to guess from the thread's status.
  **/

  public void maybeRedirectBrowser(boolean definitively)
  {
    if (_browserRedirected)
      return ;				// we've already told the browser where to go

    // See if we need to tell the browser where to go, or whether it's premature.
    if (_agentThread.isAlive() && !definitively)
      return ;				// cases 1 & 2, not started or not finished

    // If we reach this point, the FSA agent thread is done.  Try to redirect the browser
    _browserRedirected = true ;
    
    boolean activityStatus = _agentThread.wasSuccessful() ;

    if (!activityStatus && (_agentThread.getException() != null))
      note("FSA thread failed, reason: " + _agentThread.getException().toString()) ;

    try
      {					// TRY: catch marlformed URL exceptions...
	if (_successURL != null && activityStatus == true)
	  {				// update the browser page to point to SUCCESSURL
	    // URL ctor may throw java.net.MalformedURLException, we assume caller has it right
	    // and let the existing catch block handle any problems.
	    getAppletContext().showDocument(new URL(_successURL), _frame) ;
	  }				// update the browser page to point to SUCCESSURL

	if (_failURL != null && activityStatus == false)
	  getAppletContext().showDocument(new URL(_failURL), _frame) ;

	// Yes, there are success/fail/activityStatus combinations yielding no action, and that's ok.
      }					// TRY: catch marlformed URL exceptions...
    catch (java.net.MalformedURLException e)
      {					// CATCH
	note(e.toString()) ;
      }					// CATCH
  }					// maybeRedirectBrowser

  // The applet protocol tells us to start.  Guard against multiple start() calls.

  private int _startCalls = 0 ;

  public void start()
  {
    note("Start call: " + _startCalls) ;
    _startCalls++ ;
    maybeRedirectBrowser(false) ;
  }					// start()

  // The applet protocol tells us to stop.   But we don't stop unless we're done, I hope

  private int _stopCalls = 0 ;

  public void stop()
  {
    note("Stop call: " + _stopCalls) ;
    _stopCalls++ ;
    maybeRedirectBrowser(false) ;
  }

    public void actionPerformed ( ActionEvent e ) {
      
      //
      // C a n c e l
      //
		   _agentThread.stop(); // throw a ThreadDeath
         
            return ;

  
  }


}					// class FSTestApplet

