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
 ** File Name:             DirectoryChooser.java
 ** 
 ** Module Description: 
 **
 **                        Applet to allow selection of directories (not files) from the browser.
 **                        Note that if you're using applications, use the JDK2 Swing JFileChooser
 **                        class, which is much nicer.  This applet assumes we can't use swing in the
 **                        browser (much less JDK2).
 **
 ****************************************************************************/
 
import java.io.*;
import java.awt.*;
import java.applet.*;
import java.net.URL ;
import java.util.Vector;
import java.awt.event.*;
import java.util.StringTokenizer;

public class DirChooserApplet extends Applet {
                              
                                
    //   private boolean _browserRedirected = false ; // true if we've already told the browser where to go
   private boolean _initialized = false ; // true if we have initialized self and started agent thread
    //   private String _successURL = null ;	// url to use for successful agent operation
    //   private String _failURL = null ;	// url to use for unsuccessful agent operation
   private String _frame = null ;	// frame in which to render urls
   private boolean _debug = false ;	// if true, activate serve() diagnostic traces
   private DirChooserThread _agentThread = null ; // the agent and its thread which will do the work.

    // The fake button is here to synchronize events between the java applet
    // and the dir chooser thread.
    private Button fake_button = new Button ("foo");

   ///
   //  i n i t()
   //      
   public void init() {
   
    if (!_initialized)
      myInitialize() ;
    else
      note("Redundant init() calls") ;
   }
      
  // Utility routine to note anomalous but not necessarily failure situations to the java console
  // and applet status area.

  private void note(String s)
  {
    //System.out.println(s) ;
    showStatus(s) ;
  }					// note()

  // Do the one-time work of starting DirChooserThread 

  private void myInitialize()
  {
    if (_initialized)
      {
	note("Erroneous redundant call to myInitialize()") ;
	return ;
      }

    _initialized = true ;

    // The following applet parameters are all optional
    //    _successURL = getParameter("SUCCESSURL") ; // url to use for successful agent operation
    //    _failURL = getParameter("FAILURL") ; // url to use for unsuccessful agent operation
    _frame = getParameter("FRAME") ; // frame in which to render urls
    _debug = (getParameter("DEBUG") != null) ; // if present, activate serve() diagnostic traces

    if (_frame == null)
      _frame = "_self" ;

    // Start a thread to do all the interesting work, at which point this (preexisting) applet
    // thread just becomes a vehicle for noting the progress of the FSA.  The FSA and this
    // applet communicate via a shared FSAProgressIndicator object.
    _agentThread = new DirChooserThread ( this, null, fake_button ) ;
    Thread rt = new Thread ( _agentThread,  "Hi" ) ;
    rt.start() ;		// thread start() method ultimately invokes thread's run() method.
  }					// myInitialize


   /**
    *    Parse a list of decimal integers delimited by the
    *    given separator,
    **/
//    private int[] parseInt(String s, String separator )
//    {
      
//       StringTokenizer st = new StringTokenizer ( s, separator ) ;
      
//       int[] result       = new int[st.countTokens()] ;
      
//       for ( int i = 0 ; i < result.length ; i++ )
//          result[i] = Integer.parseInt ( st.nextToken() ) ;
      
//       return result ;
//    } // parseInt

   
   
         
//        // This interface is deprecated,  We shouldn't have it.
    /** @deprecated */
//    public boolean handleEvent(Event event) {
//       return super.handleEvent(event);
//    }
         
   public void destroy() {
      //collapsedImg.flush();
      //expandedImg.flush();
   }


   ///
   // actionPerformed
   //        
   
   /*
    */
   void waitCursorOn(){
      setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
   }
   
   void waitCursorOff(){
      setCursor(Cursor.getDefaultCursor());
   }


  // The applet protocol tells us to start.  Guard against multiple start() calls.

  private int _startCalls = 0 ;

  public void start()
  {
    note("Start call: " + _startCalls) ;
    _startCalls++ ;
  }					// start()

  // The applet protocol tells us to stop.   But we don't stop unless we're done, I hope

  private int _stopCalls = 0 ;

  public void stop()
  {
    note("Stop call: " + _stopCalls) ;
    _stopCalls++ ;
  }

    // JavaScript can't call into the agent thread, but it can call this method.
    // We use the `fake button' to emulate a button press.  The DirChooserApplet
    // listens for it, but no one displays it.
    public void setDirectory (String name) {
	//System.out.println ("Called with arg " + name + " len: " + name.length() );
	new EventQueue().postEvent (new ActionEvent (fake_button, ActionEvent.ACTION_FIRST, name));
    }
}
