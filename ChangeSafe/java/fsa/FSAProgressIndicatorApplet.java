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
 ** File Name:	   FSAProgressIndicatorApplet.java
 ** Author:        Dave Tenny
 ** 
 ** Module Description: A subtype of FSAProgressIndicator which
 ** works in the context of an Applet, and tries to render progress 
 ** notifications is visual or other appropriate fashion.
 ****************************************************************************/

import java.awt.*;
import java.net.URL ;
import java.applet.Applet ;
import java.awt.event.*;
import java.awt.TextArea ;
import java.awt.Color ;
import java.awt.BorderLayout ;
//import java.awt.GridLayout ;

public class FSAProgressIndicatorApplet extends    FSAProgressIndicator
{
  protected   Applet                      _applet        = null ;	// applet that owns created this indicator
  protected   TextArea                    _textArea      = null ;
  protected   StringBuffer                _stringBuffer  = null ; // HACK, see displayText caveats
  protected   FSAProgressIndicatorPanel   _fpip          = null ;
  protected   Button                      _cancelButton  = null ;
  protected   String                      _failURL       = null ; // url to use for unsuccessful agent operation
  protected   String                      _frame         = null ; // frame in which to render urls

  // Ideally we'd call System.getProperty("line.seperator")
  // But that requires property privs.  Applet crapola.
  private static String NL = "\n" ;	// works for browsers even on dos, I suspect

  public FSAProgressIndicatorApplet ( FSTestApplet applet, String textForButtonCancel, String failURL, String frame )
  {
    System.out.println ( "FSAProgressIndicatorApplet: constructor - setting BorderLayout.." ) ;
    setLayout ( new BorderLayout() ) ;
    //setLayout(new GridLayout(3,1));
    
    _applet   = applet ;
    _failURL  = failURL ;
    _frame    = frame ;
    
    if (_frame == null)
      _frame = "_self" ;
    
    _textArea = new TextArea(4, 40) ;
    _textArea.setForeground(java.awt.Color.black) ;
    _textArea.setBackground(java.awt.Color.white) ;
    System.out.println ( "FSAProgressIndicatorApplet: constructor: adding _textArea: " + _textArea ) ;
    add("Center", _textArea) ;
    //add( _textArea ) ;


    Panel idPanel = new Panel() ;
    idPanel.setLayout ( new BorderLayout(10,10) ) ;
    idPanel.setBackground(java.awt.Color.white) ;
    
    _fpip = new FSAProgressIndicatorPanel ( "P l e a s e  W a i t . . " ) ;
    System.out.println ( "FSAProgressIndicatorApplet: constructor: adding _fpip: " + _fpip ) ;
    idPanel.add ( "North", _fpip ) ;
    //add ( _fpip ) ;
    
    String _textForButtonCancel = textForButtonCancel ;
    
    if ( _textForButtonCancel == null ) _textForButtonCancel = "Cancel" ;
    _cancelButton = new Button ( _textForButtonCancel ) ;
    _cancelButton.addActionListener ( applet ) ;

    System.out.println ( "FSAProgressIndicatorApplet: constructor: adding _cancelButton: " + _cancelButton ) ;
    idPanel.add ( "East", _cancelButton ) ;
    
    add ( "South", idPanel ) ;
    
  }					// FSAProgressIndicator
  
  public void actionPerformed ( ActionEvent e ) {
      
      Object object = e.getSource() ;
        
      //
      // C a n c e l
      //
      if ( object == _cancelButton ) {
      
             try
               {              // TRY: catch marlformed URL exceptions...
                  if (_failURL != null )
                     _applet.getAppletContext().showDocument(new URL(_failURL), _frame) ;

                  // Yes, there are success/fail/activityStatus combinations yielding no action, and that's ok.
               }              // TRY: catch marlformed URL exceptions...
             catch (java.net.MalformedURLException ef)
               {              // CATCH
                  ef.printStackTrace() ;
               }              // CATCH
         
         
            return ;
      }
  
  }
  

  public void displayText(String s)
  {
    _applet.showStatus(s) ;
    // NS4.04 doesn't support 'append' method, just 'appendText', which is deprecated.  Use
    // setText instead.  This isn't at all useful or efficient for large amounts of text.
    // PERFORMANCE: either use the deprecated interface compiled -deprecated, or require a more recent
    // browser (NS 4.06?) or use something else entirely, which is probably the best thing.
    if (_stringBuffer == null)
      {					// first line
	_stringBuffer = new StringBuffer(1000) ;
	_stringBuffer.append(s) ;
      }					// first line
    else
      {					// subsequent line
	_stringBuffer.append(NL) ;
	_stringBuffer.append(s) ;
      }					// subsequent line

    // By setting the caret position, we force the text area to scroll to the
    // final message.
    String message = _stringBuffer.toString();
    _textArea.setText(message) ;
    _textArea.setCaretPosition (message.length());
    //_textArea.repaint() ; // just this doesn't seem to work, the applet must be repainted
    _applet.repaint() ;
  }					// displayText

  public void displayPercentage(int p)
  {
    //_applet.showStatus(p + "%") ;
    _fpip.displayPercentage ( p ) ;
  }					// displayPercentage
}					// class FSAProgressIndicator
