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
 ** File Name:	   FSAProgressIndicator.java
 ** Author:       Dave Tenny
 ** 
 ** Module Description: The progress indicator interface is used by the
 ** FileSystemAgent class.  An instance of this type is passed to the FSA
 ** when you wish to receive progress indications related to FSA activity.
 ** The default class isn't really meant to be used for other than regression
 ** activity, and simply prints its output to the java console.
 ** @see FileSystemAgent
 ** @see FSTestApplet
 ****************************************************************************/

import java.awt.*;

public class FSAProgressIndicator extends Panel
{

    //higher number means more noise.
    // 0 means silent
    // 1 means display text only
    // 2 means display text and progress dots
    // 3 means display text and percentages
    private int _noiselevel = 0; 


    public FSAProgressIndicator ()
    {
	_noiselevel = 3;
    }

  public FSAProgressIndicator(int noiseLeveL)
  {
      _noiselevel = noiseLeveL;
  }					// FSAProgressIndicator

  /**
     The <code>displayText</code> method should print the indicated text string
     in such a way as to overwrite any previous display from the last call to
     <code>displayText</code>, however newline-separated or other behavior may
     be used by other simple minded implementations of this class.  The idea is
     for applets to update a status area.
  **/

  public void displayText(String s)
  {
      if (_noiselevel > 0) {
	  System.out.println ();
	  System.out.print (s);
	  System.out.flush ();
      }
  }					// displayText


  /**
     The <code>displayPercentage</code> method is meant to update a graphical progress bar.
     TBD: perhaps the argument should be a float.
  **/

  public void displayPercentage(int p)
  {
      if (_noiselevel > 2) {
	  System.out.println(p + "%") ;
	  System.out.flush();
      } else if (_noiselevel > 1) {
	  System.out.print(".");
	  System.out.flush();
      }
  }					// displayPercentage
}					// class FSAProgressIndicator
