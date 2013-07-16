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
 ** File Name:	   FSTest.java
 ** Author:        Dave Tenny
 ** 
 ** Module Description:
 **
 ** FSTest is a test driver for the FileSystemAgent class.
 **
 ** This code is typically invoked from within the Lisp SERVER package
 ** test harness, though it is also useful as a debugging aid when
 ** used by hand.
 **
 ** For it to work properly requires that the lisp HTTP server be
 ** running, and that it is prepared to service the URL specified as
 ** an argument to this module.
 **
 **  Sample usage: java FSTest http://jdt-9000:8000/fstest
 **
 ** The http server must be prepared to do soemthing with the passed
 ** URL.  All this module does is initiate the request by contacting
 ** the HTTP server to process the request, and then enter slave mode
 ** to perform FileSystemAgent activities on behalf of the lisp
 ** server.
 ****************************************************************************/

import java.lang.* ;
// import FSAProgressIndicator ;
// import HttpFileSystemAgent ;

class FSTest
{
  public static void main (String[] args)
  {
    if (args.length == 0)
      {
	System.out.println("Usage: java fstest <URL>") ;
	return ;
      }

    FileSystemAgent agent = null ;

    // Open the HTTP server connection
    try
      {
	agent = new HttpFileSystemAgent(System.getProperties(), args[0]) ;
      }
    catch (Exception e)
      {
	System.out.println(e.toString()) ;
	return ;
      }

    // Activate the file system agent
    try
      {
	agent.serve(true,		// true == turn on debugging traces
		    new FSAProgressIndicator(3)) ; // 3 = very verbose
      }
    catch (Exception e)
      {
	System.out.println(e.toString()) ;
	return ;
      }
    finally
      {
	try
	  {
	    agent.close() ;
	  }
	catch (Exception e)
	  {
	    System.out.println(e.toString()) ;
	    return ;
	  }
      }
    System.out.println("FSTest completed normally") ;
  }
}

