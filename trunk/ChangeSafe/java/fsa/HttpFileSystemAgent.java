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
 ** File Name:	   HttpFileSystemAgent.java
 ** Author:        Dave Tenny
 ** 
 ** Module Description:
 **
 ** HttpFileSystemAgent is a subtype of FileSystemAgent which establishes
 ** a socket connection via an HTTP request to some URL, setting up the streams
 ** required by the parent class.
 **
 ** HttpFileSystemAgents don't assume you want the socket streams closed,
 ** if you want them closed, call the close() method defined in the base class
 ** and overloaded here.
 ****************************************************************************/

// import HttpImpl ;
// import FileSystemAgent ;
import java.io.* ;
import java.net.* ;
import java.util.Properties ;

public class HttpFileSystemAgent extends FileSystemAgent
{
  protected URL			_url ; // web server URL to which this agent communicates
  protected HttpImpl		_connection = null ;
  String                        _user_name = null;  // user name as passed in a parameter from the server

  public HttpFileSystemAgent(Properties properties, String url) throws IOException, SecurityException
  {
    _properties = properties;
    open(url) ;
  }

  public HttpFileSystemAgent(Properties properties, String url, String u_name) throws IOException, SecurityException
  {
    _properties = properties;
    _user_name = u_name;
    open(url) ;
  }

  public void open(String url) throws IOException, SecurityException
  {
    String user_name ;

    // Unfortunately, getting the user name just gives us a zero length string under Netscape.
    // Don't assume that the user will have the same name to access e-zChange
    // as he uses on his machine.
    // user_name = System.getProperty("user.name") ;  // throws SecurityException
    // if (0 == user_name.length())
    user_name = _user_name;
    // There are so many things which can go wrong with opening an url, especially for our special
    // service.  We deliberately quash some of the errors here to return just two flavors.
    try
      {
	_url = new URL(url) ;
	if (user_name == null)
	    _connection = new HttpImpl(_url) ;
	else
	    _connection = new HttpImpl(_url, "X-Backdoor " + user_name) ;

	// We'll read from and write to the connection.  Don't let it cache.
	_connection.setDoInput(true) ;
	_connection.setDoOutput(true) ;
	_connection.setUseCaches(false) ;
	_connection.connect() ; // throws IOException
	// The following getXxStream methods throw IOException, UnknownServiceException
	super.open(_connection.getInputStream(), _connection.getOutputStream()) ;

	// Check the response code from the server
	if (_connection.getResponseCode() != HttpURLConnection.HTTP_OK)
	  throw new IOException("Unexpected response from HTTP server: "
					    + _connection.getResponseMessage()) ;
      }
    catch (IOException e)
      {
	_connection.disconnect() ;
	_connection = null ;
	throw e ;
      }
    catch (Exception e)
      {
	_connection.disconnect() ;
	_connection = null ;
	throw new IOException("Problem opening HttpFileSystemAgent: " + e.toString()) ;
      }
  } // open

  public void close() throws IOException
  {
    if (isReady())
      {
	_connection.disconnect() ;
	super.close() ;
      }
  }
}
