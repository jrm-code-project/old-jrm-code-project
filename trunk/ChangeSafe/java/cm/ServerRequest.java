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
 ** File Name:	   ServerRequest.java
 ** Author:        Dave Tenny
 ** Creation Date: September 1999
 ** 
 ** Module Description:
 ** 
 ** Use the HTTP protocol to package command line requests to HTTP servers.
 ** Note that there are support routines on the HTTP/LISP end of this
 ** which rely on data forms packaged here (in CLI-REQUEST.LSP).
 **
 ** Request arguments are passed in two (complementary and optional) ways:
 ** 1) As URI args via the addUriArg method.
 ** 2) As POST args via the addPostArg method.
 **
 ** Upon successful connection to the server via the connect() method,
 ** you can obtain a ServerResponse object which provides HTTP response info,
 ** and access to the body of the document returned by the server.
 **
 ** TBD: method of inclusion of FSA-answered requests.
 ** TBD: method of specifying GET vs. POST
 **
 ** ****** NOTE ****** NOTE ****** NOTE ****** NOTE ****** NOTE ******
 ** This module should NOT contain CONMAN specific information, and should
 ** really reside in a separate CLI tools package, or in the WEB package.
 ** This should be a fairly generic and reusable CLI tool.
 **
 ****************************************************************************/


import java.io.* ;			// IOException, OutputStream, etc.
import java.net.* ;			// URL, URLEncoder, URLConnection
import java.util.Vector ;

public class ServerRequest
{
  // Private member variables.  Vectors are all vectors of strings.
  private Vector _uriKeys ;		// keys for uri arguments, _uriKeys[n] corresponds to _uriVals[n]
  private Vector _uriVals ;		// values for uri arguments
  private Vector _postKeys ;		// keys for post args _postKeys[n] corresponds to _postVals[n]
  private Vector _postVals ;		// values for post args
  private String _baseURI ;		// scheme, netloc, abspath information

  // Ctor. Pass _baseURI information here.
  // baseURI information should be compatible with URL ctor, and we'll potentially append
  // uri args to it with a "?" before dispatch.

  public ServerRequest(String baseURI)
  {
    _baseURI = baseURI ;
    _uriKeys = new Vector() ;
    _uriVals = new Vector() ;
    _postKeys = new Vector() ;
    _postVals = new Vector() ;
  }					// ServerRequest

  // The implementation here is a bit lame since whether the
  // returned value includes the URI arguments or not depends on
  // whether they've already been appended to _baseURI.  I just need
  // a URL for reporting connection errors though and I'm not about
  // to fix the reat of the implementation for that.
  public String getURI()
  {
      return(_baseURI);
  }


  // Add an uri arg to the uri we'll ultimately dispatch.
  // There should be a key and a value component.  An empty string is a valid value component.

  public void addUriArg(String uriKey, String uriVal)
  {
    _uriKeys.addElement(uriKey) ;
    _uriVals.addElement(uriVal) ;
  }					// addUriArg

  // Add a POST arg to content associated with the uri we'll ultimately dispatch.
  // There should be a key and a value component.  An empty string is a valid value component.

  public void addPostArg(String postKey, String postVal)
  {
    _postKeys.addElement(postKey) ;
    _postVals.addElement(postVal) ;
  }					// addPostArg

  // Encode the appropriate argument state, and attempt a connection to the HTTP server
  // named in the baseURI component specified in the CTOR.  Return a connected HttpURLConnection object
  // if we succeed from which the caller can read the response from the server,
  // or throw an exception otherwise.

  public HttpURLConnection connect()
    throws MalformedURLException,	// bad scheme, netloc, etc..
	   IOException,			// just about all forms of I/O throw this
	   UnknownServiceException	// if we can't get connection output stream
  {
    StringBuffer encodedArgs = new StringBuffer(512) ;

    // Encode uri args if present, and append to the base uri
    int i = 0 ;
    int limit = _uriKeys.size() ;

    if (limit > 0)
      {
	_baseURI = _baseURI + "?" ;	// uri <args> delimiter

	for (; i < limit ; i++)
	  {				// encode and append uri arg
	    encodedArgs.append(URLEncoder.encode((String) _uriKeys.elementAt(i))
			       + "="
			       + URLEncoder.encode((String) _uriVals.elementAt(i))) ;
	    if (i < (limit - 1))
	      encodedArgs.append('&') ;	// uri arg separator
	  }				// encode and append append uri arg
	
	// Zap the base URI specification with the fully argument-populated version
	_baseURI = _baseURI + encodedArgs.toString() ;
      }					// if (limit > 0)

    // Make the connection
    URL url = new URL(_baseURI) ;	// throws MalformedURLexception
    HttpURLConnection urlConnection = new HttpImpl(url) ;
    urlConnection.connect() ;           // throws IOException

    // Encode post args if necessary, and transmit them to the server.
    i = 0 ; limit = _postKeys.size() ;

    if (limit > 0)
      {					// encode and transmit POST arguments
	// Catch exceptions mainly to close connection quickly if we encounter errors
	OutputStream stream = urlConnection.getOutputStream() ;
	// *TBD*: probably need a crlf capable socket-oriented text stream....
	PrintWriter textStream = new PrintWriter(stream) ;

	for (; i < limit ; i++)
	  {				// encode and transmit each post arg
	    // need to call encode again, and output in appropriate POST arglist format
	    // Need HttpUrlConnection for POST requests.
	    System.out.println("*FINISH*") ;
	    textStream.println("*FINISH*") ;
	  }				// encode and transmit each post arg
	textStream.flush() ;
	stream.write('\r') ;		// just in case, CRLF termination
	stream.write('\n') ;
      }					// encode and transmit POST arguments

    // Return the HttpURLConnection object
    return urlConnection; 
  }					// connect()

  // Add args as URI args which will be properly ordered and decoded by the CLI-REQUEST.LSP
  // module.  'startIndex' is the index of the first element in 'args' to start
  // adding.

  public void addUriArgsForCliRequest (String[] args, int startIndex)
  {
    int uriIndex = _uriKeys.size() ;
    for (; startIndex < args.length ; startIndex++, uriIndex++)
      addUriArg(uriIndex + "",		// coerce arg number to string
		args[startIndex]) ; 
  }					// addUriArgsForCliRequest

  // Test interface to call the server...

  public static void main (String[] args)
  {
    // This test will generate an exception if the lisp server isn't listening for this url.
    // CONMAN::TEST-1A will test this interface.
    ServerRequest sr = new ServerRequest(args[0]) ;
    sr.addUriArgsForCliRequest(args, 0) ;

    try
      {
	URLConnection urlConnection = sr.connect() ;
	System.out.println("Content type: " + urlConnection.getContentType()) ;

	// Read what the server printed...  Note the painful transition from stream to data...
	// Sometimes flexibility sucks.
	InputStream inStream = urlConnection.getInputStream() ;
	InputStreamReader reader = new InputStreamReader(inStream) ;
	BufferedReader bufferedReader = new BufferedReader(reader) ;
        String line = bufferedReader.readLine();
	while (line != null) {
            System.out.println ("read: " + line) ;
            line = bufferedReader.readLine();
        }
        System.out.println("End of data.");
      }
    catch (Exception e)
      {
	System.out.println("Exception occurred: " + e.toString()) ;
      }
  }					// main()
	
}					// ServerRequest
