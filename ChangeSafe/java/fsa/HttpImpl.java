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
 ** File Name:	   HttpImpl.java
 ** Author:        Unknown.  Derived from uncopyrighted, undocumented,
 **                unprotected materials on the net.
 ** 
 ** Module Description:
 **
 ** HttpImpl is a stupidly named class which implements an HttpUrlConnection
 ** subtype. If you use an an Url, and call openConnection on it,
 ** you get an UrlConnection which is basically did a GET request to an HTTP
 ** server, and which will NOT allow you to write to the server.
 ** By the time you get your URLconnection, you can't use the setDoOUtput(true)
 ** flag either, it's too late.
 **
 ** So we use an HttpURLConnection for any case where you want to modify
 ** the protocol used to connect to an http server.  Theoretically we could
 ** send POST, HEAD, or other things here.  Right now we typically send GET
 ** requests, but leave our options open and call setDoOutput(true)
 ** so we can wrote to the server because we want to use our FileSystemAgent
 ** to talk to the server.
 ****************************************************************************/


import java.io.*;
import java.net.*;
import java.util.Hashtable;
import java.util.Vector;

public class HttpImpl extends HttpURLConnection
{
    Socket sock = null;			// socket we're using for HTTP server I/O
    String auth_header = null;		// authorization header (Mark, please add protocol information)
    // i.e. what HTTP or other protocols we're implementing with this

    // Instantiate, but don't connect.

    public HttpImpl(URL url)
    {
	super (url);
    }

    // Instantiate, don't connect, but perform an authorized login with the connection.

    public HttpImpl(URL url, String authorization)
    {
	super (url);
	auth_header = authorization;
    }


    // Establishes connection to HTTP server and sends request
    // Implementation of abstract method defined in URLConnection

    public void connect() throws IOException
    {
	if (connected) return;
	    					// connected is defined in URLConnection

	InetAddress dst = InetAddress.getByName(getURL().getHost());
	int port;
	if ((port = getURL().getPort()) == -1)
	    port = 80;        // default port number for HTTP
	sock = new Socket(dst, port);
	sock.setTcpNoDelay (true);		// ***** This is critical for FileSystemAgent performance! *****
	OutputStream out = sock.getOutputStream();

	send(out, getRequestMethod() + " " + url.toString() + 
	     " HTTP/1.1\r\n");
	send(out, "User-Agent: " 
	     + "Java Runtime, "
	     + System.getProperty("java.vendor") + " "
	     + System.getProperty("java.version")
	     + " ("
	     + System.getProperty("os.name") + ", "
	     + System.getProperty("os.arch") + ", "
	     + System.getProperty("os.version")
	     + ")\r\n" 
	     );
	if (auth_header instanceof String)
	    send(out, "Authorization: " + auth_header + "\r\n");
	send(out, "\r\n");

	getHeaders();
	connected  = true;
    }

    // Disconnects from HTTP server.
    // Implementation of abstract method defined in HttpURLConnection
    public void disconnect ()
    {
	if (sock != null)
	    {					// try to close the socket
		try
		    {
			sock.close();
		    }
		catch (IOException e)
		    {
		    }
		sock = null;
	    }					// try to close the socket
	connected = false;
    }

    // Implementation of abstract method defined in HttpURLConnection
  
    public boolean usingProxy ()
    {
	return false;			// this simple impl does not use proxy
    }

    // Override default provided in URLConnection

    public InputStream getInputStream() throws IOException
    {
	if (!connected)	connect();
	return sock.getInputStream();
    }

    // Override default provided in URLConnection

    public OutputStream getOutputStream() throws IOException
    {
	if (!connected)	connect();
	return sock.getOutputStream();
    }

    // Helper routine that reads header from HTTP connection
    // Note: these are class member variables used by getHeaders()

    private Hashtable keys = new Hashtable();
    private Vector headers = new Vector();
    private boolean gotten = false;

    void getHeaders() throws IOException
    {
	if (gotten)
	    return;
	gotten = true;
	InputStream in = sock.getInputStream();
	while (true) {
	    String header = recv(in);
	    if (header.length() == 0)
		break;
	    headers.addElement(header);
	    String key = getKey(header);
	    if (key != null)
		keys.put(key, getField(header));
	}
    }					// getHeaders()

    // Override default provided in URLConnection

    public String getHeaderField (int n)
    {
	// getHeaders();
	return (n < headers.size())
	    ?  getField ((String)headers.elementAt(n))
	    : null;
    }

    // Override default provided in URLConnection
    public String getHeaderField (String key)
    {
        // getHeaders();
        return (String)keys.get(key.toLowerCase());
    }

    // Override default provided in URLConnection
    public String getHeaderFieldKey (int n) 
    {
        // getHeaders();
	return (n < headers.size())
	    ? getKey ((String)headers.elementAt(n))
	    : null;
    }

    // Helper routine to send a string to an output stream
    static void send (OutputStream out, String s) throws IOException 
    {
        for (int i=0; i<s.length(); i++)
            out.write((byte)s.charAt(i));
        out.flush();
    }

    // Helper routine to read a newline-terminated string from input stream
    static String recv(InputStream in) throws IOException 
    {
        String result = "";
        int c = in.read();

        while (c >= 0 && c != '\n') {
            if (c != '\r')
                result += (char)c;
            c = in.read();
        }
        return result;
    }

    // Helper routines for parsing header field
    private static final char keySeparator = ':';
    static String getKey(String str) 
    {
        if (str == null)
            return null;
        int ind = str.indexOf(keySeparator);
	return (ind >= 0)
	    ? str.substring(0, ind).toLowerCase()
	    : null;
    }

    static String getField (String str) 
    {
        if (str == null)
            return null;
        int ind = str.indexOf(keySeparator);
	return (ind >= 0)
	    ? str.substring(ind+1).trim()
	    : str;
    }

}
