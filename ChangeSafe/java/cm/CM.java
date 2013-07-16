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
 ** File Name:	   cm.java
 ** Author:        Command dispatcher for all ConMan CLI commands.
 ** Creation Date: September 1999
 **
 ** Module Description:
 **
 ** Take the command line arguments, use ServerRequest to send them off
 ** to the ConMan server, and relay the results.
 **
 **
 ** CM.Java implements the following protocol.
 **
 ** Ideally we'd return the protocol to be interpreted by the CM CLI client in the content
 ** type, and ideally one of these types would include XML.
 **
 ** For now, we interpret the following protocols:
 **
 ** 1) "cli/response"
 **
 **    When this content type is encountered, CM.JAVA enters a loop to read commands
 **    and respond accordingly.
 **
 **    The server replies with a sequence of token/value pairs.
 **    Tokens are typically symbols, and values are interpreted based on the token.
 **    Valid tokens include:
 **
 **    TRACE b\n       ; turn on tracing for debug purposes, traces the receit of these codes.
 **		         'b' is 1 or 0
 **    RETURN x\n      ; return value for the operation
 **    DISPLAY "text"\n ; print out text to command standard output, which is encoded as a lisp string.
 **		         We read until the terminating quote.
 **    STARTFSA\n      ; start the FSA on the current connection.
 ** 		         No further chars on read on the connection by CM.JAVA
 ** 		         until the FSA exits, at which point we resume
 ** 		         reading the connection.
 **
 **    Later we may wish to add codes like...
 **
 **    QUERY "prompt-text"\n ; ask the user for a value
 **    MENU n "selection1" [... <selection n>]\n ; prompt user for value from selections
 **
 **    etc.
 **
 ** 2) "text/html"
 **    YOU WISH.  We don't support this in the client.  Yet.
 ** 3) "???/xml"
 **    You're still wishing.  We don't support this in the client.  Yet.
 ****************************************************************************/


/* import ServerRequest ; */
import java.net.URLConnection ;
import java.io.* ;
import java.lang.Object ;
import java.util.Properties ;
import java.net.ConnectException ;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.MalformedURLException;
import java.util.Calendar ;
import java.util.GregorianCalendar ;
import java.util.TimeZone;
import java.util.StringTokenizer;
import java.util.NoSuchElementException;
import java.util.Vector;

public class CM
{
  // Private member variables
  private int _failCode = 1 ;		// code to use to signal client failure
  private PrintStream _stdout = null ;	// standard output stream for CM operation output
  private PrintStream _stderr = null ;	// standard error stream for CM operation output
  private InputStream _stdin = null ;	// standard input stream for CM operation input (user input)
  private String[] _argv = null  ;	// command line argument vector
  private HttpURLConnection _urlConnection = null ; // connection we're using to read from server
  private InputStream _connectionInputStream = null ; // byte stream allowing us to read from server

    // The original current directory that we were launched from.
    // Some commands change directory, we save this to go back.
    private static String _original_current_directory = null;


  // name of the ConMan `rc' file where the workspace specific
  // information is stored.  This should always be .csf in
  // production code, but we can override it for testing so that we
  // don't trash the workspace.
  private static String _dot_conman_file_name = ".csf" ;

  private static String _validation_command = "client_self_test";
    // For future expansion.
    private static String _batch_command = "batch_mode";

  // The prefix that is prepended to the command name to identify the
  // request as a command invocation.  This value must be kept in sync
  // with that of *CM-CLI-COMMAND-URI-PREFIX* in cm-cli.lsp.  Note
  // that here we don;t include the leading and trailing slashes
  // though.
  private static String _command_URI_prefix = "csf/command/";

  // Name of the ConMan preferences file where both user and site specific preferences are stored.
  // The site should have a file of this name installed in the same directory as this class file.
  // The user can override the site options by having a file of this name in his home directory.
  private static String _preferences_file_name = ".csf-prefs" ;
  // The preferences object from which we extract configuration parameters.  Whatever preference
  // files we found are loaded to here by the call to setupPreferences() in main();
  private Properties _preferences = null;
  // The currently supported preferences are:
  private static String _pref_server_uri = "CHANGESAFE_SERVER_URI" ;
  private static String _pref_read_only  = "SET_FILE_READ_ONLY" ;
  private static String _pref_read_write = "SET_FILE_READ_WRITE" ;
  private static String _pref_last_modified = "SET_FILE_LAST_MODIFIED" ;
  private static String _pref_rcfilename = "RCFILENAME";
    private static String _pref_trace_shell_commands = "TRACE_SHELL_COMMANDS" ;
  // (end of _preferences Properties keys)

    // These are the actual values of the preference once we have deduced
    // what they are.
    private static String _server_uri    = null;
    private static String _dot_conman    = null;

  // The following two streams were nice to use, but cause synchronization problems if used
  // in conjunction with the FSA reading from _connectionInputStream because they read ahead
  // and steal FSA's bytes from the byte stream.
  //private InputStreamReader _connectionReader = null ; // bridge byte stream to character stream
  //private BufferedReader _connectionBufferedReader = null ; // buffered character stream reads from server

  private OutputStream _connectionOutputStream = null ;	// connection we're using to write to server
  private boolean _connectComplete = false ;
  private boolean _debugTrace = true ;

  // Constants
  private static char NEWLINE_CHAR = (char)'\n' ;
  private static int MinErrorNumber = 10000;

    private String canonicalize_outgoing_pathname (File file) throws IOException
    {
	// Return a String object that represents the file.  The trailing
	// separator must be there if the file is a directory and not if it isn't.
	return
		     // Yuck!  When java probes a file that happens to be a directory,
		     // the name of the file doesn't end with a slash.
		     // Except when it is the root directory.
		     // So we put the slash back on directories, except if it already
		     // has a slash.  Who is responsible here?
	     (file.isDirectory() && !file.getCanonicalPath ().endsWith (File.separator))
	     ? file.getCanonicalPath () + File.separator
	     : file.getCanonicalPath() ;
    }

    private int random_int (int limit) {
	double random_double = Math.random ();
	for (int i = 0; i < limit; i++)
	    if ((i/limit) > random_double)
		return i;
	return limit - 1;
    }

  // Construct a CM object given an argument vector from the command line.
  // No connection is attempted until the connect() method is called.

  public CM (String[] argv)
    {
	String _read_only     = null;
	String _read_write    = null;
	String _last_modified = null;
	_stdout = System.out ;
	_stderr = System.err ;
	_stdin = System.in  ;
	_argv = argv ;

	setupPreferences ();

	// *TBD*: how to ascertain host name.  Environment variable?
	// Allow override from command line.
	_server_uri = System.getProperty (_pref_server_uri);
	if (_server_uri == null) _server_uri = _preferences.getProperty(_pref_server_uri);
	if (_server_uri == null) _server_uri = "http://localhost:7999/";

	_dot_conman = System.getProperty (_pref_rcfilename);
	if (_dot_conman == null) _dot_conman = _dot_conman_file_name;

	_read_only = System.getProperty (_pref_read_only);
	if (_read_only != null) _preferences.put (_pref_read_only, _read_only);

	_read_write = System.getProperty (_pref_read_write);
	if (_read_write != null) _preferences.put (_pref_read_write, _read_write);

	_last_modified = System.getProperty (_pref_last_modified);
	if (_last_modified != null) _preferences.put (_pref_last_modified, _last_modified);
    }

    ServerRequest construct_server_request (URL server_url, String command, boolean priority_flag) {

	// Determine the url abs_path encoding for the request, which is args[0]
	// If it's missing, the server will do something appropriate for what is essentially a "/" url.
	String abs_path = (command == null)
	    ? _command_URI_prefix + ((_argv.length > 0) ? _argv[0] : "help")
	    : command;

	ServerRequest serverRequest = new ServerRequest(server_url.toString() + abs_path)  ;

	// Turn on debugging if we are validating.
	if ((_argv.length > 0) && (_argv[0].toLowerCase().equals(_validation_command)))
	    _preferences.put (_pref_trace_shell_commands, "T");
	else
	    _preferences.put (_pref_trace_shell_commands, "NIL");

	// Encode command line args compatible with CLI-REQUEST.LSP/ServerRequest.Java
	// Skip the first arg, which, if present, indicates the command name and is encoded as the
	// abs_path portion of the url.
	if (_argv.length > 1)
	    serverRequest.addUriArgsForCliRequest(_argv, 1) ;

	// Determine if we have defaulted context that we care about.
	// These named context variables which are encoded with URI keys which are not encoded integers
	// are observed as being 'default context' by the server.

	// **** NOTE ***** addUriArg keys here are known to match well-known constants in lisp.
	// They're named with the literals as used below.

	if (priority_flag)
	    // *cm-cli-uri-key-allow-busy-redirect* in cm-cli-command-syntax-constants.lsp
	    serverRequest.addUriArg("ALLOW-REDIRECT", "NIL");
	else
	    serverRequest.addUriArg("ALLOW-REDIRECT", "T");

	// User name context.
	String userName = System.getProperty("user.name") ;
	if (userName != null)
	    // *cm-cli-uri-key-user-name* in cm-cli-command-syntax-constants.lsp
	    serverRequest.addUriArg("USER-NAME", userName) ;

	// This value is necessary if we are to make sense of the
	// pathnames.

	String clientPlatform = System.getProperty ("os.name");
	if (clientPlatform != null)
	    // *cm-cli-uri-key-client-platform* in cm-cli.lsp
	    serverRequest.addUriArg ("CLIENT-PLATFORM", clientPlatform);

	// May not be necessary?  Should be able to resolve relative names in the client,
	// but if we do server relative, we need this.
	// Current directory context.
	try {
	    String currentDirectory = System.getProperty ("user.dir");
	    if (currentDirectory != null)
		// *cm-cli-uri-key-current-directory* in cm-cli.lsp
		serverRequest.addUriArg ("CURRENT-DIRECTORY",
					 canonicalize_outgoing_pathname
					 (new File (currentDirectory)));
	}
	// If we can't get the current directory, (for some reason??)
	// just punt.  Lisp will complain.
	catch (java.io.IOException e) {};

	try {
	    String userHomedir = OSServices.GetUserHomeDirectory();
	    if (userHomedir != null)
		// *cm-cli-uri-key-current-directory* in cm-cli.lsp
		serverRequest.addUriArg ("USER-HOME-DIRECTORY",
					 canonicalize_outgoing_pathname
					 (new File (userHomedir)));
	}
	// If we can't get the user's home directory, (for some reason??)
	// just punt.  Lisp will complain.
	catch (java.io.IOException e) {};

	// Pass the client's timezone offset
	GregorianCalendar now = new GregorianCalendar();
	serverRequest.addUriArg("TIMEZONE-OFFSET",
				(new Integer(now.get(Calendar.ZONE_OFFSET)/(60*1000))).toString() ); // in minutes

	// else it won't be passed, and if there is no "-user" or other communication of the user to the
	// server, the server may reject the requeset.

	// If .conman exists, add its contents to the command line arguments
	File rcPath = findRCFile(_dot_conman) ;
	String conmanFileContents = getFileContents(rcPath) ;// null or .conman file contents
	if (conmanFileContents != null)
	    {					// pass the RC file contents to the server
		// *cm-cli-conmanrc* in cm-cli.lsp
		serverRequest.addUriArg("CHANGESAFERC", conmanFileContents) ;

		// If you had an rc file, the server might like the path of the rcfile as well
		// *cm-cli-uri-key-rcpath* in cm-cli.lsp
		serverRequest.addUriArg("RCPATH", rcPath.getAbsolutePath()) ; // use getCanonicalPath instead?
	    }					// pass the RC file contents to the server

	return serverRequest;
    }

  // stream should be an InputStream for a Java Properties file.
  // If we are able to load properties from the stream then we set
  // _preferences to this new Properties object and set its defaults
  // to be the old value of _preferences.  This, the properties file
  // which is searched first is the one for which setupPropertiesFile
  // was most recently called.
  private void setupPropertiesFile(InputStream stream) throws IOException
  {
      Properties props = new Properties(_preferences);

      props.load(stream);
      // If we got an error while loading then the following wouldn't happen.
      // Loading was sucessful, make this the current preferences object.
      _preferences = props;
  }

  // Read the .csf-prefs file found in the same directory as the CM.class file.
  // See the comments about _preferences_file_name for details.
  private void setupPreferences ()
  {

      _preferences = new Properties ();

    // Get installation-wide properties first
      try {
	  java.net.URL prefs_name = getClass().getResource(_preferences_file_name);
	  InputStream s = getClass().getResourceAsStream(_preferences_file_name);
	  if (s == null)
	      System.err.println("Can't find preferences from " + prefs_name.toString());
	  else {
	      setupPropertiesFile(s);
	      s.close();
	  }
      }
      catch (Exception e) {
	  System.err.println("Could not read resource (file) " + _preferences_file_name + " in the client class directory.");
      }

      // This is a significant security hole!!
      // An attacker can get a user to unwittingly execute a random command
      // if he can edit the users .csf-prefs file.

    // then get user properties
      String path = OSServices.GetUserHomeDirectory();
      if (path != null) {
	  path = path + _preferences_file_name;
	  try {
	      FileInputStream s = new FileInputStream(path);
	      if (s != null) {
		  setupPropertiesFile(s);
		  s.close();
	      }
	  }
	  catch (FileNotFoundException e) {}
	  catch (IOException e) {
	      System.err.println("Problem reading preferences file " + path);
	  }
      }
  }


  // Search for the specified file name starting with the current working directory
  // or the directory specified in 'fileName',
  // and proceeding up in the directory hierarchy until we reach the root.
  // If we can't find the file, return null.  Otherwise return a File object which
  // describes the file we found.

  private static File findRCFile(String fileName)
  {
    File file = new File(System.getProperty ("user.dir"), fileName) ;
    if (file.exists() && !file.isDirectory())
      return file ;

    // Process parent directories.  This is a bit tricky.  Parent on the file with .csf
    // in it yields the file we just probed, so we create a file with that parent, then get *its* parent,
    // to get the parent directory of the current parent directory.  Sigh.
    String parent = System.getProperty ("user.dir");
    File parentFile = new File(parent) ;
    parent = parentFile.getParent() ;

    while (parent != null)
      {					// while (parent), look in parent directories
	file = new File(parent, fileName) ;
	if (file.exists() && !file.isDirectory())
	  return file ;
	parent = file.getParent() ;
	parentFile = new File(parent) ;
	parent = parentFile.getParent() ;
      }					// while (parent), look in parent directories

    return null ;
  }					// findRCFile()

  // Retrieve the contents of 'file' if it exists, or NULL if it does not
  // If 'file' is NULL, we return NULL.

  private static String getFileContents(File file)
  {
    String result = null ;
    if (file != null)
      if (file.exists() && !file.isDirectory())
	{				// file exists
	  try
	    {				// attempt to read file
	      FileInputStream fileInputStream = new FileInputStream(file) ; // byte stream
	      InputStreamReader reader = new InputStreamReader(fileInputStream) ; // text stream
	      StringBuffer stringBuffer = new StringBuffer(1024) ;
	      char buf[] = new char[1024] ;
	      for (int i = reader.read(buf, 0, 1024) ; i > 0 ; i = reader.read(buf, 0, 1024))
		stringBuffer.append(buf, 0, i) ;
	      fileInputStream.close() ;
	      result = stringBuffer.toString() ;
	    }				// attempt to read file
	  catch (Exception e)
	    {				// catch exception
	      System.err.println("Warning: " + e.toString()) ;
	    }				// catch exception
	}				// file exists
    return result ;
  }					// getFileContents()

    // All calls to the client redirect loop must provide an instance
    // of a class that implements this signature.

    // When all servers have queried and found busy, this function is
    // invoked.  If it returns true, we go back to the server list and
    // force some service.
    public interface priority_function {
	public abstract boolean invoke (int number_of_busy_servers, int number_of_lazy_servers);
    }

    // Main loop of redirecting client.
    public boolean client_redirect_loop (Set servers_to_try, priority_function pf) {

	String command = null; // new command if continuing
	boolean priority_flag = false;

	URL server_to_try = null;

	Set busy_servers = new Set (); // set of servers that returned busy redirects
	Set lazy_servers = new Set (); // set of servers that returned read-only redirects
	Set bad_servers =  new Set (); // set of servers that returned no-service redirects
	Set dead_servers = new Set (); // set of servers that don't respond

	while (true) {

	    while (servers_to_try.isEmpty()) {
		if (priority_flag == true)
		    return false;

		// If we tried them all, but they are all dead or bad,
		// there will be none in the busy or lazy list.
		if ((busy_servers.cardinality() + lazy_servers.cardinality()) == 0) {
		    // special case:  one server and it didn't respond.
		    // usually means the .csf-prefs file is bogus.
		    if ((dead_servers.cardinality() == 1) &&
			(bad_servers.cardinality() == 0)) {
			_stdout.println ("Couldn't connect:  " + dead_servers.pick().toString ());
			_stdout.println ("Check your " + _preferences_file_name + " file.");
                        _stdout.println ("This can also be caused by a temporary network problem.");
                        _stdout.println ("In this case, simply re-issue the command.");
		    }
		    else {
			_stdout.println ("No suitable servers found.");
			if (bad_servers.cardinality() != 0)
			    bad_servers.dump ("The following servers declined your request:", _stdout);

			if (dead_servers.cardinality() != 0)
			    dead_servers.dump ("The following servers did not respond:", _stdout);
		    }
		    // Either way, you lose.
                    _stdout.println ();
		    _stdout.println ("Your request was not serviced.");
                    _stdout.println ();
		    return false;
		}

		// This, if anywhere, is where we want to ask the user
		// if he wants to wait.
		if (!pf.invoke (busy_servers.cardinality(), lazy_servers.cardinality()))
		    // If user doesn't want to wait, just return.
		    return false;

		// If user wants to wait, reset the lists.
		priority_flag = true;
		servers_to_try = lazy_servers.union (busy_servers); // note order matters here
		busy_servers = new Set ();
		lazy_servers = new Set ();
	    }

	    //	    servers_to_try.dump ("servers_to_try:", _stdout);
	    //      _stdout.flush();

	    // bad_servers.dump ("bad_servers", _stdout);

	    // dead_servers.dump ("dead_servers", _stdout);

	    // At this point, we should have at least one server to try.
	    // Don't randomize here, let main server do that.
	    server_to_try = ((URL) (servers_to_try.pick ()));
	    servers_to_try = servers_to_try.remove (server_to_try);

	    // Give it a go.
	    if (!connect (server_to_try, command, priority_flag))
		// No one at home.
		// Place server in the dead server list.
		dead_servers = dead_servers.adjoin (server_to_try);
	    else {
		// Hey, someone is home.
		try {
		    return serveCliResponse (false);
		}
		catch (RedirectNoService redirect) {
		    // This server can't handle this request.
		    // Place it in the `bad servers' set.
		    bad_servers = bad_servers.adjoin (server_to_try);
		    servers_to_try =
			servers_to_try.union
			(redirect.alternateServers().difference
			 (lazy_servers.union (busy_servers.union (dead_servers.union (bad_servers)))));
		}

		catch (RedirectReadOnly redirect) {
		    // Server is read-write, but command is read-only.
		    // Place server in the `lazy' list.
		    lazy_servers = lazy_servers.adjoin (server_to_try);
		    // More alternates
		    servers_to_try =
			servers_to_try.union
			(redirect.alternateServers().difference
			 (lazy_servers.union (busy_servers.union (dead_servers.union (bad_servers)))));
		}

		catch (RedirectBusy redirect) {
		    // This server is too busy, maybe try others.
		    // Place it in the `busy' list.
		    busy_servers = busy_servers.adjoin (server_to_try);
		    servers_to_try =
			// add the new alternates to the servers_to_try
			servers_to_try.union
			(redirect.alternateServers().difference
			 (lazy_servers.union (busy_servers.union (dead_servers.union (bad_servers)))));
		}

		catch (RedirectContinue redirect) {
		    // redirect.alternateServers().dump ("Alternate servers: ", _stdout);
		    // _stdout.flush();

		    // Done with part one of the command,
		    // attempt to restart the whole shebang with part two.

		    // Treat the server that issued the RedirectContinue as `busy',
		    // so we move away from it.  The point of the continue is that the 
		    // server that issues it doesn't want to handle the followup. 
		    busy_servers = busy_servers.adjoin (server_to_try);

		    priority_flag = false; // new command starts at low priority
		    command = redirect.newCommand();
		    servers_to_try =
			servers_to_try.union
			(lazy_servers.union
			 (busy_servers.union
			  (bad_servers.union (redirect.alternateServers()))))
			.difference (dead_servers);

		    busy_servers = new Set ();
		    lazy_servers = new Set ();
		    bad_servers = new Set ();
		}
	    }
	}
    } // end of ClIentREdIrEctLoop


  // Establish a connection for the CM client object.
  // Return true if the connection succeeds, or false if it fails.

    public boolean connect (URL server_to_try, String command, boolean priority_flag)
    {
	ServerRequest request =  construct_server_request (server_to_try, command, priority_flag);

	_connectComplete = false;

	// _stdout.println (request.getURI());

	// Invoke the command server side, set up streams to read results.
	try
	    {
		// *TBD*: use HttpURLConnection to have finer control over responses.
		// We could specify a connect header of Accept: "cli/response", and reply sensibly
		// to text/html or other responses.  This is particularly necessary for
		// 4xx and 5xx classes of resposnes from HTTP servers, which URLconnection simply classes
		// as "connection refused".  *FINISH*
		_urlConnection = request.connect() ;
		_connectionOutputStream = _urlConnection.getOutputStream() ; // byte stream to write to server
		_connectionInputStream = _urlConnection.getInputStream() ; // byte stream to read from server
		//_connectionReader = new InputStreamReader(_connectionInputStream) ; // text stream
		//_connectionBufferedReader = new BufferedReader(_connectionReader) ; // buffered text stream

		String contentType = _urlConnection.getContentType() ;
		if (contentType != null && contentType.equals("cli/response"))
		    _connectComplete = true ;
		else {
		    _stderr.println("Response code " + _urlConnection.getResponseCode());
		    _stderr.println("Response message " + _urlConnection.getResponseMessage());
		    _stderr.println("Unexpected server content type: " + _urlConnection.getContentType()) ;}
	    }
	catch (ConnectException e) {
	  // _stderr.println("Connection Exception occurred: " + e.toString()) ;
	}
	catch (IOException e) {
	  // _stderr.println("IO Exception occurred: " + e.toString()) ;
	}
	catch (Exception e) {
	    _stderr.println("Exception occurred: " + e.toString()) ;
	}
	_stdout.flush();
	_stderr.flush();
    return _connectComplete ;
  }					// connect()

  // Return true if we appear to have a working connection following connect()
  // May be called at any time.  If it isn't true, you shouldn't be using the server streams.

  public boolean isConnected ()
  {
    return _connectComplete ;		// currently doesn't handle connection closed on urlconnection.
  }					// isConnected()

  /**
     Read into argBuffer until we encounter a newline, *or*, of we spy a '"' as our first
     non-blank character, read a potentially multi-line string.

     Any leading blanks are stripped.

     We do not reposition the Stringbuffer.
  **/

  private void readArgs(StringBuffer argBuffer) throws IOException
  {
    int ch ;
    while ((ch = _connectionInputStream.read()) == ' ')
      ; // do nothing, stripping leading blanks
    if ( ch == '"' )
      readString(argBuffer) ; // we have a string coming
    else
      while ( ch != NEWLINE_CHAR && ch != -1 ) {
	argBuffer.append((char) ch) ;
	ch = _connectionInputStream.read() ;
      }
  }					// readArgs()

  /**
     Read a lisp string with escapes until we encounter the terminating quote character
     (currently an implicit '"'), and also read the newline which should follow the quote.
     Warn if the newline doesn't immediately follow the quote.
  **/
  private void readString (StringBuffer argBuffer) throws IOException
  {
    int ch ;
    while ((ch = _connectionInputStream.read()) != '"') {
       if (ch == '\\')
         ch = _connectionInputStream.read() ;
       if (ch == -1) break ;
       argBuffer.append((char)ch) ;
    }

    // read terminating newline, which should follow strings
    if ( ch != -1 )
       ch = _connectionInputStream.read() ;	// was BufferedReader
    if (ch != NEWLINE_CHAR)
      _stderr.println("Warning: cli/response protocol string argument not followed by newline.") ;
  }					// readString()

  /**
    Read a command and accompanying args from the connection.
    Return the command and args in the passed StringBuffers, and return as a function value
    true if a command was read, and false if we're at EOF and should exit the serve/slave loop.

    If a blank separates the command from its arguments, it is omitted from the resulting argBuffer.
  **/

  private boolean readCommand(StringBuffer commandBuffer, StringBuffer argBuffer)
  {
    if (_debugTrace)
      {
	_stderr.print("request : ") ;
	_stderr.flush() ;
      }

    commandBuffer.setLength(0) ;
    argBuffer.setLength(0) ;

    try
      {
	// Read the command
	int ch ;
	while ((ch = _connectionInputStream.read()) != -1) // was BufferedReader
	  {
	    if (ch == NEWLINE_CHAR || ch == ' ')
	      break ;
	    commandBuffer.append((char) ch) ;
	  }

	// Found the token-terminating blank or newline
	if (_debugTrace)
	  {
	      _stderr.print ((commandBuffer.length() > 0)
			     ? commandBuffer.toString()
			     : "<<EOF>>");
	      _stderr.flush() ;
	  }				// _debugTrace

	// Read args if they're present
	if (ch != NEWLINE_CHAR)
	  // It should be a blank separating token from args
	  readArgs(argBuffer) ;		// we have args, read multi-line string or until end of line
      }					// try
    catch (Exception e)
      {
	_stderr.println("Exception reading command: " + e.toString()) ;
      }

    if (_debugTrace)
      if (argBuffer.length() > 0)
	{
	  _stderr.print("  ") ;
	  _stderr.println(argBuffer.toString()) ;
	  _stderr.flush() ;
	}
      else
	_stderr.println() ;

    return (commandBuffer.length() > 0)  ;
  }					// readCommand()

  // Return true if contents of 'buf' match the 'command' string, or false otherwise.

  private static boolean compareCommand(StringBuffer buf, String command)
  {
    int buflen = buf.length() ;

    if (buflen != command.length())
      return false ;

    for (int i = 0 ; i < buflen ; i++)
      if (buf.charAt(i) != command.charAt(i))
	return false ;
    return true ;
  }					// compareCommand

  /**
     Enter slave mode, a read loop, processing requests from the server on the other end of
     the connection, and supporting the protocol documented in the module header.
     This function serves the HTTP content type "cli/response"

     debugTrace should be true if you want diagnostic traces of the command received.
     Note that the cli/response protocol is currently mostly unidirectional for commands it
     processes, so tracing seldom reveals information returned to the server.  The intent
     is that we really want to perform stateless connections something like a normal user agent.

     Return true if activities are successful, false if we trapped some exception or otherwise
     encountered an error in processing the requesets and had to terminate.
  **/

    public boolean serveCliResponse (boolean debugTrace) 
	throws RedirectNoService, RedirectBusy, RedirectContinue, RedirectReadOnly
  {
    _debugTrace = debugTrace ;
    if (!isConnected())
      {					// not ready
	_stderr.println("CM client did not successfully connect to the server.") ;
	return false ;
      }

    StringBuffer commandBuffer = new StringBuffer(16) ;
    StringBuffer argBuffer = new StringBuffer(1024) ;

    while (readCommand (commandBuffer, argBuffer))
      {					// process commands supported by cli/response protocol.
	  if (compareCommand (commandBuffer, "RETURN")) {
	      processReturn (argBuffer) ;
	      break;
	  }
	else if (compareCommand(commandBuffer, "DISPLAY"))
	  processDisplay(argBuffer) ;
	else if (compareCommand(commandBuffer, "STARTFSA"))
	  processStartFSA(argBuffer) ;
	else if (compareCommand(commandBuffer, "TRACE"))
	  processTrace(argBuffer) ;
	else if (compareCommand (commandBuffer, "REDIRECT"))
	    processRedirect (argBuffer);
	else if (compareCommand (commandBuffer, "CD"))
	    processChangeDirectory (argBuffer);
	else
	  _stderr.println("CM client doesn't support the " + commandBuffer.toString() + " directive.") ;
      }					// process commands supported by cli/response protocol
    return true ;
  }					// serveCliResponse()

  // A RETURN cli/response directive was encountered.  Exit the client with the return code
  // indicated by the arguments to the command.

  private void processReturn(StringBuffer argBuffer)
  {
    // It'd be nicer to return a value from this method, and in turn from serveCliResposne, and
    // the main() routine.  But for now, we've taken a shortcut, because I'm not sure that main()
    // even generates a system return status if we declare it to have a non-void return type.
     String code = argBuffer.toString();
     int icode=Integer.parseInt(code);
     if (icode != 0){
	 _stdout.println (
			  "ChangeSafe status: " +
			   ((icode >= MinErrorNumber)
			   ? "Error["
			   : "Warning[") + code + "]");
       System.exit(1);
     }
  }					// processReturn()

  // Display text to the user.  The argBuffer has a string relayed by the server, which may
  // contain embedded goodies like newlines.

  private void processDisplay(StringBuffer argBuffer)
  {
      _stdout.println (argBuffer.toString());
      _stdout.flush();
  }					// processDisplay()

  // Invoke the file system agent.  Run it against the connection we have.
  // When the agent exits, we return to normal command-receive mode for the CM module.
  // Unlike the use of FSA's in other ChangeSafe modules, we do NOT relinquish the current
  // server connection, and we do NOT start a new connection.  We just continue to use the existing
  // connection.

  private void processStartFSA(StringBuffer argBuffer)
  {
    // Shouldn't have any args in argbuffer.
    if (argBuffer.length() > 0)
      _stderr.println("STARTFSA shouldn't have command arguments.") ;

    FSAProgressIndicator progress_indicator = new FSAProgressIndicator (2);  //false = not debugging, 2 = somewhat verbose

    try
      {					// run the FSA
	// *TBD*: pass the trace flag for this module?  Or take an arg for processStartFSA
	FileSystemAgent agent = new FileSystemAgent(_preferences, _connectionInputStream, _connectionOutputStream) ;
	agent.serve(false, progress_indicator) ;
	// We deliberately do not perform a close() operation on the FSA, since the CM module
	// is not done with the connection to the server yet.  We just let the FSA float away on a GC
      }					// run the FSA
    catch (Exception e)
      {
	// *TBD*: should probably return the failure code for CM.java here, rather than
	// trapping and expecting to continue.
	_stderr.println("File system agent activity resulted in an exception: " + e.toString()) ;
      }
    // This is the source of the `mysterious newline'
    // progress_indicator.displayText(""); // force a newline

    // Note, if there were higher-order streams built on _connectionInputStream, we would need
    // to flush/resynchronize them here with respect to _connectionInputStream
  }					// processStartFSA()

  // Enable tracing of command processing

  private void processTrace(StringBuffer argBuffer)
  {
    if (argBuffer.length() == 0)
      {
	_stderr.println("Missing argument for TRACE request.") ;
	return ;
      }

    _debugTrace = (Integer.parseInt(argBuffer.toString()) == 1);
  }					// processTrace()

    private void processRedirect (StringBuffer argBuffer) 
	throws RedirectNoService, RedirectBusy, RedirectContinue, RedirectReadOnly
    {
	// _stdout.println ("REDIRECT");

	// get redirect type
	StringTokenizer st = new StringTokenizer (new String (argBuffer));
	try {

	    String redirect_type = st.nextToken ();
	    String new_command = null;
	    Set alternates = new Set ();

	    if (redirect_type.equals (":CONTINUE"))
		new_command = st.nextToken();

	    while (st.hasMoreTokens ()) 
		alternates = alternates.adjoin (new URL (st.nextToken()));

	    _urlConnection.disconnect ();

	    // alternates.dump ("Alternate servers: ", _stdout);
            // _stdout.flush();

	    if (redirect_type.equals (":NO-SERVICE"))
		throw new RedirectNoService (alternates);

	    if (redirect_type.equals (":BUSY"))
		throw new RedirectBusy (alternates);

	    if (redirect_type.equals (":READ-ONLY"))
		throw new RedirectReadOnly (alternates);

	    if (redirect_type.equals (":CONTINUE"))
		throw new RedirectContinue (alternates, new_command);

	    _stderr.println ("Unknown redirect type " + redirect_type);
	}
	catch (NoSuchElementException nse) {
	    _stderr.println ("Ran out of elements while handling redirect.");
	}
	catch (MalformedURLException mue) {
	    _stderr.println ("Malformed URL while handling redirect.");
	}
    }

    private static void change_directory (String new_directory)
    {
	try {
	    Properties system_props = System.getProperties ();
	    system_props.put ("user.dir", new_directory);
	    System.setProperties (system_props);
	}
	catch (SecurityException se) {
	    System.err.println ("Unable to change to directory " + new_directory + ", aborting.");
	    System.exit(-1);
	}
    }

    private void processChangeDirectory (StringBuffer argBuffer)
    {
	change_directory (argBuffer.toString());
    }

    private void validate_read_write (File tmp_file) {
	try {
	    FileOutputStream foo = new FileOutputStream (tmp_file);
	    foo.write ("Test".getBytes());
	    foo.close ();
	    OSServices.SetFileReadOnly (_preferences, tmp_file, true);
	    if (tmp_file.canWrite ())
		_stdout.println ("WARNING:  Failed to set file to read-only state.  Check value of SET_FILE_READ_ONLY.");
	    OSServices.SetFileReadOnly (_preferences, tmp_file, false);
	    _stdout.println ((tmp_file.canWrite ())
			     ? "Success:  File modes will be correctly set."
			     : "WARNING:  Failed to set file to read-write state.  Check value of SET_FILE_READ_WRITE.");
	    _stdout.println ();

	    tmp_file.delete ();
	}
	catch (Exception e) {
	    _stdout.println ("WARNING:  Failed to create test file " + tmp_file);
	}
    }

    private void validate_last_modified (File tmp_file) {
	try {
	    long last_modified;
	    FileOutputStream foo = new FileOutputStream (tmp_file);
	    foo.write ("Test".getBytes());
	    foo.close ();

	    last_modified = tmp_file.lastModified();
	    OSServices.SetFileLastModified (_preferences, tmp_file, 3141592654L);
	    _stdout.println ((last_modified == tmp_file.lastModified ())
			     ? "WARNING:  Failed to set file timestamp.  Check value of SET_FILE_LAST_MODIFIED."
			     : "Success:  File timestamps will be correctly maintained.");
	    _stdout.println ();
	    tmp_file.delete ();
	}
	catch (Exception e) {
	    _stdout.println ("WARNING:  Failed to create test file " + tmp_file);
	}
    }

    //
    // Validate the client setup.
    //

    // This function is gross.
    private void validate ()
    {
	File rcPath = findRCFile(_dot_conman) ;
	String userHomedir = OSServices.GetUserHomeDirectory ();

	// Print some standard java variables that we are interested in.
	_stdout.println ();
	_stdout.println ("ChangeSafe client self-test.");
	_stdout.println ("    os.name: " + System.getProperty ("os.name"));
	_stdout.println ("    java.vendor: " + System.getProperty ("java.vendor"));
	_stdout.println ("    java.version: " + System.getProperty ("java.version"));
	_stdout.println ("    java.class.path: " + System.getProperty ("java.class.path"));

	// Verify that user.name is set, print it so user knows.
        _stdout.println ();
	_stdout.println ((System.getProperty ("user.name") == null)
			 ? "WARNING:  System property user.name is null.  ChangeSafe does not know who you are."
			 : ("    User name is " + System.getProperty ("user.name")));

	// Verify that user.home is set, and print it.
	if (userHomedir == null) {
	    _stdout.println ("WARNING:  System property user.home is null.  ChangeSafe does not know where your home directory is.");
	    _stdout.println ("          You may have to define user.home on the ChangeSafe command line.");
	}
	// Check for JVIEW bug.
	else if ((System.getProperty ("java.vendor").indexOf ("Microsoft") != -1)
		 && (userHomedir.indexOf ("WINNT") != -1)) {
	    _stdout.println ("WARNING:  Microsoft bug.  Your home directory appears to be " + userHomedir);
	    _stdout.println ("          You should add /d:user.home=%HOMEDRIVE%%HOMEPATH%\\ to the command line.");
	}
	else
	    _stdout.println ("    Your home directory is " + userHomedir);

	_stdout.println ("    Current directory is " +
			 System.getProperty ("user.dir") +
			 System.getProperty ("file.separator"));

	// Print out the location of the .csf file.
	// If the user is not in a workspace, this might catch some random csf file.
        _stdout.println ();
	_stdout.println ((rcPath != null)
			 ? ("    " + _dot_conman + " file found at " + rcPath.getAbsolutePath())  
			 : ("    No " + _dot_conman + " file found."));

	// Report the client's timezone offset
        _stdout.println ();
	GregorianCalendar now = new GregorianCalendar ();
	TimeZone z = now.getTimeZone();
	_stdout.println ("    Time zone is " + z.getID());
	if (z.useDaylightTime())
	    _stdout.println ((z.inDaylightTime (now.getTime()))
			     ? "    Daylight savings is in effect."
			     : "    Daylight savings is not in effect.");

        _stdout.println ();
        _stdout.println ("    File system agent protocol version: " + FileSystemAgentProtocol.getVersionString ());

	// See if we can find the conman prefs.  Print out location of conman prefs, if possible.
	try {
            _stdout.println ();
	    java.net.URL prefs_name = getClass().getResource(_preferences_file_name);

	    if (System.getProperty ("java.vendor").indexOf ("Microsoft") != -1)
		_stdout.println ("    Preferences resource is " + prefs_name.toString ());
	    // Unfortunately, the Sun JVM doesn't let us know where it is getting
	    // them from, so we don't bother in that case.
	    else {
		_stdout.println ("    Preferences will be read from " + _preferences_file_name +
				 " in the same directory as " + getClass().toString () + " file.");
                // _stdout.println ("    Preferences file is: " + prefs_name.toString ());
	    }

	    if (getClass().getResourceAsStream(_preferences_file_name) == null)
		System.err.println("WARNING:  Can't get preferences from " + prefs_name.toString());
	}
	catch (Exception e) {
	    System.err.println("WARNING:  Preferences resource " + _preferences_file_name +
			       " wasn't found in client class directory.");
	}

	// Look for preferences in user home directory, as well.
	if (userHomedir != null) {
	    String path = userHomedir + System.getProperty ("file.separator") + _preferences_file_name;
	    try {
		FileInputStream s = new FileInputStream (path);
		if (s != null) {
		    _stdout.println ("    User preferences found in " + path);
		    s.close ();
		}
	    }
	    catch (FileNotFoundException e) {
		_stdout.println ("    No user preferences file (" + path + ") found.");
	    }
	    catch (IOException e) {
		_stdout.println ("    User preferences file found in " + path + ", but it could not be read.");
	    }
	}

	_stdout.println ("        " + _pref_server_uri    + " = " + _server_uri);

	// If we use the Microsoft JVM, these work internally.
	if (System.getProperty ("java.vendor").indexOf ("Microsoft") == -1) {
	    // This is a sucky way of finding a temporary file.
	    String tmp_dir = "C:\\";
	    if (System.getProperty ("os.name").indexOf ("Windows") != -1) {
		File probe = new File ("C:\\temp");
		if (probe.exists() && probe.isDirectory ())
		    tmp_dir = "C:\\temp\\";
		else {
		    probe = new File ("C:\\tmp");
		    if (probe.exists() && probe.isDirectory ())
			tmp_dir = "C:\\tmp\\";
		    else {
			probe = new File ("D:\\temp");
			if (probe.exists() && probe.isDirectory ())
			    tmp_dir = "D:\\temp\\";
			else {
			    probe = new File ("D:\\tmp");
			    if (probe.exists() && probe.isDirectory ())
				tmp_dir = "D:\\tmp\\";
			}}}
	    }
	    else {
		tmp_dir = "/tmp/";
	    }
	    // Generate a temporary name.
	    int count = 0;
	    File tmp_file = null;
	    while (true) {
		String tmp_file_name = tmp_dir + "cm" + count + ".txt";
		tmp_file = new File (tmp_file_name);
		if (!tmp_file.exists ()) break;
	    }

	    if (_preferences.get (_pref_read_only) != null) {
		_stdout.println ("        " + _pref_read_only     + " = " + _preferences.get (_pref_read_only));
	    }
	    else
		_stdout.println ("WARNING:  No " + _pref_read_only + ".  File mode will not be set.");
	    if (_preferences.get(_pref_read_write) != null) {
		_stdout.println ("        " + _pref_read_write     + " = " + _preferences.get(_pref_read_write));
	    }
	    else
		_stdout.println ("WARNING:  No " + _pref_read_write + ".  File mode will not be set.");
	    if (_preferences.get(_pref_last_modified) != null) {
		_stdout.println ("        " + _pref_last_modified     + " = " + _preferences.get(_pref_last_modified));
	    }
	    else
		_stdout.println ("WARNING:  No " + _pref_last_modified + ".  File timestamps will not be maintained.");
	    _stdout.println ("");
	    if ((_preferences.get (_pref_read_only) != null) &&
		(_preferences.get (_pref_read_write) != null)) validate_read_write (tmp_file);
	    if (_preferences.get (_pref_last_modified) != null) validate_last_modified (tmp_file);

	}
	else {
	    _stdout.println ("        No " + _pref_read_only     + " necessary with Microsoft JVM");
	    _stdout.println ("        No " + _pref_read_write    + " necessary with Microsoft JVM");
	    _stdout.println ("        No " + _pref_last_modified + " necessary with Microsoft JVM");
	}
	_stdout.println ("End of ChangeSafe client self-test.");
    }

    public static void main (String[] args) {
	boolean success = false;
	CM cm = new CM (args);

	_original_current_directory = System.getProperty ("user.dir");


	if ((args.length > 0) && (args[0].toLowerCase().equals (_batch_command))) {
	    cm._stdout.println ("-batch-mode unsupported.");
	    System.exit (0);
	}
	// Magic command.  If the command is _validation_command, we don't go to the
	// server.  We report on the current client setup and test the various callouts.
	else if ((args.length > 0) && (args[0].toLowerCase().equals(_validation_command))) {
	    cm.validate ();
	    System.exit (0);
	}
	else {
	    try
		{
		    // Why this?  Because we need it for the inner class defined below.
		    final CM _cm = cm;

		    if (_cm.client_redirect_loop
			(new Set ().adjoin (new URL (_cm._server_uri)),
			 // This is how we spell LAMBDA in Java
			 new priority_function () {
				 public boolean invoke (int busy_server_count, int lazy_server_count) {
				     if (lazy_server_count == 0) {
					 if (busy_server_count > 1)
					     _cm._stdout.println ("Servers are busy, waiting for service.");
					 else
					     _cm._stdout.println ("Server is busy, waiting for service.");
				     }
				     else if (busy_server_count > 0) {
					 // all non-lazy servers are busy,
					 // but some lazy servers exist, so
					 // we might not have to wait, but then again...
					 _cm._stdout.println ("Read-only servers are busy, " +
							      "enqueuing on first available server.");
					 _cm._stdout.println ("A wait for service may occur.");
				     }
				     // too confusing for user.
				     // else {
				     //   _cm._stdout.println ("No read-only servers found, using read-write server.");
				     //   _cm._stdout.println ("A wait for service may occur.");
 				     // }

				     // returning true indicates that we wish to wait for service.
				     // at some point, we may make this an option.
				     return true;
				 }}))  // good thing that Java doesn't use parens, eh?
			success = true;
		    else {
			// If we were unable to connect, tell the user how to run the validate
			// program.  If the user *did* connect, the help function should tell him.
			if ((args.length == 0) || (args[0].toLowerCase().equals("help")))
			    cm._stdout.println ("Use the " 
						+ _validation_command
						+ " command to check client settings.");
		    }
		}
	    catch (Exception e)
		{
		    cm._stderr.println("Exception occurred: " + e.toString());
		}
	    finally 
		{
		    change_directory (_original_current_directory);
		    System.exit (success ? 0 : 1);
		}
	}
    }

}					// class CM
