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
 ** File Name:	   FileSystemAgent.java
 ** Author:        Dave Tenny
 **
 ** Module Description:
 **
 ** A FileSystemAgent uses socket-based communications protocols to provide client-side
 ** file system (disk management) services to some process on the other end of the socket.
 ** It typically acts as a slave to whoever is on the other end of the socket, and doesn't
 ** close the socket unless the close method is invoked.
 **
 ** @see HttpFileSystemAgent
 ** @see SocketFileSystemAgent
 ** @note This is not a thread-safe class.  All access to a single instance must be synchronized
 ** by the caller if necessary.
 ****************************************************************************/

import java.io.* ;
import java.net.* ;
//import FSAProgressIndicator ;
import java.util.Calendar ;
import java.util.Locale;
import java.util.Properties ;
import java.util.StringTokenizer ;
import java.util.TimeZone ;
import java.util.Vector ;

public class FileSystemAgent
{

  //
  // Member variables
  //
    private static int	STANDARD_BUFFER_SIZE = 8192 ; // 8 KB

    protected Properties _properties = null; // Various properties and options.
  protected InputStream	 _inputStream = null ; // stream (socket-derived) we can read from connection
  protected OutputStream _outputStream = null ; // stream (socket-derived) we can write to connection
  protected boolean	 _debugTrace = false ; // true if diagnostics requested.
  protected FSAProgressIndicator _progressIndicator = null ; // optional, may remain null, see serve()
  private   StringBuffer _argStringBuffer = new StringBuffer(STANDARD_BUFFER_SIZE) ; // readArgs result accumulator
  private   StringBuffer _responseStringBuffer = new StringBuffer(STANDARD_BUFFER_SIZE) ; // generic response accumulator
  private   StringBuffer _escapeStringBuffer = new StringBuffer(STANDARD_BUFFER_SIZE) ; // for escapeString()

  // The following slots need multi-valued representations (probably intermediate objects encapsulating
  // the following information) when we support more than one concurrently open file.
  private   boolean	 _textMode = false ;// true if file is open for text-mode
  private   String       _record_terminator = "NIL";
  private   boolean	 _inputMode = false ; // true if file is open for input
  private   File	 _openFile = null ; // file currently open, if any
  private   int		 _openFileHandle = 0 ; // pseudo-handle returned by OPEN request, zero == not-in-use

  private   InputStream	 _fileInputStream = null ; // file source stream
  private   OutputStream _fileOutputStream = null ; // file destination stream

  // These buffers are used by the various file read/write routines like bufferText
  // These variables are initialized in lazy fashion, we only use them if we need them for file
  // read/write operations

    // This method is heinous!
    // private   ByteArrayOutputStream _processedByteBuffer = null ;// processed bytes for relay

    private byte [] _processedByteBuffer = null; //processed bytes for relay
    private int     _processedByteBufferPtr = 0; //fill pointer for above


  private   byte[]	_UPBBuffer = null ; // unprocessed bytes for processing or relay
  private   int         _UPBCount = 0 ;	// number of bytes in unprocessed byte buffer
  private   int         _UPBStart = 0 ; // position of bytes remaining to be processed in _UPBbuffer
  private   boolean	_fileInputEOF = false ;	// true if _fileInputStream hit EOF

    private boolean _need_newline = false;
  //
  // Methods
  //

  protected FileSystemAgent()
  {				// only used in ctors of subtypes, and typically by default.
      // OSServices.MessageBox (0, "File system agent starting", "File System Agent", 0);
  }

  // TO-DO: specify finalize() method?  It's always a protocol error if we haven't had the CLOS
  // request for all opened files. However uncaught exceptions siganlled during finalization are ignored.
  // Best we could do is log some event to some log manager or output sink.  For now, we rely
  // on proper observance of the protocols, namely, CLOSE any files you OPEN.

  /**
    The <code>FileSystemAgent</code> constructor should be called with the input and output streams
    derived from a socket which is presented to or managed by subtypes or callers of FileSystemAgent.

    @exception IOException if you so much as sneeze or look at an URL funny.  Seriously,
    there are many, many ways IOException arises during the initialization of FileSystemAgents,
    sockets, read operations, etc.

    **/

  public FileSystemAgent(Properties properties, InputStream inputStream, OutputStream outputStream) throws IOException
  {
      // OSServices.MessageBox (0, "File system agent starting", "File System Agent", 0);
      _properties = properties;
      open(inputStream, outputStream) ;
  }

  // NAG #1, all functions should have descriptions, (why is this necessary in the big picture,
  // architectural considerations, etc).  Is the constant below defined somewhere in the java lib?

    // NAG #2: Please use wordCap method names, not underbar_separated names.
    // OBviously thE esoTeric SiGnifiCanCe of sTuDlycaPsificaTion escapes me.  i Must bE
    // sUffErIng From autostudlycapsiFiBogotiFicatioN.  oh weLl.

    // Why not return ((unsigned long) (unsigned int) i) ? (Not that I've figured out if that's equivalent)
    // Does that work?  If i is a negative number, it can't be represented in an unsigned int.

  private static long int_to_unsigned_long (int i)
  {
      // Convert a signed integer to an unsigned long by 2's complement operation if necessary.
      // This allows us to avoid sign-extension problems.

      return (i < 0)
	  ? (long)i + 4294967296L // (expt 2 32)
	  : (long)i;
  }

    private boolean root_pathname_p (String pathname)
    {
	// pathname names an absolute path ending with a slash.
	// We need to determine whether it is a root path.  This
	// is a bit of a kludge necessitated
	// by poorly defined java pathname semantics.

	// This works by simply examining the characters in the
	// pathname, it really shouldn't do this because we'll
	// have to special case each OS syntax.  But I haven't
	// found a reliable and portable way to determine root status.
	return (pathname.length() < 2) ||
	    ((pathname.length() == 3) && (pathname.endsWith (":\\")));
    }

    private File canonicalize_incoming_pathname (String pathname)
    {
	// Return a FILE object that represents the string.  Note that certain
	// java implementations (NS, for example) are touchy about whether the
	// trailing file separator is allowed or not.  In particular, it must
	// be there if you are referring to a root directory, but it must not
	// be there otherwise.
	return new File ((!pathname.endsWith (File.separator) ||
			  root_pathname_p (pathname))
			 ? pathname
			 : pathname.substring (0, pathname.length () - 1));
    }

    private String canonicalize_outgoing_pathname (File file) throws IOException
    {
	// Return a String object that represents the file.  The trailing
	// separator must be there if the file is a directory and not if it isn't.
	return escapeString
	    (
		     // Yuck!  When java probes a file that happens to be a directory,
		     // the name of the file doesn't end with a slash.
		     // Except when it is the root directory.
		     // So we put the slash back on directories, except if it already
		     // has a slash.  Who is responsible here?
	     (file.isDirectory() && !file.getCanonicalPath ().endsWith (File.separator))
	     ? file.getCanonicalPath () + File.separator
	     : file.getCanonicalPath()) ;
    }

  /**
    The isReady() function returns true if the FileSystemAgent is in a state that it can
    do work via serve(), or false if it isn't.  Usually, if the agent isn't ready to serve(),
    it's because it's socket hasn't been opened, or was closed and needs to be opened again.
    **/

  public boolean isReady()
  {
    return (_inputStream != null) && (_outputStream != null) ;
  }

  /**
    The <code>close</code> must be overloaded by subtypes and should close the socket associated with
    the FileSystemAgent.  Users of FileSystemAgent are not required to call <code>close</code>, in fact they
    may wish to keep the socket open for further work.

    Subtypes should be sure to call the <code>super.close()</code>.
    @exception IOException if socket cannot be closed.
    **/

  public void close() throws IOException
  {
    _inputStream = null ;
    _outputStream = null ;
  }

  /**
    The <code>open</code> function must be called from subtypes of <code>FileSystemAgent</code>
    to provide the streams upon which FileSystemAgent logic is performed.

    @param <code>inputStream</code> is a an instance of class <code>InputStream</code>, typically
    derived from a socket via the <code>getInputStream</code> method.

    @param <code>outputStream</code> is an instance of class <code>OutputStream</code>, typically
    derived from a socket via the <code>getOutputStream</code> method.
    **/

  protected void open(InputStream inputStream, OutputStream outputStream) throws IOException
  {
    // Buffering the socket input stream doesn't seem to make any difference.
    // Differences appear to be in the noise.  So this is more a safeguard than anything else.
    _inputStream = new BufferedInputStream(inputStream, STANDARD_BUFFER_SIZE) ;
    // Buffering the socket output stream is a HUGE win.  Took average command time from <200ms to
    // about <15 ms.
    _outputStream = new BufferedOutputStream(outputStream, STANDARD_BUFFER_SIZE) ;
  }

  private static byte NEWLINE = (byte)'\n' ;	// note explicit demotions here, not char-set portable
  private static byte CARRIAGE_RETURN = (byte)'\r' ;

  /*
    InputStream.read(buf [, offset, len]) is documented to block until it reads "some" bytes,
    but not necessarily ALL bytes as specified by LEN.  Some java.io streams like DataInputStream
    have a method called readFully() which will get ALL the requested bytes.

    In testing this program, the only time I got less than requested was in reading large transmissions
    over the socket (i.e. those which exceeded 512 bytes in size.).  But there are no guarantees.
    So if you're reading bytes and expect a specific number of them, use this method.

    Read 'nBytes' from 'stream' into 'buffer', putting them in starting at offset zero.

    TO-DO: we may which to add some timeout controls to this routine which signal an error
    if we don't receive the desired bytes in a certain amount of time.
    */

  private static void readNBytes(InputStream stream, byte[] buffer, int nBytes)
       throws FileSystemAgentException, IOException
  {
    int totalBytes = 0 ;
    while (totalBytes < nBytes)
      {
	int nRead = stream.read(buffer, totalBytes, nBytes - totalBytes) ;
	if (nRead == -1)
	  throw new FileSystemAgentException("Expected " + nBytes +
					     " bytes to be read, received EOF after reading " +
					     totalBytes + "bytes.") ;
	totalBytes += nRead ;
      }					// while loop
  }					// readNBytes


  /*
   Read the 4 byte command and the following space or terminating newline.
   If there is not a terminating newline, we assume there is a space, and that arguments follow
   which need to be read. (Caller's responsibility).

   Read into caller-supplied buffer (to avoid consing).
   Return true if there are arguments to the command, false otherwise.
   */

  private boolean readCommand(byte[] buffer) throws IOException, FileSystemAgentException
  {
    if (_debugTrace)
      {
	System.out.print("request : ") ;
	System.out.flush() ;
      }

    // We deliberately don't use readNBytes() here, there is some error checking value here.
    // in not using it.  However, we currently fallibly assume that we'll get all the command bytes
    // if we get any of them.
    //
    // int nChars = _inputStream.read(buffer) ;
    //
    //
    // if (nChars != 5)
    //  // should read all 5 chars, otherwise we didn't get a proper request
    //  throw (new FileSystemAgentException("Invalid command request received from server, " +
    //					  nChars + " character(s) received in buffer '" +
    //					  new String(buffer) + "'")) ;

    // Unfortunately, this code is failing.  We sometimes get less than 5 bytes.
    // Reverting to readNBytes()

    readNBytes (_inputStream, buffer, 5);

    if (_debugTrace)
      // If buffer has newline, we don't need to print one.
      // If it doesn't have newline, readArgs() will print more info anyway, and wants it on same line
      System.out.write(buffer, 0, 5) ;

    return (buffer[4] != NEWLINE) ;	// if not a newline, it has arguments
  }					// readCommand

  /*
    Read arguments which followed the command returned by readCommand.
    It is an error to call this method if readCommand returned false.
    Convert arguments read as bytes to a string, since it's expected that the calling routine
    will immediately parse the result.
    */

  private String readArgs() throws IOException, FileSystemAgentException
  {
    byte b = 0 ;
    while ((b = (byte) _inputStream.read()) != NEWLINE
	   && b != -1)			// not eof
      {					// while there are bytes to process
	// WARNING: this byte-to-char conversion isn't valid for some language encodings.
	// Consider using an InputStreamReader to do this work in portable fashion, at the cost of
	// performance (garbage creation, etc..)  This isn't the only place that unkosher char conversions
	// are performed, beware.
	_argStringBuffer.append((char) b) ;
      }					// while there are bytes to process

    if (b != NEWLINE)
      throw new FileSystemAgentException("Premature EOF reading command arguments, didn't find newline") ;

    String result = _argStringBuffer.toString() ;
    _argStringBuffer.setLength(0) ;		// reset the accumulator before next use.

    if (_debugTrace)
      System.out.println(result) ;

    return result ;
  }					// readArgs

  /*
    Send a responseCode over the socket.  A responseCode is 3-digit byte encoded string, i.e. "200".
    */

  private void sendResponse(byte[] responseCode) throws IOException
  {
    if (_debugTrace)
      {
	System.out.print("response: ") ;
	System.out.write(responseCode, 0, responseCode.length) ;
	System.out.println() ;
      }

    _outputStream.write(responseCode) ;
    _outputStream.write(NEWLINE) ;
    _outputStream.flush() ;
  }					// sendResponse

  private void sendResponseAndInformation(byte[] responseCode, String information) throws IOException
  {
    if (_debugTrace)
      System.out.println("response: " + new String(responseCode) + " " + information) ;

    _outputStream.write(responseCode) ;
    _outputStream.write(" ".getBytes()) ; // consing?
    _outputStream.write(information.getBytes()) ; // definite consing
    _outputStream.write(NEWLINE) ;
    _outputStream.flush() ;
  }					// sendResponseAndInformation

  /*
    The response which was last sent with sendResponse or sendResponseAndInformation
    was a of the preliminary variety which prefixes a multi-line sequence of response text,
    terminated by a final empty line.

    The following routine sends the string text, and terminates the line with a newline.
    When all text has been sent, you instruct this routine to send the termination sequence
    (an empty line) by specifying an argument of null.

    This routine is only for multi-line response text, and will not send zero-length text strings.
    */

  private void sendMultiLineResponseText(String text) throws IOException, FileSystemAgentException
  {
    if (_debugTrace)
      System.out.println("resptext: " + text) ;

    if ((text != null) && (text.length() == 0))
      throw new FileSystemAgentException("Programming error, multi-line response segment was empty") ;
    if (text != null)
      _outputStream.write(text.getBytes()) ;
    // The newline always goes
    _outputStream.write(NEWLINE) ;
    _outputStream.flush() ;
  }					// sendMultiLineResponseText

  /*
    Retrieve a file's modification time and convert it to a LISP compatible universal time,
    defined as the number of SECONDS since 00:00:00 Jan 1 1900 GMT.
    Java is fairly useless about the results of file timestamps because of platform dependencies,
    so we may have to do ugly things here, including going to a command shell to get the information.

    Java seems to record and manipulate dates as the number of MILLISECONDS since 00:00:00 Jan 1 1970 GMT.
    So the result that we return is the date returned by java, plus the lisp-based interpretation
    of 00:00:00 Jan 1 1970 GMT added to it.  This value is:

    (encode-universal-time 0 0 0 1 1 1970 0) => 2208988800

    WARNING: probably system dependent.
    */

  private static long getFileModificationTime(File file) throws SecurityException
  {
    // It's possible we need to modify the lastModified() time to account for GMT relative
    // values.  The constant we're adding is zero-relative GMT.
    // We're returning SECONDS, not MILLISECONDS.
    return (file.lastModified()/1000) + 2208988800L ;
  }					// getFileModificationTime

  /*
    Attempt to delete a directory.  If contents is not null, it is assumed that the user requested
    a recursive operation, and contents specifies the results of file.list() and reflects any contents
    which may exist.  If contents is specified, attempt to delete directory contents
    first.  Return true if all deletes work, false otherwise.

    Possible protocol change (to think about):
    Right now, we delete everything we can.  We may want to fail the overall operation which fails,
    and leave other files on disk, reporting the element which failed in the response code.
    */

  private boolean deleteDirectory(File directory, String[] contents) throws SecurityException
  {
    boolean result = true ;
    if (contents != null)
      for (int i = 0 ; i < contents.length ; i++)
	{				// for each element in directory
	  File subFile = new File(directory, contents[i]) ; // must manually merge 'contents' pathnames
	  if (subFile.isDirectory())
	    result = (result && deleteDirectory(subFile, subFile.list())) ; // delete subdirectory contents
	  else
	    result = (result && noisilyDelete(subFile)) ; // delete file in directory
	}				// for each element in directory
    return (result && noisilyDelete(directory)) ; // delete the directory itself
  }					// deleteDirectory


  // This routine is mostly a debugging aid.  We may wisht to change it for applets, etc..

  private boolean noisilyDelete(File file)
  {
      // If the file is read only, set it to read/write before attempting
      // deletion.
      boolean readonly = !file.canWrite ();
      if (readonly) OSServices.SetFileReadOnly (_properties, file, false);
      boolean deleted = file.delete ();
      if (!deleted)
	  {
	      System.out.println("Error deleting " + file.getAbsolutePath()) ;
	      // If we failed to delete it, set it back to read only if we changed it.
	      if (readonly) OSServices.SetFileReadOnly (_properties, file, true);
	      return false ;
	  }
      return true ;
  }					// noisilyDelete

  // Utility routine to copy a file in binary mode
  // Buffer is optional, used to reduce consing.

  private void copyFile(File oldFile, File newFile, byte[] buffer)
       throws SecurityException, FileNotFoundException, IOException
  {

    // Buffering these streams doesn't seem to add any real performance, but it was only tested
    // on small files.
    InputStream in = new FileInputStream(oldFile) ;
    OutputStream out = new FileOutputStream(newFile) ;

    if (buffer == null)
      buffer = new byte[STANDARD_BUFFER_SIZE] ;

    int count ;
    while ((count = in.read(buffer)) > 0)
      out.write(buffer, 0, count) ;

    out.close() ;
    in.close() ;


    OSServices.SetFileReadOnly (_properties, newFile, !oldFile.canWrite());
    OSServices.SetFileExecutable (_properties, newFile, OSServices.GetFileExecutable (_properties, oldFile));
  }					// copyFile

  // Touch the file, creating it if necessary.

  private static void touchFile(File file) throws SecurityException, IOException
  {
    if (!file.exists() || file.length() == 0)
      {					// write and unwrite a byte to a zero length file
	// Turns out we don't have to, simply creating the FileOutputStream will update the timestamp
	// Or create the file.  In Franz' Lisp, this isn't true!
	FileOutputStream outFile = new FileOutputStream(file) ;
	outFile.close() ;
      }					// write and unwrite a byte to a zero length file
    else
      {					// rewrite the first byte in the file
	RandomAccessFile rwFile = new RandomAccessFile(file, "rw") ;
	long position = rwFile.getFilePointer() ;
	int firstByte = rwFile.read() ;
	rwFile.seek(position) ;
	rwFile.write(firstByte) ;
	rwFile.close() ;
      }					// rewrite the first byte in the file
  }					// touchFile

  /*
    Open a file.  See the processOPEN method for more details, and the FILE-SYSTEM-OPEN
    generic lisp function for yet more details.  Return the stream-handle, an integer.
    */

  private int openFile(File file, boolean inputMode, boolean textMode,
		       String record_terminator,
		       String ifExists, String ifDoesNotExist)
       throws IOException, FileSystemAgentException
  {
    // TO-DO: check to see if we have file open.  For now, we assume server has done it.
    _textMode = textMode ;
    _openFile = file ;
    _inputMode = inputMode ;
    _record_terminator = record_terminator;
    _fileInputEOF = false ;
    _UPBCount = 0 ;
    _UPBStart = 0 ;

    if (_openFileHandle != 0)
      throw new FileSystemAgentException("Stream handle " + _openFileHandle + " is already open.") ;

    // NOTE: :ifExists NIL and :ifDoesNotExist NIL are handled by processOPEN()
    // in order to report OK response codes with NIL result.  All error signalling behavior
    // for these options is processed here.

    if (!file.exists())
      {					// file doesn't exist
	if (ifDoesNotExist.equals("ERROR"))
	  throw new FileSystemAgentException
	    ("File doesn't exist, ifDoesNotExist behavior requested is ERROR");
	if (ifDoesNotExist.equals("CREATE") && _inputMode)
	  throw new FileSystemAgentException
	    ("File doesn't exist, ifDoesNotExist behavior CREATE is inappropriate for input operations") ;
	if (ifDoesNotExist.equals("NIL") && _inputMode)
	  throw new FileSystemAgentException
	    ("Program control flow error, ifDoesNotExist NIL should be handled by processOPEN()") ;
      }					// file doesn't exist
    else
      {					// file exists
	if (!_inputMode)
	  {				// ifExists is observed only for output mode
	    if (ifExists.equals("ERROR"))
	      throw new FileSystemAgentException
		("File exists, ifExists behavior requested is ERROR");
	    if (ifExists.equals("NIL"))
	      throw new FileSystemAgentException
		("Program control flow error, ifExists NIL should be handled by processOPEN()") ;
	  }				// ifExists is observed only for output mode
	// Otherwise we assume various options which dictate overwriting, appending, etc..
	// Note that not all lisp OPEN options are supported, only those described by GF FILE-SYSTEM-OPEN
      }					// file exists

    try
      {					// try
	if (_inputMode)
	  {				// Open file for input
	    // Read bytes, whether text or binary mode, we do the canonicalization of text records.
	      // Open as random access to enable us to use readFully method.
	      _fileInputStream = //new BufferedInputStream (
						       new FileInputStream(file)
						       //		, STANDARD_BUFFER_SIZE       )
		;
	  }				// Open file for input
	else
	  {				// open file for output, writes bytes in all cases
	    // Unclear if the BufferedOutputStream for files is really a win since we do most of our
	    // own buffering.
	    if (ifExists.equals("APPEND"))
	      _fileOutputStream =	// append mode on the FileOutputStream CTOR
		new BufferedOutputStream(new FileOutputStream(file.getCanonicalPath(), true),
					 STANDARD_BUFFER_SIZE) ;
	    // WARNING: semantics of NEW-VERSION are not correct *TO-DO*
	    // else it's NEW-VERSION, SUPERSEDE, or OVERWRITE
	    else
	      // write bytes, don't append
	      _fileOutputStream = new BufferedOutputStream(new FileOutputStream(file),
							   STANDARD_BUFFER_SIZE) ;
	  }				// open file for output, writes bytes in all cases
      }					// try
    catch (IOException e)
      {					// catch
	// Clear any evidence of the current file open attempt if there were errors
	_openFile = null ;
	_fileInputStream = null ;
	_fileOutputStream = null ;
	throw e ;			// resignal the error
      }					// catch

    return ++_openFileHandle ;
  }					// openFile

  /*
    Close the machinery associated with the integer stream handle which was returned by a call
    to openFile().
    */

  private void closeFile(int streamHandle) throws IOException, FileSystemAgentException
  {
    if ((streamHandle != _openFileHandle) || (_openFileHandle == 0))
      throw new FileSystemAgentException("Stream handle " + streamHandle + " is not open.") ;

    _openFileHandle-- ;

    if (_fileInputStream != null)
      {
	InputStream fileStream = _fileInputStream ; // cache reference in event of throw
	_fileInputStream = null ;	// clear so we don't try to close it again or otherwise use it
	fileStream.close() ;
      }

    if (_fileOutputStream != null)
      {
	OutputStream fileStream = _fileOutputStream ; // cache reference in event of throw
	_fileOutputStream = null ;	// clear so we don't try to close it again or otherwise use it
	fileStream.close() ;
      }
  }					// closeFile

  /*
    TO-DO/PERFORMANCE/HACK: this is bogus.  Just have lisp read a byte buffer, and do all
    line splitting and buffering in lisp.  It's more efficient there anyway, and grodiously
    complex and unpleasant to read here.  Get rid of the TXTL interface (?)

    Buffer a chunk of text, one or more logical text lines, and return the total bytes
    buffered.  The resulting buffer will be sent to callers of the GETL protocol.
    See processGETL() for more details.  The stream handle indicates which open file we're
    buffering, against the time we support multiple concurrent open files.

    Note that the buffer we use may be larger than the number of bytes we place in it.
    We return the number of bytes placed in the buffer, which will be zero if we reach EOF.

    NOTE: The reasons for not using the java BufferedReader and methods like readLine() are many.
    I'd like to use them and have more international character set friendly I/O.  But the interfaces
    have numerous flaws, like telling you if the last line of a file is properly terminated,
    and consing all over the place.    Se we use our byte-level I/O for text input.
    But internationalization will be a bitch. This code WILL NOT work on other than ASCII data right now.

    Just so you know, coroutines this ugly never happen in lisp :-)  (I'm kidding, mostly...)
    Yes, I know, this is awful code, and will be loathed for cyber-generations to come,
    until the day it is rewritten by some superlative java know-it-all.  More power to you.

    JRM sez: So this is now even more complicated!  We want to translate line ending to
    newline in a system dependent way, i.e.  a bare CR or NL on a MS platform should *not*
    be translated.

    */

  private int bufferText(int streamHandle) throws FileSystemAgentException, IOException
  {
    // Initialize buffers if necessary
    if (_processedByteBuffer == null)
      {
	  // _processedByteBuffer = new ByteArrayOutputStream(STANDARD_BUFFER_SIZE + 200) ;
	  _processedByteBuffer = new byte [STANDARD_BUFFER_SIZE + 200];
	_UPBBuffer = new byte[STANDARD_BUFFER_SIZE] ;
      }

      if (_record_terminator.equals(":CR")) return bufferTextCR (streamHandle);
      else if (_record_terminator.equals(":LF")) return bufferTextLF (streamHandle);
      else if (_record_terminator.equals(":CRLF")) return bufferTextCRLF (streamHandle);
      else throw new FileSystemAgentException ("Bad record_terminator " + _record_terminator);
  }


    private void process_byte (byte b)
    {
	if (_processedByteBufferPtr == _processedByteBuffer.length) {
	    byte [] new_buffer = new byte [_processedByteBuffer.length * 2];
	    System.arraycopy (_processedByteBuffer, 0, new_buffer, 0, _processedByteBuffer.length);
	    _processedByteBuffer = new_buffer;
	}
	_processedByteBuffer [_processedByteBufferPtr++] = b;
    }

    private int bufferTextCR (int streamHandle) throws  FileSystemAgentException, IOException
    {
	while (true) {
	    int inchar;
	    if (_UPBCount == _UPBBuffer.length) {
		// Should re-allocate, but I'm lazy...
		throw new FileSystemAgentException ("Buffer overrun in buffertext.");
	    }
	    inchar = _fileInputStream.read ();
	    if (inchar == -1) {
		_fileInputEOF = true;
		if (_processedByteBufferPtr != 0)
		    process_byte (NEWLINE);
		return _processedByteBufferPtr ;
	    }
	    if (inchar == CARRIAGE_RETURN) {
		process_byte (NEWLINE);
		return _processedByteBufferPtr ;
	    }
	    else
		process_byte ((byte) inchar);
	}
    }

    private int bufferTextLF (int streamHandle) throws  FileSystemAgentException, IOException
    {
	while (true) {
	    int inchar;
	    if (_UPBCount == _UPBBuffer.length) {
		// Should re-allocate, but I'm lazy...
		throw new FileSystemAgentException ("Buffer overrun in buffertext.");
	    }
	    inchar = _fileInputStream.read ();
	    if (inchar == -1) {
		_fileInputEOF = true;
		if (_processedByteBufferPtr != 0)
		    process_byte (NEWLINE);
		return _processedByteBufferPtr ;
	    }
	    if (inchar == NEWLINE) {
		process_byte (NEWLINE);
		return _processedByteBufferPtr ;
	    }
	    else
		process_byte ((byte) inchar);
	}
    }

    private int bufferTextCRLF (int streamHandle) throws  FileSystemAgentException, IOException
    {
	int state = 0;
	// State machine.
	while (true) {
	    int inchar;
	    if (_UPBCount == _UPBBuffer.length) {
		// Should re-allocate, but I'm lazy...
		throw new FileSystemAgentException ("Buffer overrun in buffertext.");
	    }
	    inchar = _fileInputStream.read ();
	    if (inchar == -1) {
		_fileInputEOF = true;
		if (state == 1) {
		    process_byte (CARRIAGE_RETURN);
		}
		if (_processedByteBufferPtr != 0)
		    process_byte (NEWLINE);
		return _processedByteBufferPtr;
	    }
	    switch (state) {
	    case 0:
		// Initial state. No newline or CR seen.
		if (inchar == CARRIAGE_RETURN) {
		    state = 1;
		    break;
		}
		process_byte ((byte) inchar);
		break;
            default:
		// CR seen
		if (inchar == NEWLINE) {
		    process_byte (NEWLINE);
		    return _processedByteBufferPtr;
		}
		else {
		    process_byte (CARRIAGE_RETURN);
		    process_byte ((byte) inchar);
		}
		state = 0;
		break;
	    }}
    }
  /*
    Send the indicated number of bytes buffered by the bufferText() call.
    Unlike sendMultiLineResponse, a trailing newline is NOT included
    */

  private void sendBufferedText(int nBytes) throws IOException
  {
    if (_debugTrace)
      System.out.println("sendtext: <" + nBytes + " expected and "
                         + _processedByteBufferPtr  + " actual bytes>") ;

    try
      {
	  _outputStream.write (_processedByteBuffer, 0, _processedByteBufferPtr);
      }
    finally
      {
	_processedByteBufferPtr = 0 ;
      }
    _outputStream.flush() ;
  }					// sendBufferedText

  /*
    Send the contents of _UPBBuffer for nBytes, with appropriate diagnostic traces thrown in.
    */

  private void sendBufferedBytes(int nBytes) throws IOException
  {
    if (_debugTrace)
      System.out.println("sendbyte: <" + nBytes + " bytes>") ;

    if (nBytes > 0)
      {
	_outputStream.write(_UPBBuffer, 0, nBytes) ;
	_outputStream.flush() ;
      }
  }

  /*
    Ensure that we have a byte buffer large enough to read nBytes.
    We often allocate a larger buffer because we intend to reuse the buffer
    for multiple read or write operations during the lifetime of the FSA.
    */

  private void ensureInputByteBuffer(int nBytes) throws FileSystemAgentException
  {
    // It should always be zero here.  For PUTL, we always flush the buffer.
    // For GETL, we ensure the buffer is flushed on CLOS.  If we support multiple concurrent
    // open files, we need per-file buffering and buffer-management variables.
    if (_UPBCount != 0)
      throw new FileSystemAgentException("Logic error in ensureInputByteBuffer") ;

    // if (_debugTrace) {
    //   System.out.println("ensureInputByteBuffer (" + nBytes + ");");
    //   System.out.flush();
    //   }

    if ((nBytes > STANDARD_BUFFER_SIZE) || (nBytes < 0))
	System.out.println ("Unusual buffer size requested:  " + nBytes + " bytes.");

    if ((_UPBBuffer == null) || (_UPBBuffer.length < nBytes))
	_UPBBuffer = new byte [Math.max (nBytes,
					 (_UPBBuffer == null)
					 ? STANDARD_BUFFER_SIZE
					 : (_UPBBuffer.length * 2))];
  }					// ensureInputByteBuffer

  /*
    Read bytes for text or binary transfers from the socket client
    to the FSA disk.  All text is assumed to be in canonical form
    for the FSA based on the FSA platform type.  We don't do the work here.

    It is an error if we don't receive the indicated number of bytes.
    */

  private void getBytesAndWriteToDisk(int nBytes) throws IOException, FileSystemAgentException
  {
    if (_debugTrace)
      {
	System.out.print("putbytes: <" + nBytes + " bytes expected, ") ;
	System.out.flush() ;
      }

    // Ensure that we have a buffer big enough to receive bytes
    ensureInputByteBuffer(nBytes) ;

    // Read **ALL** the butes into the buffer
    readNBytes(_inputStream, _UPBBuffer, nBytes) ; // rigid blocking read of N bytes.

    if (_debugTrace)
      System.out.println(nBytes + " bytes received>") ;

    _fileOutputStream.write(_UPBBuffer, 0, nBytes) ;
  }					// getBytesAndWriteToDisk

  /*
    Add quotes to string, and escape all characters in the input string, such that
    the result is a string which will read as a lisp string.
    E.g.:  E:\Program Files\Foobar	=> "E:\\Program Files\\Foobar"
  */

  private String escapeString(String input)
  {
    _escapeStringBuffer.setLength(0) ;	// note, StringBuffer shared with getStringTokens
    _escapeStringBuffer.append('"') ;
    for (int i = 0 ; i < input.length() ; i++)
      {
	char c = input.charAt(i) ;
	if (c == '"' || c == '\\')
	  _escapeStringBuffer.append('\\') ;	// escape the next char
	_escapeStringBuffer.append(c) ;
      }					// for loop
    _escapeStringBuffer.append('"') ;

    // it might be more efficient to have caller just iterate over stringbuffer contents and write
    // them to a destination stream, rather than consing a new string right here.
    return _escapeStringBuffer.toString() ;
  }					// escapeString

  /*
    This method is vaguely similar to a StringTokenizer, and was in fact not needed in early
    buggy revs because StringTokenizer was sufficient.  What this method does that StringTokenizer doesn't
    is as follows:
    1) Gathers all tokens into a result vector of strings (not a feature, just a byproduct)
    2) Parses quoted substrings as single strings. In effect, it reads the result of
       escapeString() and returns a java-string which has had quotes removed and escapes resolved.

    StringTokenizer doesn't have the ability to do #2 at all.
    We could potentially have used StreamTokenizer, however this machinery isn't entirely suitable
    either, assuming fairly language-specific syntax (C/C++/Java).  It also requires use of a Reader
    object, etc..  So this method method is a leaner, meaner, and simpler mechanism of performing the
    task at hand. (Hopefully)
  */

  private Vector getStringTokens(String input)
  {
    Vector v = new Vector(6, 6) ;	// 6 should be most tokens any routine in FSA wants
    int	   stringSize = input.length() ;
    int	   currentPosition = 0 ;

    while (currentPosition < stringSize)
      {					// parse a token
	_escapeStringBuffer.setLength(0) ; // reset our token character accumulation buffer
	boolean inString = false ;	// not parsing a string right now

	while (input.charAt(currentPosition) == ' ' && currentPosition < stringSize)
	  currentPosition++ ;		// skip delimiter spaces

	if (currentPosition == stringSize)
	  break ;			// done parsing all tokens

	// Now parse the token.  Must know if we're parsing string token or simple token
	if (input.charAt(currentPosition) == '"')
	  {
	    inString = true ;
	    currentPosition++ ;		// don't include string quote in token
	  }

	// Accrue token chars for a single token
	while (currentPosition < stringSize)
	  {				// while accumulating token chars
	    char ch = input.charAt(currentPosition) ;

	    if (inString && ch == '"')
	      {				// reached end of string token
		currentPosition++ ;	// advance past closing quote
		v.addElement(_escapeStringBuffer.toString()) ; // save the token
		_escapeStringBuffer.setLength(0) ;
		break ;			// done parsing current token
	      }				// reached end of string token
	    else if (inString && ch == '\\')
	      {				// read escaped character in a string
		if (++currentPosition < stringSize) // skip escape
		  _escapeStringBuffer.append(input.charAt(currentPosition++)) ; // grab and skip quoted char
	      }				// read escaped character in a string
	    else if (ch == ' ')
	      {				// blank ends token unless we're in string
		currentPosition++ ;	// skip past blank
		if (inString)
		  _escapeStringBuffer.append(ch) ; // accumulate blank in quoted string
		else
		  {			// end of non-string token, save it
		    v.addElement(_escapeStringBuffer.toString()) ;
		    _escapeStringBuffer.setLength(0) ;
		    break ;		// break current token accumulation loop
		  }			// end of non-string token, save it
	      }				// blank ends token unless we're in string
	    else
	      {				// some arbitrary character in the middle of a token
		_escapeStringBuffer.append(ch) ;
		currentPosition++ ;
	      }				// some arbitrary character in the middle of a token
	  }				// while accumulating token chars
      }					// parse a token

    // See if there was a token accumulating when the loop terminated, possibly because of
    // malformed strings or simply end of string
    if (_escapeStringBuffer.length() > 0)
      v.addElement(_escapeStringBuffer.toString()) ;

    return v ;
  }					// getStringTokens


  /**
    Byte-encoded 4-byte commands which we will process.
    **/

  /** Phony command sent when ending a chunked reply. **/
  private static byte[] REQUEST_END_CHUNK = {48, 13, 10, 13};

  private static byte[] REQUEST_CDIR = "CDIR".getBytes() ; // create directory
  private static byte[] REQUEST_CFIL = "CFIL".getBytes() ; // copy file
  private static byte[] REQUEST_CHMD = "CHMD".getBytes() ; // Change mode (sets read only bit)
  private static byte[] REQUEST_CLOS = "CLOS".getBytes() ; // close file
  private static byte[] REQUEST_CRCB = "CRCB".getBytes() ; // CRC Binary file
  private static byte[] REQUEST_CRCT = "CRCT".getBytes() ; // CRC Text file
  private static byte[] REQUEST_CURR = "CURR".getBytes() ; // get current directory path name
  private static byte[] REQUEST_DDIR = "DDIR".getBytes() ; // delete directory
  private static byte[] REQUEST_DEBG = "DEBG".getBytes() ; // turn on debugging
  private static byte[] REQUEST_DFIL = "DFIL".getBytes() ; // delete file
  private static byte[] REQUEST_DTCT = "DTCT".getBytes() ; // detect file content type
  private static byte[] REQUEST_DTRT = "DTRT".getBytes() ; // detect file record terminator
  private static byte[] REQUEST_GETB = "GETB".getBytes() ; // get bytes (of binary file)
  private static byte[] REQUEST_GPAR = "GPAR".getBytes() ; // get parent directory
  private static byte[] REQUEST_HELO = "HELO".getBytes() ; // say hello
  private static byte[] REQUEST_OPEN = "OPEN".getBytes() ; // open file
  private static byte[] REQUEST_PDIR = "PDIR".getBytes() ; // probe directory
  private static byte[] REQUEST_PFIL = "PFIL".getBytes() ; // probe file
  private static byte[] REQUEST_PLAT = "PLAT".getBytes() ; // get platform information
  private static byte[] REQUEST_PROG = "PROG".getBytes() ; // display progress indicator
  private static byte[] REQUEST_PUTB = "PUTB".getBytes() ; // put bytes (of binary file) (to disk file)
  private static byte[] REQUEST_QUIT = "QUIT".getBytes() ; // cease SERVE activity and close connection.
  private static byte[] REQUEST_RENM = "RENM".getBytes() ; // rename file/directory
  private static byte[] REQUEST_TUCH = "TUCH".getBytes() ; // touch file
  private static byte[] REQUEST_WHAT = "WHAT".getBytes() ; // ask about a value

  // Routine to compare two 4-byte command buffers.  Return true if they compare equal.
  private static boolean compareCommand(byte[] bytes1, byte[] bytes2)
  {
    for (int i = 0 ; i < 4 ; i++)
      if (bytes1[i] != bytes2[i])
	return false ;
    return true ;
  }					// compareCommand

  // Refer to FILE-SYSTEM.LSP for more details on the following codes.

  static final byte[] RESPONSE_PRELIMINARY_BEGINNING_TRANSFER = "125".getBytes() ;

  static final byte[] RESPONSE_COMPLETION_ATOMIC_COMMAND = "200".getBytes() ;
  static final byte[] RESPONSE_COMPLETION_DIRECTORY_STATUS_COMPLETE = "212".getBytes() ;
  static final byte[] RESPONSE_COMPLETION_CONNECTION_CLOSED = "221".getBytes() ;
  static final byte[] RESPONSE_COMPLETION_DATA_SEGMENT_TRANSFER_COMPLETE = "226".getBytes() ;

  static final byte[] RESPONSE_FAILURE_ERROR_PROCESSING = "451".getBytes() ;
  static final byte[] RESPONSE_FAILURE_PERMISSION_DENIED = "453".getBytes() ;
  static final byte[] RESPONSE_FAILURE_DIRECTORY_NOT_EMPTY = "454".getBytes() ;

  static final byte[] RESPONSE_ERROR_UNRECOGNIZED_COMMAND = "500".getBytes() ;
  static final byte[] RESPONSE_ERROR_ARGUMENT_SYNTAX = "501".getBytes() ;
  static final byte[] RESPONSE_ERROR_BAD_COMMAND_SEQUENCE = "503".getBytes() ;
  static final byte[] RESPONSE_ERROR_INVALID_PARAMETER = "504".getBytes() ;
  static final byte[] RESPONSE_ERROR_FILE_INACCESSIBLE = "550".getBytes() ;
  static final byte[] RESPONSE_ERROR_MULTILINE_RESPONSE_ABORTED = "550".getBytes() ;
  static final byte[] RESPONSE_ERROR_INVALID_PATHNAME = "553".getBytes() ;
  static final byte[] RESPONSE_ERROR_NOT_A_DIRECTORY = "554".getBytes() ;
  static final byte[] RESPONSE_ERROR_REASON_UNCLASSIFIED = "555".getBytes() ;

  /**
    The <code>serve</code> function enters a slave mode, waiting in a <code>read</code>
    loop on the socket for the process
    on the other end of the socket to tell us what to do, and executing those instructions until
    we're instructed to quit.

    @param debugTrace, if true, will print diagnostics to the system console
    showing the activity of the file system agent.

    @param progressIndicator, if not null, may periodically have its display methods invoked.
    Note that it is the server driving the serve() method which defines when display methods
    are used, via the PROG service.

    @return A success boolean which is TRUE if the activity which used the FSA was successful, false
    if the activity was unsuccessful.  The server for which the FSA is a slave dictates the
    success code.

    @exception  FileSystemAgentException  if any abnormal flow of control or protocol
    violations are detected during agent operation.

    @exception  IOException  if errors occur while reading from or writing to the agent streams.

    Note that errors while writing to the underlying file system as part of agent file system
    operations are NOT thrown, they are trapped and reported to the which is communicating
    with the FileSystemAgent.
    **/

  public boolean serve(boolean debugTrace, FSAProgressIndicator progressIndicator)
    throws FileSystemAgentException, IOException
  {
    _debugTrace = true ;
    _progressIndicator = progressIndicator ;

    if (!isReady())
      throw new FileSystemAgentException("Serve() call made when agent is not ready.") ;

    // This would be better served by token manipulation, but for now, we use their string form.
    // All tokens are 4 characters, followed by a terminating newline, for a total of 5 chars.
    // Strictly speaking, we don't need to allocate these here, but we do it to minimize consing
    byte[] command = new byte[5] ;

    while (true)
      {
	// Performance note: would be nice to reuse String command/arg buffers too.
	// Right now we cons one for every command.  But this is small spam.
	boolean hasArgs = readCommand(command) ;

	// PERFORMANCE: This should be done as a table driven dispatch.  TO-DO
        if (compareCommand (command, REQUEST_END_CHUNK)) // bogus command when input is chunked
          continue;
	else if (compareCommand(command, REQUEST_CDIR))	  processCDIR(hasArgs) ;
	else if (compareCommand(command, REQUEST_CFIL))	  processCFIL(hasArgs) ;
	else if (compareCommand(command, REQUEST_CHMD))	  processCHMD(hasArgs) ;
	else if (compareCommand(command, REQUEST_CLOS))	  processCLOS(hasArgs) ;
	else if (compareCommand(command, REQUEST_CRCB))	  processCRCB(hasArgs) ;
	else if (compareCommand(command, REQUEST_CRCT))	  processCRCT(hasArgs) ;
	else if (compareCommand(command, REQUEST_CURR))	  processCURR(hasArgs) ;
	else if (compareCommand(command, REQUEST_DDIR))	  processDDIR(hasArgs) ;
	else if (compareCommand(command, REQUEST_DEBG))   processDEBG(hasArgs) ;
	else if (compareCommand(command, REQUEST_DFIL))	  processDFIL(hasArgs) ;
	else if (compareCommand(command, REQUEST_DTCT))	  processDTCT(hasArgs) ;
	else if (compareCommand(command, REQUEST_DTRT))	  processDTRT(hasArgs) ;
	else if (compareCommand(command, REQUEST_GETB))	  processGETB(hasArgs) ;
	else if (compareCommand(command, REQUEST_GPAR))	  processGPAR(hasArgs) ;
	else if (compareCommand(command, REQUEST_HELO))	  processHELO(hasArgs) ;
	else if (compareCommand(command, REQUEST_OPEN))	  processOPEN(hasArgs) ;
	else if (compareCommand(command, REQUEST_PDIR))	  processPDIR(hasArgs) ;
	else if (compareCommand(command, REQUEST_PFIL))	  processPFIL(hasArgs) ;
	else if (compareCommand(command, REQUEST_PLAT))	  processPLAT(hasArgs) ;
	else if (compareCommand(command, REQUEST_PROG))	  processPROG(hasArgs) ;
	else if (compareCommand(command, REQUEST_PUTB))	  processPUTB(hasArgs) ;
	else if (compareCommand(command, REQUEST_QUIT))	  return processQUIT(hasArgs) ;
	else if (compareCommand(command, REQUEST_RENM))	  processRENM(hasArgs) ;
	else if (compareCommand(command, REQUEST_TUCH))	  processTUCH(hasArgs) ;
        else if (compareCommand(command, REQUEST_WHAT))   processWHAT(hasArgs) ;
	else
	  sendResponse(RESPONSE_ERROR_UNRECOGNIZED_COMMAND) ;
      }					// while (true)
  }					// serve()

  /*
    This routine is used to better handle exceptions for multi-segment responses.
    We want to someday attempt to resynchronize with an socket partner if we experience
    handshaking or other I/O anomalies.  For now, we try to determine if we're at a certain stage
    in handshaking, and throw an error if we can't easily recover, or send a command failure response
    if we think we're at a point where we can recover.

    This routine is typically called in the catch block of a multi-segmented response.

    'Exception' is the exception which was caught by the invoking catch block.

    The 'completedPreliminaryResponse' flag indicates that the 'beginning transfer' message was transmitted.

    The 'CompletedBufferTransfer' flag indicates that we successfully completed transmission of the
    buffer(s) in the multi-segment protocol, in either direction (reading or writing).

    If the first is true and the second isn't, things could have gone wrong anywhere, and we give up
    (for now).

    If neither or both are true, then we're at well known states from a handshaking standpoint,
    and we can send an appropriate failure response. (Because we know some exception occurred).

    This routine guarantees either to throw a FileSystemAgent exception to abort FSA operation,
    or to send a failure response to the socket and return.
    */

  private void catchHandlerForMultiSegmentResponse(Exception e,
						   boolean completedPreliminaryResponse,
						   boolean completedBufferTransfer)
       throws FileSystemAgentException, IOException
  {
    if (completedPreliminaryResponse)
      // We know the error arise while sending buffer or final response
      if (!completedBufferTransfer)
	// Entity on other end of socket is probably waiting to send or receive bytes, consider this
	// agent and operation hosed.  TO-DO: We should probably attempt to close open files here.
	throw new FileSystemAgentException("Unexpected exception: " + e.toString()) ;
      else
	// Sending the final response failed (or something leading up to that), also not good
	// TO-DO: We should probably attempt to close open files here.
	throw new FileSystemAgentException("Unexpected exception: " + e.toString()) ;
    else
      // Assume we didn't send the preliminary response, and that some other exception was
      // raised.
      sendResponseAndInformation(RESPONSE_FAILURE_ERROR_PROCESSING, e.toString()) ;
  }					// catchHandlerForMultiSegmentResponse


  // Process the HELO command.  It is an error if this command receives arguments.

  private void processHELO(boolean hasArgs) throws IOException
  {
    if (hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// no args expected
    else
      sendResponseAndInformation (RESPONSE_COMPLETION_ATOMIC_COMMAND, FileSystemAgentProtocol.getVersionString()) ;
  }					// processHELO

    private void processDEBG(boolean hasArgs) throws IOException, FileSystemAgentException
    {

    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ; // args were expected
    else
      {					// read success argument, respond, and prepare for connectionclose
	String success = null ;
	try
	  {				// try
	    success = readArgs() ;	// should be "T" or "NIL"
	    if ((success != null) && success.equals("T"))
		_debugTrace = true;
	    else
		_debugTrace = false;
	  }
	finally
	  {
	    sendResponse(RESPONSE_COMPLETION_ATOMIC_COMMAND) ;
	  }
      }
    }

    private void processWHAT (boolean hasArgs) throws IOException, FileSystemAgentException
    {
        if (!hasArgs)
            sendResponse (RESPONSE_ERROR_ARGUMENT_SYNTAX); // I want args
        else {
            String success = null;
            try {
                success = readArgs();
            }
            finally {
                sendResponseAndInformation (RESPONSE_COMPLETION_ATOMIC_COMMAND, "\"ChickenButt\"");
            }
        }
    }

  // Process the QUIT command, which expects to an argument of "T" or "NIL"
  // indicating a successful or unsuccessful activity for which the FSA was used. This status is returned
  // by the processQUIT() and serve() methods.

  private boolean processQUIT(boolean hasArgs) throws IOException, FileSystemAgentException
  {
    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ; // args were expected
    else
      {					// read success argument, respond, and prepare for connectionclose
	String success = null ;
	try
	  {				// try
	    success = readArgs() ;	// should be "T" or "NIL"
	  }
	finally
	  {
	    sendResponse(RESPONSE_COMPLETION_CONNECTION_CLOSED) ;
	    if (_need_newline) _progressIndicator.displayText("") ;
	  }
	return ((success != null) && success.equals("T")) ;
      }

    return false ;
  }					// processQUIT

  // Process the PLAT command.  It is an error if this command receives arguments.
  // This command wants us to boil down the FSA system type to one of several types.
  // We must reply with a string naming one of lisp's SERVER:*FILE-SYSTEM-PLATFORMS*.
  // and a string naming the RECORD-TERMINATOR

  private void processPLAT(boolean hasArgs) throws IOException
  {
    if (hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// no args expected
    else
      {					// retrieve PLAT information and reply
	String result = null ;
	try
	  {				// try
	    String osName = System.getProperty("os.name") ; // throws SecurityException
	    result = "\"" + osName + "\" ";

	    String sep = System.getProperty ("line.separator") ; // throws SecurityException
	    if (sep.equals ("\r")) result += ":CR ";
	    else if (sep.equals ("\n")) result += ":LF ";
	    else if (sep.equals ("\r\n")) result += ":CRLF ";
	    else result += ":UNKNOWN ";

	    // Tell server what time we think it is.
	    result += (Calendar.getInstance().getTime().getTime()/1000) + 2208988800L ;

            // Tell server where we are.
            result += " \"" + Locale.getDefault().getLanguage() + "-"
                + Locale.getDefault().getCountry() + "\"";
	  }				// try
	catch (SecurityException e)
	  {				// catch
	    // Leave it so lisp can at least read the :UNKNOWN from the string.
	    result = ":UNKNOWN " + e.toString() ;// this will signal server-side error most likely...
	  }				// catch

	sendResponseAndInformation(RESPONSE_COMPLETION_ATOMIC_COMMAND, result) ;
      }					// retrieve PLAT information and reply.
  }					// processPLAT

  /*
    Process the PROG (progress indicator) command.

    PROG takes two arguments:
    1) a quoted text string used as an argument for the FSAProgressIndicator.displayText method, or NIL.
    2) a number parsed as an integer and used as an argument FSAProgressIndicator.displayPercentage method,
       or NIL.

    If either argument is nil, its respective progress indicator method is not called.

    PROG returns T if the FSA has a FSAProgressIndicator object, NIL if it does not.
    */

  private void processPROG(boolean hasArgs) throws IOException
  {
    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// args were  expected
    else
      {					// retrieve PROG args and reply
	byte[] response = RESPONSE_COMPLETION_ATOMIC_COMMAND ;
	String result ;

	if (_progressIndicator == null)
	  result = "NIL" ;
	else
	  {				// parse args, invoke progress indicator
	    result = "T" ;

	    try
	      {				// TRY: parse args, invoke progress indicator methods
		String remainingArgs = readArgs() ;
		Vector tokens = getStringTokens(remainingArgs) ;
		String displayText = (String) tokens.elementAt(0) ;
		String displayPercentage = (String) tokens.elementAt(1) ;

		if (!displayText.equals("NIL"))
		  _progressIndicator.displayText(displayText) ;
		if (!displayPercentage.equals("NIL"))
		  _progressIndicator.displayPercentage(Integer.parseInt(displayPercentage)) ;

		_need_newline = ! displayText.endsWith ("\n");

	      }				// TRY: parse args, invoke progress indicator methods
	    catch (Exception e)
	      {				// CATCH
		// Don't know, don't care, report local processing error.
		response = RESPONSE_FAILURE_ERROR_PROCESSING ;
		result = e.toString() ;
	      }				// CATCH

	    // Report the result.
	    sendResponseAndInformation(response, result) ;
	  }				// parse args, invoke progress indicator
      }					// retrieve PROG args and reply.
  }					// processPROG

    private String deduce_record_terminator (File file) throws IOException
    {
	InputStream in;
	String default_record_terminator;

	String sep = System.getProperty ("line.separator") ; // throws SecurityException
	if (sep.equals ("\r")) default_record_terminator = ":CR";
	else if (sep.equals ("\n")) default_record_terminator = ":LF";
	else if (sep.equals ("\r\n")) default_record_terminator = ":CRLF";
	else default_record_terminator = ":UNKNOWN ";

	if (file.canRead ()) {


	    try {
		in = new BufferedInputStream (new FileInputStream (file.getCanonicalPath()));
	    }
	    catch (FileNotFoundException e) {
		return default_record_terminator;
	    }

	    int char_count = 0;
	    int cr_count = 0;
	    int lf_count = 0;
	    int crlf_count = 0;

	    int previous_char = 0;

	    // Examine the first 20 lines, or, if that is more than 512 chars,
	    // examine at least the first 3 lines.
	    while ((char_count < 512 && cr_count < 20 && lf_count < 20 && crlf_count < 20)
		   || (cr_count < 3 && lf_count < 3 && crlf_count < 3)) {
		int this_char;
		this_char = in.read ();
		if (this_char == -1) {
		    if (previous_char == CARRIAGE_RETURN) {cr_count += 1; break;}
		    if (previous_char == NEWLINE)         {lf_count += 1; break;}
		    break;
		}
		if (previous_char == CARRIAGE_RETURN) {
		    if (this_char == NEWLINE)
			crlf_count += 1;
		    else
			cr_count += 1;
		}
		else if (this_char == NEWLINE)
		    lf_count += 1;
		else
		    char_count += 1;

		previous_char = this_char;
	    }
	    in.close();

	    // Simple maximum should tell us.
	    boolean crlf_wins = ((crlf_count >= lf_count) && (crlf_count >= cr_count));
	    boolean lf_wins = ((lf_count >= crlf_count) && (lf_count >= cr_count));
	    boolean cr_wins = ((cr_count >= crlf_count) && (cr_count >= lf_count));

	    if (default_record_terminator.equals(":CRLF") && crlf_wins) return ":CRLF";
	    else if (default_record_terminator.equals(":LF") && lf_wins) return ":LF";
	    else if (default_record_terminator.equals(":CR") && cr_wins) return ":CR";
	    else if (crlf_wins) return ":CRLF";
	    else if (cr_wins) return ":CR";
	    else if (lf_wins) return ":LF";
	    else return default_record_terminator;
	}
	// If we can't read the file, just assume it is ok.
	else return default_record_terminator;
    }

    // This function is used by both probe file and probe directory.
    // Unified here so we don't get them out of sync.

    private String get_file_info (File file) throws IOException
    {
	_responseStringBuffer.setLength(0) ; // prepare response accumulator
	_responseStringBuffer.append ("T ") ; //  file exists (+ token separator)
	_responseStringBuffer.append (canonicalize_outgoing_pathname (file));
	_responseStringBuffer.append (" ") ; // trailing token separator
	_responseStringBuffer.append (file.length ()) ; // integer automatically converted to string
	_responseStringBuffer.append (" ") ; // token separator
	_responseStringBuffer.append (getFileModificationTime (file)) ; // tricky stuff here, see fcn.
	_responseStringBuffer.append (file.isDirectory() ? " T" : " NIL"); // note token seps here!
	_responseStringBuffer.append (file.canRead()     ? " T" : " NIL");
	_responseStringBuffer.append (file.canWrite()    ? " T" : " NIL");
	_responseStringBuffer.append (OSServices.GetFileExecutable(_properties, file) ? " T" : " NIL");
	return _responseStringBuffer.toString() ; // gather up accumulated response
    }


  /*
    Process the PFIL (probe file) command, which expects one argument, the name of the file to probe.
    PFIL, returns the following information about a file, all in string form:

    EXISTS-P is T or NIL and indicates whether the specified path exists.
    If EXISTS-P is true, the rest of these values follow, otherwise they do not.

    ADJUSTED-PATH-STRING may equal original specification
    SIZE is size in bytes on client FSA file system
    MODIFICATION-DATE is universal time (seconds since 00:00:00 Jan 1 1900 GMT ignoring leap year)
    ISDIR-P is T or NIL indicating whether or not the indicated path-string is a directory.
    CANREAD  is T or NIL indicating whether we can open the file for reading.
    CANWRITE is T or NIL indicating whether we can open the file for writing.
    */

  private void processPFIL(boolean hasArgs) throws IOException
  {
    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// args were  expected
    else
      {					// retrieve PFIL information and reply
	byte[] response = RESPONSE_COMPLETION_ATOMIC_COMMAND ;
	String result ;

	try
	  {				// try: get the file information
	      String remainingArgs = readArgs ();
	      Vector tokens = getStringTokens (remainingArgs);
	      File file = canonicalize_incoming_pathname ((String) tokens.elementAt (0));
	    // Now query the file properties and build up a response string.
	      result = file.exists() ? get_file_info (file) : "NIL";
	  }				// try: get the file information
	catch (Exception e)
	  {				// catch: something went wrong while getting info
	    // Don't know, don't care, report transient local processing error.
	    response = RESPONSE_FAILURE_ERROR_PROCESSING ;
	    result = e.toString() ;
	  }				// catch: something went wrong while getting info

	// Report the result.
	sendResponseAndInformation(response, result) ;
      }					// retrieve PFIL information and reply.
  }					// processPFIL

  /*
    Process the PDIR (probe directory) command, which expects one argument, the name of the
    directory to probe.

    PDIR, Probe Directory, returns a multi-line response with information about every
    file or directory which is contained in the specified directory.

    It is an error if the specified directory does not exist, or is not a directory.

    Every line in the response has the same format and content as that reported by the
    PFIL directive **EXCEPT** for the EXISTS-P flag.  By definition, all information
    reported by the PDIR command implies existing files.  See the processPFIL documentation
    for further information.
    */

  private void processPDIR(boolean hasArgs) throws IOException, FileSystemAgentException
  {
    if (!hasArgs)
      {
	sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ; // args were expected
	return ;
      }

    boolean completedPreliminaryResponse = false ; // used for error handling

    try
      {				// try: get the file information
	  String remainingArgs = readArgs ();
	  Vector tokens = getStringTokens (remainingArgs);
	  File directory = canonicalize_incoming_pathname ((String) tokens.elementAt (0));

	// Verify that a valid directory was specified.
	if (!directory.exists())
	  {
	    sendResponse(RESPONSE_ERROR_FILE_INACCESSIBLE) ;
	    return ;
	  }
	if (!directory.isDirectory())
	  {
	    sendResponse(RESPONSE_ERROR_NOT_A_DIRECTORY) ;
	    return ;
	  }

	// Send the intermediate okay response.
	sendResponse(RESPONSE_PRELIMINARY_BEGINNING_TRANSFER) ;
	completedPreliminaryResponse = true ;

	// Get the directory entries, and send a response line for each one
	String[] fileNames = directory.list() ;

	if (fileNames != null) {
	    // Query the file properties, build response strings, and send them.
	    for (int i = 0 ; i < fileNames.length ; i++)
		// Note the necessary use of this CTOR to merge directory and sub-element names.
		// Any time you use the list() call, it returns just namestrings, not full pathnames.
		sendMultiLineResponseText (get_file_info (new File (directory, fileNames[i])));
	}
      }					// try: get the file information
    catch (Exception e)
      {					// catch: something went wrong while getting info
	// NOTE: It's possible this should use catchHandlerForMultiSegmentResponse, but it was a retrofit
	// and no brain cells were available when it was implemented to see if it should be used here.

	// If we blew up before sending the preliminary response, the preliminary response
	// is now the final response.  Otherwise we should send a empty response line segment
	// and the final response is an abort response.
	if (completedPreliminaryResponse)
	  {				// send empty line segment and negative final response
	    sendMultiLineResponseText(null) ; // assemes last send operation completed successfully!
	    sendResponseAndInformation(RESPONSE_ERROR_MULTILINE_RESPONSE_ABORTED, e.toString()) ;
	    return ;
	  }				// send empty line segment and negative final response
	else
	  {				// Send negative preliminary response
	    sendResponseAndInformation(RESPONSE_FAILURE_ERROR_PROCESSING, e.toString()) ;
	    return ;
	  }				// Send negative preliminary response
      }					// catch: something went wrong while getting info

    // Signal end of sequence and send a positive response
    sendMultiLineResponseText(null) ;
    sendResponse(RESPONSE_COMPLETION_DIRECTORY_STATUS_COMPLETE) ;
  }					// processPDIR

  /*
    Process the CURR command.  It is an error if this command receives arguments.
    Return the path string describing the current directory.  Note that this string
    must be acceptable to subsequent calls to the PDIR function.
    */

  private void processCURR(boolean hasArgs) throws IOException
  {
    if (hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// no args expected
    else
      {					// retrieve CURR information and reply
	byte[] response = RESPONSE_COMPLETION_ATOMIC_COMMAND ;
	String result ;
	try
	  {				// try
	    // WARNING: java current directory selection may be system dependent.
	    String currentDir = System.getProperty("user.dir") ; // throws SecurityException
	    File file = new File(currentDir) ;
	    result = file.getCanonicalPath() ;
	  }				// try
	catch (SecurityException e)
	  {				// catch
	    result = e.toString() ;// this will signal server-side error most likely...
	    response = RESPONSE_FAILURE_ERROR_PROCESSING ;
	  }				// catch

	// result should be terminated with a directory terminator, so we ensure that it is
	// on those systems that do it wrong.  ~jrm

	sendResponseAndInformation(response,
				   result.endsWith (File.separator)
						  ? result
						  : result + File.separator);
      }					// retrieve CURR information and reply.
  }					// processCURR


  /*
    Process the GPAR command, which takes one argument (a path specifier) accompanying the request.
    Return the path string describing the parent directory of the requested path specifier.
    The path specifier need not exist as an entity on disk.
    If there is no parent, we return the empty string.
    */

  private void processGPAR(boolean hasArgs) throws IOException, FileSystemAgentException
  {
    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// args expected
    else
      {					// retrieve GPAR information and reply
	byte[] response = RESPONSE_COMPLETION_ATOMIC_COMMAND ;
	String result ;
	try
	  {				// try
	      String remainingArgs = readArgs ();
	      Vector tokens = getStringTokens (remainingArgs);
	      File file = canonicalize_incoming_pathname ((String) tokens.elementAt (0));

	    // getParent() returns null if no parent, convert to empty string for response
	    result = file.getParent() ;
	    if (result == null)
		result = "" ;
	  }				// try
	catch (SecurityException e)
	  {				// catch
	    result = e.toString() ;// this will signal server-side error most likely...
	    response = RESPONSE_FAILURE_ERROR_PROCESSING ;
	  }				// catch

	sendResponseAndInformation(response,
				   result.length () == 0 || result.endsWith (File.separator)
				   ? result
				   : result + File.separator);
      }					// retrieve GPAR information and reply.
  }					// processGPAR

  /*
    Process the CDIR (create directory) command,
    which takes one argument (a path specifier) accompanying the request.
    Indicate success or failure in the response code.
    */

  private void processCDIR(boolean hasArgs) throws IOException, FileSystemAgentException
  {
    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// args expected
    else
      {					// retrieve CDIR information and reply
	byte[] response = RESPONSE_COMPLETION_ATOMIC_COMMAND ;
	String result = "" ;

	try
	  {				// try
	      String remainingArgs = readArgs ();
	      Vector tokens = getStringTokens (remainingArgs);
	      File file = canonicalize_incoming_pathname ((String) tokens.elementAt (0)); // the path specifier
	    if (!file.mkdir())
	      response = RESPONSE_ERROR_REASON_UNCLASSIFIED ;
	  }				// try
	catch (SecurityException e)
	  {				// catch
	    // Most likely reason to be in this exception handler: canWrite() failed.
	    // We might wish to further classify error as RESPOSNE_FAILURE_PERMISSION_DENIED
	    result = e.toString() ;// this will signal server-side error most likely...
	    response = RESPONSE_FAILURE_ERROR_PROCESSING ;
	  }				// catch

	sendResponseAndInformation(response, result) ;
      }					// retrieve CDIR information and reply.
  }					// processCDIR

  /*
    Process the DFIL (delete file) command,
    which takes one argument (a path specifier) accompanying the request.
    Indicate success or failure in the response code.
    */

  private void processDFIL(boolean hasArgs) throws IOException, FileSystemAgentException
  {
    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// args expected
    else
      {					// retrieve DFIL information and reply
	byte[] response = RESPONSE_COMPLETION_ATOMIC_COMMAND ;
	String result = "" ;
	File file = null;
	boolean readonly = false;

	try
	  {				// try
	      String remainingArgs = readArgs ();
	      Vector tokens = getStringTokens (remainingArgs);
	      file = canonicalize_incoming_pathname ((String) tokens.elementAt (0)); // path specifier

	    readonly = !file.canWrite ();
	    // Make sure we can write it before attempting to delete it.
	    if (readonly) OSServices.SetFileReadOnly (_properties, file, false);
	    if (!file.delete()) {
	      response = RESPONSE_ERROR_REASON_UNCLASSIFIED ;
	      // If we couldn't delete it, set it back to readonly.
	      if (readonly) OSServices.SetFileReadOnly (_properties, file, true);
	    }
	  }				// try
	catch (SecurityException e)
	  {				// catch
	    result = e.toString() ;
	    response = RESPONSE_FAILURE_ERROR_PROCESSING ;
	    // If we got far enough to modify the read only bit,
	    // set it back.
	    if ((file != null) && readonly) OSServices.SetFileReadOnly (_properties, file, true);
	  }				// catch

	sendResponseAndInformation(response, result) ;
      }					// retrieve DFIL information and reply.
  }					// processDFIL

  /*
    Process the DDIR (delete directory) command,
    which takes two arguments (1) a path specifier, and (2) T or F indicating a recursive operation
    is desired (i.e. delete all contents of the directory prior to deleting the directory).
    Indicate success or failure in the response code.
    */

  private void processDDIR(boolean hasArgs) throws IOException, FileSystemAgentException
  {
    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// args expected
    else
      {					// retrieve DDIR information and reply
	byte[] response = RESPONSE_COMPLETION_ATOMIC_COMMAND ;
	String result = "" ;

	try
	  {				// try
	    // We could count the number of args and signal an appropriate error.
	    // For now, we just rely on the catch block.
	    String remainingArgs = readArgs() ;	// path specifier and T or F
	    Vector tokens =  getStringTokens(remainingArgs) ;
	    File file = canonicalize_incoming_pathname ((String) tokens.elementAt (0));
	    String recursiveString = ((String) tokens.elementAt(1)).toUpperCase() ;
	    boolean recursive = recursiveString.equals("T") ;

	    if (!file.isDirectory())
	      response = RESPONSE_ERROR_NOT_A_DIRECTORY ;

	    String[] contents = null ;
	    if (recursive)
	      contents = file.list() ;

	    if (!recursive && contents != null && contents.length != 0)
	      response = RESPONSE_FAILURE_DIRECTORY_NOT_EMPTY ;
	    else
	      if (!deleteDirectory(file, contents))
		response = RESPONSE_FAILURE_ERROR_PROCESSING ;
	  }				// try
	catch (Exception e)
	  {				// catch
	    // Most likely reason to be in this exception handler: checkDelete(), tokenizer errors, etc..
	    // We might wish to further classify error as RESPOSNE_FAILURE_PERMISSION_DENIED
	    result = e.toString() ;
	    response = RESPONSE_FAILURE_ERROR_PROCESSING ;
	  }				// catch

	sendResponseAndInformation(response, result) ;
      }					// retrieve DDIR information and reply.
  }					// processDDIR

  /*
    Process the RENM (rename) command, which takes two arguments:
    (1) a path specifier naming the file/directory as it is currently labelled,
    (2) a path specifier naming the file/directory as it should be newly labelled.
    Note the lisp generic function semantics, do not use this method to MOVE files outside
    the directory in which they currently reside.
    */

  private void processRENM(boolean hasArgs) throws IOException, FileSystemAgentException
  {
    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// args expected
    else
      {					// retrieve RENM information and reply
	byte[] response = RESPONSE_COMPLETION_ATOMIC_COMMAND ;
	String result = "" ;

	try
	  {				// try
	    String remainingArgs = readArgs() ;
	    Vector tokens = getStringTokens(remainingArgs) ;
	    File oldFile = canonicalize_incoming_pathname ((String) tokens.elementAt(0));
	    File newFile = canonicalize_incoming_pathname ((String) tokens.elementAt(1));

	    // TO-DO? Verify pathname components are consistent except for name/type specifiers.

	    // Attempt the rename operation
	    if (!oldFile.renameTo(newFile))
	      response = RESPONSE_FAILURE_ERROR_PROCESSING ;
	  }				// try
	catch (Exception e)
	  {				// catch
	    result = e.toString() ;
	    response = RESPONSE_FAILURE_ERROR_PROCESSING ;
	  }				// catch

	sendResponseAndInformation(response, result) ;
      }					// retrieve RENM information and reply.
  }					// processRENM

  /*
    Process the CFIL (copy file) command, which takes two arguments:
    (1) a path specifier naming the source file.
    (2) a path specifier naming the target file.
    Note the lisp generic function semantics, both arguments must name files, not directories.
    */

  private void processCFIL(boolean hasArgs) throws IOException, FileSystemAgentException
  {
    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// args expected
    else
      {					// retrieve CFIL information and reply
	byte[] response = RESPONSE_COMPLETION_ATOMIC_COMMAND ;
	String result = "" ;

	try
	  {				// try
	    String remainingArgs = readArgs() ;
	    Vector tokens = getStringTokens(remainingArgs) ;
	    String oldName = (String) tokens.elementAt(0) ;
	    String newName = (String) tokens.elementAt(1) ;
	    File oldFile = new File(oldName) ;
	    File newFile = new File(newName) ;

	    // Check arg validity.
	    if (oldFile.isDirectory() || !oldFile.exists())
	      {
		response = RESPONSE_ERROR_INVALID_PARAMETER ;
		result = oldName ;
	      }
	    else if (newFile.isDirectory()) // we'll overwrite new file if necessary
	      {
		response = RESPONSE_ERROR_INVALID_PARAMETER ;
		result = newName ;
	      }
	    else
	      copyFile(oldFile, newFile, null) ; // Attempt the copy operation, works or throws
	  }				// try
	catch (Exception e)
	  {				// catch
	    result = e.toString() ;
	    response = RESPONSE_FAILURE_ERROR_PROCESSING ;
	  }				// catch

	sendResponseAndInformation(response, result) ;
      }					// retrieve CFIL information and reply.
  }					// processCFIL

  /*
    Process the TUCH (touch) command, which takes four arguments:
    (1) a path specifier naming the file to be touched.  If it exists, it must name a file.
    (2) the time to set the last-modified date to.
    (3) A string indicating what to do if the file does not exist.
	Currently either "CREATE" or "IGNORE"
    (4) T or NIL whether to set the file read-only as well.
    */

  private void processTUCH(boolean hasArgs) throws IOException, FileSystemAgentException
  {
    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// args expected
    else
      {					// retrieve TUCH information and reply
	byte[] response = RESPONSE_COMPLETION_ATOMIC_COMMAND ;
	String result = "T" ;		// assume we're able to touch the file

	String remainingArgs = null;
	Vector tokens = null;
	String fileName = null;
	long modtime = 0;
	String ifDoesNotExistBehavior = null;
	String setReadoNly = null;
	String setExec = null;
	File file = null;

	try
	  {				// try
	      remainingArgs = readArgs() ;
	      tokens = getStringTokens(remainingArgs) ;
	      fileName = (String) tokens.elementAt(0) ;
	      modtime = Long.parseLong ((String) tokens.elementAt (1));
	      ifDoesNotExistBehavior = (String) tokens.elementAt(2) ;
	      setReadoNly = (String) tokens.elementAt(3);
	      setExec = (String) tokens.elementAt(4);
	      file = new File(fileName) ;

	    // Check arg validity.  Assume keywords arg from server is valid.
	    if (file.isDirectory())
	      response = RESPONSE_ERROR_INVALID_PARAMETER ;
	    else if (!file.exists() && !ifDoesNotExistBehavior.equals("CREATE"))
	      result = "NIL" ;
	    else
	      touchFile(file) ;		// will touch or create, we return true
	  }				// try
	catch (Exception e)
	  {				// catch
	    result = e.toString() ;
	    response = RESPONSE_FAILURE_ERROR_PROCESSING ;
	  }				// catch

	// we lie to the caller!  We don't attempt the command until
	// after we tell the caller it is done.  This is because we
	// cannot in general promise that we can do it anyway, so if we
	// fail, we fail silently.
	// By letting the caller proceed at this point, however, it might
	// be able to run in parallel with this possibly expensive operation.
	sendResponseAndInformation(response, result) ;

	if (response == RESPONSE_COMPLETION_ATOMIC_COMMAND && result.equals("T")) {
	    try {
	      // Attempt to set the file modification time.
	      OSServices.SetFileLastModified (_properties, file, modtime);
	    }
	    catch (Exception e) {};
	    try {
	      // Attempt to set the file read only, if requested.
	      if (setReadoNly.equals("T"))
		  OSServices.SetFileReadOnly (_properties, file, true);
	      if (setExec.equals("T"))
		  OSServices.SetFileExecutable (_properties, file, true);
	    }
	    catch (Exception e) {};
	}
      }					// retrieve TUCH information and reply.
  }					// processTUCH

  private void processCHMD(boolean hasArgs) throws IOException, FileSystemAgentException
  {
    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// args expected
    else
      {					// retrieve CHMD information and reply
	byte[] response = RESPONSE_COMPLETION_ATOMIC_COMMAND ;
	String result = "T" ;		// assume we're able to chmod the file
	String remainingArgs = null;
	Vector tokens = null;
	String fileName = null;
	String mode = null;
	File file = null;

	boolean readonly = false;

	try
	  {				// try
	      remainingArgs = readArgs() ;
	      tokens = getStringTokens(remainingArgs) ;
	      fileName = (String) tokens.elementAt(0) ;
	      mode = (String) tokens.elementAt(1) ;

	      file = new File(fileName) ;

	    // Check arg validity.  Assume keywords arg from server is valid.
	    if (file.isDirectory())
	      response = RESPONSE_ERROR_INVALID_PARAMETER ;
	    else if (!file.exists())
	      response = RESPONSE_ERROR_INVALID_PARAMETER ;
	    else {

		if (mode.equals(":READ-WRITE"))
		    readonly = false;
		else if (mode.equals (":READ-ONLY"))
		    readonly = true;
		else {
		    response = RESPONSE_ERROR_INVALID_PARAMETER ;
		    result = "invalid file mode" ;
		}}
	  }
	catch (Exception e)
	  {				// catch
	    result = e.toString() ;
	    response = RESPONSE_FAILURE_ERROR_PROCESSING ;
	  }				// catch

	// we lie to the caller!  We don't attempt the command until
	// after we tell the caller it is done.  This is because we
	// cannot in general promise that we can do it anyway, so if we
	// fail, we fail silently.
	// By letting the caller proceed at this point, however, it might
	// be able to run in parallel with this possibly expensive operation.
	sendResponseAndInformation(response, result) ;


	if (response == RESPONSE_COMPLETION_ATOMIC_COMMAND) {
	    try {
		OSServices.SetFileReadOnly (_properties, file, readonly);
	    }
	    catch (Exception e) {};
	}

      }					// retrieve CHMD information and reply.
  }

  /*
    Process the OPEN (file open) command, which takes five arguments
    (1) A path specifier naming the file to be opened.
    (2) A direction specifier, which must be OUTPUT or INPUT.
    (3) A character mode, which must be TEXT or BINARY.
    (4) An if-exists specifier, which must be ERROR, NEW-VERSION, OVERWRITE, APPEND, SUPERSEDE, or NIL.
    (5) An if-does-not-exist specifier, which must be ERROR, CREATE, or NIL.

    Return an integer stream handle which may be used by protocol invoker to perform subsequent
    operations. We don't currently support concurrent open OPEN files, though it's a SMOP to do so,
    since the basic structure and protocol for stream handles is implemented.

    The ifExists and ifDoesNotExist options are complex.  We might move some of the processing
    out of the FSA and into the lisp server.  But the OVERWRITE/APPEND and other options need to be done
    by the FSA.
    */

  private void processOPEN(boolean hasArgs) throws IOException, FileSystemAgentException
  {
    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// args expected
    else
      {					// retrieve OPEN information and reply
	byte[] response = RESPONSE_COMPLETION_ATOMIC_COMMAND ;
	String result = "NIL" ;		// if exceptions are thrown or no file is opened, this is the return

	try
	  {				// try
	    String remainingArgs = readArgs() ;
	    Vector tokens = getStringTokens(remainingArgs) ;
	    String fileName = (String) tokens.elementAt(0) ;
	    boolean inputMode = ((String) tokens.elementAt(1)).equals("INPUT") ;
	    boolean textMode = ((String) tokens.elementAt(2)).equals("TEXT") ;
	    String record_terminator = "NIL";
	    String ifExists = (String) tokens.elementAt(3) ;
	    String ifDoesNotExist = (String) tokens.elementAt(4) ;
	    File file = new File(fileName) ;

	    // We skip most arg checking here.  We could spend a lot of time on it,
	    // but for now we assume that what we support is in sync with what the server sends.
	    // We only check the things that the server can't know about, like whether the person
	    // has requested opening a directory, etc..  (Well, they can find that out too with PFIL,
	    // but we check again anyway).
	    if (file.isDirectory())
	      {
		response = RESPONSE_ERROR_INVALID_PARAMETER ;
		result = "file to be opened is a directory" ;
	      }
	    // The following situations are basically error situations where we return "OK" and "NIL"
	    // and must process them here, since signalling in openFILE isn't prepared to do this when
	    // handled in the catch block here.
	    else if (!file.exists() && inputMode)
	      {
		// Not an error if they requested NIL for this behavior
		// Note that CREATE is not valid for input.
		if (!ifDoesNotExist.equals("NIL"))
		  {
		    response = RESPONSE_ERROR_FILE_INACCESSIBLE ;
		    result = "Input file does not exist" ;
		  }
	      }
	    // Handle ifExists and ifDoesNotExist for output mode
	    // This is tricky.  Read the lisp OPEN function documentation for details.
	    else if (!inputMode && file.exists() && ifExists.equals("NIL"))
	      {				// do nothing, return nil as an error condition
	      }				// do nothing, return nil as an error condition
	    else if (!inputMode && !file.exists() && ifDoesNotExist.equals("NIL"))
	      {				// do nothing, return nil as error condition
	      }				// do nothing, return nil as error condition
	    // Done with error situations where we return NIL
	    else
	      // Open file or signal error.  Returns int handle
	      result = Integer.toString(openFile(file, inputMode, textMode, record_terminator, ifExists, ifDoesNotExist)) ;
	  }				// try
	catch (Exception e)
	  {				// catch
	    result = e.toString() ;
	    response = RESPONSE_FAILURE_ERROR_PROCESSING ;
	  }				// catch

	sendResponseAndInformation(response, result) ;
      }					// retrieve OPEN information and reply.
  }					// processOPEN


  /*
    Process the CLOS (file close) command, which takes one argument:
    (1) the stream handle which was returned by the OPEN service.
    */

  private void processCLOS(boolean hasArgs) throws IOException, FileSystemAgentException
  {
    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// args expected
    else
      {					// retrieve CLOS information and reply
	byte[] response = RESPONSE_COMPLETION_ATOMIC_COMMAND ;
	String result = "" ;

	try
	  {				// try
	    String streamHandleString = readArgs() ;
	    int    streamHandle = Integer.parseInt(streamHandleString) ; // throws NumberFormatException
	    closeFile(streamHandle) ;
	  }				// try
	catch (Exception e)
	  {				// catch
	    result = e.toString() ;
	    response = RESPONSE_FAILURE_ERROR_PROCESSING ;
	  }				// catch

	sendResponseAndInformation(response, result) ;
      }					// retrieve CLOS information and reply.
  }					// processCLOS

  /*
    Process PUTB logic.

    This routine simply saves cut&paste between the two routines.

    isTextStream should be true if the stream is semantically a text stream, false otherwise,
    it is used for diagnostics only.
    */

  private void processPutBytes(boolean hasArgs, boolean isTextStream)
       throws IOException, FileSystemAgentException
  {
    if (!hasArgs)
      sendResponseAndInformation(RESPONSE_ERROR_ARGUMENT_SYNTAX, "Missing stream handle and nBytes") ;
    else if (_inputMode)
      sendResponseAndInformation(RESPONSE_ERROR_BAD_COMMAND_SEQUENCE,
				 "Stream handle wasn't opened for output") ;
    else if (_textMode != isTextStream)
      sendResponseAndInformation(RESPONSE_ERROR_INVALID_PARAMETER,
				 "Stream handle wasn't opened for " +
				 (isTextStream ? "text" : "binary" + " operations")) ;
    else
      {					// retrieve PutBytes information and reply
	boolean completedPreliminaryResponse = false ; // used for error handling
	boolean completedBufferRead = false ;

	try
	  {				// try
	    String remainingArgs = readArgs() ;
	    Vector tokens = getStringTokens(remainingArgs) ;
	    String streamHandleString = (String) tokens.elementAt(0) ;
	    String byteString = (String) tokens.elementAt(1) ;
	    int    streamHandle = Integer.parseInt(streamHandleString) ; // throws NumberFormatException
	    int	   nBytes = Integer.parseInt(byteString) ;

	    // Send the preliminary acknowledgement
	    sendResponse(RESPONSE_PRELIMINARY_BEGINNING_TRANSFER) ;
	    completedPreliminaryResponse = true ;
	    // Receive the buffer
	    getBytesAndWriteToDisk(nBytes) ; // relay the bytes
	    completedBufferRead = true ;
	    // Send the final terminating completion reply
	    sendResponse(RESPONSE_COMPLETION_DATA_SEGMENT_TRANSFER_COMPLETE) ;
	  }				// try
	catch (Exception e)
	  {				// catch
	    catchHandlerForMultiSegmentResponse(e, completedPreliminaryResponse, completedBufferRead) ;
	  }				// catch
      }					// retrieve PutBytes information and reply.
  }					// processPutBytes

  /*
    Process the GETB (Get Bytes) command, which takes two arguments:
    (1) the stream handle which was returned by the OPEN service.
    (2) the maximum number of bytes to transmit.

    This is a multi-phased response.  See the GF for FILE-SYSTEM-READ-BYTES on SOCKET-FILE-SYSTEM.
    for details.  Return zero bytes in the expected response if we hit eof.

    NOTE: the terminating 126 response isn't strictly necessary for this command (and others).
    We might want to eliminate it to avoid the 5ms processing time which is required for the
    handshake.  It mostly serves as a sanity check.
    */

  private void processGETB(boolean hasArgs) throws IOException, FileSystemAgentException
  {
    if (!hasArgs)
      sendResponseAndInformation(RESPONSE_ERROR_ARGUMENT_SYNTAX, "Missing stream handle and/or n-bytes") ;
    else if (!_inputMode)
      sendResponseAndInformation(RESPONSE_ERROR_BAD_COMMAND_SEQUENCE,
				 "Stream handle wasn't opened for input") ;
    else if (_textMode)
      sendResponseAndInformation(RESPONSE_ERROR_INVALID_PARAMETER,
				 "Stream handle wasn't opened for binary operations") ;
    else
      {					// retrieve GETB information and reply
	boolean completedPreliminaryResponse = false ; // used for error handling
	boolean completedBufferSend = false ;

	try
	  {				// try
	    String remainingArgs = readArgs() ;
	    Vector tokens = getStringTokens(remainingArgs) ;
	    String streamHandleString = (String) tokens.elementAt(0) ;
	    String byteString = (String) tokens.elementAt(1) ;
	    int    streamHandle = Integer.parseInt(streamHandleString) ; // throws NumberFormatException
	    int	   nBytes = Integer.parseInt(byteString) ;
	    ensureInputByteBuffer(nBytes) ; // ensure that there are sufficient bytes in the buffer

	    // Read the bytes.  Note that we don't use the readNBytes primitive since it insists on nBytes,
	    // and the nBytes requested here by the FSA user is present only if the file has that many
	    // bytes remaining.

	    // Make an effort to get all the bytes requested.  The file system depends on
	    // this to infer end of file.  (If it doesn't get back all the bytes it asked for,
	    // it assumes that it has reached eof).
	    int actualBytes = 0;
	    while (nBytes > 0) {
		int result = _fileInputStream.read(_UPBBuffer, actualBytes, nBytes) ;
		if (result == -1) break;
		actualBytes += result;
		nBytes -= result;
	    }

	    // Send the preliminary acknowledgement, along with the number of bytes we'll actually return
	    sendResponseAndInformation(RESPONSE_PRELIMINARY_BEGINNING_TRANSFER,
				       Integer.toString(actualBytes)) ;
	    completedPreliminaryResponse = true ;
	    // Send the buffer
	    sendBufferedBytes(actualBytes) ;// knows to do nothing for zero bytes
	    completedBufferSend = true ;
	    // Send the final terminating completion reply
	    sendResponse(RESPONSE_COMPLETION_DATA_SEGMENT_TRANSFER_COMPLETE) ;
	  }				// try
	catch (Exception e)
	  {				// catch
	    catchHandlerForMultiSegmentResponse(e, completedPreliminaryResponse, completedBufferSend) ;
	  }				// catch
      }					// retrieve GETB information and reply.
  }					// processGETB

  /*
    Process the PUTB (Put Bytes) command.  This is the only command which
    reads bytes in a multi-segment fashion from the other end of the socket.

    Accompanying the PUTB request line are the following arguments:
    (1) The stream handle which was returned by the OPEN service, indicating the file open for output.
    (2) The number of bytes which will be transferred in the next segment.
    */

  private void processPUTB(boolean hasArgs) throws IOException, FileSystemAgentException
  {
    processPutBytes(hasArgs, false) ;	// false == binary mode
  }					// processPUTB

  /*
    Process the DTRT (detect record terminator) command, which expects one argument, the
    name of the file to probe.

    DTRT, returns the following information about a file, all in string form:
    :CR :LF :CRLF
    */

  private void processDTRT(boolean hasArgs) throws IOException
  {
    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// args were  expected
    else
      {					// retrieve DTRT information and reply
	byte[] response = RESPONSE_COMPLETION_ATOMIC_COMMAND ;
	String result ;

	try
	  {				// try: get the file information
		String remainingArgs = readArgs() ;
		Vector tokens = getStringTokens(remainingArgs) ;
		File file = canonicalize_incoming_pathname ((String) tokens.elementAt(0));
	    // Now deduce the record terminator build up a response string.
	    if (file.exists())
	      {				// file exists, accumulate rest of information
		_responseStringBuffer.setLength(0) ; // prepare response accumulator
		_responseStringBuffer.append (deduce_record_terminator (file));
		result = _responseStringBuffer.toString() ; // gather up accumulcated response
	      }				// file exists, accumulate rest of information
	    else
	      {				// file does not exist, no further information need be gathered
		result = "NIL" ;
	      }				// file does not exist, no further information need be gathered
	  }				// try: get the file information
	catch (Exception e)
	  {				// catch: something went wrong while getting info
	    // Don't know, don't care, report transient local processing error.
	    response = RESPONSE_FAILURE_ERROR_PROCESSING ;
	    result = e.toString() ;
	  }				// catch: something went wrong while getting info

	// Report the result.
	sendResponseAndInformation(response, result) ;
      }					// retrieve DTRT information and reply.
  }					// processDTRT

  /*
    Process the DTCT (detect file content type) command, which takes one argument:
    (1) the name of the file to examine.

    See the lisp generic function documentation and socket-file-system subtype documentation
    for FILE-SYSTEM-FILE-CONTENT-TYPE for more details.
    */

  private void processDTCT(boolean hasArgs) throws IOException, FileSystemAgentException
  {
    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// args were  expected
    else
      {					// retrieve DTCT information and reply
	boolean completedPreliminaryResponse = false ;
	FileInputStream in = null ;

	try
	  {				// try: get the file information
		String remainingArgs = readArgs() ;
		Vector tokens = getStringTokens(remainingArgs) ;
		File file = canonicalize_incoming_pathname ((String) tokens.elementAt(0)); // the path-string of the file we're to probe

	    if (!file.exists())
	      {				// file doesn't exist, we're done
		sendResponseAndInformation(RESPONSE_COMPLETION_ATOMIC_COMMAND, ":NOFILE") ;
		return ;
	      }				// file doesn't exist, we're done

	    if (file.isDirectory())
	      {				// file is a directory, we're done
		sendResponseAndInformation(RESPONSE_COMPLETION_ATOMIC_COMMAND, ":DIRECTORY") ;
		return ;
	      }				// file is a directory, we're done

	    long longLength = file.length() ;
	    int length = (longLength > Integer.MAX_VALUE) ? Integer.MAX_VALUE : (int) longLength ;

	    if (length == 0)
	     {				// no bytes to sample, return that fact
	       sendResponseAndInformation(RESPONSE_COMPLETION_ATOMIC_COMMAND, ":ZERO") ;
	       return ;
	     }				// no bytes to sample, return that fact

	    // Get some file bytes and return them in a multi-segmented response.

	    if (length > 1024)
	      length = 1024 ;		// just sample the first 1024 bytes.

	    // Obtain the sample
	    in = new FileInputStream(file) ;

	    // Since we've bypassed the openFile method, we must ensure that this the buffer management
	    // variables required by ensureInputByteBuffer are set appropriately
	    _UPBCount = 0 ;
	    ensureInputByteBuffer(length) ; // make sure _UPBBuffer has space

	    int bytesRead = in.read(_UPBBuffer, 0, length) ;
	    if (bytesRead < length)
	      // Bad news actually, but for now we'll issue a warning and return zero bytes
	      {
		System.out.println("Error sampling bytes in DTCT processing, expected " + length
				   + " and received only " + bytesRead + ". Returning zero bytes.") ;
		length = 0 ;
	      }				// bytesRead < length

	    in.close() ;
	    in = null ;			// sentinel to catch handler for file closing

	    // Send the preliminary response with the number of bytes which will be in our sample
	    sendResponseAndInformation(RESPONSE_PRELIMINARY_BEGINNING_TRANSFER,
				       Integer.toString(length)) ;
	    completedPreliminaryResponse = true ;

	    // Send the bytes, if there are any
	    sendBufferedBytes(length) ;

	    // Send the terminating response
	    sendResponse(RESPONSE_COMPLETION_DATA_SEGMENT_TRANSFER_COMPLETE) ;
	  }				// try: get the file information
	catch (Exception e)
	  {				// catch: something went wrong while getting info
	    if (in != null)
	      in.close() ;		// close the file we had open

	    if (completedPreliminaryResponse)
	      // Error occured while writing response bytes or final response.
	      // Terminate FSA operation until we implement a smart RESYNC capability, since
	      // right now we don't know where we were in handshaking.
	      throw new FileSystemAgentException("Unexpected exception: " + e.toString()) ;
	    else
	      // Error occured before any response was sent, send a negative response
	      sendResponseAndInformation(RESPONSE_FAILURE_ERROR_PROCESSING, e.toString()) ;
	  }				// catch: something went wrong while getting info
      }					// retrieve DTCT information and reply.
  }					// processDTCT

  /*
    Process the CRCB (CRC Binary) command, which takes one argument:
    (1) the name of the file to examine.

    See the lisp generic function documentation and socket-file-system subtype documentation
    for FILE-SYSTEM-FILE-CRC for more details.
    */

  private void processCRCB(boolean hasArgs) throws IOException, FileSystemAgentException
  {
    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// args were  expected
    else
	{
	    byte [] response = RESPONSE_COMPLETION_ATOMIC_COMMAND;
	    String result;
	    try {
		String remainingArgs = readArgs() ;
		Vector tokens = getStringTokens(remainingArgs) ;
		File file = canonicalize_incoming_pathname ((String) tokens.elementAt(0));
		if (file.exists()) {
		    FileInputStream instream = new FileInputStream (file);
		    byte buffer [] = new byte [STANDARD_BUFFER_SIZE];
		    int answer = crc.INITIAL_CRC;
		    int count = 0;

		    while (true) {
			count = instream.read (buffer);
			if (count == -1) break;
			answer = crc.update_crc (answer, buffer, count);
		    }
		    instream.close();
		    result = Long.toString (int_to_unsigned_long (answer));
		}
		else {
		    response = RESPONSE_ERROR_INVALID_PARAMETER;
		    result = file.getPath ();
		}
	    }
	catch (Exception e)
	  {				// catch: something went wrong while getting info
	    // Don't know, don't care, report transient local processing error.
	    response = RESPONSE_FAILURE_ERROR_PROCESSING ;
	    result = e.toString() ;
	  }				// catch: something went wrong while getting info

	// Report the result.
	sendResponseAndInformation(response, result) ;
      }					// retrieve CRCB information and reply.
  }

  /*
    Process the CRCT (CRC Text) command, which takes one argument:
    (1) the name of the file to examine.

    See the lisp generic function documentation and socket-file-system subtype documentation
    for FILE-SYSTEM-FILE-CRC for more details.
    */

  private void processCRCT(boolean hasArgs) throws IOException, FileSystemAgentException
  {
    if (!hasArgs)
      sendResponse(RESPONSE_ERROR_ARGUMENT_SYNTAX) ;	// args were  expected
    else
	{
	    byte [] response = RESPONSE_COMPLETION_ATOMIC_COMMAND;
	    String result;
	    try {
		String remainingArgs = readArgs() ;
		Vector tokens = getStringTokens(remainingArgs) ;
		File file = canonicalize_incoming_pathname ((String) tokens.elementAt(0));
		if (file.exists()) {
		    byte bnl [] = {NEWLINE};
		    int handle = openFile (file, true, true, deduce_record_terminator (file), "NIL", "ERROR");
		    //System.out.println ("rec sep is " + _record_terminator);

		    int answer = crc.INITIAL_CRC;
		    int size;

		    while (true) {
			size = bufferText (_openFileHandle);
			//System.out.println ("size is " + size);
			if (size == 0)
			    break;
			answer = crc.update_crc (answer, bnl, 1);
			answer = crc.update_crc (answer, _processedByteBuffer, size - 1);
			_processedByteBufferPtr = 0;
			if (_fileInputEOF)
			    break;

		    }
		    closeFile (handle);

		    result = Long.toString (int_to_unsigned_long (answer));
		}
		else {
		    response = RESPONSE_ERROR_INVALID_PARAMETER;
		    result = file.getPath ();
		}
	    }
	    catch (Exception e)
		{				// catch: something went wrong while getting info
		    // Don't know, don't care, report transient local processing error.
		    response = RESPONSE_FAILURE_ERROR_PROCESSING ;
		    result = e.toString() ;
		}				// catch: something went wrong while getting info

	    // Report the result.
	    sendResponseAndInformation(response, result) ;
	}					// retrieve CRCT information and reply.
  }

}
