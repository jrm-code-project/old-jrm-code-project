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
 ** File Name:	   SunServices.java
 ** Author:        Joe Marshall
 **
 ** Module Description:
 **
 ** SunServices is a possible concrete implementation of OSServices.
 ** The methods in SunServices should be portable Java code and not
 ** require any non-portable code to run (thus it is the lowest common
 ** denominator implementation.)
 **
 ** @see OSServices
 **
 ****************************************************************************/

import java.io.*;
import java.util.Calendar;
import java.util.Date;
import java.util.StringTokenizer;
import java.util.Vector;
import java.util.Date;

public class SunServices implements OSServicesInterface {

    private boolean TraceP (java.util.Properties properties) {
	String trace_string = properties.getProperty ("TRACE_SHELL_COMMANDS");
	return (trace_string != null) &&
	    (trace_string.equals ("T"));
    }

    private void display_command_array (String [] Cmdarray) {
	int i;
	System.out.println ("Invoking program \"" + Cmdarray[0] + "\" on the following arguments:");
	for (i = 1; i < Cmdarray.length; i++)
	    System.out.println ("    Arg " + i + ":  " + Cmdarray[i]);
	System.out.flush() ;
	return;
    }

    // Give up on command after this timeout
    private static long timeout = 30 * 1000;  // milliseconds
    private static long sleepytime = 100;     // milliseconds, how long to sleep for

    // When a command takes too long to run, we remember it and don't
    // do it again.  It's been suggested that instead of never
    // executing the command again (in the life of the client), that
    // instead we adopt a more forgiving scheme like periodically
    // retrying the command.  Naha feels that the eventual way to deal
    // with this is for the server to tell the client everything about
    // all of the timestamp and attribute setting as one large batch
    // of things for all of the just-downloaded files.  The client can
    // then execute that batch of operations off-line from the server.
    private Vector bad_commands = new Vector();

    private boolean accursedP (String command_name) {
        for(int i = bad_commands.size(); (--i) >= 0; ) {
	    try {
	        if( bad_commands.elementAt(i).equals(command_name) )
	            return true;
            }
	    catch (ArrayIndexOutOfBoundsException e) {
                System.out.println("Can't happen:  index out of range in accursedP");
	    }
	}
	return false;
    }

    private void accurse (String command_name) {
        if (!accursedP(command_name))
	    bad_commands.addElement(command_name);
    }

    private int invoke_cmdarray (String [] Cmdarray, boolean tracep) {
	int result = -1;	// exit value returned by command invoked
        boolean finished = false;
	long expire = 0;
	InputStream child_stdout = null;
	InputStream child_stderr = null;

	if (tracep) display_command_array (Cmdarray);

        if (accursedP(Cmdarray[0])) {
            if (tracep) System.out.println("Command will be skipped because of previous timeouts");
        } else {
	    try {
		Process p = Runtime.getRuntime().exec(Cmdarray);
		expire = (new Date()).getTime() + timeout;

		if (tracep) {
		    child_stdout = p.getInputStream ();
		    child_stderr = p.getErrorStream ();
		}

		for (; !finished && (new Date()).getTime() < expire;) {
		     try {
			 result = p.exitValue();
			 finished = true;
		     }
		     catch (IllegalThreadStateException e) {
			 try {
			     Thread.sleep(sleepytime);   // maybe yeild() instead.
			 }
			 catch (InterruptedException e1) { }
		     }
		}
		if (!finished) {
		    accurse(Cmdarray[0]);
		    p.destroy();
		}

		if (tracep) {
		    if (finished) 
		        System.out.println ("Program exited with code " + result + ".");
		    else
		        System.out.println ("Program was aborted after timeout.");

		    if (child_stdout.available () > 0) {
			System.out.println ("Program standard output follows:");
			while (child_stdout.available () > 0)
			    System.out.write (child_stdout.read ());
			System.out.println ("");
		    }
		    else
			System.out.println ("The program wrote nothing to its standard output.");

		    if (child_stderr.available () > 0) {
			System.out.println ("Program error output follows:");
			while (child_stderr.available () > 0)
			    System.out.write (child_stderr.read ());
			System.out.println ("");
		    }
		    else
			System.out.println ("The program wrote nothing to its error output.");
		}
		else {
		    if (!finished) {
		        display_command_array (Cmdarray);
			System.out.println ("program failed to finish after timeout.");
		    }
		}
	    }
	    catch (java.io.IOException e) {
		System.out.println ("Failed: " + e);
	    }
	}
	return result;
    }

    /**
     * @see OSServices
     * @note MessageBox simply prints a string of the form "Caption: text" to
     * System.out
     */
    public int MessageBox (int hWnd, String lpText, String lpCaption, int uType) {
	System.out.println (lpCaption + ":  " + lpText);
	System.out.flush() ;
	return 0;
    }

    public String GetUserHomeDirectory () {
	return System.getProperty ("user.home");
    }

    /**
     * @see OSServices
     * @note <code>SetFileModified</code> cannot be implemented in Java.
     */
    public void SetFileLastModified (java.util.Properties properties, java.io.File file, long lastModified) {
	boolean tracep = TraceP (properties);
	String Command = properties.getProperty ("SET_FILE_LAST_MODIFIED");
	if (Command == null) {
	    if (tracep)
		System.out.println ("No value for SET_FILE_LAST_MODIFIED");
	    return;
	}

	StringTokenizer stok = new StringTokenizer (Command);
	// System.out.println ("");
	// System.out.println ("Command is:  " + Command);
	int TokencounT = stok.countTokens();
        int i;
        String [] Cmdarray = new String [TokencounT + 1];
	for (i = 0; i < TokencounT; i++) Cmdarray[i] = stok.nextToken();
	i -= 1;
	// Ya gotta love this:  The touch command wants LOCAL time!
	// (and in a bizarre format, to boot)  So we convert lastModified,
	// which is a Lisp universal time, to a unix time, then adjust for
	// local time (respecting the timezone, of course!)
        // Remembering, of course, that of all the fields, Month is returned
        // zero based, instead of 1 based.

	// Wait!  It get's better!  Not only does touch want a local time, it
	// interprets times in the past based on the current time zone.  
        // What does that mean?  You have to manually adjust for daylight savings
        // time conditional on whether the time you are adjusting for is in the 
        // same daylight savings time mode as it is right now.

	// Java is helpful in returning the daylight savings time correction in
	// milliseconds, so that you are especially accurate.

	// Of course, the local time format DIFFERS depending on the unix platform.

	// And you wonder why I hate unix.
	Calendar timestamp = Calendar.getInstance();
	timestamp.setTime (new Date (((long)lastModified - 2208988800L) * 1000L));

	// Correct for broken interpretation of DST.
	Calendar today = Calendar.getInstance ();
	int dst_correction = timestamp.get (Calendar.DST_OFFSET) - today.get (Calendar.DST_OFFSET);
	// Why is the granularity of DST in milliseconds?!
	timestamp.add (Calendar.MILLISECOND, dst_correction);

	// I defy you to understand DateFormat.
	// It's easier to pad the strings by hand.
	String year   = Integer.toString (timestamp.get (Calendar.YEAR));
	String month  = Integer.toString (timestamp.get (Calendar.MONTH) + 1); // why?
	if (month.length() < 2) month = "0" + month;
	String day    = Integer.toString (timestamp.get (Calendar.DAY_OF_MONTH));
	if (day.length() < 2) day = "0" + day;
	String hour   = Integer.toString (timestamp.get (Calendar.HOUR_OF_DAY));
	if (hour.length() < 2) hour = "0" + hour;
	String minute = Integer.toString (timestamp.get (Calendar.MINUTE));
	if (minute.length() < 2) minute = "0" + minute;
	String second = Integer.toString (timestamp.get (Calendar.SECOND));
	if (second.length() < 2) second = "0" + second;

	// System.out.println (Cmdarray[i]);
	// Look for insane format.
	if (Cmdarray[i].compareTo("MMDDhhmmCCYY.ss") == 0)
	    Cmdarray[i] = month + day + hour + minute + year + "." + second;
	// assume sane format.
	else
	    Cmdarray[i] = year + month + day + hour + minute + "." + second;
	i += 1;
	Cmdarray[i] = file.getAbsolutePath ();

	// System.out.println ("");
	// System.out.println ("Command args are:  " + Command);
	// for (i = 0; i < TokencounT + 1; i++) System.out.println ("arg " + i + ": " + Cmdarray[i]);
	invoke_cmdarray (Cmdarray, tracep);
    }

    /** @see OSServices#SetFileReadOnly (java.io.File file, boolean readOnly) */
    public void SetFileReadOnly (java.util.Properties properties, java.io.File file, boolean readOnly) {
	boolean tracep = TraceP (properties);
	String Command = properties.getProperty (readOnly ? "SET_FILE_READ_ONLY" : "SET_FILE_READ_WRITE");

	if (Command == null) {
	    if (tracep)
		System.out.println ("No value for " + (readOnly ? "SET_FILE_READ_ONLY" : "SET_FILE_READ_WRITE"));
	    return;
	}

	// This is getting uglier and uglier!
	StringTokenizer stok = new StringTokenizer (Command);
	int TokencounT = stok.countTokens();
	int i;
	String [] Cmdarray = new String [TokencounT + 1];
	for (i = 0; i < TokencounT; i++) Cmdarray[i] = stok.nextToken();
	Cmdarray[i] = file.getAbsolutePath ();
	invoke_cmdarray (Cmdarray, tracep);
    }

    public void SetFileExecutable (java.util.Properties properties, java.io.File file, boolean executable) {
	boolean tracep = TraceP (properties);
	String Command = properties.getProperty (executable ? "SET_FILE_EXECUTABLE" : "SET_FILE_NOT_EXECUTABLE");

	if (Command == null) {
	    if (tracep)
		System.out.println ("No value for " + (executable ? "SET_FILE_EXECUTABLE" : "SET_FILE_NOT_EXECUTABLE"));
	    return;
	}

	// This is getting uglier and uglier!
	StringTokenizer stok = new StringTokenizer (Command);
	int TokencounT = stok.countTokens();
	int i;
	String [] Cmdarray = new String [TokencounT + 1];
	for (i = 0; i < TokencounT; i++) Cmdarray[i] = stok.nextToken();
	Cmdarray[i] = file.getAbsolutePath ();
	invoke_cmdarray (Cmdarray, tracep);
    }

    public boolean GetFileExecutable (java.util.Properties properties, java.io.File file) {
	boolean tracep = TraceP (properties);
	String Command = properties.getProperty ("GET_FILE_EXECUTABLE");

	if (Command == null) {
	    if (tracep)
		System.out.println ("No value for GET_FILE_EXECUTABLE");
	    return false;
	}

	// This is getting uglier and uglier!
	StringTokenizer stok = new StringTokenizer (Command);
	int TokencounT = stok.countTokens();
	int i;
	String [] Cmdarray = new String [TokencounT + 1];
	for (i = 0; i < TokencounT; i++) Cmdarray[i] = stok.nextToken();
	Cmdarray[i] = file.getAbsolutePath ();
	return invoke_cmdarray (Cmdarray, tracep) == 0;
    }
}
