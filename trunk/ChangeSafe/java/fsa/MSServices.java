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
 ** File Name:	   MSServices.java
 ** Author:        Joe Marshall
 ** 
 ** Module Description:
 **
 ** MSServices is a possible concrete implementation of OSServices.
 ** The methods in MSServices are specific to the Microsoft Virtual
 ** Machine on windows.  If you need portable services, consider
 ** SunServices.java
 **
 ****************************************************************************/

import com.ms.win32.*;

/**
 * @author jrm@content-integrity.com
 * @see Osservices
 */

public class MSServices implements OSServicesInterface
{
    /** @see OSServices#MessageBox(int hWnd, String lpText, String lpCaption, int uType) */
    public int MessageBox (int hWnd, String lpText, String lpCaption, int uType) {
	return User32.MessageBox (hWnd, lpText, lpCaption, uType);
    }

    static final int MAX_ENV_VALUE = 1024;

    public String GetEnv (String variable_name)
    {
	StringBuffer env_value = new StringBuffer(MAX_ENV_VALUE-1);
	int res = Kernel32.GetEnvironmentVariable (variable_name, env_value, MAX_ENV_VALUE);
	return (res == 0 || res > MAX_ENV_VALUE) ? "" : env_value.toString();
    }

    public String GetUserHomeDirectory () {
	return GetEnv ("HOMEDRIVE") + GetEnv ("HOMEPATH");
    }

    // LISP format: number of seconds since Jan 1, 1970.
    // NT format:  number of 100-ns intervals since Jan 1, 1601.
    // Trivia:  9435484800 seconds elapsed between Jan 1, 1601 and
    //          Jan 1, 1970

    private long nt_time_from_lisp_time (long lisp_time) {
	return (lisp_time + 9435484800L) * 10000000L;
    }

    private long lisp_time_from_nt_time (long nt_time) {
	return nt_time / 10000000L - 9435484800L;
    }

    private boolean modify_file_timestamp (java.io.File file, long lastModified) {
	// attempt to modify the thing.  If we could, we return true,
	// if not, false.
	int file_handle = 
	    Kernel32.CreateFile (file.getAbsolutePath (), 
				 wing.GENERIC_WRITE, // dwDesiredAccess = write access
				 0,	// dwSharedMode = unshared
				 null,               // security attributes
				 wino.OPEN_ALWAYS, // create if not there.
				 // It is unclear to me whether this flag is useful,
				 // but it seems that it ought to be what we are doing.
				 winf.FILE_FLAG_BACKUP_SEMANTICS,	// dwFlagsAndAttribues
				 0	// hTemplateFile
				 );
	if (file_handle != wini.INVALID_HANDLE_VALUE) {
	    FILETIME creation_time = new FILETIME    ();
	    FILETIME last_access_time = new FILETIME ();
	    FILETIME last_write_time = new FILETIME  ();
	    FILETIME new_last_write_time = new FILETIME (nt_time_from_lisp_time (lastModified));

	    Kernel32.GetFileTime (file_handle, creation_time, last_access_time, last_write_time);
	    Kernel32.SetFileTime (file_handle,
				  creation_time.getTimeLong () < new_last_write_time.getTimeLong ()
				      ? creation_time
				  : new_last_write_time,
				  last_access_time,
				  new_last_write_time);
	    Kernel32.CloseHandle (file_handle);
	    return true;
	}
	else 
	    return false;
    }

    /** @see OSServices#SetFileLastModified(java.util.Properties properties, java.io.File file, long lastModified) */
    public void SetFileLastModified (java.util.Properties properties, java.io.File file, long lastModified) {
	if (modify_file_timestamp (file, lastModified))
	    return;
	else {
	    // maybe it was read only?
	    int attributes = Kernel32.GetFileAttributes (file.getAbsolutePath ());
	    if ((attributes != -1) && ((attributes & winf.FILE_ATTRIBUTE_READONLY) != 0)) {
		SetFileReadOnly (properties, file, false);
		modify_file_timestamp (file, lastModified);
		SetFileReadOnly (properties, file, true);
	    }
	}
    }
    
    /** @see OSServices#SetFileReadOnly (java.util.Properties properties, java.io.File file, boolean readOnly) */
    public void SetFileReadOnly (java.util.Properties properties, java.io.File file, boolean readOnly)
    {
	int oldAttributes = Kernel32.GetFileAttributes (file.getAbsolutePath ());
	/* System.out.print ("SetFileReadOnly, oldAttributes = " + oldAttributes); */
	/* Don't screw around with the weird bits. */
	if ((oldAttributes != -1)                        && 
	    ((oldAttributes & winf.FILE_ATTRIBUTE_HIDDEN) == 0)     &&
	    ((oldAttributes & winf.FILE_ATTRIBUTE_SYSTEM) == 0)     &&
	    ((oldAttributes & winf.FILE_ATTRIBUTE_DIRECTORY) == 0)  &&
	    ((oldAttributes & winf.FILE_ATTRIBUTE_TEMPORARY) == 0)  &&
	    ((oldAttributes & winf.FILE_ATTRIBUTE_COMPRESSED) == 0) &&
	    ((oldAttributes & winf.FILE_ATTRIBUTE_OFFLINE) == 0)) {
	    int newAttributes = readOnly 
		? oldAttributes | winf.FILE_ATTRIBUTE_READONLY 
		: oldAttributes & ~winf.FILE_ATTRIBUTE_READONLY;
	    /* System.out.print ("SetFileReadOnly, newAttributes = " + oldAttributes); */
	    Kernel32.SetFileAttributes (file.getAbsolutePath (), newAttributes);
	}
    }

    public void SetFileExecutable (java.util.Properties properties, java.io.File file, boolean executable) {
    }

    public boolean GetFileExecutable (java.util.Properties properties, java.io.File file) {
	return false;
    }

}
