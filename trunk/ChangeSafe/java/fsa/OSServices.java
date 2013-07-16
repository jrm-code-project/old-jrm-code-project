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
 ** File Name:	   OSServices.java
 ** Author:        Joe Marshall
 **
 ** Module Description:
 ** Trampoline class to provide OSServices.
 **
 ** @see SunServices
 ** @see MSServices
 **
 ****************************************************************************/
import java.lang.reflect.*;

public class OSServices
    // Semantically, this class
    // implements OSServicesInterface
    // but technically, it doesn't.  Why?  This class provides a static interface
    // to the methods in OSServicesInterface

{
    static boolean initialized = false;
    static OSServicesInterface concrete_interface;

    // Return true if the Microsoft VM classes are available for use, false otherwise.

    private static boolean using_ms ()
    {
	return System.getProperty ("java.vendor").indexOf ("Microsoft") != -1;
    }

    private static boolean using_windows ()
    {
	return System.getProperty ("os.name").toLowerCase().indexOf ("windows") != -1;
    }

    private static OSServicesInterface create_services (String name) throws
	ClassNotFoundException,
	IllegalAccessException,
	InstantiationException,
	InvocationTargetException,
	NoSuchMethodException
    {
	// The Class.getConstructor() and Class.newInstance() methods require
	// an argument signature so they can resolve the correct method.
	// We intend to just create one of these with no arguments, so this
	// object represents a null argument list.
	Class no_arguments [] = {};

	// System.out.print ("Loading ");
	// System.out.print (name);
	// System.out.flush ();

	// Cons up one of these suckers on the fly.
	return (OSServicesInterface)
	    Class.forName (name)
	    .getConstructor (no_arguments)
	    .newInstance (no_arguments);
    }

    private static void initialize () throws
	ClassNotFoundException,
	IllegalAccessException,
	InstantiationException,
	InvocationTargetException,
	NoSuchMethodException
    {
	concrete_interface = create_services (using_ms() ? "MSServices" : "SunServices");
	initialized = true;
    }

    private static void maybe_initialize () {
	if (!initialized) {
	    try {
		initialize ();
	    }
	    catch (Exception e) {
		// Uhh....  We don't want to re-throw because then all
		// OSServices methods would have to redeclare these ugly
		// exceptions.
	    }
	}
    }

    /** @see OSServices#MessageBox(int hWnd, String lpText, String lpCaption, int uType) */
    public static int MessageBox (int hWnd, String lpText, String lpCaption, int uType)
    {
	maybe_initialize ();
	return concrete_interface.MessageBox (hWnd, lpText, lpCaption, uType);
    }

    public static String GetUserHomeDirectory () 
    {
	maybe_initialize ();
	return concrete_interface.GetUserHomeDirectory ();
    }

    /** @see OSServices#SetFileLastModified(java.util.Properties properties, java.io.File file, long lastModified) */
    public static void SetFileLastModified (java.util.Properties properties, java.io.File file, long lastModified)
    {
	maybe_initialize ();
	concrete_interface.SetFileLastModified (properties, file, lastModified);
    }

    /** @see OSServices#SetFileReadOnly(java.io.File file, boolean readonly ) */
    public static void SetFileReadOnly (java.util.Properties properties, java.io.File file, boolean readonly)
    {
	maybe_initialize ();
	concrete_interface.SetFileReadOnly (properties, file, readonly);
    }

    public static void SetFileExecutable (java.util.Properties properties, java.io.File file, boolean executable)
    {
	if (using_windows()) return;

	maybe_initialize ();
	concrete_interface.SetFileExecutable (properties, file, executable);
    }

    public static boolean GetFileExecutable (java.util.Properties properties, java.io.File file)
    {
	if (using_windows()) return false;

	maybe_initialize ();
	return concrete_interface.GetFileExecutable (properties, file);
    }

}
