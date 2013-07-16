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
 **
 ** OSServices is an abstract base class describing the API to non-portable
 ** OS specific services.  At runtime, you would use the reflection mechanism
 ** to create a class that implements OSServices to provide the concrete
 ** implementation of this abstract class.  
 ** @see SunServices
 ** @see MSServices
 **
 ****************************************************************************/

/**
 * OSServices is an abstract base class describing the API to non-portable
 * OS specific services.  At runtime, you would use the reflection mechanism
 * to create a class that implements OSServices to provide the concrete
 * implementation of this abstract class.
 *
 * @note This probably isn't *quite* the right way to do this, but I think 
 * it is close.  Ideally, a client would not have to instantiate a concrete
 * osservices object and invoke the methods, but would simply call some static
 * methods provided by the OSServices class.  The static methods would then
 * simply trampoline to the appropriate concrete methods.  To do this, I think
 * that OSServices would need a static member of type OSServices.  This would be
 * initialized to a concrete subclass that did the actual work.  Something like
 * this:
 *
 * class OSServices {
 *    OSServices concrete_methods; 
 *
 *    private abstract int ConcreteFooMethod (int arg);
 *
 *    public static int Foo (int arg) {
 *        return concrete_methods.ConcreteFooMethod (arg);
 *        }
 *
 *  }
 *
 *  I think this would have the desired behavior (provided, of course,
 *  that you arranged concrete_methods to be appropriately initialized),
 *  but the verbosity is appalling.
 *
 *  I also think that maybe we should *derive* the static trampoline
 *  class from the OSServices class.
 *
 * @author jrm@content-integrity.com
 * @see SunServices
 * @see MSServices
 */

abstract public interface OSServicesInterface {

    /**
     * <code>MessageBox</code> displays a `MessageBox' with a given text
     * and caption.  On MS, it is like the WinApi MessageBox call, otherwise
     * it simply prints the caption and the text to System.out.
     * 
     * @param <code>hWnd</code> is a handle to the parent window.  It should
     * always be set to zero.
     * 
     * @param <code>lpText</code> is a <code>String</code> that is the message to
     * be displayed.
     * 
     * @param <code>lpCaption</code> is the <code>String</code> caption to be
     * placed on the message box.
     * 
     * @param <code>uType</code> is an <code>int</code> that describes the type
     * of message box.  It should always be zero.
     */
    public abstract int MessageBox (int hWnd, String lpText, String lpCaption, int uType);
     
    /**
     * <code>GetUserHomeDirectory</code> returns a string representing the pathname
     * of the user's home directory.
     */
    public abstract String GetUserHomeDirectory ();

    /**
     * <code>SetFileLastModified</code> changes the lastModified value for
     * file, if possible.  This function is allowed to leave the current
     * timestamp unmodified if there is no mechanism to change the timestamp.
     * 
     * @param <code>properties</code> is an instance of <code>java.util.Properties</code>
     * that may contain an entry for SET_FILE_LAST_MODIFIED
     *
     * @param <code>file</code> is an instance of <code>java.io.File</code>
     * that is the file whose timestamp is to be changed.
     * 
     * @param <code>lastModified</code> is a <code>long</code> new timestamp.
     */
    public abstract void SetFileLastModified (java.util.Properties properties, java.io.File file, long lastModified);

    /**
     * <code>SetFileLastModified</code> changes the read only access value for
     * file, if possible.  This function is allowed to leave the file
     * access unmodified if there is no mechanism to change it.
     * 
     * @param <code>properties</code> is an instance of <code>java.util.Properties</code>
     * that may contain an entry for SET_FILE_READ_ONLY and SET_FILE_READ_WRITE.
     *
     * @param <code>file</code> is an instance of <code>java.io.File</code>
     * that is the file whose access is to be changed.
     * 
     * @param <code>readOnly</code> is a <code>boolean</code> indicating whether
     * the file should be set to read only or read write.
     */
    public abstract void SetFileReadOnly (java.util.Properties properties, java.io.File file, boolean readOnly);
    public abstract void SetFileExecutable (java.util.Properties properties, java.io.File file, boolean executable);
    public abstract boolean GetFileExecutable (java.util.Properties properties, java.io.File file);
}
