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
 ** File Name:	   UserPrefs.java
 ** Author:        Mark Nahabedian
 ** Creation Date: 1999-12-08
 ** 
 ** Module Description:
 **
 ** A class for getting user preference information from a file.
 ** Given a properties file in the user's ome directory, we can load
 ** that file into a Properties object and extract information from
 ** there.
 **
 ****************************************************************************/



import java.util.Properties ;
import java.io.* ;

public class UserPrefs 
{
    // The name of the preferences file to look for in the user's home directory
    private String _file_name = null ;
    private Properties _properties = null ;
  
    public UserPrefs(String file_name) throws SecurityException
    {
	String _prefs_path = null ;  // full file name of user preferences file

	_file_name = file_name ;
	_properties = new Properties();

	String userHomedir = System.getProperty ("user.home");
	if (userHomedir != null)
	    _prefs_path = userHomedir + _file_name ;

	if (_prefs_path != null) {
	    try {
		FileInputStream fs = new FileInputStream(_prefs_path);
		_properties.load(fs);
		fs.close() ;
	    }
	    catch (FileNotFoundException e) {}
	    catch (IOException e) {
		System.err.println("Problem reading preferences file " + _prefs_path);
	    }
	}
    }

    public String getProperty(String property_key)
    {
	return _properties.getProperty(property_key) ;
    }

    public String getProperty(String property_key, String default_value)
    {
	return _properties.getProperty(property_key, default_value) ;
    }

}

