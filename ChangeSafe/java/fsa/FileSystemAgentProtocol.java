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
 ** File Name:	   FileSystemAgentProtocol.java
 ** Author:        Joe Marshall
 ** 
 ** Module Description:
 **
 ** A simple class that only holds the protocol version number.
 **
 ****************************************************************************/

public class FileSystemAgentProtocol {

    // Must match *socket-file-system-protocol-version* in
    // server/file-system.lsp
    private static int PROTOCOL_VERSION = 1; 


    public static String getVersionString () 
    {
	return Integer.toString (PROTOCOL_VERSION);
    }

}
