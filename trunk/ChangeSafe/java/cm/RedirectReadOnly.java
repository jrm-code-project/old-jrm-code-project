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
 ** File Name:	   RedirectReadOnly.java
 ** Author:        jrm
 ** Creation Date: July 2000
 **
 ** Module Description:
 **
 ** Represents a redirect command issued because a request that could be
 ** satisfied by a read-only server was issued to a read-write server.  This
 ** kind of redirect shifts the load away from the read-write servers.
 **
 */

/**
 * @author jrm@content-integrity.com
 */

class RedirectReadOnly extends Throwable {
    Set alternate_servers;

    /**
     *  <code>RedirectReadOnly</code> constructs an RedirectReadOnly object.
     *
     *  @param <code>alternates</code> is a <code>Set</code> of
     *  <code>java.net.URL</code> objects that are given by the busy server
     *  as alternative servers to try.  This set may be empty.  It is not
     *  authoritative.
     */
    public RedirectReadOnly (Set alternates) {
	alternate_servers = alternates;
    }

    /**
     *  <code>alternateServers</code> returns the Set of <code>java.net.URL</code>
     *  that the busy server recommended be tried.
     */
    public Set alternateServers () {
	return alternate_servers;
    }
}
