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
 ** File Name:	   Set.java
 ** Author:        jrm
 ** Creation Date: July 2000
 **
 ** Module Description:
 **
 ** A simple way of representing immutable Sets of java objects.
 **
 */

/**
 * @author jrm@content-integrity.com
 */


public class Set {

    private Object my_element;
    private Set my_more;

    /**
     *  <code>Set</code> constructs an immutable empty set.
     */
    public Set () {
	my_element = null;
	my_more = null;
    }

    private Set (Object new_element, Set inner_set) {
	my_element = new_element;
	my_more = inner_set;
    }

    /**
     *  <code>isEmpty</code> returns true if the Set is empty,
     *  returns false otherwise.
     */
    public boolean isEmpty () {
	return (my_element == null);
    }

    /**
     *  <code>cardinality</code> returns the number of unique elements
     *  in Set.  An empty Set has cardinality 0.
     */
    public int cardinality () {
	return (my_element == null)
	    ? 0
	    : my_more.cardinality() + 1;
    }

    /**
     *
     *  <code>member</code> tests an object for membership in a Set.
     *  <code>member</code> returns true the object is a member, and
     *  false if it is not.  The <code>Object.equals</code> operator is used to
     *  test object identity.
     *
     * @param <code>thing</code> is an instance of <code>java.lang.Object</code>
     */
    public boolean member (Object thing) {
	return (my_element == null)
	    ? false
	    : my_element.equals (thing) || my_more.member (thing);
    }

    /**
     *  <code>adjoin</code> returns a new Set that contains all the
     *  objects of the Set in addition to the element adjoined.  The
     *  resulting Set may be <code>==</code> to the original.  The
     *  <code>Object.equals</code> operator is used to test object identity.
     *
     * @param <code>thing</code> is an instance of <code>java.lang.Object</code>
     */
    public Set adjoin (Object thing) {
	return this.member (thing)
	    ? this
	    : new Set (thing, this);
    }

    /**
     *  <code>pick</code> returns an element of Set, or null if the
     *  Set is empty.  The elements of a Set are selected in no
     *  defined order.
     */
    public Object pick () {
	return my_element;
    }

    /**
     *  <code>nth</code> returns the nth element of Set, or null
     *  if the set is smaller than n.  It is an error to specify an integer 
     *  less than zero.
     */
    public Object nth (int n) {
	return ((n == 0) || (my_element == null))
	    ? my_element
	    : my_more.nth (n - 1);
    }

    /**
     *  <code>remove</code> returns a new Set that contains all the
     *  objects of the Set except the element removed.  The
     *  resulting Set may be <code>==</code> to the original if the
     *  element was not in the Set.  The <code>Object.equals</code> operator is used to
     *  test object identity.
     *
     * @param <code>thing</code> is an instance of <code>java.lang.Object</code>
     */
    public Set remove (Object element) {
	return (my_element == null)
	    ? this
	    : my_element.equals (element)
	    ? my_more.remove (element)
	    : new Set (my_element, my_more.remove (element));
    }

    /**
     *  <code>union</code> returns a new Set that contains all the
     *  objects of both Set and <code>other</code>.
     *  The resulting Set may be <code>==</code> to the original.
     *  The <code>Object.equals</code> operator is used to test object identity.
     *
     * @param <code>other</code> is a Set.
     */
    public Set union (Set other) {
	return (my_element == null)
	    ? other
	    : my_more.union (other.adjoin (my_element));
    }

    /**
     *  <code>difference</code> returns a new Set that contains all the
     *  objects of the Set that are not members of <code>other</code>.
     *  The resulting Set may be <code>==</code> to the original.
     *  The <code>Object.equals</code> operator is used to test object identity.
     *
     * @param <code>other</code> is a Set.
     */
    public Set difference (Set other) {
	return (my_element == null)
	    ? this
	    : (other.member (my_element))
	    ? my_more.difference (other)
	    : new Set (my_element, my_more.difference (other));
    }

    public void dump (String set_name, java.io.PrintStream s) {
	Set scan = this;
	s.println (set_name);
        while (!scan.isEmpty()) {
	    s.println ("    " + scan.pick().toString());
	    scan = scan.remove (scan.pick());
	}
    }
}
