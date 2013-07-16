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
 ** File Name:             Queue.java
 ** Author:                Ade Lucas
 **                        
 ****************************************************************************/

/**
 * Queue can act as a buffer for objects produced by instances of
 * 'Producer' classes. Instances of 'Producer' classes place the
 * objects that they produce in an instance of a Queue class.
 *
 * The objects remain there until a 'Consumer' object pulls them out
 * of the Queue object.
 *
 * If the Queue object is empty, a 'Consumer' object that wants to get
 * an object from it must wait until a 'Producer' object puts an object in
 * the Queue object.
 * <b>Notes</b><br>
 * <li>The Queue uses the 'Guarded Suspension' Concurrency Pattern to suspend execution of
 * the pull() method call until a precondition is satisfied -- object entry in request queue.
 * The push method adds objects to a queue, and the pull method removes objects from the queue.
 * When the queue is empty, the get method should wait until a call to add an object to the queue
 * for it to return. Both methods are synchronized to allow concurrent access to a Queue object.
 * Simply making both methods synchronized creates a problem when there is a call to a Queue object's
 * pull method and the queue is empty. The pull method waits for a call to the push method to provide
 * it with an object to return. However because they are both synchronized, calls to the push method
 * cannot execute until the pull method returns, and the pull emthod will never return until a call to
 * the push method executes.<br>
 * A solution is to add a precondition to the pull method so that it does not execute when the queue is empty.
 * @author Ade Lucas
 */
import java.util.Vector ;

public class Queue {

   private Vector data = new Vector() ;
   
   /**
    * Put an object on the end of the queue
    * @param obj the object to put at the end of the queue
    */
    synchronized public void push ( Object entry ) {
    
      data.addElement ( entry ) ;
      notify() ;
    } // push ( entry )
    
    
    /**
     * Get an entry from the the front of the queue.
     * If the queue is empty, wait until it is not empty.
     */
    synchronized public Object pull() {
    
      while ( data.size() == 0 ) {
      
         try {
            wait() ;
         } catch ( InterruptedException e ) {}
      }
      
      Object entry = data.elementAt(0) ;
      data.removeElementAt(0) ;
      
      return entry ;
   } // pull()
   
   
   /**
    * Return the number of entries in this queue
    */
   public int size() {
   
      return data.size() ;
   } //size()
   
} // class Queue         
