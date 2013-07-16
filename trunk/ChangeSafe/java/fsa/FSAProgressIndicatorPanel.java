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
 ** File Name:             FSAProgressIndicatorPanel.java
 ** Author:                Ade Lucas
 **                        
 ****************************************************************************/



import java.awt.*;
import java.awt.event.*;
import java.lang.Runnable;
import java.awt.Panel;

 /** 
  * The FSAProgressIndicatorPanel creates and displays a bar that displays a
  * a percentage. It can be ued to indicate the percentage complete of a
  * lengthy task.
  * <b>Notes</b><br>
  * <li>Avoid calling the progress indicator display methods from the AWT thread -
  * this will prevent repaint problems.
  * <li>If the thread calling the progress indicator is processor intensive, use 
  * the Queue'd display method to minimize the impact of the indicator on
  * calling thread performance.
  * @author Ade Lucas
  ***************************************************************************/
class FSAProgressIndicatorPanel extends Panel
                                implements Runnable
{
   protected   Panel     center  ;
   protected   Panel     bottom  ;
   protected   Label     message ;
   protected   Label     pText   ;
   protected   Color     barColor = Color.blue ;
   private     Queue     queue   ;
   private     Dimension d       ;
   private     Graphics  g       ;
   private     int       sixteenth   ;
   private     int       third   ;
   private     boolean bDone = false ;
   
   
   /**
    * Constructs a progress bar panel with a centered progress % and zero indentation
    */            
   public FSAProgressIndicatorPanel () {
   
      this ( null ) ;      
   }
  
  
   /**
    * Constructs a progress bar panel with a centered progress % and zero indentation
    * and supplied message string displayed
    * @param s message string
    */            
   public FSAProgressIndicatorPanel ( String s )
   {
      setLayout ( new GridLayout(3,1) ) ;

      Panel north = new Panel()  ;
      message     = new Label(s) ;
      
      north.add  ( message ) ;
      north.setBackground ( Color.white ) ;
      add ( north ) ;
      
      center = new Panel() ;
      pText       = new Label("0%") ;
      center.setBackground ( Color.white ) ;
      center.add ( pText ) ;
      add ( center ) ;

      bottom = new Panel() ;
      bottom.setBackground ( Color.white ) ;
      
      add ( bottom ) ;
      
      queue = new Queue() ;
   }
   
   
   protected void display ( ProgressIndication pi ) {
         
      int                  percent  =  pi.getPercent() ;
      String               dispStr  =  pi.getMessage() ;
      
      if ( dispStr != null )
         message.setText ( dispStr ) ;

      g     = bottom.getGraphics() ;

		g.setColor ( barColor ) ;

      if ( !bDone ) {
         
         bDone = true ;
      
         // Draw border for indicator bar
         g.draw3DRect ( sixteenth-1, third-1, d.width-sixteenth-sixteenth+1, (third*2), true ) ;
      }

      int width =  (sixteenth * 14 * percent) / 100 ;

      int j=0;
      for ( int i =0; i<width/10; i++ ) {
      
         g.fill3DRect ( sixteenth+j, third, 10, third*2, true ) ;
         j += 10 ;
      }
   }
   
   /**
    * Called by the system when indicator instance created in a separate thread instance.
    * Do not call directly.
    */
   public   void run() {
   
      d     = bottom.getSize()     ;
      sixteenth = d.width / 16         ;
      third = d.height / 3         ;
      
      for (;;) {
      
         ProgressIndication   pi       = (ProgressIndication)queue.pull() ;
         display ( pi ) ;
      }
   }


   /**
    *  The <code>displayText</code> method should print the indicated text string
    *  in such a way as to overwrite any previous display from the last call to
    *  <code>displayText</code>, however newline-separated or other behavior may
    *  be used by other simple minded implementations of this class.  The idea is
    *  for applets to update a status area.
    **/
   public void displayText ( String s ) {
  
      message.setText ( s ) ;
   } // displayText
   
   /**
    * Set the bar color
    * @param color new color
    */
   public void setBarColor ( Color color ) {
   
      barColor = color ;
   }


   /**
    * Set the percentage complete for the process being tracked.
    * Update bar only
    * @param p percentage complete as an integer
    */ 
   public void displayPercentage ( int percent ) {

      d     = bottom.getSize()     ;
      sixteenth = d.width / 16         ;
      third = d.height / 3         ;
      g     = bottom.getGraphics() ;
      
		g.setColor ( barColor ) ;
      
      if ( !bDone ) {
         
         bDone = true ;
      
         // Draw border for indicator bar
         g.draw3DRect ( sixteenth-1, third-1, d.width-sixteenth-sixteenth+1, (third*2), true ) ;
      }

      int width =  (sixteenth * 14 * percent) / 100 ;

      int j=0;
      for ( int i =0; i<width/10; i++ ) {
      
         g.fill3DRect ( sixteenth+j, third, 10, third*2, true ) ;
         j += 10 ;
      }
      pText.setText ( ""+percent + "%" ) ;
   }
   
   /**
    * Set the percentage complete for the process being tracked.
    * Update bar and percentage text.
    * @param p percentage complete as an integer
    */ 
   public void displayPercentageText ( int percent ) {

      displayPercentage ( percent ) ;
      pText.setText ( ""+percent + "%" ) ;
   }


   /**
    * Submit display update request into Queue and return immediately.
    * The progress indicator will pick up this request as
    * soon as any pending display update have been completed.
    * @param p percentage complete as an integer
    * @see Queue
    * Example:<br>
	 * readProgress = new FSAProgressIndicatorPanel ( "Reading file ... " ) ;<br>
	 * readProgress.show() ;<br>
    *   
    * rpt          = new Thread ( readProgress, "Read Progress Thread" ) ;<br>
    * rpt.start() ;<br>
    * ...<br>
    * readProgress.qDisplayPercentage ( "Final Stage...", (int)(100 * all / max) ) ;
    **/
   public void qDisplayPercentage ( String message, int percent ) {
   
      ProgressIndication   pi = new ProgressIndication ( message, percent  ) ;
      queue.push ( pi ) ;         
   } // displayPercentage

}
