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
 ** File Name:             DirChooserThread.java
 ** Author:                Ade Lucas
 ** 
 ** Module Description: 
 **
 **                        Class to allow selection of directories (not files) from the browser.
 **
 ****************************************************************************/
 
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.text.*;
import java.io.*;
import netscape.javascript.*;


public class DirChooserThread
  implements Runnable,
             KeyListener,
             ItemSelectable,
             ActionListener,
             LayoutManager
{
private     ActionListener actionListener ;
private     ItemListener   itemListener   ;

protected   Label          statusBar  = new Label ( " Starting.." ) ;
protected   boolean        isWindows  = false       ;
protected   File           parentFile = null        ;
protected   File []        currentFiles             ;
protected   DirChooserApplet  _applet ;                        // applet which spawned us

// Properties
protected   Color          bgColor              = Color.white ;  
protected   Color          fgColor              = Color.black ;
protected   Color          messagesColor        = Color.black ;
protected   Color          stColor              = Color.white ;
protected   String         sInitPath            = null        ;

protected   String         textForBanner        = new String ( "Browse for Folder" ) ;

protected   String         _frame               = null ;       // frame in which to render urls

protected   Exception      _failureException    = null ;       // set to the exception which caused failure,
protected   boolean        _avoid_update        = false ;


static public String SELECT = "Open";
static public String CANCEL = "Cancel";

Dialog  dialog;
String  title;
Frame   frame;
boolean frameNotProvided;

private Hashtable filters = new Hashtable();

ImageButton upButton        = new ImageButton(null, "folderUp.gif");
Label       pathFieldLbl    = new Label("Up a level", Label.LEFT);
TextField   pathField       = new TextField();
MultiColumnList listPanel   = new MultiColumnList();
Label       filterChoiceLbl = new Label("Drive: ", Label.LEFT);
String      folderValue     = null ;
Choice      filterChoice    = new Choice();
String      _currPath        = "" ;
boolean     _beepEnabled     = false ;
Button      _fake_button ;


//---------- Runnable Implementation ---------------------
/**
 * This prevents the caller from blocking on show(), which
 * if this class is used on an awt event thread would
 * cause a deadlock.
 */
public void run() {
    _avoid_update = false ;
    getRoots() ;
    setRoots() ;
    
    if ( sInitPath != null )
      setDirectory ( sInitPath ) ;
      
    else setDirectory ( File.separator ) ;
}

//---------- KeyListener Implementation ------------------
/** Depending on the source of the keystroke, interpret
  * it appropriately (escape always cancels; return or
  * space cancel if originated from cancel button, otherwise
  * select.)
  */
public void    keyTyped(KeyEvent evt) { }
public void keyReleased(KeyEvent evt) { }
public void  keyPressed(KeyEvent evt) {
    if (evt.getKeyCode() == KeyEvent.VK_ESCAPE) {
        fireActionEvent(CANCEL);
    } else if ((evt.getKeyCode() == KeyEvent.VK_ENTER )
            || (evt.getKeyCode() == KeyEvent.VK_ACCEPT)) {
        fireActionEvent(SELECT);
    } 
}

//---------- ActionListener Implementation ---------------
/** events which indicate that the dialog is finished and
  * should close filter through here.
  */
public void actionPerformed(ActionEvent evt) {
    
    if (frameNotProvided) frame.dispose();
    if (actionListener != null)
        actionListener.actionPerformed(evt);
        
   if (evt.getSource() == _fake_button) {
       _avoid_update = true;
       String path   = (String)evt.getActionCommand () ;
       
       path          = path == null ? File.separator : path ;
       path          = path.length() == 0 ? File.separator : path ;
       //System.out.println ( "actionPerformed: got: " +  path ) ;
       
       if ( path.length() > 1 ) {

          _beepEnabled = true ;
         setDirectory ( path ) ;
       }
                
       else setDirectory ( getFilenameFilter() ) ;
       
       _avoid_update = false;
   }
}

public void addActionListener(ActionListener listener)
    {   actionListener = AWTEventMulticaster.add(actionListener, listener); }

public void removeActionListener(ActionListener listener)
    {   actionListener = AWTEventMulticaster.remove(actionListener, listener); }

/** fireActionEvent is used to indicate that the dialog is
  * complete, and should close itself.  The actionPerformed()
  * implementation will clean up, and forward the notification
  * on to any interested listeners.  The actionCommand passed
  * will indicate whether the dialog was accepted or cancelled.
  */
public void fireActionEvent(String cmd) {
    actionPerformed (new ActionEvent
        (this, ActionEvent.ACTION_PERFORMED, cmd));
}

//---------- ItemListener Implementation ------------------------------
/**
  * Returns selected objects as an array of File.
  */
public Object[] getSelectedObjects() {
    Object[] items = listPanel.getSelectedObjects();
    if (items==null) return null;
    File[] files = new File[items.length];

    String dir = getDirectory();
    for (int i=0; items!=null && i<items.length; i++)
        files[i] = new File(dir, (String)items[i]);

    return files;
}
public void addItemListener(ItemListener listener)
    { itemListener = AWTEventMulticaster.add(itemListener, listener); }

public void removeItemListener(ItemListener listener)
    { itemListener = AWTEventMulticaster.remove(itemListener, listener); }


//---------- LayoutManager Implementation ---------------
public void addLayoutComponent(String name, Component comp) {}
public void removeLayoutComponent(Component comp) {}

public Dimension   minimumLayoutSize(Container parent) {return preferredLayoutSize(parent);}
public Dimension preferredLayoutSize(Container parent) {
    Dimension dlgSize = getPreferredSize();
    Insets insets = parent.getInsets();
    Dimension clientSize = new Dimension(
        dlgSize.width - insets.left - insets.right,
        dlgSize.height - insets.top - insets.bottom);
    return clientSize;
}

public void layoutContainer(Container parent) {
    int TOP=12, BOTTOM=12, LEFT=16, RIGHT=16, GAPX=8, GAPY=6;
    Insets insets = parent.getInsets();
    Dimension parentSize = parent.getSize();
    Dimension clientSize = new Dimension(
        parentSize.width - insets.left - insets.right,
        parentSize.height - insets.top - insets.bottom);
    Dimension upButtonDim = upButton.getPreferredSize();
    Dimension pathFieldLblDim = pathFieldLbl.getPreferredSize();
    Dimension pathFieldDim = pathField.getPreferredSize();
    Dimension filterChoiceLblDim = filterChoiceLbl.getPreferredSize();
    Dimension filterChoiceDim = filterChoice.getPreferredSize();
    Dimension statusBarDim = statusBar.getPreferredSize() ;

    int x = insets.left + LEFT, y = insets.top + TOP;
    upButton.setBounds(x, y, upButtonDim.width, upButtonDim.height);

    x += upButtonDim.width + GAPX;
    pathFieldLbl.setBounds(x, y, pathFieldLblDim.width, pathFieldLblDim.height);

    x += pathFieldLblDim.width;
    pathField.setBounds(x, y, clientSize.width-12-x+insets.left+6, pathFieldDim.height);

    y += upButtonDim.height + GAPY;
    int listPanelH = clientSize.height-TOP-BOTTOM-4*GAPY
                    -upButtonDim.height-filterChoiceDim.height-statusBarDim.height;
    listPanel.setBounds(insets.left + 6, y, clientSize.width-12, listPanelH);
    y += listPanelH + 2*GAPY;

    int  labelW = filterChoiceLblDim.width ;
    int buttonW = 0 ;
    int  fieldW = clientSize.width-12-labelW ;

    x = insets.left + 6 ;

    int   itemH = Math.max(filterChoiceLblDim.height, filterChoiceDim.height);
    filterChoiceLbl.setBounds(x, y,  labelW, itemH); x += labelW;
    filterChoice   .setBounds(x, y,  fieldW, itemH); x += fieldW + 2*GAPX;

    y += filterChoiceDim.height + 2*GAPY;
    x = insets.left + 6 ;
    statusBar      .setBounds(x, y,  clientSize.width-12, itemH-2);
}


protected boolean hasDirChildren ( File theFile ) {

   String[] allFileStrings = theFile.list() ;
   String sPath    = (theFile.getAbsolutePath()).endsWith ( ""+File.separatorChar ) ? theFile.getAbsolutePath() : theFile.getAbsolutePath() + File.separatorChar ;
   
   for ( int i = 0 ; i < allFileStrings.length ; i++ ) {
   
      File aFile = new File ( sPath + allFileStrings[i] ) ;
      
      if ( aFile.isDirectory() ) return true ;
   }
   
   return false ;
}


/* getPreferredSize defines the size of the overall applet canvas.
   Adjusting this will cause changes mostly to the folder display area (the list panel),
   since everything else has mostly fixed amounts of required space. */

public Dimension getPreferredSize()
    { return new Dimension(400,590); }

void createDialog() {
    
    _applet.setLayout(this);

    _applet.add(upButton);
    _applet.add(pathFieldLbl);
    _applet.add(pathField); pathField.setEditable(false);
    _applet.add(listPanel);
    _applet.add(filterChoiceLbl);
    _applet.add(filterChoice);
    _applet.add ( statusBar ) ;
    
    
    Color lt = new Color ( 0xCCCCCC ) ;
    //Color lt = Colors.atIntensity(bgColor, 0.9375);
    _applet.setBackground(bgColor);
    _applet.setForeground(fgColor);
    
    
    listPanel.setBackground(lt);
    listPanel.setForeground(fgColor);
    
    pathField.setBackground(bgColor);
    pathField.setForeground(fgColor);
    
    filterChoice.setBackground(lt);
    filterChoice.setForeground(fgColor);
    
    statusBar.setBackground(bgColor);
    statusBar.setForeground(bgColor);
    
    pathFieldLbl.setBackground(bgColor);
    pathFieldLbl.setForeground(fgColor);
    filterChoiceLbl.setBackground(bgColor);
    filterChoiceLbl.setForeground(fgColor);
    
    initListeners();

}
void initListeners() {

    upButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e)
        {   
        
            try {
                              
               //            
               // TRY: Check for Netscape style security
               try {
               
                   //                 
                   // TRY: check for ClassNotFoundException, indicating IE compile
                   try {         
                     
                     Class.forName ( "netscape.security.PrivilegeManager" ) ;
                     netscape.security.PrivilegeManager.enablePrivilege ( "UniversalFileAccess"   ) ;
                     netscape.security.PrivilegeManager.enablePrivilege ( "UniversalPropertyRead" ) ;
                   }             
                   
                   //
                   // TRY: check for ClassNotFoundException, indicating IE compile
                   
                   //
                   // Probable compile, NS security classes not found
                   catch ( ClassNotFoundException ef ) { 
                     // Fall though.
                   }
               }

               //
               // NS security call failed, permission denied         
               catch ( netscape.security.ForbiddenTargetException ez ) { 
                     
                  statusBar.setText( " Permission denied: " + ez.toString() ) ;
                  return ;
               }
               
               //
               // *Looks* like we have permission: Try to run the directory chooser logic
               String path = getDirectory();
               File absolute = new File(path);
               String par = absolute.getParent();
               File parent = (par==null)? absolute: new File(par);
               setDirectory(parent.getPath());
            }
      
            catch ( Exception  ex) {
               
               statusBar.setText ( " Exception: " + ex.toString() ) ;
            }
        }
    });

    pathField.addTextListener(new TextListener() {
        public void textValueChanged(TextEvent e)
        {   
            
            try {
                              
               //            
               // TRY: Check for Netscape style security
               try {
               
                   //                 
                   // TRY: check for ClassNotFoundException, indicating IE compile
                   try {         
                     
                     Class.forName ( "netscape.security.PrivilegeManager" ) ;
                     netscape.security.PrivilegeManager.enablePrivilege ( "UniversalFileAccess"   ) ;
                     netscape.security.PrivilegeManager.enablePrivilege ( "UniversalPropertyRead" ) ;
                   }             
                   
                   //
                   // TRY: check for ClassNotFoundException, indicating IE compile
                   
                   //
                   // Probable compile, NS security classes not found
                   catch ( ClassNotFoundException eg ) { 
                     // Fall though.
                   }
               }

               //
               // NS security call failed, permission denied         
               catch ( netscape.security.ForbiddenTargetException eh ) { 
                     
                  statusBar.setText( " Permission denied: " + eh.toString() ) ;
                  return ;
               }
               
               //
               // *Looks* like we have permission: Try to run the directory chooser logic
               String path = getDirectory();
               
               String testPath = path.endsWith ( File.separator ) ? path.substring ( 0, path.length()-1 ) : path ;
               if ( testPath.compareTo ( _currPath ) == 0 ) return ;
               
               _currPath   = testPath ;
                  
               File dir = new File(fixupDir(path));
               String[] files = dir.list();

               if ( files == null ) return ;

               statusBar.setText ( " Fetching folders list for: " + path ) ;
               
               sort(dir, files);

               listPanel.clear();

               for (int i=0; i<files.length; i++) {
                   
                   File file = new File(dir, files[i]);
                   
                   if ( file.isDirectory() ) {
                       
                       if ( hasDirChildren ( file ) )
                           listPanel.addItem(files[i], "folderClosed.gif");
                           
                       else
                           listPanel.addItem(files[i], "folderOpen.gif");
                   }
               }
               listPanel.validate();
               listPanel.repaint();
               setFile("");
            }
      
            catch ( Exception  ex ) {
               
               statusBar.setText ( " Exception: " + ex.toString() ) ;
            }
            
            statusBar.setText ( " Ready." ) ;
            
        }
    });

    listPanel.addItemListener(new ItemListener() {
        public void itemStateChanged(ItemEvent e)
        {   Object[] selected = e.getItemSelectable().getSelectedObjects();

            //System.out.println ( "listPanel.itemStateChanged:" ) ;
            
            if (selected == null || selected.length == 0)
                setFile("");
            else if (selected.length == 1)
                setFile((String) selected[0]);
            else {
                String names = quote((String) selected[0]);
                for (int i=1; i<selected.length; i++)
                    names += " " + quote((String) selected[i]);
                setFile(names);
            }
            // also forward the event to any listeners
            if (itemListener != null)
                itemListener.itemStateChanged( new ItemEvent(
                    DirChooserThread.this, ItemEvent.ITEM_STATE_CHANGED,
                    e.getItem(), e.getStateChange()));
        }
    });
    // listPanel will get actions only immediately following
    // a listPanel.itemStateChanged; therefore the text in the
    // file field will be the name of an entry in the listPanel.
    // User mods to the file field will be routed through the
    // fileField actions.
    listPanel.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e)
        {   
        
            // TRY: ask for privs and run dir chooser
            try {
                              
               //            
               // TRY: Check for Netscape style security
               try {
               
                   //                 
                   // TRY: check for ClassNotFoundException, indicating IE compile
                   try {         
                     
                     Class.forName ( "netscape.security.PrivilegeManager" ) ;
                     netscape.security.PrivilegeManager.enablePrivilege ( "UniversalFileAccess"   ) ;
                     netscape.security.PrivilegeManager.enablePrivilege ( "UniversalPropertyRead" ) ;
                   }             
                   
                   //
                   // TRY: check for ClassNotFoundException, indicating IE compile
                   
                   //
                   // Probable compile, NS security classes not found
                   catch ( ClassNotFoundException ef ) { 
                     // Fall though.
                   }
               }

               //
               // NS security call failed, permission denied         
               catch ( netscape.security.ForbiddenTargetException eg ) { 
                     
                  statusBar.setText( " Permission denied: " + eg.toString() ) ;
                  return ;
               }
               
               //
               // *Looks* like we have permission: Try to run the directory chooser logic
               String name = getFile();
               //System.out.println ( "listPanel.actionPerformed:" ) ;
               File sel = null;
               if (name != null && name.length() > 0) {
            
                   sel = new File(getDirectory(), name);
                   //System.out.println ( "listPanel.actionPerformed: sel: " + sel + " name: " + name ) ;
               }
            
               if (sel != null && sel.isDirectory()) {
            
                    setDirectory(sel.getAbsolutePath());
               }
               else {
                  //System.out.println ( "listPanel.actionPerformed: NOT firing action event (SELECT)" ) ;
                  //fireActionEvent(SELECT);
               }
            
            }
            
            catch ( Exception  eh ) {
               
               eh.printStackTrace() ;
               statusBar.setText ( " Exception: " + eh.toString() ) ;
            }
      
        }
    });

    filterChoice.addItemListener(new ItemListener() {
        public void itemStateChanged(ItemEvent e)
        {   
        
        //System.out.println ( "filterChoice.itemStateChanged:" ) ;
        
        setDirectory ( getFilenameFilter() ) ; } });
}

private String quote(String s) {
    StringBuffer buf = new StringBuffer(s.length() + 2);
    buf.append('"');
    buf.append(s);
    buf.append('"');
    return buf.toString();
}

private void sort(File dir, String[] list) {
    if ( list == null ) return ;
    Collator collator = Collator.getInstance();
    CollationKey[] keys = new CollationKey[list.length];
    for (int i=0; i<list.length; i++)
        keys[i] = collator.getCollationKey(list[i]);

    ArraySorter.sort(keys, new Comparator() {
        public boolean less(Object lhs, Object rhs)
        { return (((CollationKey)lhs).compareTo((CollationKey)rhs) < 0); }
    });
    int marker = 0;
    for (int i=0; i<keys.length; i++) {         // pull out the dirs first...
        String filename = keys[i].getSourceString();
        File file = new File(dir, filename);
        if (file.isDirectory()) {
            list[marker++] = filename;
            keys[i] = null;
    }   }
    for (int i=0; i<keys.length; i++) {        // what's left are the non-dir files.
        if (keys[i] != null)
            list[marker++] = keys[i].getSourceString();
    }
}



public static final int LOAD = FileDialog.LOAD;
public static final int SAVE = FileDialog.SAVE;
private int mode = LOAD;

protected void setPath ( String path ) throws JSException {

  if ( _avoid_update ) return ;
  
  String args[] = new String [1];

  args[0] = (! path.endsWith (File.separator) && path.length() > 0)
   ? path + File.separator
   : path;
  JSObject.getWindow(_applet).call ("choose_directory", args);
}

protected void init() {

      getAppletParameters  () ;
      _applet.setBackground        ( bgColor ) ;
      _applet.setForeground        ( fgColor ) ;

      //new EventQueue().postEvent (new ActionEvent (_fake_button, ActionEvent.ACTION_FIRST, sInitPath));
   }

   /**
    *    Parse a list of decimal integers delimited by the
    *    given spearator,
    **/
   private int[] parseInt(String s, String separator )
   {
      
      StringTokenizer st = new StringTokenizer ( s, separator ) ;
      
      int[] result       = new int[st.countTokens()] ;
      
      for ( int i = 0 ; i < result.length ; i++ )
         result[i] = Integer.parseInt ( st.nextToken() ) ;
      
      return result ;
   } // parseInt


   /**
    * Get the parameters from the HTML that invoked this applet.
    **/
   private void getAppletParameters() {
      
      //System.out.println ( "getAppletParameters: entered: _applet: " + _applet ) ;
      
      try {
         
         int[] ints ;
         String sParam ;
         
         sParam              = _applet.getParameter ( "background-color" )                              ;
         //System.out.println ( "sParam background-color: " + sParam ) ;

         if ( sParam        != null ) {
         
            ints             = parseInt     ( sParam, " "                                             ) ;
            bgColor          = new Color    ( ints[0], ints[1], ints[2]                               ) ;
         }
             
         sParam              = _applet.getParameter ( "foreground-color" )                              ;
         //System.out.println ( "sParam foreground-color: " + sParam ) ;
         
         if ( sParam        != null ) {
         
            ints                = parseInt     ( sParam, " "                                          ) ;
            fgColor             = new Color    ( ints[0], ints[1], ints[2]                            ) ;
         }
         
         sParam              = _applet.getParameter ( "status-color" )                                  ;
         //System.out.println ( "sParam status-color: " + sParam ) ;
         
         if ( sParam        != null ) {
         
            ints                = parseInt     ( sParam, " "                                          ) ;
            stColor             = new Color    ( ints[0], ints[1], ints[2]                            ) ;
         }
         
         sParam              = _applet.getParameter ( "messages-color" )                                ;
         //System.out.println ( "sParam messages-color: " + sParam ) ;
         
         if ( sParam        != null ) {
         
            ints                = parseInt     ( sParam, " "                                          ) ;
            messagesColor       = new Color    ( ints[0], ints[1], ints[2]                            ) ;
         }
         
         sParam              = _applet.getParameter ( "text-for-banner" )                         ;
         //System.out.println ( "text-for-banner: " + sParam ) ;
         
         if ( sParam == null )
            sParam = "" ;
            
         textForBanner       = new String   ( sParam                                            ) ;
         
         sParam              = _applet.getParameter ( "init-path"       )                         ;
         //System.out.println ( "init-path: " + sParam ) ;
         
         if ( sParam == null )
            sParam = "" ;
            
         sInitPath           = new String   ( sParam                                            ) ;
         _frame = _applet.getParameter("FRAME") ; // frame in which to render urls
         if (_frame == null)
            _frame = "_self" ;


      }
      
      catch ( Exception e ) {
      
         e.printStackTrace() ;
      }
   } // getParameters
   
    //---------- Constructors ------------------------------
    public DirChooserThread(DirChooserApplet applet, Frame parent, Button fake_button)
        { this(applet, parent, null, fake_button ) ;
        }

    public DirChooserThread(DirChooserApplet applet, Frame parent, String title, Button fake_button)
        { this(applet, parent, title, LOAD, fake_button );
        }

    public DirChooserThread(DirChooserApplet applet, Frame parent, String title, int mode, Button fake_button)
        {   
            _applet      = applet      ;
            _fake_button = fake_button ;
            _fake_button.addActionListener ( this ) ;
            
            init() ;
            this.frame = parent;
            
            this.title = title == null ? textForBanner : title ;
            this.mode  = mode;
            createDialog();
        }

    //---------- Public Methods ------------------------------
    public void setMode(int mode)
        { this.mode = mode; }

    public int getMode()
        { return mode; }


    private String fixupDir(String path)
        {   if ((File.separatorChar == '\\')    // fixup Windows bug
             && (path.length() == 3)            // 'C:\'  fails to list;
             && (path.charAt(1) == ':'))        // have to use 'C:\.'
                path += ".";
            return path;
        }
        
    public void setDirectory ( File newDir )
        {

            // TRY: ask for privs and run dir chooser
            try {
                              
               //            
               // TRY: Check for Netscape style security
               try {
               
                   //                 
                   // TRY: check for ClassNotFoundException, indicating IE compile
                   try {         
                     
                     Class.forName ( "netscape.security.PrivilegeManager" ) ;
                     netscape.security.PrivilegeManager.enablePrivilege ( "UniversalFileAccess"   ) ;
                     netscape.security.PrivilegeManager.enablePrivilege ( "UniversalPropertyRead" ) ;
                   }             
                   
                   //
                   // TRY: check for ClassNotFoundException, indicating IE compile
                   
                   //
                   // Probable compile, NS security classes not found
                   catch ( ClassNotFoundException e ) { 
                     // Fall though.
                   }
               }

               //
               // NS security call failed, permission denied         
               catch ( netscape.security.ForbiddenTargetException e ) { 
                     
                  statusBar.setText( " Permission denied: " + e.toString() ) ;
                  return ;
               }
               
               //
               // *Looks* like we have permission: Try to run the directory chooser logic
               String path ;
                  
               if ( newDir != null ) {
            
                  path = newDir.getAbsolutePath() ;

                  String testPath = path.endsWith ( File.separator ) ? path.substring ( 0, path.length()-1 ) : path ;
                  if ( testPath.compareTo ( _currPath ) == 0 ) return ;

                  if ( newDir.exists() ) {
                  
                     pathField.setText( path ) ;
                     setPath ( path ) ;
                   }
                     
                   else { return ; }
               }
            
               else {
            
                  path   = new File( new File( File.separator ).getAbsolutePath() ).getParent() ;
                  newDir = new File ( path ) ;
                  
                  if ( newDir.exists() ) {
                  
                     pathField.setText ( path ) ;
                     setPath ( path ) ;
                  }
                  else { Toolkit.getDefaultToolkit().beep() ; Toolkit.getDefaultToolkit().beep() ; return ; }
               }
            
               if ( isWindows ) {
            
                  if ( path.length() < 3 ) return ;
                  setPath ( path ) ;
                  //System.out.println ( "setDirectory.File: path: " + path ) ;
                  String sPart  = ( path.substring ( 0, 3 ) ).toUpperCase() ;
                  setFilenameFilter ( sPart ) ;
               }
            
                  if ( !path.endsWith ( ""+File.separatorChar ) )
                     if ( _beepEnabled ) {
                        if ( path.compareTo ( _currPath ) != 0 ) 
                           Toolkit.getDefaultToolkit().beep() ;
                        _beepEnabled = false ;
                     }
                     
                  //System.out.println("_currPath: " + _currPath ) ;
               }

               catch ( Exception  ex) {
                  
                  //if ( ex instanceof java.io.IOException ) return ;
                  //statusBar.setText ( " Exception(fil): " + ex.toString() ) ;
               }

            }

    public void setDirectory ( String path )
        {   
        
            // TRY: ask for privs and run dir chooser
            try {
                              
               //            
               // TRY: Check for Netscape style security
               try {
               
                   //                 
                   // TRY: check for ClassNotFoundException, indicating IE compile
                   try {         
                     
                     Class.forName ( "netscape.security.PrivilegeManager" ) ;
                     netscape.security.PrivilegeManager.enablePrivilege ( "UniversalFileAccess"   ) ;
                     netscape.security.PrivilegeManager.enablePrivilege ( "UniversalPropertyRead" ) ;
                   }             
                   
                   //
                   // TRY: check for ClassNotFoundException, indicating IE compile
                   
                   //
                   // Probable compile, NS security classes not found
                   catch ( ClassNotFoundException e ) { 
                     // Fall though.
                   }
               }

               //
               // NS security call failed, permission denied         
               catch ( netscape.security.ForbiddenTargetException e ) { 
                     
                  statusBar.setText( " Permission denied: " + e.toString() ) ;
                  return ;
               }
               
               //
               // *Looks* like we have permission: Try to run the directory chooser logic
               if (path == null || path.length() == 0)
                   path = new File(new File(File.separator).getAbsolutePath()).getParent();
                   
               String testPath = path.endsWith ( File.separator ) ? path.substring ( 0, path.length()-1 ) : path ;
               if ( testPath.compareTo ( _currPath ) == 0 ) return ;
                   
               File newDir = new File(path);

               if (newDir.exists()) {
               
                   pathField.setText( path ) ;
                   setPath ( path ) ;
               }
               else { return ; }

               if ( isWindows ) {
                  //System.out.println ( "setDirectory.String: path: " + path ) ;
                  if ( path.length() < 3 ) return ;
                  setPath ( path ) ;
                  String sPart  = ( path.substring ( 0, 3 ) ).toUpperCase() ;
                  setFilenameFilter ( sPart ) ;
               }
            
               if ( !path.endsWith ( ""+File.separatorChar ) )
                  if ( _beepEnabled ) {
                     if ( path.compareTo ( _currPath ) != 0 ) 
                        Toolkit.getDefaultToolkit().beep() ;
                     _beepEnabled = false ;
                  }

              //System.out.println("_currPath: " + _currPath ) ;
            }
      
            catch ( Exception  ex) {
               //if ( ex instanceof java.io.IOException ) return ;
               //statusBar.setText ( " Exception(str): " + ex.toString() ) ;
            }
        }

    /** Return the Selector's current directory.
      * If current is null, return default cwd.
      */
   public String getDirectory() {
         
      // TRY: ask for privs and run dir chooser
      try {
                        
         //            
         // TRY: Check for Netscape style security
         try {
         
             //                 
             // TRY: check for ClassNotFoundException, indicating IE compile
             try {         
               
               Class.forName ( "netscape.security.PrivilegeManager" ) ;
               netscape.security.PrivilegeManager.enablePrivilege ( "UniversalFileAccess"   ) ;
               netscape.security.PrivilegeManager.enablePrivilege ( "UniversalPropertyRead" ) ;
             }             
             
             //
             // TRY: check for ClassNotFoundException, indicating IE compile
             
             //
             // Probable compile, NS security classes not found
             catch ( ClassNotFoundException e ) { 
               // Fall though.
             }
         }

         //
         // NS security call failed, permission denied         
         catch ( netscape.security.ForbiddenTargetException e ) { 
               
            statusBar.setText( " Permission denied: " + e.toString() ) ;
            return null ;
         }
         
         //
         // *Looks* like we have permission: Try to run the directory chooser logic
            statusBar.setText ( " Fetching folders list for: " + pathField.getText() ) ;
            
            String path = pathField.getText();
            if (path == null || path.length() == 0)
                path = new File(new File( File.separator ).getAbsolutePath()).getParent();
            File dir = new File(path);
            File canon = null;
            try                   { canon = new File(dir.getCanonicalPath()); }
            catch (IOException e) { canon = new File(dir.getAbsolutePath ()); }
            path = canon.getPath();
            
            statusBar.setText ( " Ready." ) ;
            return path;
      }

      catch ( Exception  e) {
         
         e.printStackTrace() ;
         statusBar.setText ( " Exception: " + e.toString() ) ;
      }
      
      return null ;
   }


    public void setFile(String file)
        { 
          folderValue = file ; }

    public String getFile()
        { 
         String file = folderValue ; return (file==null)? "": file;
         }


    public int getFilterCount()
        { return filterChoice.getItemCount();  }

    public String getFilterDescription()
        { return filterChoice.getSelectedItem(); }

    public String getFilterDescription(int ix)
        { return filterChoice.getItem(ix);  }

    public File getFilenameFilter()
        { return getFilenameFilter(filterChoice.getSelectedItem());  }

    public File getFilenameFilter(String desc)
        {   if (desc == null) return null;
            return (File) filters.get(desc);
        }
    public void setFilenameFilter(String desc)
        {   
            filterChoice.select(desc);
            //...trigger event?
        }
    public void addFilenameFilter(String desc, File flt)
        {   int ix = filterChoice.getSelectedIndex();
            filterChoice.add(desc);
            filters.put(desc, flt);
            if (ix == -1) ix = 0;
            if (filterChoice.getSelectedIndex() != ix)
                filterChoice.select(ix);
        }



            
   /**
    * Returns all root partitians on this system. On Windows, this
    * will be the A: through Z: drives.
    * @return File[] array of File handle partitions found.
    */
   protected File[] getWindowsRoots()
   {
       
       Vector rootsVector = new Vector();

       //
       // Create the A: drive whether it is mounted or not
       File floppy = new File ( "A" + ":" + "\\" ) ;
       rootsVector.addElement ( floppy ) ;

       //
       // Run through all possible mount points and check
       // for their existance.
       for ( char c = 'C'; c <= 'Z'; c++ ) {
          
          char device[] = { c, ':', '\\' } ;
          String deviceName = new String ( device ) ;
          File deviceFile = new File ( deviceName ) ;
       
          try {
          
               if ( deviceFile != null && deviceFile.exists() )
                  rootsVector.addElement ( deviceFile ) ;
          } 
          
          catch ( Exception e ) {
          
               statusBar.setText ( " " + e.toString() ) ;
               File [] nothing = new File[0] ;
               return nothing ;
          }
       }
       
       File[] roots = new File [ rootsVector.size() ] ;
       rootsVector.copyInto ( roots ) ;
       return roots ;
   }
    
   /**
    * The unix file system has a root of "/".
    **/
   private File[] getUnixRoots()
   {
     File[] roots = new File[1];
     roots[0] = new File("/");
     return roots;
   }              // getUnixRoots

   /**
    * Returns the root partitions of the system.
    * For a system of unknown type, returns an empty array.
    **/
   private void getRoots()
   {


         // TRY: ask for privs and run dir chooser
         try {
                           
            //            
            // TRY: Check for Netscape style security
            try {
            
                //                 
                // TRY: check for ClassNotFoundException, indicating IE compile
                try {         
                  
                  Class.forName ( "netscape.security.PrivilegeManager" ) ;
                  netscape.security.PrivilegeManager.enablePrivilege ( "UniversalFileAccess"   ) ;
                  netscape.security.PrivilegeManager.enablePrivilege ( "UniversalPropertyRead" ) ;
                }             
                
                //
                // TRY: check for ClassNotFoundException, indicating IE compile
                
                //
                // Probable compile, NS security classes not found
                catch ( ClassNotFoundException e ) { 
                  // Fall though.
                }
            }

            //
            // NS security call failed, permission denied         
            catch ( netscape.security.ForbiddenTargetException e ) { 
                  
               statusBar.setText( " Permission denied: " + e.toString() ) ;
               return ;
            }
            
            //
            // *Looks* like we have permission: Try to run the directory chooser logic
            statusBar.setText ( " Fetching root directories..." ) ;
       
            parentFile = null ;
       
            if ( File.separatorChar == '\\' ) {
               
               isWindows = true ;
               currentFiles = getWindowsRoots() ;
               statusBar.setText ( " Ready." ) ;
            }
             
            else
               if ( File.separatorChar == '/' ) {
                  
                  currentFiles = getUnixRoots() ;
                  statusBar.setText( " Ready." ) ;
               }
               
               else {

                  statusBar.setText( " Unknown operating system" ) ;
                  File [] roots = new File[0] ;
                  currentFiles = roots ;
               }
         }
      
      catch ( Exception  e) {
         
         e.printStackTrace() ;
         statusBar.setText ( " Exception: " + e.toString() ) ;
      }

   }

   protected void setRoots() {

      //System.out.println ( "setRoots: entered: len: " + currentFiles.length ) ;
      
      for ( int i=0 ; i<currentFiles.length ; i++ ) {
          //System.out.println ( "setRoots: " + currentFiles[i].getPath().toUpperCase() ) ;
          addFilenameFilter ( currentFiles[i].getPath().toUpperCase(), currentFiles[i] ) ;
      }
   }
}
