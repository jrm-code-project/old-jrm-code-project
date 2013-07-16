
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;

/** ImageButton is a lightweight component that acts as a Button,
  * responding to user clicks and keyboard input by sending an
  * ActionEvent when appropriate.  It extends ImageLabel to
  * use that display/layout functionality.  The button's action
  * command defaults to the label text, or can be set with
  * setActionCommand(String);  Button may be enabled/disabled
  * with makeEnabled(boolean); default is enabled.  This implementation
  * provides 'flyby' buttons, where the border only shows when the
  * mouse is over the button.
  */
public class ImageButton extends ImageLabel
  implements KeyListener, MouseListener, MouseMotionListener, FocusListener
{
Image normal;
Image grayed;

// bgColor is used to calculate the grayed image.
static public Color bgColor = new Color(192,192,192);


  protected boolean enabled   = true;
  protected boolean pressed   = false;
  protected boolean mouseover = false;
  private boolean enableFocusRect = true;
  ActionListener actionListener;
  String actionCommand;

    public ImageButton(String label)
        { this(label, label, null, IMAGE_ABOVE); }

    public ImageButton(String label, String resourceName)
        { this(label, label, resourceName, IMAGE_ABOVE); }

    public ImageButton(String label, String resourceName, int orientation)
        { this(label, label, resourceName, orientation); }

    public ImageButton(String label, String actionCommand, String resourceName)
        { this(label, actionCommand, resourceName, IMAGE_ABOVE); }

    public ImageButton(String label,
                       String actionCommand,
                       String resourceName,
                       int orientation) {
        super(label, resourceName, orientation);
        this.actionCommand = actionCommand;
        init();
    }

    public ImageButton(String label, Image img, int orientation)
        {   super(label, img, orientation);
            init();
        }

    private void init() {
        setBorderStyle(Border.RAISED);
        setBorderWidth(3);

        addKeyListener(this);
        addMouseListener(this);
        addMouseMotionListener(this);
        addFocusListener(this);

        Image img = getImage();     // if we were created with an
        if (img != null)            // image, we need to create the
            setImage(img);          // normal and grayed copies
    }

    /** Create a grayed copy of image, and use super.setImage
      * to set it appropriately
      */
    public void setImage(Image img) {
        normal = img;
        ImageFilter dimmer = new GrayFilter(bgColor);
        grayed = createImage(new FilteredImageSource(img.getSource(), dimmer));
        MediaTracker tracker = new MediaTracker(this);
        tracker.addImage(grayed, 0);
        try { tracker.waitForID(0); } catch (Exception e) {}
        super.setImage(enabled? normal: grayed);
    }

    public void setActionCommand(String s) { actionCommand = s; }
    public String getActionCommand() { return actionCommand; }

    public void  makeEnabled() { enabled = true;  super.setImage(normal); repaint(); }
    public void makeDisabled() { enabled = false; super.setImage(grayed); repaint(); }

    /** hackery/pukery to optionally turn off the black focus rect
      * around the focussed button, as it is deemed (by me) to be
      * ugly in the presence of flyby borders.
      */
    public boolean focusBorderEnabled() { return enableFocusRect; }
    public void enableFocusBorder(boolean f)
        { enableFocusRect = f; repaint(); }

    /** overridden to take into account the disable flag */
    public boolean hasFocusBorder()
        { return enableFocusRect && super.hasFocusBorder(); }

    /** abstract state:  'active' means indicating it's state, as
      * when mouse is over button; 'pushed' means user is pressing
      * the button; 'raised' means user is not pressing button.
      */
    boolean buttonActive() { return enabled && mouseover; }
    boolean buttonPushed() { return buttonActive() &&  pressed; }
    boolean buttonRaised() { return buttonActive() && !pressed; }

    /** use abstract button state to determine which type of border
      * is appropriate
      */
    public boolean isBorderNone()
        { return !buttonActive() || super.isBorderNone(); }

    public boolean isBorderRaised()
        {   return buttonActive() && (pressed? super.isBorderEmbossed()
                                             : super.isBorderRaised  ()); }

    public boolean isBorderEmbossed()
        {   return buttonActive() && (pressed? super.isBorderRaised  ()
                                             : super.isBorderEmbossed()); }


    //---------- KeyListener Implementation ------------------
    /** presume we only get keys when we already have the focus,
      * so don't worry about checking for it.
      *  I don't know what's the difference between VK_ENTER and
      * VK_ACCEPT, so react to either.  VK_SPACE is also deemed
      * (by me) to be an appropriate activator.
      */
    public void keyPressed (KeyEvent evt) {
        if (evt.getKeyCode() == KeyEvent.VK_ACCEPT
         || evt.getKeyCode() == KeyEvent.VK_ENTER
         || evt.getKeyCode() == KeyEvent.VK_SPACE)
            fireActionEvent();
    }
    public void keyReleased(KeyEvent evt) { }
    public void keyTyped   (KeyEvent evt) { }

    //---------- MouseListener Implementation ------------------
    public void mouseClicked(MouseEvent e) {}

    public void mouseEntered(MouseEvent e)
        { mouseover = true; stateChanged(); }

    public void mouseExited(MouseEvent e)
        { mouseover = false; stateChanged(); }

    public void mousePressed(MouseEvent e)
        {   if (enabled) {
                requestFocus(); requestFocus(); // bugbug
                pressed = true;
                stateChanged();
        }   }
    public void mouseReleased(MouseEvent e)
        {   if (enabled && mouseover)
                fireActionEvent();
            pressed = false;
            stateChanged();
        }
    //---------- MouseMotionListener Implementation ------------------
    public void mouseDragged(MouseEvent e)
        {   boolean over = contains(e.getX(), e.getY());
            if (mouseover != over) {
                mouseover = over;
                stateChanged();
        }   }

    public void mouseMoved(MouseEvent e)
        {   boolean over = contains(e.getX(), e.getY());
            if (mouseover != over) {
                mouseover = over;
                stateChanged();
        }   }

    private void stateChanged()
        {   Graphics g = getGraphics();
            if (g != null) try {
                drawBorder(g);
            } finally { g.dispose(); }
        }


    //---------- FocusListener Implementation ------------------
    public void focusGained(FocusEvent e)
        { setFocusBorder(true); repaint(); }

    public void focusLost(FocusEvent e)
        { setFocusBorder(false); repaint(); }


    public void addActionListener(ActionListener listener) {
        actionListener = AWTEventMulticaster.add(actionListener, listener);
    }
    public void removeActionListener(ActionListener listener) {
        actionListener = AWTEventMulticaster.remove(actionListener, listener);
    }
    public void fireActionEvent() {
        if (actionListener != null)
            actionListener.actionPerformed(new ActionEvent(
                this, ActionEvent.ACTION_PERFORMED, getActionCommand()));
    }
}
