
import java.awt.*;
import java.awt.image.*;
import java.io.InputStream;
/* import ImageLoader; */

/** ImageLabel is a lightweight component that displays a text
  * label, with an associated image.  As with the normal Label
  * class, it is nonresponsive.  The image, or the label, can be
  * omitted, if desired.  Image position can be set to: above, below,
  * left of, or right of the text:  setOrientation(int);  Border
  * style can be selected from: none, raised, embossed:
  * setBorderStyle(Border);  Border width can be controlled:
  * setBorderWidth(int);  the outer 1-pixel boundary of the border
  * is reserved for the focus rect, and the raising/embossing uses
  * the remaining pixels.  setBorderWidth(0) eliminates the focus
  * rect as well as the raised/embossed border.
  * Gap between image and text can be set with setInternalGap(int);
  * Insets around image/text, separating them from the borders, can
  * be controlled by setInsets(Insets);
  * Ability to recieve focus in indicated by using the
  * setFocusTraversable(boolean) method.
  * Display of the focus rectangle in the outer pixel of the border
  * is controlled with setFocusBorder(boolean).
  */
public class ImageLabel extends Component implements LabelledItem {

public static final int IMAGE_ABOVE = 1;
public static final int IMAGE_BELOW = 2;
public static final int IMAGE_LEFT  = 3;
public static final int IMAGE_RIGHT = 4;

/** amount of space between border/bezel and image/label. (T,L,B,R)*/
public static Insets defaultInsets = new Insets(4,4,4,4);

int orientation = IMAGE_ABOVE;
Border border = Border.NONE;
int borderWidth = 2;
int gap = 4;
Insets insets = defaultInsets;
boolean canFocus;
boolean focusBorder;

Image  image;
String label;

    public ImageLabel(String label)
        { this(label, (Image)null, IMAGE_ABOVE); }

    public ImageLabel(String label, String imgName)
        { this(label, imgName, IMAGE_ABOVE); }

    public ImageLabel(String label, String imgName, int orientation)
        {   this.label = label;
            this.orientation = orientation;
            if (imgName!=null && imgName.length()>0)
                image = ImageLoader.loadImage(imgName);
        }

    public ImageLabel(String label, Image image, int orientation)
        {   this.label = label;
            this.image = image;
            this.orientation = orientation;
        }

    public int getOrientation() { return orientation; }
    public void setOrientation(int orientation)
        { this.orientation = orientation; invalidate(); repaint(); }

    public void setBorderStyle(Border b)
        { border = b; repaint(); }
    public Border getBorderStyle() { return border; }
    public boolean isBorderNone()     { return border == Border.NONE; }
    public boolean isBorderRaised()   { return border == Border.RAISED; }
    public boolean isBorderEmbossed() { return border == Border.EMBOSSED; }


    /** controls drawing of the 'focus' border around item.
      * Descendants that handle focus should call this to enable
      * display of a focus rect around item. */
    public boolean hasFocusBorder() { return focusBorder; }
    public void setFocusBorder(boolean f)
        { focusBorder = f; repaint(); }

    public  int getBorderWidth() { return borderWidth; }
    public void setBorderWidth(int borderWidth)
        { this.borderWidth = borderWidth; invalidate(); repaint(); }

    public  int getInternalGap() { return gap; }
    public void setInternalGap(int gap)
        { this.gap = gap; invalidate(); repaint(); }

    public  Insets getInsets() { return insets; }
    public void setInsets(Insets insets)
        { this.insets = insets; invalidate(); repaint(); }

    public  Image getImage() { return image; }
    public void setImage(Image image)
        { this.image = image; invalidate(); repaint(); }

    public String getLabel() { return label; }
    public void setLabel(String s)
        { label = s; invalidate(); repaint(); }

    public boolean isFocusTraversable() { return canFocus && isVisible(); }
    public void setFocusTraversable(boolean f)
        { canFocus = f; repaint(); }

public Dimension getPreferredSize() {
    int w=0, h=0, textW=0, textH=0;
    if (image!=null) {
        w = image.getWidth (null);
        h = image.getHeight(null);
    }
    Font font = getFont();
    if (label != null && label.length() > 0 && font != null) {
        FontMetrics fm = getFontMetrics(font);
        textW = fm.stringWidth(label);
        textH = fm.getHeight();
    }
    switch (orientation) {
        case IMAGE_ABOVE: case IMAGE_BELOW:
            h += textH; if (textH>0 && image!=null) h += gap;
            w = Math.max(w, textW);
            break;
        case IMAGE_LEFT : case IMAGE_RIGHT:
            w += textW; if (textW>0 && image!=null) w += gap;
            h = Math.max(h, textH);
            break;
    }
    return new Dimension(
        w + 2*borderWidth + insets.left + insets.right,
        h + 2*borderWidth + insets.top + insets.bottom);
}

public void update(Graphics g) { paint(g); }
public void paint (Graphics g) {
    clearBackground(g);
    drawBorder(g);
    drawImage(g);
    drawLabel(g);
}

public void clearBackground(Graphics g) {
    int width = getSize().width;
    int height = getSize().height;

    Color interior = getBackground();

    g.setColor(interior);
    g.fillRect(0,0, width, height);
}
public void drawBorder(Graphics g) {
    int width = getSize().width;
    int height = getSize().height;

    Color interior = getBackground();

    if (borderWidth > 0) {
        g.setColor(hasFocusBorder()? Color.black: interior);
        g.drawRect(0,0, width-1, height-1);
    }
    g.setColor(interior);
    for (int i=1; i<borderWidth; i++) {
        if (isBorderNone())
            g.drawRect  (i,i, width-2*i-1, height-2*i-1);
        else if (isBorderRaised())
            g.draw3DRect(i,i, width-2*i-1, height-2*i-1, true);
        else if (isBorderEmbossed())
            g.draw3DRect(i,i, width-2*i-1, height-2*i-1, false);
    }
}
public void drawImage(Graphics g) {
    int width = getSize().width;
    int height = getSize().height;

    Color interior = getBackground();

    width  -= (2*borderWidth + insets.left + insets.right);
    height -= (2*borderWidth + insets.top + insets.bottom);

    if (image != null) {
        int imageX = borderWidth + insets.left;
        int imageY = borderWidth + insets.top;
        int imageW = image.getWidth (null);
        int imageH = image.getHeight(null);
        switch (orientation) {
        case IMAGE_ABOVE: imageX+=(width-imageW)/2;                            break;
        case IMAGE_BELOW: imageX+=(width-imageW)/2; imageY+=(height-imageH)  ; break;
        case IMAGE_LEFT :                           imageY+=(height-imageH)/2; break;
        case IMAGE_RIGHT: imageX+=(width-imageW)  ; imageY+=(height-imageH)/2; break;
        }
        g.drawImage(image, imageX, imageY, null);
    }
}
public void drawLabel(Graphics g) {
    int width = getSize().width;
    int height = getSize().height;

    Color interior = getBackground();

    width  -= (2*borderWidth + insets.left + insets.right);
    height -= (2*borderWidth + insets.top + insets.bottom);

    Font font = getFont();
    if (label != null && label.length() > 0 && font != null) {
        FontMetrics fm = getFontMetrics(font);
        int textW = fm.stringWidth(label);
        int textH = fm.getHeight();
        int textX = borderWidth + insets.left;
        int textY = borderWidth + insets.top + fm.getMaxAscent();
        switch (orientation) {
        case IMAGE_ABOVE: textX+=(width-textW)/2; textY+=(height-textH)  ; break;
        case IMAGE_BELOW: textX+=(width-textW)/2;                          break;
        case IMAGE_LEFT : textX+=(width-textW)  ; textY+=(height-textH)/2; break;
        case IMAGE_RIGHT:                         textY+=(height-textH)/2; break;
        }
        g.setColor(getForeground());
        g.drawString(label, textX, textY);
    }
}
}
