
import java.awt.*;
import java.awt.event.*;


class ListPane extends Panel implements LayoutManager
{
    Image   offscreen;
    int hgap = 4, vgap = 0;

    public ListPane() { setLayout(this); }

    public void invalidate() {
        super.invalidate();
        if (offscreen != null)
            offscreen.flush();
        offscreen = null;
    }

    public void update(Graphics g) {
        Dimension size = getSize();
        if (offscreen == null)
            offscreen = createImage((size.width <1)? 1: size.width,
                                    (size.height<1)? 1: size.height);

        Graphics og = offscreen.getGraphics();
        if (og != null) try {
            og.setClip(g.getClip());

            og.setColor(getBackground());
            og.fillRect(0, 0, size.width, size.height);

            super.paint(og);
        }
        finally  { og.dispose(); }

        paint(g);
    }
    public void  paint(Graphics g) {
        if (offscreen != null)
           g.drawImage(offscreen, 0, 0, null);
    }

    //--------------- LayoutManager Implementation ------------//
    public void addLayoutComponent(String name, Component comp) {}
    public void removeLayoutComponent(Component comp) {}

    public Dimension minimumLayoutSize(Container target) {
        return preferredLayoutSize(target);
    }
    public Dimension preferredLayoutSize(Container target) {
        Insets insets = target.getInsets();
        Dimension compMax = new Dimension(0, 0);
        int maxheight = target.getBounds().height
                        - (insets.top + insets.bottom + vgap*2);
        int nmembers  = target.getComponentCount();
        int visiblecount = 0;
        for (int i=0; i<nmembers; i++) {
            Component m = target.getComponent(i);
            if (m.isVisible()) {
                ++visiblecount;
                Dimension d = m.getPreferredSize();
                compMax.width  = Math.max( compMax.width,  d.width);
                compMax.height = Math.max(compMax.height, d.height);
        }   }
        if (visiblecount > 0) {
            int nrows = Math.max(1, (maxheight+compMax.height/4)/compMax.height);
            int ncols = (visiblecount+nrows-1) / nrows;
            compMax.height = compMax.height*nrows + vgap*(nrows-1);
            compMax.width  = compMax.width *ncols + hgap*(ncols-1);
        }
        compMax.height += insets.top + insets.bottom + vgap*2;
        compMax.width  += insets.left + insets.right + hgap*2;
        return compMax;
    }
    public void layoutContainer(Container target) {
        Insets insets = target.getInsets();
        Dimension size = target.getSize();
        Dimension compMax = new Dimension(0, 0);
        int maxheight = size.height - (insets.top + insets.bottom + vgap*2);
        int nmembers = target.getComponentCount();
        int visiblecount = 0;
        for (int i=0; i<nmembers; i++) {
            Component m = target.getComponent(i);
            if (m.isVisible()) {
                ++visiblecount;
                Dimension d = m.getPreferredSize();
                compMax.width  = Math.max(compMax.width ,  d.width);
                compMax.height = Math.max(compMax.height, d.height);
        }   }
        if (visiblecount > 0) {
            int nrows = Math.max(1, (maxheight+compMax.height/4)/compMax.height);
            int ncols = (visiblecount+nrows-1) / nrows;
            int row=0, col=0, currComp=0;

            for (int i=0; i<nmembers; i++) {
                Component m = target.getComponent(i);
                if (m.isVisible()) {
                    Dimension d = m.getPreferredSize();
                    int x = insets.left + hgap + col*(compMax.width +hgap);
                    int y = insets.top  + vgap + row*(compMax.height+vgap);

                    m.setBounds(x, y, d.width, d.height);

                    // move index to next component
                    ++currComp; if (++row >= nrows) { row = 0; ++col; }
        }   }   }
        repaint();
    }
}
