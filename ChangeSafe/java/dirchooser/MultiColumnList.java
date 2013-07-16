
import java.awt.*;
import java.awt.event.*;


public class MultiColumnList
     extends Panel
  implements ItemSelectable,
             ActionListener,
             KeyListener,
             MouseListener
{
static public String ACCEPT = "Select";
static public String CANCEL = "Cancel";
static public Font listfont = new Font("SansSerif", Font.PLAIN, 11);
static Insets itemInsets = new Insets(0,2,0,2);

    ListPane    listPanel       = new ListPane();
    ScrollPane  listScrollPane  = new ScrollPane(ScrollPane.SCROLLBARS_NEVER);
    Scrollbar   scroller        = new Scrollbar(Scrollbar.HORIZONTAL);

    public MultiColumnList()
        {   setLayout(new BorderLayout());
            add("Center", listScrollPane);
            add("South" , scroller);
            listScrollPane.add(listPanel);

            scroller.addAdjustmentListener(new AdjustmentListener() {
                public void adjustmentValueChanged(AdjustmentEvent e)
                {   int at = scroller.getValue();
                    listScrollPane.setScrollPosition(at, 0);    }
            });

            setFont(listfont);
            adjustScroller();
        }

    public void setBackground(Color c)
        {   super.setBackground(c);
            Color lt = Colors.atIntensity(c, 0.9375);
            listPanel.setBackground(lt);
        }
    public void setForeground(Color c)
        {   super.setForeground(c);
            double intensity = Colors.intensity(c);
            if (intensity > 0.6) intensity = 0.6;
            Color fore = Colors.atIntensity(c, intensity);
            listPanel.setForeground(fore);
        }

    public void invalidate()
        {   super.invalidate(); }

    public void validate()
        {   super.validate();
            adjustScroller();
            if (selected == null)
                selected = new Component[countItems()];
            listPanel.repaint();
        }

    public int countItems() { return listPanel.getComponentCount(); }

    private void adjustScroller()
        {   Dimension panelDim = listPanel.getSize();
            Dimension viewable = listScrollPane.getViewportSize();
            Point at = listScrollPane.getScrollPosition();
            scroller.setValues(at.x, viewable.width, 0, panelDim.width);

            int nmembers = listPanel.getComponentCount();
            int ix=0, diff=0; Component first = null;
            while (ix<nmembers && diff<64) {
                Component m = listPanel.getComponent(ix++);
                if (m.isVisible()) {
                    if (first==null) first = m;
                    else diff = m.getLocation().x - first.getLocation().x;
            }   }
            scroller.setBlockIncrement((diff<64)? 64: diff);
        }

    public void clear()
        {   listPanel.removeAll();
            invalidateSelect();
            validate();
        }
    void invalidateSelect()
        {   current = -1;
            selected = null;
            repaint();
        }

    public void addItem(String label, String imageName)
        {   ImageLabel item = new ImageLabel(label, imageName, ImageLabel.IMAGE_LEFT);
            item.setBorderWidth(1);
            item.setInsets(itemInsets);
            item.addMouseListener(this);
            item.addKeyListener(this);
            listPanel.add(item);
            invalidateSelect();
        }


    ItemListener   itemListener;
    ActionListener actionListener;

    int indexOf(Component item)
        {   int itemcount = listPanel.getComponentCount();
            for (int i=0; i<itemcount; i++) {
                if (item == listPanel.getComponent(i))
                    return i;
            }
            return -1;
        }

    //---------- ItemSelectable Implementation ---------------------
    /** Returns the selected items as an array of Strings, or null
      * if no items are selected.
      */
    public Object[] getSelectedObjects() {
        if (selected == null)  return null;
        int itemcount = countItems();
        int selectedcount = 0;
        for (int i=0; i<itemcount; i++) {
            if (selected[i] != null)
                ++selectedcount;
        }
        if (selectedcount == 0)  return null;
        String[] items = new String[selectedcount];
        for (int i=0, curr=0; i<itemcount; i++) {
            if (selected[i] != null) {
                String label = "?";
                if (selected[i] instanceof ImageLabel)
                    label = ((ImageLabel) selected[i]).getLabel();
                items[curr++] = label;
        }   }
        return items;
    }

    public void addItemListener(ItemListener listener)
        { itemListener = AWTEventMulticaster.add(itemListener, listener); }

    public void removeItemListener(ItemListener listener)
        { itemListener = AWTEventMulticaster.remove(itemListener, listener); }

    public void fireItemSelectEvent(Component item, boolean selected)
        {   int index = indexOf(item);
            if (selected)
                   select(index);
            else deselect(index);

            if (itemListener != null)
                itemListener.itemStateChanged( new ItemEvent(
                    this, ItemEvent.ITEM_STATE_CHANGED,
                    item, selected? ItemEvent.SELECTED: ItemEvent.DESELECTED));
        }

    //---------- ActionListener Implementation ---------------
    public void actionPerformed(ActionEvent evt)
        {   if (actionListener != null)
                actionListener.actionPerformed(evt);
        }
    public void addActionListener(ActionListener listener)
        { actionListener = AWTEventMulticaster.add(actionListener, listener); }

    public void removeActionListener(ActionListener listener)
        { actionListener = AWTEventMulticaster.remove(actionListener, listener); }

    public void fireActionEvent(String cmd)
        {   actionPerformed(new ActionEvent
                (this, ActionEvent.ACTION_PERFORMED, cmd));
        }


    //---------- KeyListener Implementation ------------------
    public void    keyTyped(KeyEvent evt) { }
    public void keyReleased(KeyEvent evt) { }
    public void  keyPressed(KeyEvent evt) {
        int index = -1;
        switch (evt.getKeyCode()) {
        case KeyEvent.VK_ESCAPE: if (isSelection()) clearSelection(); else fireActionEvent(CANCEL); break;
        case KeyEvent.VK_ENTER :
        case KeyEvent.VK_ACCEPT: fireActionEvent(ACCEPT); break;

        case KeyEvent.VK_LEFT : index =  leftOf(current); break;
        case KeyEvent.VK_RIGHT: index = rightOf(current); break;
        case KeyEvent.VK_UP   : index =   above(current); break;
        case KeyEvent.VK_DOWN : index =   below(current); break;
        case KeyEvent.VK_HOME : index =                0; break;
        case KeyEvent.VK_END  : index =   countItems()-1; break;
        }
        if (index != -1) {
            if (evt.isShiftDown() && evt.isControlDown())
                controlShiftTo(index);
            else if (evt.isShiftDown())
                shiftTo(index);
            else if (evt.isControlDown())
                controlTo(index);
            else goTo(index);
        }
    }

    //---------- MouseListener Implementation ------------------
    public void mouseEntered (MouseEvent e) {}
    public void mouseExited  (MouseEvent e) {}
    public void mousePressed (MouseEvent e) {}
    public void mouseReleased(MouseEvent e) {}
    public void mouseClicked (MouseEvent e) {
        if (e.getClickCount() == 1) {
            Component comp = (Component) e.getSource();
            int ix = indexOf(comp);
            if (e.isShiftDown() && e.isControlDown())
                controlShiftTo(ix);
            else if (e.isShiftDown())
                shiftTo(ix);
            else if (e.isControlDown())
                controlTo(ix);
            else goTo(ix);
            comp.requestFocus();
        }
        else if (e.getClickCount() == 2)
            fireActionEvent(ACCEPT);
    }

    //-----------------Selection implementation-----------------------
    Component[] selected;
    int current = -1;
    Color selectedBG = new Color(SystemColor.textHighlight    .getRGB());
    Color selectedFG = new Color(SystemColor.textHighlightText.getRGB());

    public boolean isSelection() {
        if (selected == null) return false;
        for (int ix=0; ix<selected.length; ix++)
            if (selected[ix] != null) return true;
        return false;
    }
    public boolean isSelected(Component elt) {
        if (selected == null) return false;
        for (int ix=0; ix<selected.length; ix++)
            if (selected[ix] == elt) return true;
        return false;
    }
    public boolean isSelected(int ix) {
        return (selected != null && ix != -1 && selected[ix] != null);
    }

    public void clearSelection() {
        for (int i=0; i<selected.length; i++)
            if (isSelected(i))
                deselect(i);
        current = -1;
    }

    void select(int index) {
        if (current>=0 && current<selected.length) {
            ImageLabel prev = (ImageLabel) listPanel.getComponent(current);
            prev.setFocusBorder(false);
        }
        ImageLabel comp = (ImageLabel) listPanel.getComponent(index);
        if (comp != null) {
            selected[index] = comp;
            current = index;
            comp.setBackground(selectedBG);
            comp.setForeground(selectedFG);
            comp.setFocusBorder(true);
            comp.repaint();
        }
    }
    void deselect(int index) {
        if (current>=0 && current<selected.length) {
            ImageLabel prev = (ImageLabel) listPanel.getComponent(current);
            prev.setFocusBorder(false);
        }
        ImageLabel comp = (ImageLabel) listPanel.getComponent(index);
        if (comp != null) {
            selected[index] = null;
            current = index;
            comp.setBackground(listPanel.getBackground());
            comp.setForeground(listPanel.getForeground());
            comp.setFocusBorder(true);
            comp.repaint();
        }
    }
    // reset selection:  clear all but index.
    void goTo(int index) {
        for (int i=0; i<selected.length; i++) {
            if (i != index && isSelected(i))
                fireItemSelectEvent(listPanel.getComponent(i), false);
        }
        fireItemSelectEvent(listPanel.getComponent(index), true);
    }
    // modify selection:  toggle the indicated item.
    void controlTo(int index) {
        fireItemSelectEvent(listPanel.getComponent(index), !isSelected(index));
    }
    // add to selection:  range from current to index.
    void shiftTo(int index) {
        int step = (index<current)? -1: 1;
        for (int i=current; i!=index; i+=step)
            fireItemSelectEvent(listPanel.getComponent(i), true);
        fireItemSelectEvent(listPanel.getComponent(index), true);
    }
    // modify selection:  toggle range from current to index.
    void controlShiftTo(int index) {
        int step = (index<current)? -1: 1;
        for (int i=current; i!=index; i+=step)
            fireItemSelectEvent(listPanel.getComponent(i), !isSelected(i));
        fireItemSelectEvent(listPanel.getComponent(index), !isSelected(index));
    }


    int leftOf(int index) {
        if (index<0 || index>=selected.length) return -1;
        Rectangle first = listPanel.getComponent(index).getBounds();
        while (--index >= 0) {
            Rectangle next = listPanel.getComponent(index).getBounds();
            if (next.x < first.x)
                if (((next.y <= first.y) && ( next.y+ next.height > first.y))
                 || ((first.y <= next.y) && (first.y+first.height >  next.y)))
                    return index;
        }
        return -1;
    }
    int rightOf(int index) {
        if (index<0 || index>=selected.length) return -1;
        Rectangle first = listPanel.getComponent(index).getBounds();
        while (++index < selected.length) {
            Rectangle next = listPanel.getComponent(index).getBounds();
            if (next.x > first.x)
                if (((next.y <= first.y) && ( next.y+ next.height > first.y))
                 || ((first.y <= next.y) && (first.y+first.height >  next.y)))
                    return index;
        }
        return -1;
    }
    int above(int index) {
        if (index<=0) return -1;
        return index - 1;
    }
    int below(int index) {
        if (index>=selected.length-1) return -1;
        return index + 1;
    }
}
