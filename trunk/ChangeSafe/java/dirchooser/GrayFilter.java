import java.awt.*;
import java.awt.image.*;

/**  Filters image by converting all non-background pixels
  * to a single color.
  */
public class GrayFilter extends RGBImageFilter {

    int bgColor;
    int bgR, bgG, bgB;

    public GrayFilter(Color bg) {
        canFilterIndexColorModel = true;
        bgColor = bg.getRGB();
        bgR = (bgColor >> 16) & 0xff;
        bgG = (bgColor >>  8) & 0xff;
        bgB = (bgColor      ) & 0xff;
    }
    public int filterRGB(int x, int y, int rgb) {
        if (rgb == bgColor) return rgb;
        int a = (rgb >> 24) & 0xff;
        int r = (rgb >> 16) & 0xff;
        int g = (rgb >>  8) & 0xff;
        int b = (rgb      ) & 0xff;
        int i = 8;  // scale factor
        r = ((r - bgR) / i) + bgR;
        g = ((g - bgG) / i) + bgG;
        b = ((b - bgB) / i) + bgB;

        return (a<<24) | (r<<16) | (g<<8) | b;
    }
}
