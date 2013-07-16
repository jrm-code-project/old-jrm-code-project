
import java.awt.Color;

public class Colors {
    public static final Color hintOfBlue   = new Color(192,255,255);
    public static final Color powderBlue   = new Color(128,255,255);
    public static final Color lightBlue    = new Color(  0,255,255);
    public static final Color medLightBlue = new Color(128,128,255);
    public static final Color mediumBlue   = new Color(128,128,192);
    public static final Color brightBlue   = new Color(  0,  0,255);
    public static final Color navyBlue     = new Color(  0,  0,128);
    public static final Color imperialBlue = new Color( 64,  0,128);
    public static final Color midnightBlue = new Color(  0,  0, 64);

    public static final Color black        = new Color(  0,  0,  0);
    public static final Color veryDarkGray = new Color( 32, 32, 32);
    public static final Color darkGray     = new Color( 64, 64, 64);
    public static final Color medDarkGray  = new Color( 96, 96, 96);
    public static final Color medGray      = new Color(128,128,128);
    public static final Color medLightGray = new Color(160,160,160);
    public static final Color lightGray    = new Color(192,192,192);
    public static final Color veryLightGray= new Color(224,224,224);
    public static final Color white        = new Color(255,255,255);

    public static final Color veryDarkGreen = new Color(  0, 32,  0);
    public static final Color darkGreen     = new Color(  0, 64,  0);
    public static final Color medDarkGreen  = new Color(  0, 96,  0);
    public static final Color medGreen      = new Color(  0,128,  0);
    public static final Color medLightGreen = new Color(  0,160,  0);
    public static final Color lightGreen    = new Color(  0,192,  0);
    public static final Color verylightGreen= new Color(  0,224,  0);
    public static final Color brightGreen   = new Color(  0,255,  0);

    public static final Color verydarkRed   = new Color( 32,  0,  0);
    public static final Color darkRed       = new Color( 64,  0,  0);
    public static final Color medDarkRed    = new Color( 96,  0,  0);
    public static final Color medRed        = new Color(128,  0,  0);
    public static final Color medLightRed   = new Color(160,  0,  0);
    public static final Color lightRed      = new Color(192,  0,  0);
    public static final Color verylightRed  = new Color(224,  0,  0);
    public static final Color brightRed     = new Color(255,  0,  0);
    public static final Color angryRed      = new Color(255,  0,  0);

    public static final Color sunrise       = new Color(255, 64,  0);
    public static final Color pumpkin       = new Color(255, 96,  0);
    public static final Color orange        = new Color(255,128,  0);
    public static final Color lightOrange   = new Color(255,160,  0);
    public static final Color deepYellow    = new Color(255,192,  0);
    public static final Color yellow        = new Color(255,224,  0);
    public static final Color brightYellow  = new Color(255,255,  0);
    public static final Color siena         = new Color(224, 96,  0);
    public static final Color sicklyBrown   = new Color(192, 96,  0);
    public static final Color burntBrown    = new Color(160, 96,  0);
    public static final Color richBrown     = new Color(128, 64,  0);
    public static final Color beige         = new Color(255,224,192);
    public static final Color tan           = new Color(255,202,168);
    public static final Color skinTone      = new Color(255,180,144);


    /**  Given any color, return white or black; whichever contrasts  
      * better.  Constants taken from question 9 of the color faq at
      *  http://www.inforamp.net/~poynton/notes/colour_and_gamma/ColorFAQ.html       
      */    
    static public Color getContrastingTextColor(Color c)
        {   final double brightness = c.  getRed()*0.2125
                                    + c.getGreen()*0.7145 
                                    + c. getBlue()*0.0721;       
            return brightness<128.0 ? Color.white : Color.black;    
        }

    static public Color atIntensity(Color c, double i) 
        {   i = Math.max(0.0, Math.min(1.0, i));
            double currentI = intensity(c);
            if (currentI < .001) 
                return new Color((int)i*255,(int)i*255,(int)i*255);
            return intensify(c, i / currentI);
        }
    static public Color intensify(Color c, double i) 
        {
            return new Color(
                Math.max(0,Math.min(255, (int)(c.getRed  ()*i) )), 
                Math.max(0,Math.min(255, (int)(c.getGreen()*i) )), 
                Math.max(0,Math.min(255, (int)(c.getBlue ()*i) ))            
            );
        }
    static public double intensity(Color c) 
        {
            return (c.getRed() + c.getGreen() + c.getBlue()) / 765.0;
        }
    static public Color quarterBright(Color c) 
        {
            return new Color (
                c.getRed  () / 4, 
                c.getGreen() / 4, 
                c.getBlue () / 4
            );
        }
    static public Color halfBright(Color c) 
        {
            return new Color (
                c.getRed  () / 2, 
                c.getGreen() / 2, 
                c.getBlue () / 2
            );
        }
    static public Color twiceBright(Color c) 
        {
            //if (intensity(c) > 0.95)  return Color.white;
            return new Color (
                c.getRed()   + ((256 - c.getRed  ()) / 2), 
                c.getGreen() + ((256 - c.getGreen()) / 2), 
                c.getBlue()  + ((256 - c.getBlue ()) / 2) 
            );
        }
}
