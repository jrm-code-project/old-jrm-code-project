import java.util.Hashtable;
import java.io.InputStream;
import java.awt.*;


/** loads images which are in this class' directory */

public class ImageLoader {

private static Hashtable cache = new Hashtable();

private static Component trackerComponent = new Canvas();


public static Image loadImage(String resourceName) {
    Image cached = (Image) cache.get(resourceName);
    if (cached != null) return cached;
    try {
        InputStream resource = ImageLoader.class.getResourceAsStream(resourceName);
        byte[] bytes = new byte[resource.available()];
        resource.read(bytes);
        Image image = Toolkit.getDefaultToolkit().createImage(bytes);
        MediaTracker tracker = new MediaTracker(trackerComponent);
        tracker.addImage(image, 0);
        tracker.waitForID(0);
        cache.put(resourceName, image);
        return image;
    }
    catch(Exception ex) {
        System.out.println("ImageLoader() can't load '" + resourceName
            + "' with class '" + ImageLoader.class.getName() + "'.");
        ex.printStackTrace();
    }
    return null;
}

public static void flushCache() { cache.clear(); cache = new Hashtable(); }
}
