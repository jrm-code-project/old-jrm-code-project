
import java.util.*;


/** This is a version of C.A.R Hoare's Quick Sort 
* algorithm.  This will handle arrays that are already
* sorted??, and arrays with duplicate keys.
*
* Requires that vector contain items that implement Comparable,
* or that a Comparator be provided.
*/
public class ArraySorter {

    static Comparator asComparable = new Comparator() {
        public boolean less(Object lhs, Object rhs) 
            { return ((Comparable) lhs).lessThan((Comparable) rhs); } 
    };
   
    // default assumes elements implement Comparable
    static public void sort(Object[] array)
        { sort(array, asComparable, 0, array.length-1); }
    
    // use provided Comparator instead.
    static public void sort(Object[] array, Comparator cmp)
        { sort(array, cmp, 0, array.length-1); }
    
    static protected void sort(Object[] array, Comparator cmp, int left, int right) {
        int lo = left;
        int hi = right;

        if (right > left) {
            Object mid = array[(left+right)/2];
            while (lo <= hi) {
                while ( (lo < right) && cmp.less(array[lo], mid) )
                    ++lo;
                while ( (hi > left ) && cmp.less(mid, array[hi]) )
                    --hi;
                if (lo <= hi) { swap(array, lo, hi);  ++lo;  --hi; }
            }
            if (left <  hi)  sort(array, cmp, left,  hi);
            if (lo < right)  sort(array, cmp, lo, right);
    }   }
    static private void swap(Object[] array, int i, int j) {
        Object T = array[i];                    // t = a[i];
        array[i] = array[j];                    // a[i] = a[j];
        array[j] = T;                           // a[j] = t;
    }
}  
