namespace CSharpPart;

public class arrayMerger
{
    static int[] merge(int[] xs, int[] ys)
    {
        return xs.Concat(ys).OrderBy(x => x).ToArray();
    }
    
    int[] xs = { 3, 5, 12 };
    int[] ys = { 2, 3, 4, 7 };

    merge(xs, ys);
}