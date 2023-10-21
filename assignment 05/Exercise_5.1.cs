public static void Main(string[] args)
{
    /* Old solution, doesn't follow intention of exercise */
    // static int[] merge(int[] xs, int[] ys)
    // {
    //     return xs.Concat(ys).OrderBy(x => x).ToArray();
    // }
    
    static int[] Merge(int[] xs, int[] ys)
    {
        int[] mergedArray = new int[xs.Length + ys.Length];
        int i = 0, j = 0, k = 0;
            
        while (i < xs.Length && j < ys.Length)
        {
            if (xs[i] < ys[j])
            {
                mergedArray[k] = xs[i];
                i++;
            }
            else
            {
                mergedArray[k] = ys[j];
                j++;
            }
            k++;
        }
            
        while (i < xs.Length)
        {
            mergedArray[k] = xs[i];
            i++;
            k++;
        }
            
        while (j < ys.Length)
        {
            mergedArray[k] = ys[j];
            j++;
            k++;
        }
            
        return mergedArray;
    }
}
