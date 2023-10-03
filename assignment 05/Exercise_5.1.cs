public static void Main(string[] args)
{
    static int[] merge(int[] xs, int[] ys)
    {
        return xs.Concat(ys).OrderBy(x => x).ToArray();
    }
}
