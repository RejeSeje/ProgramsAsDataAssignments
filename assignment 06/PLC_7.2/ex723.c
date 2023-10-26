// Exercise 7.3 (iii) 

// As we don't feel the exercise is clear on what the specific objective of the exercise is,
// we have decided to understand it as having the main function initialize an array and then  
// showcasing our histogram function on said array.

//To call the function with this example, the argument n needs, of course, to be [7].

void main(int n) {
 
    int arr[7];
    arr[0] = 1;
    arr[1] = 2;
    arr[2] = 1;
    arr[3] = 1;
    arr[4] = 1;
    arr[5] = 2;
    arr[6] = 0;

    int max;
    max = 3;

    int freq[4];
    freq[0] = 0;
    freq[1] = 0;
    freq[2] = 0;
    freq[3] = 0;

    histogram(n, arr, max, freq);

    print freq[0];
    print freq[1];
    print freq[2];
    print freq[3];
}

void histogram(int n, int ns[], int max, int freq[]) {
    int i;
    i = 0;

    while (i < n) {
        freq[ns[i]] = freq[ns[i]] + 1; 
        i = i + 1;
    }
}
