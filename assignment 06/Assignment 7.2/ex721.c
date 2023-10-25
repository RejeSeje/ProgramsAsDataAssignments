// Exercise 7.2 (i) 

void main(int n) {
    int arr[4];
    arr[0] = 7;
    arr[1] = 13;
    arr[2] = 9;
    arr[3] = 8;
    int sump;
    sump = 0;

    arrsum(n, arr, &sump);
    print sump;
    println;
}

void arrsum(int n, int arr[], int *sump) {        
    while (n > 0) {
        n = n - 1;
        *sump = *sump + arr[n];
    }
}


