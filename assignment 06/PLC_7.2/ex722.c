// Exercise 7.2 (ii) 

void main(int n) {
    int arr[20];
    
    squares(n, arr);

    int sump;
    sump = 0;

    arrsum(n, arr, &sump);
    print sump;
    println;
}

void squares(int n, int arr[]) {
    int i;
    i = 0;
    while (i < n) {
        arr[i] = i;
        arr[i] = arr[i] * arr[i];
        i = i + 1;
    }
}

void arrsum(int n, int arr[], int *sump) {        
    while (n > 0) {
        n = n - 1;
        *sump = *sump + arr[n];
    }
}