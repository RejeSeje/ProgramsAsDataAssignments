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
    for (i = 0; i < n; i = i + 1) {
        arr[i] = i * i;
    }
}

void arrsum(int n, int arr[], int *sump) {
    int i;
    for (i = 0; i < n; i = i + 1) {
        *sump = *sump + arr[i];
    }
}