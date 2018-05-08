
int main() {
    int[] p;
    int x;
    int i;
    int j;
    int a;
    p = new int[5];
    x = 5;

    for (i = 0; i < x; i = i +1){
        p[i] = i;
    }

    for (j = 0; j < x; j = j+1){
        if(p[j] == 3)
            print_i(j);
        else
            a = 3;
    }
    return 0;
}
