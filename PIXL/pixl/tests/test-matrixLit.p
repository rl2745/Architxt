int main() {
	pixel matrix m;
        pixel p;
        pixel a;
        p  = (1,2,2,3);
        m = [p,p;p,p];
        a = m[1][1];
        print(a.A);
}
