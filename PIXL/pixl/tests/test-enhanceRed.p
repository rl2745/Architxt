int main()
{
        pixel p;
        int r;
        int amt;
        pixel p2;
        int r2;
        string s1;
        string s2;

        p = (100,2,3,4);
        r = p.R;
        p2 = enhanceRed(p, 20);
        r2 = p2.R;
        print(r2);
}
