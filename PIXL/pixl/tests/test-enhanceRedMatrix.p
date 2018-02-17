int main()
{
        pixel p1;
        pixel p2;

        pixel matrix pm1;
        pixel matrix pm2;

        p1 = (1,2,3,4);
        p2 = (5,6,7,8);

        pm1 = [p1,p1,p1;p2,p2,p1];
        pm2 = enhanceRedMatrix(pm1, 25);

        p1 = pm2[0][0];
        print(p1.R);
}
