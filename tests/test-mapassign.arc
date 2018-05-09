int main() {
    Map baseMap;
    point p;
    bool b;
    string s;

    b = true;
    baseMap = new Map[8,10];
    s = "hello";
    p = (b, s);
    baseMap[0][0] = p;

    print_i(3);
    return 0;
}

