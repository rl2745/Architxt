int main()
{
	pixel matrix p1;
	pixel matrix p2;
    pixel matrix p3;

	p1 = read("monalisa.jpg");
	p2 = read("monalisa2.jpg");

	p3 = p2 && p1;
    
    write(p3,"out6","jpg");
}