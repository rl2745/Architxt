int main() {
	pixel matrix m1;
	pixel matrix m2;
	pixel p1;
	pixel p2;
	pixel p3;

	p1 = (0,0,0,0);
	p2 = (1,1,1,1);

	m1 = [p1,p1,p1;p2,p1,p1;p1,p1,p1];
	m2 = cropPixelMatrix(m1, 1, 2, 0, 1);
	p3 = m2[0][0];
	print(p3.R);
}