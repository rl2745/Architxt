int main() {
	int matrix m;
	int matrix new;
	m = [0,0,0,0;1,1,1,1;2,2,2,2;3,3,3,3];
    new = cropIntMatrix(m, 1 , 3, 1, 3);
    print(new[0][0]);
}