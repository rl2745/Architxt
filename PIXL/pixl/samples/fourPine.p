int main() {
    pixel matrix inputMatrix;
    pixel matrix outputMatrix;
    int i;
    int j;
    int average;
    pixel p;
    pixel matrix pmGray;
    int amt;

    inputMatrix = read("pine.jpg");
    outputMatrix = inputMatrix;

    for (i = 0; i < inputMatrix.rows; i=i+1)
    {
        for (j = 0; j < inputMatrix.cols; j=j+1)
        {
            p = inputMatrix[i][j];
            average = (p.R + p.G + p.B)/3;
            if (j>inputMatrix.cols/3) {
                if (i<inputMatrix.rows/3) {
                    outputMatrix[i][j] = invert(inputMatrix[i][j]);
                }
                else if (j <= inputMatrix.cols*2/3) {
                    outputMatrix[i][j] = enhanceGreen(inputMatrix[i][j], -100);
                }
                else {
                    outputMatrix[i][j] = enhanceRed(inputMatrix[i][j], -200);
                }
            }
            else {
                if (i<inputMatrix.rows/3) {
                    outputMatrix[i][j] = enhanceRed(inputMatrix[i][j], 180);
                }
                else {
                    outputMatrix[i][j] = enhanceGreen(inputMatrix[i][j], 100);
                }
            }
        }
    }
    outputMatrix = |outputMatrix;
    write(outputMatrix,"pinePop","jpg");
}
