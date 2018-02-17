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

    for (i = 0; i < inputMatrix.rows; i=i+2)
    {
        for (j = 0; j < inputMatrix.cols; j=j+2)
        {
            p = inputMatrix[i][j];
            average = (i*p.R + j*p.G + p.B)/3;
            outputMatrix[i][j] = enhanceRed(inputMatrix[i][j], (i-j)/1);
            outputMatrix[i+1][j] = enhanceRed(inputMatrix[i][j], (i-2*j)/1);
            outputMatrix[i][j+1] = enhanceBlue(inputMatrix[i][j], (i+j)/1);
        }
    }

    write(outputMatrix,"cropPop-out","jpg");
}
