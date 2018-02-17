int main() {
    pixel matrix inputMatrix;
    pixel matrix outputMatrix;
    int i;
    int j;
    pixel p;
    pixel matrix pmGray;
    int amt;

    inputMatrix = read("pine.jpg");
    outputMatrix = |inputMatrix;

    for (i = 0; i < inputMatrix.rows; i=i+1)
    {
        for (j = 0; j < inputMatrix.cols; j=j+1)
        {
            p = inputMatrix[i][j];
            if (j>inputMatrix.cols/3) {
                if (i<inputMatrix.rows/3) {
                    outputMatrix[i][j] = invert(enhanceRed(inputMatrix[i][j],i*70-j*20));
                }
                else if (j <= inputMatrix.cols*2/3) {
                    outputMatrix[i][j] = enhanceGreen(inputMatrix[i][j], -i+j*3);
                }
                else {
                    outputMatrix[i][j] = enhanceRed(inputMatrix[i][j], -i*2);
                }
            }
            else {
                if (i<inputMatrix.rows/3) {
                    outputMatrix[i][j] = invert(enhanceRed(inputMatrix[i][j], 8*i+j*2));
                }
                else {
                    outputMatrix[i][j] = enhanceRed(inputMatrix[i][j], 3*i*j);
                }
            }
        }
    }
    outputMatrix = outputMatrix<<0:inputMatrix.rows/2,0:inputMatrix.cols-1>>;
    write(outputMatrix,"flipPop-out","jpg");


}
