int main() {
    pixel matrix inputMatrix;
    pixel matrix outputMatrix;
    int i;
    int j;
    int average;
    pixel p;
    pixel matrix pmGray;
    int amt;
    
    inputMatrix = read("trump.jpg");
    outputMatrix = inputMatrix;
    
    for (i = 0; i < inputMatrix.rows; i=i+1)
    {
        for (j = 0; j < inputMatrix.cols; j=j+1)
        {
            p = inputMatrix[i][j];
            average = (i*p.R + p.G + p.B)/3;
            if (p.R>=120) {
                outputMatrix[i][j] = enhanceRed(inputMatrix[i][j],-100);
            }
            
        }
    }

    write(outputMatrix,"trumpPop2","jpg");
}