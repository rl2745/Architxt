int main() {
    pixel matrix inputMatrix;
    pixel matrix outputMatrix;
    
    inputMatrix = read("trump.jpg");
    outputMatrix = enhanceBlueMatrix(inputMatrix, -100);
    outputMatrix = enhanceRedMatrix(inputMatrix, -50);
    outputMatrix = enhanceGreenMatrix(inputMatrix, -20);
    write(outputMatrix,"editedTrump","jpg");
}