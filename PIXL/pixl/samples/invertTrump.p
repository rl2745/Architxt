int main() {
    pixel matrix inputMatrix;
    pixel matrix outputMatrix;
    inputMatrix = read("trump.jpg");

    outputMatrix = invertMatrix(inputMatrix);
    write(outputMatrix,"enhancedTrump","jpg");
}