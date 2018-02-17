int main() {
    pixel matrix inputMatrix;
    pixel matrix outputMatrix;
    
    inputMatrix = read("trump.jpg");
    
    outputMatrix = smoothImage(inputMatrix);
    outputMatrix = smoothImage(outputMatrix);
    
    write(outputMatrix,"editedTrump","jpg");
}