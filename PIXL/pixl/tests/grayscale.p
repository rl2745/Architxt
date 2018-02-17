int main() {
    int amt;    
    pixel matrix inputMatrix;
    pixel matrix outputMatrix;

    amt = 0;
    inputMatrix = read("trump.jpg");
    
    for (amt=0;amt<10;amt=amt++) {
        outputMatrix = grayscale(inputMatrix, amt*10);
        write(outputMatrix,"newTrump","jpg");
    }
}