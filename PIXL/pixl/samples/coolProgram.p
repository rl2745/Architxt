int main() {
    int i;
    int j;
    pixel matrix cropped;
    pixel matrix flipped;
    pixel matrix img;

    img = read("penguin.jpg");
    for (i = 0; i < 32; i=i+1) {
        for (j = 0; j < 32; j=j+1) {
            cropped = img<<i:i+224,j:j+224>>;
            write(cropped, "penguins/img" + str_of_int(i) + "_" + str_of_int(j), "jpg");
            flipped = ~cropped;
            write(flipped, "penguins/img" + str_of_int(i) + "_" + str_of_int(j) + "f", "jpg");
        }
    }
    return 0;
}
