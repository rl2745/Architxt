int increment(int a) {
    return a+1;
}

int decrement(int a) {
    return a-1;
}

int max(int a, int b) {
    if (a < b) {
        return b;
    }
    else {
        return a;
    }
}

int abs(int x) {
    if (x > 0) {
        return x;
    }
    else {
        return -x;
    }
}

pixel copyPixel(pixel p) {
    int r;
    int g;
    int b;
    int a;
    pixel q;

    r = p.R;
    g = p.G;
    b = p.B;
    a = p.A;

    q = (r, g, b, a);
    return q;
}

pixel matrix cropPixelMatrix(pixel matrix pm, int r1, int r2, int c1, int c2) {
    int i;
    int j;
    pixel p;
    pixel matrix pm2;

    pm2 = matrix(r2-r1, c2-c1, pixel);

    for (i = r1; i < r2; i=i+1)
    {
        for (j = c1; j < c2; j=j+1)
        {
            pm2[i-r1][j-c1] = copyPixel(pm[i][j]);
        }
    }

    return pm2;
}

int matrix cropIntMatrix(int matrix m, int r1, int r2, int c1, int c2) {
    int i;
    int j;
    int a;
    int matrix m2;

    m2 = matrix(r2-r1, c2-c1, int);

    for (i = r1; i < r2; i=i+1)
    {
        for (j = c1; j < c2; j=j+1)
        {
            m2[i-r1][j-c1] = m[i][j];
        }
    }

    return m2;
}

int matrix flipIntMatrixH(int matrix m) {
    int height;
    int width;
    int i;
    int j;
    int matrix m2;
    height = m.rows;
    width = m.cols;

    m2 = matrix(m.rows, m.cols, int);

    for (i = 0; i < height; i=i+1)
    {
        for (j = 0; j < width; j=j+1)
        {
            m2[i][j] = m[i][width-1-j];
        }
    }

    return m2;
}

pixel matrix flipPixelMatrixH(pixel matrix pm) {
    int height;
    int width;
    int i;
    int j;
    pixel matrix pm2;
    height = pm.rows;
    width = pm.cols;

    pm2 = matrix(pm.rows, pm.cols, pixel);

    for (i = 0; i < height; i=i+1)
    {
        for (j = 0; j < width; j=j+1)
        {
            pm2[i][j] = copyPixel(pm[i][width-1-j]);
        }
    }

    return pm2;
}

int matrix flipIntMatrixV(int matrix m) {
    int height;
    int width;
    int i;
    int j;
    int matrix m2;
    height = m.rows;
    width = m.cols;

    m2 = matrix(m.rows, m.cols, int);

    for (i = 0; i < width; i=i+1)
    {
        for (j = 0; j < height; j=j+1)
        {
            m2[j][i] = m[height-1-j][i];
        }
    }

    return m2;
}

pixel matrix flipPixelMatrixV(pixel matrix pm) {
    int height;
    int width;
    int i;
    int j;
    pixel matrix pm2;
    height = pm.rows;
    width = pm.cols;

    pm2 = matrix(pm.rows, pm.cols, pixel);


    for (i = 0; i < width; i=i+1)
    {
        for (j = 0; j < height; j=j+1)
        {
            pm2[j][i] = copyPixel(pm[height-1-j][i]);
        }
    }

    return pm2;
}

bool pixelEquality(pixel p1, pixel p2) {
    if ((p1.R == p2.R) && (p1.G == p2.G) && (p1.B == p2.B) && (p1.A == p2.A))
    {
        return true;
    }
    else
    {
        return false;
    }
}

bool pixelSimilarity(pixel p1, pixel p2) {
    int diff;
    diff = 30;
    if ((absInt(p1.R - p2.R) < diff) && (absInt(p1.G - p2.G) < diff) && (absInt(p1.B - p2.B) < diff) && (absInt(p1.A - p2.A) < diff))
    {
        return true;
    }
    else
    {
        return false;
    }
}

int absInt(int a)
{
    if (a < 0)
    {
        return -a;
    }
    else
    {
        return a;
    }
}

pixel matrix matrixAnd(pixel matrix pm1, pixel matrix pm2) {
    int i;
    int j;

    pixel matrix pm3;

    pm3 = matrix(pm1.rows, pm1.cols, pixel);

    for (i = 0; i < pm1.rows; i=i+1)
    {
        for (j = 0; j < pm1.cols; j=j+1)
        {
            if (pixelSimilarity(pm1[i][j], pm2[i][j]))
            {
                pm3[i][j] = (255,255,255,255);
            }

            else
            {
                pm3[i][j] = copyPixel(pm1[i][j]);
            }
        }
    }

    return pm3;
}

pixel matrix invertMatrix(pixel matrix pm) {
    int i;
    int j;

    pixel matrix pm1;
    pm1 = pm;

    for (i = 0; i < pm.rows; i=i+1)
    {
        for (j = 0; j < pm.cols; j=j+1) {
            pm1[i][j] = (255,255,255,255) - pm[i][j];
        }
    }

    return pm1;
}

pixel invert(pixel og) {
    pixel p;
    p = (255,255,255,255) - og;
    return p;
}

pixel addPixel(pixel p1, pixel p2) {
    pixel p3;
    p3 = (0,0,0,0);
    p3.R = p1.R + p2.R;
    p3.G = p1.G + p2.G;
    p3.B = p1.B + p2.B;
    p3.A = max(p1.A, p2.A);

    return p3;
}

pixel matrix grayscale(pixel matrix pm) {
    int i;
    int j;
    int average;
    pixel p;
    pixel matrix pmGray;

    pmGray = matrix(pm.rows, pm.cols, pixel);

    for (i = 0; i < pm.rows; i=i+1)
    {
        for (j = 0; j < pm.cols; j=j+1)
        {
            p = pm[i][j];
            average = (p.R + p.G + p.B)/3;
            pmGray[i][j] = (average, average, average, p.A);
        }
    }

    return pmGray;
}

pixel subtractPixel(pixel p1, pixel p2) {
    pixel p3;
    p3 = (0,0,0,0);
    p3.R = abs(p1.R - p2.R);
    p3.G = abs(p1.G - p2.G);
    p3.B = abs(p1.B - p2.B);
    p3.A = max(p1.A, p2.A);

    return p3;
}

int matrix addIntMatrix(int matrix a, int matrix b) {
    int matrix m;
    int i;
    int j;
    m = matrix(a.rows, a.cols, int);
    for (i = 0; i < a.rows; i=i+1)
    {
        for (j = 0; j < a.cols; j=j+1)
        {
            m[i][j] = a[i][j] + b[i][j];
        }
    }

    return m;
}

int matrix subtractIntMatrix(int matrix a, int matrix b) {
    int matrix m;
    int i;
    int j;
    m = matrix(a.rows, a.cols, int);
    for (i = 0; i < a.rows; i=i+1)
    {
        for (j = 0; j < a.cols; j=j+1)
        {
            m[i][j] = a[i][j] - b[i][j];
        }
    }

    return m;
}

pixel enhanceRed(pixel p, int amount) {
  int currentRed;
  int newRed;
  currentRed = p.R;
  newRed = currentRed + amount;
  if (newRed >= 255) {
      newRed = 255;
  }
  p.R = newRed;
  return p;
}

pixel enhanceGreen(pixel p, int amount) {
  int currentGreen;
  int newGreen;
  currentGreen = p.G;
  newGreen = currentGreen + amount;
  if (newGreen >= 255) {
      newGreen = 255;
  }
  p.G = newGreen;
  return p;
}

pixel enhanceBlue(pixel p, int amount) {
  int currentBlue;
  int newBlue;
  currentBlue = p.B;
  newBlue = currentBlue + amount;
  if (newBlue >= 255) {
      newBlue = 255;
  }
  p.B = newBlue;
  return p;
}

pixel matrix enhanceRedMatrix(pixel matrix m, int amount) {
    int i;
    int j;
    pixel p;
    int red;
    pixel matrix pm;
    pm = matrix(m.rows, m.cols, pixel);

    for (i = 0; i < m.rows; i=i+1)
    {
        for (j = 0; j < m.cols; j=j+1)
        {
            p = m[i][j];
            red = p.R;
            red = red + amount;
            p.R = red;
            pm[i][j] = p;
        }
    }

    return pm;

}

pixel matrix enhanceGreenMatrix(pixel matrix m, int amount) {
    int i;
    int j;
    pixel p;
    int green;
    pixel matrix pm;
    pm = matrix(m.rows, m.cols, pixel);

    for (i = 0; i < m.rows; i=i+1)
    {
        for (j = 0; j < m.cols; j=j+1)
        {
            p = m[i][j];
            green = p.G;
            green = green + amount;
            p.G = green;
            pm[i][j] = p;
        }
    }

    return pm;

}


pixel matrix enhanceBlueMatrix(pixel matrix m, int amount) {
    int i;
    int j;
    pixel p;
    int blue;
    pixel matrix pm;
    pm = matrix(m.rows, m.cols, pixel);

    for (i = 0; i < m.rows; i=i+1)
    {
        for (j = 0; j < m.cols; j=j+1)
        {
            p = m[i][j];
            blue = p.B;
            blue = blue + amount;
            p.B = blue;
            pm[i][j] = p;
        }
    }

    return pm;

}
