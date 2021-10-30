#include <iostream>
using namespace std;

const int LIMIT_WHEN_USE_MULTIPLY = 1;

void printMatrix(double** matrix, int r, int c = -1)
{
    if (c <= 0) c = r;
    for (int i = 0; i < r; i++)
    {
        for (int j = 0; j < c; j++)
        {
            cout << matrix[i][j] << " ";
        }
        cout << endl;
    }
    cout << endl;
}

double getRandNumber()
{
	//return (rand() % 100 + 1) / 10.0;
    return (rand() % 10 + 1);
}

int log2(int x) {
    int result = 1;
    while ((x >>= 1) != 0) {
        result++;
    }
    return result;
}

int getNewDimension(int asize_r, int asize_c, int bsize_r, int bsize_c)
{
    return 1 << log2(max(max(asize_r, asize_c), max(bsize_r, bsize_c)));
}

double** addition2SquareMatrix(double** a, int size, int size_r, int size_c) {
    double** result = new double*[size];
	for (int i = 0; i < size; i++)
	{
        result[i] = new double[size];
		for (int j = 0; j < size; j++)
		{
            result[i][j] = 0;
		}
	}

    for (int i = 0; i < size_r; i++) {
        for (int j = 0; j < size_c; j++) {
            result[i][j] = a[i][j];
        }
    }
    return result;
}

void splitMatrix(double** a, double** a11, double** a12, double** a21, double** a22, int size) {
	const int new_size = size >> 1;

    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++)
        {
	        if (i < new_size && j < new_size)
	        {
                a11[i][j] = a[i][j];
	        }
            else if (i < new_size && j >= new_size)
            {
                a12[i][j - new_size] = a[i][j];
            }
            else if (i >= new_size && j < new_size)
            {
                a21[i - new_size][j] = a[i][j];
            }
            else
            {
                a22[i - new_size][j - new_size] = a[i][j];
            }
        }
    }
}

double** collectMatrix(double** a11, double** a12, double** a21, double** a22, int size) {
    const int new_size = size << 1;
    double** a = new double*[new_size];
	for (int i = 0; i < new_size; i++)
	{
        a[i] = new double[new_size];
	}

    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++)
        {
            a[i][j] = a11[i][j];
            a[i][j + size] = a12[i][j];
            a[i + size][j] = a21[i][j];
            a[i + size][j + size] = a22[i][j];
        }
    }
    return a;
}

double** multiply(double** a, double** b, int n)
{
    double** result = new double* [n];
	for (int i = 0; i < n; i++)
	{
        result[i] = new double[n];
		for (int j = 0; j < n; j++)
		{
            result[i][j] = 0;
			for (int k = 0; k < n; k++)
			{
                result[i][j] += a[i][k] * b[k][j];
			}
		}
	}
    return result;
}

double** sumOrSubMatrix(double** a, double** b, int size, char ar_operator)
{
    double** result = new double* [size];
    for (int i = 0; i < size; i++)
    {
        result[i] = new double[size];
        for (int j = 0; j < size; j++)
        {
			if (ar_operator == '+')
			{
                result[i][j] = a[i][j] + b[i][j];
			}
            else if (ar_operator == '-')
            {
                result[i][j] = a[i][j] - b[i][j];
            }
        }
    }
    return result;
}

double** multiplyStrassen(double** a, double** b, int size) {
    if (size <= LIMIT_WHEN_USE_MULTIPLY) {
        return multiply(a, b, size);
    }
    const int new_size = size >> 1;

    double** a11 = new double* [new_size];
    double** a12 = new double* [new_size];
    double** a21 = new double* [new_size];
    double** a22 = new double* [new_size];
    double** b11 = new double* [new_size];
    double** b12 = new double* [new_size];
    double** b21 = new double* [new_size];
    double** b22 = new double* [new_size];

    for (int i = 0; i < new_size; i++)
    {
        a11[i] = new double[new_size];
        a12[i] = new double[new_size];
        a21[i] = new double[new_size];
        a22[i] = new double[new_size];
        b11[i] = new double[new_size];
        b12[i] = new double[new_size];
        b21[i] = new double[new_size];
        b22[i] = new double[new_size];
    }

    splitMatrix(a, a11, a12, a21, a22, size);
    splitMatrix(b, b11, b12, b21, b22, size);

    double** p1 = multiplyStrassen(sumOrSubMatrix(a11, a22, new_size, '+'), sumOrSubMatrix(b11, b22, new_size, '+'), new_size);
    double** p2 = multiplyStrassen(sumOrSubMatrix(a21, a22, new_size, '+'), b11, new_size);
    double** p3 = multiplyStrassen(a11, sumOrSubMatrix(b12, b22, new_size, '-'), new_size);
    double** p4 = multiplyStrassen(a22, sumOrSubMatrix(b21, b11, new_size, '-'), new_size);
    double** p5 = multiplyStrassen(sumOrSubMatrix(a11, a12, new_size, '+'), b22, new_size);
    double** p6 = multiplyStrassen(sumOrSubMatrix(a21, a11, new_size, '-'), sumOrSubMatrix(b11, b12, new_size, '+'), new_size);
    double** p7 = multiplyStrassen(sumOrSubMatrix(a12, a22, new_size, '-'), sumOrSubMatrix(b21, b22, new_size, '+'), new_size);

    double** c11 = sumOrSubMatrix(sumOrSubMatrix(p1, p4, new_size, '+'), sumOrSubMatrix(p7, p5, new_size, '-'), new_size, '+');
    double** c12 = sumOrSubMatrix(p3, p5, new_size, '+');
    double** c21 = sumOrSubMatrix(p2, p4, new_size, '+');
    double** c22 = sumOrSubMatrix(sumOrSubMatrix(p1, p2, new_size, '-'), sumOrSubMatrix(p3, p6, new_size, '+'), new_size, '+');

    return collectMatrix(c11, c12, c21, c22, new_size);
}

int main()
{
    srand(time(nullptr));
    int a_r = 2, a_c = 3, b_r = a_c, b_c = 4;
    int n = getNewDimension(a_r, a_c, b_r, b_c);
    double** a_input = new double* [a_r];
   
	for (int i = 0; i < a_r; i++)
	{
        a_input[i] = new double[a_c];
		for (int j = 0; j < a_c; j++)
		{
            a_input[i][j] = getRandNumber();
		}
	}
    cout << "A" << endl;
    printMatrix(a_input, a_r, a_c);
    double** b_input = new double* [b_r];
    for (int i = 0; i < b_r; i++)
    {
        b_input[i] = new double[b_c];
        for (int j = 0; j < b_c; j++)
        {
            b_input[i][j] = getRandNumber();
        }
    }
    cout << "B" << endl;
    printMatrix(b_input, b_r, b_c);

    double** a = addition2SquareMatrix(a_input, n, a_r, a_c);
    double** b = addition2SquareMatrix(b_input, n, b_r, b_c);
    double** result = multiply(a, b, n);
    printMatrix(result, a_r, b_c);
    double** resultStrassen = multiplyStrassen(a, b, n);
    printMatrix(resultStrassen, a_r, b_c);
	return 0;
}