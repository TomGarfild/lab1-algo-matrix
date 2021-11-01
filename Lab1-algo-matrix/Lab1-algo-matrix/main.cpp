#include <chrono>
#include <iostream>
using namespace std;

const int LIMIT_WHEN_USE_MULTIPLY = 64;
const double EPSILON = 1e-6;

class matrix
{
public:
    int rows, columns;
    double** table;
    matrix(int);
    matrix(int, int);
    ~matrix();
    void print() const;
};

matrix::matrix(int size)
{
    rows = columns = size;
    table = new double* [rows];
    for (int i = 0; i < rows; i++)
    {
        table[i] = new double[columns] {};
    }
}

matrix::matrix(int r, int c)
{
    rows = r;
    columns = c;
    table = new double* [rows];
	for (int i = 0; i < rows; i++)
	{
        table[i] = new double[columns]{};
	}
}

matrix::~matrix()
{
	for (int i = 0; i < rows; i++)
	{
        delete[] table[i];
	}
    delete[] table;
}

void matrix::print() const
{
    for (int i = 0; i < rows; i++)
    {
        for (int j = 0; j < columns; j++)
        {
            cout << table[i][j] << " ";
        }
        cout << endl;
    }
    cout << endl;
}

matrix& operator+(const matrix& left, const matrix& right)
{
	const int r = left.rows;
	const int c = left.columns;
    matrix result(r, c);
    for (int i = 0; i < r; i++)
    {
        for (int j = 0; j < c; j++)
        {
            result.table[i][j] = left.table[i][j] + right.table[i][j];
        }
    }
    return result;
}

matrix& operator-(const matrix& left, const matrix& right)
{
    const int r = left.rows;
    const int c = left.columns;
    matrix result(r, c);
    for (int i = 0; i < r; i++)
    {
        for (int j = 0; j < c; j++)
        {
            result.table[i][j] = left.table[i][j] - right.table[i][j];
        }
    }
    return result;
}

matrix& operator*(matrix& left, matrix& right)
{
    const int r = left.rows;
    const int c = right.columns;
    matrix result(r, c);
    for (int i = 0; i < r; i++)
    {
        for (int j = 0; j < c; j++)
        {
            for (int k = 0; k < right.rows; k++)
            {
                result.table[i][j] += left.table[i][k] * right.table[k][j];
            }
        }
    }
    return result;
}

void operator*=(matrix& left, matrix& right)
{
    const int r = left.rows;
    const int c = right.columns;
    matrix result(r, c);
    for (int i = 0; i < r; i++)
    {
        for (int j = 0; j < c; j++)
        {
            for (int k = 0; k < right.rows; k++)
            {
                result.table[i][j] += left.table[i][k] * right.table[k][j];
            }
        }
    }
    left = result;
}

matrix operator*(matrix matrix, double number)
{
    for (int i = 0; i < matrix.rows; i++)
    {
        for (int j = 0; j < matrix.columns; j++)
        {
            matrix.table[i][j] *= number;
        }
    }
    return matrix;
}

bool operator==(const matrix& left, const matrix& right)
{
    for (int i = 0; i < left.rows; i++)
    {
        for (int j = 0; j < left.columns; j++)
        {
            if (fabs(left.table[i][j] - right.table[i][j]) > EPSILON)
            {
                return false;
            }
        }
    }
    return true;
}

bool twoMatrixEqual(double** a, double** b, int r, int c)
{
	for (int i = 0; i < r; i++)
	{
		for (int j = 0; j < c; j++)
		{
			if (fabs(a[i][j]-b[i][j]) > EPSILON)
			{
                return false;
			}
		}
	}	

    return true;
}

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
	return (rand() % 100 + 1) / 10.0;
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

void splitMatrix(const matrix& a, matrix& a11, matrix& a12, matrix& a21, matrix& a22) {
    const int size = a.rows;
	const int new_size = size >> 1;

    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++)
        {
            if (i < new_size && j < new_size)
            {
                a11.table[i][j] = a.table[i][j];
            }
            else if (i < new_size && j >= new_size)
            {
                a12.table[i][j - new_size] = a.table[i][j];
            }
            else if (i >= new_size && j < new_size)
            {
                a21.table[i - new_size][j] = a.table[i][j];
            }
            else
            {
                a22.table[i - new_size][j - new_size] = a.table[i][j];
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

matrix& collectMatrix(matrix& a11, matrix& a12, matrix& a21, matrix& a22) {
    const int size = a11.rows;
    const int new_size = size << 1;
    matrix a(new_size);
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++)
        {
            a.table[i][j] = a11.table[i][j];
            a.table[i][j + size] = a12.table[i][j];
            a.table[i + size][j] = a21.table[i][j];
            a.table[i + size][j + size] = a22.table[i][j];
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

matrix multiplyStrassen(matrix a, matrix b) {
    const int size = a.rows;
    if (size <= LIMIT_WHEN_USE_MULTIPLY) {
        return a * b;
    }
    const int new_size = size >> 1;

    matrix a11(new_size);
    matrix a12(new_size);
    matrix a21(new_size);
    matrix a22(new_size);
    matrix b11(new_size);
    matrix b12(new_size);
    matrix b21(new_size);
    matrix b22(new_size);

    splitMatrix(a, a11, a12, a21, a22);
    splitMatrix(b, b11, b12, b21, b22);

    matrix p1 = multiplyStrassen(a11 + a22, b11 + b22);
    matrix p2 = multiplyStrassen(a21 + a22, b11);
    matrix p3 = multiplyStrassen(a11, b12 - b22);
    matrix p4 = multiplyStrassen(a22, b21 - b11);
    matrix p5 = multiplyStrassen(a11 + a12, b22);
    matrix p6 = multiplyStrassen(a21 - a11, b11 + b12);
    matrix p7 = multiplyStrassen(a12 - a22, b21 + b22);

    matrix c11 = p1 + p4 + p7 - p5;
    matrix c12 = p3 + p5;
    matrix c21 = p2 + p4;
    matrix c22 = p1 - p2 + p3 + p6;

    return collectMatrix(c11, c12, c21, c22);
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

    delete[] a11;
    delete[] a12;
    delete[] a21;
    delete[] a22;
    delete[] b11;
    delete[] b12;
    delete[] b21;
    delete[] b22;
	
    double** c11 = sumOrSubMatrix(sumOrSubMatrix(p1, p4, new_size, '+'), sumOrSubMatrix(p7, p5, new_size, '-'), new_size, '+');
    double** c12 = sumOrSubMatrix(p3, p5, new_size, '+');
    double** c21 = sumOrSubMatrix(p2, p4, new_size, '+');
    double** c22 = sumOrSubMatrix(sumOrSubMatrix(p1, p2, new_size, '-'), sumOrSubMatrix(p3, p6, new_size, '+'), new_size, '+');

    delete[] p1;
    delete[] p2;
    delete[] p3;
    delete[] p4;
    delete[] p5;
    delete[] p6;
    delete[] p7;
	
    return collectMatrix(c11, c12, c21, c22, new_size);
}

//Kolya's part

double** transposeMatrix(double** a, int n) {
    double** a_t = new double* [n];
    for (int i = 0; i < n; i++)
    {
        a_t[i] = new double[n];
        for (int j = 0; j < n; j++)
        {
            a_t[i][j] = a[j][i];
        }
    }
    return a_t;
}

void multiplyNumberOnMatrix(double number, double** a, int size) {
    for (int i = 0; i < size; i++)
    {
        for (int j = 0; j < size; j++)
        {
            a[i][j] *= number;
        }
    }
}

void calculateB(double** e, double** b, int n) {
    double** t = new double* [n];
    for (int i = 0; i < n; i++)
    {
        t[i] = new double[n];
        for (int j = 0; j < n; j++)
        {
            t[i][j] = e[i][j];
        }
        t[i][i]++;
    }

    double** result = multiplyStrassen(t, b, n);

    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < n; j++)
        {
            b[i][j] = result[i][j];
        }
        delete[] t[i];
        delete[] result[i];
    }
    delete[]t;
    delete[]result;
}

void calculateE(double** a, double** b, double** e, int n) {
    double** t = multiplyStrassen(b, a, n);
    multiplyNumberOnMatrix(-1, t, n);

    for (int i = 0; i < n; i++)
    {
        t[i][i]++;
    }
    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < n; j++)
        {
            e[i][j] = t[i][j];
        }
        delete[] t[i];
    }
    delete[]t;
}

bool E_passCriterionOfSmallness(double** e, int n) {
    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < n; j++)
        {
            if (e[i][j] > 0.0001) {
                return true;
            }
        }
    }
    return false;
}

double** reverseMatrixByNewtonsMethod(double** a, int n) {
    double** result = transposeMatrix(a, n);
    double t1 = DBL_MIN, t2 = DBL_MIN, t;
    double current_sum = 0;
    for (int i = 0; i < n; i++)
    {
        current_sum = 0;
        for (int j = 0; j < n; j++)
        {
            current_sum += a[i][j];
        }
        t1 = (t1 < current_sum) ? current_sum : t1;
    }
    for (int i = 0; i < n; i++)
    {
        current_sum = 0;
        for (int j = 0; j < n; j++)
        {
            current_sum += a[j][i];
        }
        t2 = (t2 < current_sum) ? current_sum : t2;
    }
    t = 1 / (t1 * t2);
    multiplyNumberOnMatrix(t, result, n);

    double** e = new double* [n];
    for (int i = 0; i < n; i++)
    {
        e[i] = new double[n];
    }
    do {
        calculateE(a, result, e, n);
        calculateB(e, result, n);
    } while (E_passCriterionOfSmallness(e, n));


    for (int i = 0; i < n; i++)
    {
        delete[] e[i];
    }
    delete[] e;

    return result;
}

int main()
{
    matrix a2(4, 4);
    matrix b2(4, 4);
    for (int i = 0; i < a2.rows; i++)
    {
        for (int j = 0; j < a2.columns; j++)
        {
            a2.table[i][j] = rand()%10+1;
            b2.table[i][j] = rand() % 10 + 1;
        }
    }
	a2.print();
    b2.print();

    a2 *= b2;
	a2.print();
	
    srand(time(nullptr));
    int a_r = 128, a_c = 128, b_r = a_c, b_c = 128;
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
    
    double** b_input = new double* [b_r];
    for (int i = 0; i < b_r; i++)
    {
        b_input[i] = new double[b_c];
        for (int j = 0; j < b_c; j++)
        {
            b_input[i][j] = getRandNumber();
        }
    }

    double** a = addition2SquareMatrix(a_input, n, a_r, a_c);
    double** b = addition2SquareMatrix(b_input, n, b_r, b_c);
    std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
    double** result = multiply(a, b, n);
    std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
    std::chrono::steady_clock::time_point strassen_begin = std::chrono::steady_clock::now();
    double** resultStrassen = multiplyStrassen(a, b, n);
    std::chrono::steady_clock::time_point strassen_end = std::chrono::steady_clock::now();
    

	if (n <= 16)
	{
        cout << "A" << endl;
		printMatrix(a_input, a_r, a_c);
        cout << "B" << endl;
		printMatrix(b_input, b_r, b_c);
        printMatrix(result, a_r, b_c);
        printMatrix(resultStrassen, a_r, b_c);
	}
    cout << boolalpha << twoMatrixEqual(result, resultStrassen, a_r, b_c);
    cout << " " << chrono::duration_cast<std::chrono::milliseconds>(end - begin).count() << "ms";
	cout << " " << chrono::duration_cast<std::chrono::milliseconds>(strassen_end - strassen_begin).count() << "ms";
    delete a_input;
    delete b_input;
    delete a;
    delete b;
    delete result;
    delete resultStrassen;
	return 0;
}