#include <chrono>
#include <iostream>
#include <cmath>
#include <cfloat>
#include <limits>

using namespace std;

const int LIMIT_WHEN_USE_MULTIPLY = 64;
const double EPSILON = 1e-6;

double getRandNumber()
{
	return rand() % 10;
	//return (rand() % 100 + 1) / 10.0;
}

class Matrix {
public:
	int rows;
	int columns;
	double** table;
	Matrix(): rows(0), columns(0), table(nullptr)
	{
	}

	~Matrix() {
		for (int i = 0; i < this->rows; i++)
		{
			delete[] this->table[i];
		}
		delete[] this->table;
	}
	Matrix(int rows, int columns) {
		this->columns = columns;
		this->rows = rows;
		this->table = new double* [this->rows];
		for (int i = 0; i < this->rows; i++)
		{
			this->table[i] = new double[this->columns];
			for (int j = 0; j < this->columns; j++)
			{
				this->table[i][j] = 0;
			}
		}
	}
	Matrix(int size) {
		this->columns = size;
		this->rows = size;
		this->table = new double* [this->rows];
		for (int i = 0; i < this->rows; i++)
		{
			this->table[i] = new double[this->columns];
			for (int j = 0; j < this->columns; j++)
			{
				this->table[i][j] = 0;
			}
		}
	}
	Matrix(const Matrix& other) {
		this->columns = other.columns;
		this->rows = other.rows;
		this->table = new double* [this->rows];
		for (int i = 0; i < this->rows; i++)
		{
			this->table[i] = new double[this->columns];
			for (int j = 0; j < this->columns; j++)
			{
				this->table[i][j] = other.table[i][j];
			}
		}

	}
	Matrix& operator=(const Matrix& other) {
		this->rows = other.rows;
		this->columns = other.columns;
		this->table = new double* [this->rows];


		for (int i = 0; i < this->rows; i++)
		{
			this->table[i] = new double[this->columns];
			for (int j = 0; j < this->columns; j++)
			{
				this->table[i][j] = other.table[i][j];
			}
		}
		return *this;
	}
	double* operator[](std::size_t i) 
	{
		return table[i];
	}
	Matrix operator+(const Matrix& other) const
	{
		Matrix result(other.rows, other.columns);
		for (int i = 0; i < this->rows; i++)
		{
			for (int j = 0; j < this->columns; j++)
			{
				result.table[i][j] = this->table[i][j] + other.table[i][j];
			}
		}
		return result;
	}
	Matrix operator-(const Matrix& other) const
	{
		Matrix result(other.rows, other.columns);
		for (int i = 0; i < this->rows; i++)
		{
			for (int j = 0; j < this->columns; j++)
			{
				result.table[i][j] = this->table[i][j] - other.table[i][j];
			}
		}
		return result;
	}
	Matrix operator*(const Matrix& other) const
	{
		Matrix result(this->rows, other.columns);

		for (int i = 0; i < this->rows; i++)
		{
			for (int j = 0; j < other.columns; j++)
			{
				for (int k = 0; k < other.rows; k++)
				{
					result.table[i][j] += this->table[i][k] * other.table[k][j];
				}
			}
		}
		return result;
	}
	Matrix operator*(double number) const
	{
		Matrix result(this->rows, this->columns);
		for (int i = 0; i < this->rows; i++)
		{
			for (int j = 0; j < this->columns; j++)
			{
				result.table[i][j] = this->table[i][j] * number;
			}
		}
		return result;
	}
	bool operator==(const Matrix& other) const {
		if (this->columns != other.columns || this->rows != other.rows)
		{
			return false;
		}
		
		for (int i = 0; i < this->rows; i++)
		{
			for (int j = 0; j < this->columns; j++)
			{
				if (fabs(this->table[i][j] - other.table[i][j]) > EPSILON)
				{
					return false;
				}
			}
		}
		return true;
	}
	
	Matrix transpose() const
	{
		Matrix result(this->columns, this->rows);
		for (int i = 0; i < this->columns; i++)
		{
			for (int j = 0; j < this->rows; j++)
			{
				result.table[i][j] = this->table[j][i];
			}
		}
		return result;
	}
	void randomizer() {
		
		for (int i = 0; i < this->rows; i++)
		{
			for (int j = 0; j < this->columns; j++)
			{
				this->table[i][j] = getRandNumber();
			}
		}
	}
	Matrix resize(const int new_rows, const int new_columns) const
	{
		Matrix result(new_rows, new_columns);
		for (int i = 0; i < new_rows; i++)
		{
			for (int j = 0; j < new_columns; j++)
			{
				if (i < this->rows && j < this->columns)
				{
					result.table[i][j] = this->table[i][j];
				}
			}
		}

		return result;
	}
};
ostream& operator<<(ostream& os, const Matrix& matrix)
{
	for (int i = 0; i < matrix.rows; i++)
	{
		for (int j = 0; j < matrix.columns; j++)
		{
			os << matrix.table[i][j] << " ";
		}
		os << "\n";
	}
	return os;
}
Matrix identity_matrix(int n) {
	Matrix result(n, n);
	for (int i = 0; i < n; i++)
	{
			result.table[i][i] = 1;
	}
	return result;
}


int log2(int x) {
	if (x < 1) return NAN;
	if (x == 1) return 0;
	int result = 1;
	x--;
	while ((x >>= 1) != 0) {
		result++;
	}
	return result;
}

int getNewDimension(int asize_r, int asize_c, int bsize_r, int bsize_c)
{
	return 1 << log2(max(max(asize_r, asize_c), max(bsize_r, bsize_c)));
}

void splitMatrix(const Matrix& a, Matrix& a11, Matrix& a12, Matrix& a21, Matrix& a22) {
	const int size = a.columns;
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

Matrix collectMatrix(const Matrix& a11, const Matrix& a12, const Matrix& a21, const Matrix& a22) {
	const int size = a11.columns;
	const int new_size = size << 1;
	Matrix a(new_size);

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


Matrix multiplyStrassen(const Matrix& a, const Matrix& b) {
	const int size = getNewDimension(a.rows, a.columns, b.rows, b.columns);
	if (size <= LIMIT_WHEN_USE_MULTIPLY) {
		return a * b;
	}
	const int a_r = a.rows;
	const int b_c = b.columns;
	Matrix ra = a.resize(size, size);
	Matrix rb = b.resize(size, size);
	
	const int new_size = size >> 1;

	Matrix a11(new_size);
	Matrix a12(new_size);
	Matrix a21(new_size);
	Matrix a22(new_size);
	Matrix b11(new_size);
	Matrix b12(new_size);
	Matrix b21(new_size);
	Matrix b22(new_size);

	splitMatrix(ra, a11, a12, a21, a22);
	splitMatrix(rb, b11, b12, b21, b22);

	Matrix p1 = multiplyStrassen(a11 + a22, b11 + b22);
	Matrix p2 = multiplyStrassen(a21 + a22, b11);
	Matrix p3 = multiplyStrassen(a11, b12 - b22);
	Matrix p4 = multiplyStrassen(a22, b21 - b11);
	Matrix p5 = multiplyStrassen(a11 + a12, b22);
	Matrix p6 = multiplyStrassen(a21 - a11, b11 + b12);
	Matrix p7 = multiplyStrassen(a12 - a22, b21 + b22);

	Matrix c11 = p1 + p4 + p7 - p5;
	Matrix c12 = p3 + p5;
	Matrix c21 = p2 + p4;
	Matrix c22 = p1 - p2 + p3 + p6;

	Matrix result = collectMatrix(c11, c12, c21, c22);
	result = result.resize(a_r, b_c);
	
	return result;
}

//Kolya's part

bool E_passCriterionOfSmallness(const Matrix& e) {
	for (int i = 0; i < e.rows; i++)
	{
		for (int j = 0; j < e.columns; j++)
		{
			if (e.table[i][j] > 0.0001) {
				return true;
			}
		}
	}
	return false;
}

Matrix reverseMatrixByNewtonsMethod(Matrix A) {
	double t1 = DBL_MIN, t2 = DBL_MIN, t;
	double current_sum = 0;
	for (int i = 0; i < A.rows; i++)
	{
		current_sum = 0;
		for (int j = 0; j < A.columns; j++)
		{
			current_sum += A.table[i][j];
		}
		t1 = (t1 < current_sum) ? current_sum : t1;
	}
	for (int i = 0; i < A.rows; i++)
	{
		current_sum = 0;
		for (int j = 0; j < A.columns; j++)
		{
			current_sum += A.table[j][i];
		}
		t2 = (t2 < current_sum) ? current_sum : t2;
	}
	t = 1 / (t1 * t2);
	Matrix B = A.transpose() * t;
	Matrix E(A.rows, A.columns);
	Matrix I = identity_matrix(A.columns);
	do {
		E = I - B * A;
		B = (I + E) * B;
		
	} while (E_passCriterionOfSmallness(E));

	return B;
}

void test_strassen()
{
	int a_r = 512, a_c = 511, b_r = a_c, b_c = 512;
	cout << "--------- testing strassen ---------\n";
	
	Matrix a(a_r, a_c);
	a.randomizer();

	Matrix b(b_r, b_c);
	b.randomizer();

	std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
	Matrix result = a * b;
	std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
	std::chrono::steady_clock::time_point strassen_begin = std::chrono::steady_clock::now();
	Matrix resultStrassen = multiplyStrassen(a, b);
	std::chrono::steady_clock::time_point strassen_end = std::chrono::steady_clock::now();


	if (a_r <= 16)
	{
		cout << "A" << endl << a;
		cout << "B" << endl << b;
		cout << result << endl;
		cout << resultStrassen << endl;
	}
	cout << "Is result correct: " << boolalpha << (result == resultStrassen) << "\n";
	cout << "Multiplication time: " << chrono::duration_cast<std::chrono::milliseconds>(end - begin).count() << "ms\n";
	cout << "Strassen multiplication time: " << chrono::duration_cast<std::chrono::milliseconds>(strassen_end - strassen_begin).count() << "ms\n";
	cout << "--------- end of testing strassen ---------\n\n";
}

//Linear Least Squares by Paul Koba

/**
 * x - MxN matrix, where x[i][j] is value of j-th variable during i-th experiment
 * y - Mx1 matrix, where y[i][0] is result of i-th experiment
 *
 * Return value: Nx1 matrix of calculated coefficients.
 */
Matrix ordinaryLeastSquares(const Matrix& x, const Matrix& y) 
{
	if(x.rows != y.rows)
		throw invalid_argument("Invalid matrix dimensions1");

	if(x.rows < x.columns)
		throw invalid_argument("Expected more trials than variables");

	Matrix transposed = x.transpose();
	Matrix product = multiplyStrassen(transposed, x);
	Matrix inverse = reverseMatrixByNewtonsMethod(product);

	return multiplyStrassen(inverse, transposed) * y;
}

/**
 * x - MxN matrix, where x[i][j] is value of j-th variable during i-th experiment
 * y - Mx1 matrix, where y[i][0] is result of i-th experiment
 * coeffs - Nx1 matrix, where coeffs[i][0] is the i-th coefficient
 *
 * Return value: mean squared error
 */
double meanSquaredError(const Matrix& x, const Matrix& y, const Matrix& coeffs)
{
	if(x.rows != y.rows || x.columns != coeffs.rows)
		throw invalid_argument("Invalid matrix dimensions");

	int m = x.rows;
	int n = x.columns;

    double result = 0.;

    for(int i = 0; i < m; ++i) {
        double current = 0.;
        for(int j = 0; j < n; ++j) {
            current += x.table[i][j] * coeffs.table[j][0];
        }
        result += pow(current - y.table[i][0], 2);
    }

    return result / m;
}

void testRegression() 
{
	cout << "--------- testing regression ---------\n";
	const int trials = 100;

	Matrix x(4, 3);
	Matrix y(4, 1);
	Matrix z(3, 1);

	x.randomizer();
	y.randomizer();

	z = ordinaryLeastSquares(x, y);

	cout << "Matrix X:\n"<< x << "Matrix Y:\n" << y << "\n";
	
	cout << "Calculated coefficients:\n" << z << "\n";
	cout << "Calculated mean squared error: " << meanSquaredError(x, y, z) << "\n";

	double r = numeric_limits<double>::max();

	for(int i = 0; i < trials; ++i) {
		z.randomizer();
		r = min(r, meanSquaredError(x, y, z));
	}

	cout << "Minimal mean squared error after " << trials << " trials with random coeffs: " << r << "\n";
	cout << "--------- end of testing regression ---------\n\n";
}

//finding reversed matrix using Gauss-Jordan method by Tochoniy Volodymyr

void swapRaws(double* &x, double* &y) {
	double* temp = x;
	x = y;
	y = temp;
}

Matrix getReversedMatrixByGaussMethod(Matrix a) {
	if (a.rows != a.columns) {
		cout << "Matrix has to be square\n";
		return Matrix(0);
	}

	//initialization
	int n = a.rows;
	Matrix result(n);
	for (int i = 0; i < n; i++) 
		result.table[i][i] = 1;

	//forward
	for (int i = 0; i < n; i++) {
		int nonZeroElemIndex = i;
		while (nonZeroElemIndex < n && a.table[nonZeroElemIndex][i] == 0) nonZeroElemIndex++;
		if (nonZeroElemIndex == n) {  //determinator == 0
			result = result.resize(0, 0);
			cout << "determinator is zero\n";
			return result;
		}
		swapRaws(a.table[i], a.table[nonZeroElemIndex]);  //looking for non zero element

		double d = a.table[i][i];
		for (int j = 0; j < n; j++) a.table[i][j] /= d, result.table[i][j] /= d;
		for (int r = i + 1; r < n; r++) {
			double k = a.table[r][i];
			for (int j = 0; j < n; j++) a.table[r][j] -= a.table[i][j] * k, result.table[r][j] -= result.table[i][j] * k;
		}
	}

	//backward
	for (int i = n - 1; i >= 1; i--) {
		for (int r = i - 1; r >= 0; r--) {
			double k = a.table[r][i];
			for (int j = 0; j < n; j++) a.table[r][j] -= a.table[i][j] * k, result.table[r][j] -= result.table[i][j] * k;
		}
	}

	return result;
}

void checkerOfGaussMethod(int size) {
	cout << "--------- testing gauss method ---------\n";
	Matrix a = Matrix(size);
	a.randomizer();
	cout << "Matrix A:\n";
	cout << a;
	cout << "Matrix A^-1:\n";
	cout << getReversedMatrixByGaussMethod(a);
	cout << "--------- end of testing gauss method ---------\n\n";
}

int main()
{
	srand(time(nullptr));
	test_strassen();
	testRegression();
	checkerOfGaussMethod(3);
	return 0;
}
