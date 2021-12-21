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

int main()
{
	srand(time(nullptr));
	test_strassen();
	return 0;
}
