#include <chrono>
#include <iostream>
using namespace std;

const int LIMIT_WHEN_USE_MULTIPLY = 64;
const double EPSILON = 1e-6;

class Matrix {
public:
	int rows;
	int columns;
	double** table;
	Matrix() {}
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
	Matrix operator=(const Matrix& other) {
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
	Matrix operator+(const Matrix& other) {
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
	Matrix operator-(const Matrix& other) {
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
	Matrix operator*(const Matrix& other) {
		Matrix result(other.rows, other.columns);

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
	Matrix operator*(double number) {
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
	Matrix transpose() {
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
	void print() {
		for (int i = 0; i < this->rows; i++)
		{
			for (int j = 0; j < this->columns; j++)
			{
				cout << this->table[i][j] << " ";
			}
			cout << "\n";
		}
	}
	void randomizer() {
		
		for (int i = 0; i < this->rows; i++)
		{
			for (int j = 0; j < this->columns; j++)
			{
				this->table[i][j] = rand() % 100;
			}
		}
	}
};
Matrix identity_matrix(int n) {
	Matrix result(n, n);
	for (int i = 0; i < n; i++)
	{
			result.table[i][i] = 1;
	}
	return result;
}

bool twoMatrixEqual(double** a, double** b, int r, int c)
{
	for (int i = 0; i < r; i++)
	{
		for (int j = 0; j < c; j++)
		{
			if (fabs(a[i][j] - b[i][j]) > EPSILON)
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
	double** result = new double* [size];
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
	double** a = new double* [new_size];
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

//םו ענובא גזו
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
//םו ענובא
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
		/*for (int i = 0; i < n; i++)
		{
			for (int j = 0; j < n; j++)
			{
				cout << result[i][j] << " ";
			}
			cout << endl;
		}
		cout << endl;*/
	} while (E_passCriterionOfSmallness(e, n));


	for (int i = 0; i < n; i++)
	{
		delete[] e[i];
	}
	delete[] e;

	return result;
}

bool E_passCriterionOfSmallness(Matrix e) {
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
	cout << t << endl;
	Matrix B = A.transpose() * t;
	Matrix E(A.rows, A.columns);
	Matrix I = identity_matrix(A.columns);
	do {
		E = I - B * A;
		B = (I + E) * B;
		
	} while (E_passCriterionOfSmallness(E));

	return B;
}

int main()
{
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