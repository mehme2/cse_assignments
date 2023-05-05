#include <stdio.h>
#include <math.h>

typedef struct {
	double val[3][3];
} matrix;

typedef struct {
	float x, y, z;
} vector;

typedef struct {
	float a, b, c;
} third_order_polynomial;

void print_matrix(matrix initial_matrix) {
	printf("\n");
	for(int r = 0; r < 3; r++) {
		for(int c = 0; c < 3; c++) {
			printf("%.4f  ", initial_matrix.val[r][c]);
		}
		printf("\n");
	}
}

float determinant_of_matrix(matrix* m) {
	float res = 0;
	res += m->val[0][0] * (m->val[1][1] * m->val[2][2] - m->val[1][2] * m->val[2][1]);
	res -= m->val[0][1] * (m->val[1][0] * m->val[2][2] - m->val[1][2] * m->val[2][0]);
	res += m->val[0][2] * (m->val[1][1] * m->val[2][1] - m->val[1][1] * m->val[2][0]);
	return res;
}

void inverse_matrix(matrix* i, matrix* inv) {
	float det = determinant_of_matrix(i);
	if(det == 0) {
		printf("\nError in inverse_matrix: The matrix is not invertible.\n");
	}
	else {
		float scalar = 1 / (det < 0 ? -det : det);
		inv->val[0][0] = scalar * (i->val[1][1] * i->val[2][2] - i->val[1][2] * i->val[2][1]);
		inv->val[0][1] = -scalar * (i->val[0][1] * i->val[2][2] - i->val[0][2] * i->val[2][1]);
		inv->val[0][2] = scalar * (i->val[0][1] * i->val[1][2] - i->val[0][2] * i->val[1][1]);
		inv->val[1][0] = -scalar * (i->val[1][0] * i->val[2][2] - i->val[1][2] * i->val[2][0]);
		inv->val[1][1] = scalar * (i->val[0][0] * i->val[2][2] - i->val[0][2] * i->val[2][0]);
		inv->val[1][2] = -scalar * (i->val[1][0] * i->val[2][1] - i->val[1][1] * i->val[2][0]);
		inv->val[2][0] = scalar * (i->val[1][0] * i->val[2][1] - i->val[1][1] * i->val[2][0]);
		inv->val[2][1] = scalar * (i->val[0][0] * i->val[2][1] - i->val[0][1] * i->val[2][0]);
		inv->val[2][2] = scalar * (i->val[0][0] * i->val[1][1] - i->val[0][1] * i->val[1][0]);
		printf("\nInverted matrix:\n");
		print_matrix(*inv);
	}
}

double find_orthogonal(vector v1, vector v2, vector* out) {
	out->x = v1.y * v2.z - v1.z * v2.y;
	out->y = v1.z * v2.x - v1.x * v2.z;
	out->z = v1.x * v2.y - v1.y * v2.x;
	return acos((v1.x * v2.x + v1.y * v2.y + v1.z * v2.z)
			/ (sqrt(v1.x * v1.x + v1.y * v1.y + v1.z * v1.z)
			* sqrt(v2.x * v2.x + v2.y * v2.y + v2.z * v2.z)));
}

double get_integral(third_order_polynomial p1, third_order_polynomial p2, int a, int b) {
	return p1.a * p2.a * (pow(b, 7) - pow(a, 7)) / 7
		+ ((p1.a * p2.b + p1.b * p2.a) * (pow(b, 6) - pow(a, 6))) / 6
		+ ((p1.a * p2.c + p1.b * p2.b + p1.c * p2.a) * (pow(b, 5) - pow(a, 5))) / 5
		+ ((p1.b * p2.c + p1.c * p2.b) * (pow(b, 4) - pow(a, 4))) / 4
		+ ((p1.c * p2.c) * (pow(b, 3) - pow(a, 3))) / 3;
}

int main() {
	matrix m = {{{2, 3, 4}, {-3, -3, -2}, {-2, 1, -1}}};
	matrix inv;
	print_matrix(m);
	inverse_matrix(&m, &inv);
	third_order_polynomial p1, p2;
	vector v1, v2, out;
	double angle;
	printf("\nEnter x of v1: ");
	scanf("%f", &v1.x);
	printf("\nEnter y of v1: ");
	scanf("%f", &v1.y);
	printf("\nEnter z of v1: ");
	scanf("%f", &v1.z);
	printf("\nEnter x of v2: ");
	scanf("%f", &v2.x);
	printf("\nEnter y of v2: ");
	scanf("%f", &v2.y);
	printf("\nEnter z of v2: ");
	scanf("%f", &v2.z);
	angle = find_orthogonal(v1, v2, &out);
	printf("\nresult: angle %f x: %f y: %f z: %f\n", angle, out.x, out.y, out.z);
	int a, b;
	printf("\nEnter a of p1: ");
	scanf("%f", &p1.a);
	printf("\nEnter b of p1: ");
	scanf("%f", &p1.b);
	printf("\nEnter c of p1: ");
	scanf("%f", &p1.c);
	printf("\nEnter a of p2: ");
	scanf("%f", &p2.a);
	printf("\nEnter b of p2: ");
	scanf("%f", &p2.b);
	printf("\nEnter c of p2: ");
	scanf("%f", &p2.c);
	printf("\nEnter interval a b: ");
	scanf("%d%d", &a, &b);
	printf("\nIntegral of p1 * p2 at interval a b: %.2f\n", get_integral(p1, p2, a, b));
}
