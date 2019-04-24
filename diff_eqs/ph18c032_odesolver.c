/* File: ph18c032_odesolver.c
   Description: Program to solve ODE using three methods, and estimate errors.
   Language: C
   Version: 1.0
   Author: Shivaprasad V
   Credentials: PH18C032, M.Sc. Physics '18-'20, IIT Madras
   Date: 08 Apr 2019
*/
#include <stdio.h>
#include <math.h>
#include <unistd.h>	/* for sleep() */
#define N_MAX 1000

/* Function pointers */
typedef double (*func_ptr)(double, double);
typedef double (*algo_ptr)(func_ptr, double, double, double);

/* Function to solve the ODE using given algorithm */
double odesolve(algo_ptr algo, func_ptr f, double y0, double a, double b, int n,
		FILE *fp)
{
	int i;
	double x, y, h;

	h = (b - a) / n;
	for (y = y0, x = a, i = 0; i < n; ++i, x += h) {
		if (fp) fprintf(fp, "%lf\t%lf\n", x, y);
		y = algo(f, x, y, h);
	}
	return y;
}

/* Algorithms for calculating a single step */
double euler(func_ptr f, double xn, double yn, double h)
{
	return yn + h * f(xn, yn);
}

double rk2(func_ptr f, double xn, double yn, double h)
{
	double k1, k2;

	k1 = h * f(xn, yn);
	k2 = h * f(xn + h, yn + k1);
	return yn + (k1 + k2) / 2;
}

double rk4(func_ptr f, double xn, double yn, double h)
{
	double k1, k2, k3, k4;

	k1 = h * f(xn, yn);
	k2 = h * f(xn + h / 2, yn + k1 / 2);
	k3 = h * f(xn + h / 2, yn + k2 / 2);
	k4 = h * f(xn + h, yn + k3);
	return yn + (k1 + 2 * k2 + 2 * k3 + k4) / 6;
}

/* dy/dx = f(x, y), the function to solve */
double f(double x, double y)
{
	return -x * y;
}

int main()
{
	algo_ptr algos[] = { euler, rk2, rk4 };
	char *algo_names[] = {  "Euler method",
				"Runge-Kutta 2nd order",
				"Runge-Kutta 4th order" };
	int N = 100, i;
	char fname[30];
	FILE *fp;
	double y0 = 1, a = 0, b = 5, y, err;
	double y_actual = exp(-b * b / 2);	/* Analytical solution */

	printf(	"Gaussian Distribution\n=====================\n\n"
		"Solving the governing equation: dy/dx = -xy\n"
		"In the interval: (%.1lf, %.1lf)\nSubject to the initial "
		"condition: y(x = %.1lf) = %.1lf\n", a, b, a, y0);
	printf(	"Solutions output file name: " );
	scanf("%s", fname);
	printf(	"Writing..." );
	fflush(stdout);

	/* Solve for N = 100 using 3 methods */
	fp = fopen(fname, "w");
	fprintf(fp, "# Solution of the equation dy/dx = -xy using:\n");
	for (i = 0; i < 3; ++i) {
		fprintf(fp, "# %d) %s\n", i + 1, algo_names[i]);
		odesolve(algos[i], f, y0, a, b, N, fp);
		fprintf(fp, "\n\n");
	}
	fclose(fp);
	sleep(1);
	printf("done\n");

	printf(	"Errors output file name: " );
	scanf("%s", fname);
	printf(	"Writing..." );
	fflush(stdout);

	/* Solve for different N's */
	fp = fopen(fname, "w");
	fprintf(fp, "# Relative error in y(x = 5) while solving dy/dx = -xy\n");
	fprintf(fp, "N\t\"%s\"\t\"%s\"\t\"%s\"\n", algo_names[0], algo_names[1],
		algo_names[2]);
	for (N = 100; N <= N_MAX; N += 50) {
		fprintf(fp, "%d\t", N);
		for (i = 0; i < 3; ++i) {
			y = odesolve(algos[i], f, y0, a, b, N, NULL);
			err = fabs(y - y_actual) / y_actual;
			fprintf(fp, "%.15lf\t", err);
		}
		fprintf(fp, "\n");
	}
	fclose(fp);
	sleep(1);
	printf("done\n");

	return 0;
}
