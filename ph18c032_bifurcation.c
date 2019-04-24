#include <stdio.h>
#include <math.h>
#define EPS 1E-6

enum bool { false, true };

/* Logistic map */
double log_map(double mu, double xi)
{
	return mu * xi * (1 - xi);
}

/* Check whether population already exists, to avoid large number of
 * duplicates */
enum bool exists(double p, double pops[], size_t len)
{
	int i;

	for (i = 0; i < len && pops[i] != -1; ++i) {
		if (fabs(pops[i] - p) < EPS)
			return true;
	}
	return false;
}

int main()
{
	const double seed_pop = 0.75, h = 0.0001;	/* step size for mu */
	const int num_of_gens = 100;
	double growth_rate, p, pops[num_of_gens] = { -1 };
	int i, j, r, n;

	printf("# Growth rate\tStable populations\n");
	n = round(3 / h);		/* (b - a) / h = (4 - 1) / h */
	for (r = 0; r <= n; ++r) {
		pops[0] = seed_pop;	/* start with seed population */
		growth_rate = 1.0 + r * h;	/* a + r * h */

		/* Discard the first 10N generations to avoid transients */
		for (i = 0; i < 10 * num_of_gens; ++i)
			pops[0] = log_map(growth_rate, pops[0]);
		for (i = 0; i < num_of_gens - 1;) {
			p = log_map(growth_rate, pops[i]);
			/* Cycle will repeat if duplicate values are encountered */
			if (exists(p, pops, num_of_gens))
				break;
			pops[++i] = p;
		}
		/* Print output */
		for (j = 0; j <= i; ++j)
			printf("%lf\t%lf\n", growth_rate, pops[j]);
	}
	return 0;
}
