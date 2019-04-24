#include <stdio.h>
#include <math.h>
#define EPS 1E-14	/* small number close to machine precision */
#define LEN(array) (sizeof(array) / sizeof(array[0]))

/* Logistic map */
double log_map(double mu, double xi)
{
	return mu * xi * (1 - xi);
}

int main()
{
	/* Various seed populations and growth rates (given in question) */
	const double seed_pops[] = { 0.25, 0.5, 0.75, 0.8, 0.75 * (1 + EPS)};
	const double growth_rates[] = { 
		0.4, 2.4, 3.2, 3.6, 3.8304, 1 + sqrt(8) + 1E-6, 3.8284, 4, 4 * (1 - EPS)
	};
	const int num_of_gens = 1000;
	double pops[LEN(seed_pops)];
	int i, j, k;

	/* Loop through mu values */
	for (j = 0; j < LEN(growth_rates); ++j) {
		printf("# Growth rate = %.4f\n", growth_rates[j]);
		printf("# Number of generations\tPopulations\n");
		for (i = 1; i <= num_of_gens; ++i) {
			printf("%d", i);
			/* Loop through different seeds */
			for (k = 0; k < LEN(seed_pops); ++k) {
				if (i == 1)	/* seed generation */
					pops[k] = seed_pops[k];
				printf("\t%lf", pops[k]);
				pops[k] = log_map(growth_rates[j], pops[k]);
			}
			printf("\n");
		}
		printf("\n\n");
	}
	return 0;
}
