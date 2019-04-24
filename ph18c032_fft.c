/* 
 * File: ph18c032_fft.c
 * Description: Program to compute the DFT of given data using FFT algorithm,
 * 		and output the power spectrum.
 * Language: C	(standard: C99)
 * Version: 1.0
 * Author: Shivaprasad V
 * Credentials: PH18C032, M.Sc. Physics '18-'20, IIT Madras
 * Date: 21 Apr 2019
 */

#include <stdio.h>
#include <stdlib.h>	/* for malloc	*/
#include <math.h>	/* for M_PI	*/
#include <complex.h>	/* for complex datatype	*/

/* Declarations */
void fft(double complex *in, double complex *out, unsigned int n, int step);
unsigned long count_bytes(FILE *fp);
unsigned int read_data_lines(char *buf, FILE *fp);

/* main: the name of the data file should be passed as command-line argument */
int main(int argc, char *argv[])
{
	FILE *fp;
	char *buffer;		/* store numerical data	*/
	double tmp, ti, tf;	/* initial & final time	*/
	double complex *x, *y;	/* pointers to data & its DFT */
	int n, i, offset, bytes_read;

	/* Read data from file to buffer, checking errors */
	if (argc < 2) {
		printf("Usage: ./a.out <filename>\n");
		return 1;
	} else if ((fp = fopen(argv[1], "r")) == NULL) {
		printf("%s: no such file!\n", argv[1]);
		return 2;
	}
	buffer = malloc(count_bytes(fp));
	n = read_data_lines(buffer, fp);
	fclose(fp);

	/* Parse buffer and read the numbers into arrays */
	x = malloc(sizeof *x * n);
	y = malloc(sizeof *y * n);
	for (i = 0, offset = bytes_read = 0; i < n; ++i) {
		/* sscanf does not remember the number of bytes read from string,
		 * hence we need to keep track of it manually */
		sscanf(buffer + offset, "%lf %lf\n%n", &tf, &tmp, &bytes_read);
		offset += bytes_read;
		if (i == 0)		/* first time-value */
			ti = tf;
		x[i] = tmp + 0 * I;
		y[i] = 0 + 0 * I;
	}
	free(buffer);

	/* Perform the DFT of x and store it in y */
	fft(x, y, n, 1);
	/* For real input, the second half of the DFT [n / 2, n - 1] is just a
	 * mirror image of the first half [0, n / 2] and  does not carry any
	 * additional information. Hence we print the power spectrum of only the
	 * first half; the harmonics are multiples of 1 / T = 1 / (tf - ti) */
	for (i = 0; i <= n / 2; ++i)
		printf("%lf\t%lf\n", i / (tf - ti), cabs(y[i]));
	free(x);
	free(y);
	return 0;
}

/* fft: compute the discrete Fourier transform (DFT) of the data array in[] and
 * store the result in the array out[], using the Cooley–Tukey FFT (fast Fourier
 * transform) algorithm. Both arrays are assumed to have length <n> which should
 * be a power of 2. <step> is the interval for stepping through the even- & odd-
 * indexed elements of the array during each recursion. */
void fft(double complex *in, double complex *out, unsigned int n, int step)
{
	double complex even_sum, odd_sum;
	int k;

	if (n == 1) {
		*out = *in;		/* trivial size-1 DFT base case */
	} else {
		/* DFTs of even- and odd-indexed parts of in[] */
		fft(in, out, n / 2, 2 * step);
		fft(in + step, out + n / 2, n / 2, 2 * step);
		/* Reordering using "butterflies" */
		for (k = 0; k < n / 2; ++k) {
			even_sum = out[k];
			odd_sum = cexp(-2 * M_PI * I * k / n) * out[k + n / 2];
			out[k] = even_sum + odd_sum;
			out[k + n / 2] = even_sum - odd_sum;
		}
	}
}

/* Count the number of bytes in the file pointed to by <fp> */
unsigned long count_bytes(FILE *fp)
{
	long size;

	fseek(fp, 0, SEEK_END);
	size = ftell(fp);
	rewind(fp);
	return size;
}

/* Read the data points as a string into <buf> from the file pointed to by <fp>
 * ignoring Gnuplot-styled comments (i.e. lines starting with '#'). Returns the
 * number of lines read. */
unsigned int read_data_lines(char *buf, FILE *fp)
{
	unsigned int nl = 0;
	enum { false, true } cmnt = false;
	char c;

	while((c = getc(fp)) != EOF) {
		if (c == '#')
			cmnt = true;
		if (!cmnt)
			*buf++ = c;
		if (c == '\n') {
			if (!cmnt)
				++nl;
			cmnt = false;
		}
	}
	rewind(fp);
	return nl;
}
