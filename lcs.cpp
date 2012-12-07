/** Dynamic programming.
 *  Implementation of the Longest Common Subsequence (LCS) problem.
 *
 *  Tomas Pllaha
 * 	November 2012 
 * */

#include <iostream>
#include <set>
#include <cstring>
#include <cstdlib>
using namespace std;

/** This function implements the simple dynamic algorithm for the lcs problem.
 *  it prints intermediate results (2 tables) in cerr. and the output in stdout.
 * */
 

char* LCS(char* X, char* Y, int m, int n, char* lcs) {
	// in each square of matrixC, we will save the length of the lcs 
	// after investigating i characters from one sequence and j characters
	// from the other
	// in each square of matrixB, we will save the direction in which
	// we need to walk (starting from matrixB[m][n]) to construct the
	// lcs (u stands for up, l for left and d for diagonal (up+left)
	int matrixC[m+1][n+1];
	char matrixB[m+1][n+1];
	int i;
	int j;
	// after investigating 0 characters from each sequence, the temporary
	// lcs will obviously have length 0.
	for (i = 1; i <= m; i++) {
		matrixC[i][0] = 0;
		matrixB[i][0] = '0';
	}
	for (j = 0; j <= n; j++) {
		matrixC[0][j] = 0;
		matrixB[0][j] = '0';
	}
	// Construct matrixC and matrixB
	for (i = 1; i <= m; i++) {
		
		for (j = 1; j <= n; j++) {
			if (X[i-1] == Y[j-1]) { // when they're the same
				matrixC[i][j] = matrixC[i-1][j-1] + 1;
				matrixB[i][j] = 'd'; // diagonal
			}
			else {
				if (matrixC[i-1][j] >= matrixC[i][j-1]) {
					matrixC[i][j] = matrixC[i-1][j];
					matrixB[i][j] = 'u'; // up
				}
				else {
					matrixC[i][j] = matrixC[i][j-1];
					matrixB[i][j] = 'l'; // left
				}
			}
		}
	}
	// print temporary tables in cerr
	for (i = 0; i <=m ; i++) {
		for (j = 0; j <= n; j++) {
			cerr << matrixC[i][j] << " ";
		}
		cerr << endl;
	}
		cerr << endl;
	for (i = 0; i <=m ; i++) {
		for (j = 0; j <= n; j++) {
			cerr << matrixB[i][j] << " ";
		}
		cerr << endl;
	}
	int size = matrixC[m][n];
	lcs = new char[size+1];
	
	lcs[size] = '\0';
	int a = 0;
	// construct the lcs
	for (i = m, j = n; i > 0 && j > 0;) {
			if (matrixB[i][j] == 'd') {
				lcs[size-a-1] = Y[j-1];
				a++;
				i--;
				j--;
			}
			else {
				if (matrixB[i][j] == 'u') {
					i--;
				}
				else {
					j--;
				}
			}
	}
	//return
	return lcs;
}

	 		
int main(int num, char** args) {
	char* X;
	char* Y;
	int m;
	int n;
	char* lcs = new char;
	char* ptr;
	cout << "Please enter the length of the first sequence: ";
	cin >> m;
	X = new char[m+1];
	cout << "\nPlease enter the length of the second sequence: ";
	cin >> n;
	Y = new char[n+1];
	cout << "\nPlease enter the first sequence, as a string (no spaces): ";
	cin >> X;
	cout << "\nPlease enter the second sequence as a string (no spaces): ";
	cin >> Y;
	X[m] = '\0';
	Y[n] = '\0';
	ptr = LCS(X, Y, m, n, lcs);
	cout << "The longest common sub-sequence is \"" << ptr <<"\"." << endl;
	delete X;
	delete Y;
	delete lcs;	
	delete ptr;
	return 0; 
}

