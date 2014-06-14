#include <stdio.h>

int m[100][100];
int i, j, N;

int main(){
	
	scanf("%d", &N);

	for(i = 0; i < N; i++)
		for(j = 0; j < N; j++)
			scanf("%d", &m[i][j]);
	printf("Array(");
	for(i = 0; i < N - 1; i++){
		printf("Array(%d", m[i][0]);
		for(j = 1; j < N; j++)
			printf(",%d", m[i][j]);
		printf("), ");	
	}
	printf("Array(%d", m[N - 1][0]);
	for(j = 1; j < N; j++)
		printf(",%d", m[i][j]);
	printf("))");	

	return 0;
}