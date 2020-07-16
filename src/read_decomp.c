#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "mpi.h"

void free_element_decomp(int **elems);
void read_element_decomp(MPI_Fint fcomm, const char *filename, int *n_elems_global_out, int *n_elems_out, int **elems_out);

void read_element_decomp(MPI_Fint fcomm, const char *filename, int *n_elems_global_out, int *n_elems_out, int **elems_out)
{
	int i, j;
	int ierr;
	int comm_size, comm_rank;
	int nelems_local, nelems_total;
	int *nelems = NULL;
	int *elem_start = NULL;
	int *elems = NULL;
	int *elems_local = NULL;
	FILE *decomp = NULL;
	MPI_Comm comm;

	comm = MPI_Comm_f2c(fcomm);
	
	ierr = MPI_Comm_size(comm, &comm_size);
	ierr = MPI_Comm_rank(comm, &comm_rank);

	if (comm_rank == 0) {
		/*
		 * Allocate array of size comm_size to hold the element counts for each task
		 */
		nelems = (int *)malloc(sizeof(int) * (size_t)comm_size);
		elem_start = (int *)malloc(sizeof(int) * (size_t)comm_size);
		bzero((void *)nelems, sizeof(int) * (size_t)comm_size);

		decomp = fopen(filename, "r");


		/*
		 * Count how many elements each task is assigned
		 */
		nelems_total = 0;
		while (fscanf(decomp, "%i", &i) > 0) {
			if (i >= comm_size) {
				fprintf(stderr, "Partition %i is above comm_size=%i\n", i, comm_size);
			}
			else {
				nelems[i]++;
				nelems_total++;
			}
		}

		*n_elems_global_out = nelems_total;
	}

	ierr = MPI_Barrier(comm);

	ierr = MPI_Bcast((void *)n_elems_global_out, 1, MPI_INT, 0, comm);
	
	/*
	 * Communicate number of elements to all tasks
	 */
	ierr = MPI_Scatter((const void *)nelems, 1, MPI_INT, (void *)&nelems_local, 1, MPI_INT, 0, comm);

	elems_local = (int *)malloc(sizeof(int) * (size_t)nelems_local);

	if (comm_rank == 0) {
		/*
		 * Rewind the file
		 */
		rewind(decomp);

		/*
		 * Allocate global list of elements
		 */
		elems = (int *)malloc(sizeof(int) * (size_t)nelems_total);

		/*
		 * Find starting point in global list of elements for each task
		 */
		elem_start[0] = 0;
		for (i=1; i<comm_size; i++) {
			elem_start[i] = elem_start[i-1] + nelems[i-1];
		}

		/*
		 * Read global list of elements for each task
		 */
		j = 0;
		while (fscanf(decomp, "%i", &i) > 0) {
			if (i >= comm_size) {
				fprintf(stderr, "Partition %i is above comm_size=%i\n", i, comm_size);
			}
			else {
				elems[elem_start[i]++] = j;
			}
			j++;
		}

		for (i=0; i<comm_size; i++) {
			elem_start[i] -= nelems[i];
		}

		fclose(decomp);
	}

	ierr = MPI_Scatterv((const void *)elems, (const int *)nelems, (const int *)elem_start,
			MPI_INT, (void *)elems_local, nelems_local,
			MPI_INT, 0, comm);

	if (comm_rank == 0) {
		free(nelems);
		free(elem_start);
		free(elems);
	}

	*n_elems_out = nelems_local;
	*elems_out = elems_local;
}

void free_element_decomp(int **elems)
{
	free(*elems);
	*elems = NULL;
}
