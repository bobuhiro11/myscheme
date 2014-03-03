#include "common.h"

char pool[POOL_MAX];
int free_p = 0;

void *
myalloc(size_t s)
{
	char *rc = pool + free_p;
//	printf("[free_p: %d]\n",free_p);
	free_p += s;
	return rc;
}
