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

/*
 * allocate closure
 *
 * u.closure[0]: body start adr
 * u.closure[1]: body end   adr
 *
 * u.closure[i]: vm_data list (i > 1)
 *
 */
vm_data
gc_alloc_closure(int n, int bodyadr, int ebodyadr, int s)
{
	struct vm_obj *obj;
	int i;
	int size;

	size = sizeof(struct vm_obj) + sizeof(vm_data) * (n+2);
	obj = myalloc(size);
	obj->size          = size;
	obj->tag 	   = VM_OBJ_CLOSURE;
	obj->u.closure     = (char*)obj + sizeof(struct vm_obj);
	obj->u.closure[0]  = bodyadr << 2;
	obj->u.closure[1]  = ebodyadr << 2;

	for(i=0;i<n;i++){
		obj->u.closure[i+2] = INDEX(s,i);
	}

	return ((vm_data)obj) | 3;
}
