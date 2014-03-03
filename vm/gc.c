#include "common.h"

char *pool;
size_t free_p;

void
gc_init()
{
	pool = malloc(POOL_MAX);
	free_p = 0;
}

struct vm_obj *
myalloc(size_t s)
{
	struct vm_obj *rc = (char*)pool + free_p;
	int size;

	/* alignment */
	int p = sizeof(struct vm_obj);
	size = ((s+p-1) / p ) * p;
	free_p += size;

	/* set size */
	rc->size = size;

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
	obj->tag 	   = VM_OBJ_CLOSURE;
	obj->u.closure     = (char*)obj + sizeof(struct vm_obj);
	obj->u.closure[0]  = bodyadr << 2;
	obj->u.closure[1]  = ebodyadr << 2;

	for(i=0;i<n;i++){
		obj->u.closure[i+2] = INDEX(s,i);
	}

	return ((vm_data)obj) | 3;
}

vm_data
gc_alloc_string(char *str)
{
	struct vm_obj *obj;
	int size;

	size = sizeof(struct vm_obj) + strlen(str) +1;
	obj = myalloc(size);
	obj->tag = VM_OBJ_STRING;
	obj->u.string = (char*)obj + sizeof(struct vm_obj);
	strcpy(obj->u.string,str);

	return ((vm_data)obj) | 3;
}

vm_data
gc_alloc_symbol(char *str)
{
	struct vm_obj *obj;
	int size;

	size = sizeof(struct vm_obj) + strlen(str) +1;
	obj = myalloc(size);
	obj->tag = VM_OBJ_SYMBOL;
	obj->u.string = (char*)obj + sizeof(struct vm_obj);
	strcpy(obj->u.string,str);

	return ((vm_data)obj) | 3;
}
