#include "common.h"

static char *from_start;
static char *from_free;
static char *to_start;
static char *to_free;
static vm_data root_reg;

void
gc_dump()
{
	printf("=== gc ===\n");
	printf("from_start: %018p\n", from_start);
	printf("from_free:  %018p\n", from_free);
	printf("to_start:   %018p\n", to_start);
	printf("to_free:    %018p\n", to_free);
}

void
gc_init()
{
	from_free = from_start = malloc(POOL_MAX);
	to_free = to_start = malloc(POOL_MAX);
}

struct vm_obj *
myalloc(size_t s)
{
	struct vm_obj *rc;
	int size;

	/* alignment */
	int p = sizeof(struct vm_obj);
	size = ((s+p-1) / p ) * p;

	/* GC */
	if(from_free + size > from_start + POOL_MAX){
		copying(&root_reg);
		gc_dump();
		if(from_free + size > from_start + POOL_MAX){
			fprintf(stderr, "Error: cannot allocate.\n");
			exit(1);
		}
	}

	rc = from_free;
	from_free += size;

	/* set size */
	rc->size = size;
	rc->forwarding = 0;

	return rc;
}

/*
 * return true if already copied
 */
int
is_pointer_to_heap(struct obj *forwarding)
{
	if(to_start <= forwarding && forwarding <= (char*)to_start + POOL_MAX)
		return 1;
	else
		return 0;
}

void
copy_data(struct obj *to_adr, struct obj *from_adr, int size)
{
	int i;

	for(i=0;i<size;i++){
		*((char*)to_adr + i) =  *((char*)from_adr + i);
	}
	return;
}

struct vm_obj *
copy(struct vm_obj *obj)
{
	if(!is_pointer_to_heap(obj->forwarding)){
		copy_data(to_free, obj, obj->size);
		obj->forwarding = to_free;
		to_free += obj->size;
	}
	return obj->forwarding;
}

void
copying(vm_data *root)
{
	struct vm_obj *scan;

	scan = to_free = to_start;

	/* root */
	if(IS_OBJ(*root)){
		*root = copy( (*root) -3);
		*root = ((vm_data)*root) | 3;
	}

	while(scan < to_free){
		/* copy of child */
		if(scan->tag == VM_OBJ_PAIR){
			if(IS_OBJ((vm_data)scan->u.pair.car)){
				scan->u.pair.car = (vm_data)copy(scan->u.pair.car - 3) | 3;
			}

			if(IS_OBJ((vm_data)scan->u.pair.cdr)){
				scan->u.pair.cdr = (vm_data)copy(scan->u.pair.cdr - 3) | 3;
			}
		}

		scan = (char*)scan + scan->size;
	}

	/* swap from_start , to_start */
	scan = from_start;
	from_start = to_start;
	to_start = scan;

	/* calc from_free, to_free */
	from_free = to_free;
	to_free = to_start;
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

vm_data
gc_alloc_stack(int s)
{
	struct vm_obj *obj;
	int i,size;

	size = sizeof(struct vm_obj) + sizeof(vm_data) * s;
	obj = myalloc(size);
	obj->tag = VM_OBJ_STACK;
	obj->u.stack.size = s;
	obj->u.stack.p = (char*)obj + sizeof(struct vm_obj);

	for(i=0;i<s;i++){
		obj->u.stack.p[i] = stack[i];
	}

	return ((vm_data)obj) | 3;
}

vm_data
gc_alloc_pair()
{
	struct vm_obj *obj;

	obj = myalloc(sizeof(struct vm_obj));
	obj->tag = VM_OBJ_PAIR;

	return ((vm_data)obj) | 3;
}

int
gc_test(void)
{
	gc_init();
	int p = 0;
	int i;

	root_reg = gc_alloc_pair();
	CAR(root_reg) = gc_alloc_string("hello");
	CDR(root_reg) = gc_alloc_pair();
	CAR(CDR(root_reg)) = gc_alloc_closure(0,100,200,0);
	CDR(CDR(root_reg)) = gc_alloc_symbol("hello");

	p =  OBJ_SIZE(root_reg);
	p += OBJ_SIZE(CAR(root_reg));
	p += OBJ_SIZE(CDR(root_reg));
	p += OBJ_SIZE(CAR(CDR(root_reg)));
	p += OBJ_SIZE(CDR(CDR(root_reg)));

	for(i=0;i<1000;i++){
		myalloc(10000000987);
	}

	printf("expect size %X\n",p);
	return 0;
}
