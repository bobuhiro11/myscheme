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
	printf("from_free:  %018p[%+d]\n", from_free, from_free - from_start);
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
myalloc(size_t s, vm_data *reg_a, vm_data *reg_c, int reg_s)
{
	struct vm_obj *rc;
	int size;

	/* alignment */
	int p = sizeof(struct vm_obj);
	size = ((s+p-1) / p ) * p;

	/* GC */
	if(from_free + size > from_start + POOL_MAX){
		copying(reg_a,reg_c,reg_s);
		gc_dump();
		ht_dump(global_table);
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
	if(to_start <= forwarding && forwarding <= (char*)to_start + POOL_MAX){
		return 1;
	}else{
		return 0;
	}
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
copying(vm_data *reg_a, vm_data *reg_c, int reg_s)
{
	struct vm_obj *scan;
	int i;

	scan = to_free = to_start;

	/* sample root */
	//if(IS_OBJ(root_reg)){
	//	root_reg = copy( root_reg -3);
	//	root_reg = ((vm_data)root_reg) | 3;
	//}

	/* hashtable */
	for(i=0;i<HASHTABLE_SIZE;i++){
		if(global_table[i].key[0] != '\0'){
			if(IS_OBJ(global_table[i].data)){
				global_table[i].data = copy( global_table[i].data -3);
				global_table[i].data  = ((vm_data) global_table[i].data) | 3;
			}
		}
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

	/* if add this code, closure object gc failure???? */
	for(i=0;i<POOL_MAX;i++){
		*((char*)(to_start) + i) = 0x00;
	}
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
gc_alloc_closure(int n, int bodyadr, int ebodyadr, vm_data *reg_a, vm_data *reg_c, int reg_s)
{
	struct vm_obj *obj;
	vm_data rc;
	vm_data *closure;
	int i;
	int size;

	size = sizeof(struct vm_obj) + sizeof(vm_data) * (n+2);
	obj = myalloc(size, reg_a, reg_c, reg_s);
	obj->tag 	   = VM_OBJ_CLOSURE;
	//closure     = (char*)obj + sizeof(struct vm_obj);
	//closure[0]  = bodyadr << 2;
	//closure[1]  = ebodyadr << 2;
	//
	rc = ((vm_data)obj) | 3;
	SET_CLOSURE_BODY(rc,bodyadr);
	SET_CLOSURE_EBODY(rc,ebodyadr);

	for(i=0;i<n;i++){
		//closure[i+2] = INDEX(reg_s,i);
		SET_CLOSURE_INDEX(rc,i,INDEX(reg_s,i));
	}

	return rc;
}

vm_data
gc_alloc_string(char *str, vm_data *reg_a, vm_data *reg_c, int reg_s)
{
	struct vm_obj *obj;
	char *string;
	int size;

	size = sizeof(struct vm_obj) + strlen(str) +1;
	obj = myalloc(size, reg_a, reg_c, reg_s);
	obj->tag = VM_OBJ_STRING;
	string = (char*)obj + sizeof(struct vm_obj);
	strcpy(string,str);

	return ((vm_data)obj) | 3;
}

vm_data
gc_alloc_symbol(char *str, vm_data *reg_a, vm_data *reg_c, int reg_s)
{
	struct vm_obj *obj;
	char *symbol;
	int size;

	size = sizeof(struct vm_obj) + strlen(str) +1;
	obj = myalloc(size, reg_a, reg_c, reg_s);
	obj->tag = VM_OBJ_SYMBOL;
	symbol = (char*)obj + sizeof(struct vm_obj);
	strcpy(symbol, str);

	return ((vm_data)obj) | 3;
}

vm_data
gc_alloc_stack(vm_data *reg_a, vm_data *reg_c, int reg_s)
{
	struct vm_obj *obj;
	vm_data *_stack;
	int i,size;

	size = sizeof(struct vm_obj) + sizeof(vm_data) * reg_s;
	obj = myalloc(size, reg_a, reg_c, reg_s);
	obj->tag = VM_OBJ_STACK;
	obj->u.stack.size = reg_s;
	_stack = (char*)obj + sizeof(struct vm_obj);

	for(i=0;i<reg_s;i++){
		_stack[i] = stack[i];
	}

	return ((vm_data)obj) | 3;
}

vm_data
gc_alloc_pair(vm_data car, vm_data cdr, vm_data *reg_a, vm_data *reg_c, int reg_s)
{
	struct vm_obj *obj;

	obj = myalloc(sizeof(struct vm_obj), reg_a, reg_c, reg_s);
	obj->tag = VM_OBJ_PAIR;
	obj->u.pair.car = car;
	obj->u.pair.cdr = cdr;

	return ((vm_data)obj) | 3;
}

#ifdef GC_MAIN
int
main(void)
{
	gc_init();
	int p = 0;
	int i;

	global_table = ht_create();
	ht_init(global_table);
	ht_dump(global_table);

	for(i=0;i<1000;i++){
		myalloc(100,NULL,NULL,0);
	}

	return 0;
}
#endif
