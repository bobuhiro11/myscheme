#include "common.h"

/*
 * opecode and operand
 *
 * opecode:
 * 	CODE_*****
 * operand:
 * 	0x....00 -> number
 * 	0x..0001 -> true
 * 	0x..1001 -> false
 * 	0x..1101 -> nil
 * 	0x....11 -> string (char *)
 */
vm_code code[CODE_MAX];

vm_data stack[STACK_MAX];

struct hashtable *global_table;

/*
 * translate code(string) to code(vm_code)
 */
vm_code
get_vm_code(const char* s)
{
	vm_code rc = CODE_INVALID;
	char *endp,*p;
	int val;

	if(!strcmp(s,"halt"))			rc =  CODE_HALT;
	else if(!strcmp(s,"refer-local"))	rc =  CODE_REFER_LOCAL;
	else if(!strcmp(s,"refer-free"))	rc =  CODE_REFER_FREE;
	else if(!strcmp(s,"refer-global"))	rc =  CODE_REFER_GLOBAL;
	else if(!strcmp(s,"indirect"))		rc =  CODE_INDIRECT;
	else if(!strcmp(s,"constant"))		rc =  CODE_CONSTANT;
	else if(!strcmp(s,"close"))		rc =  CODE_CLOSE;
	else if(!strcmp(s,"box"))		rc =  CODE_BOX;
	else if(!strcmp(s,"test"))		rc =  CODE_TEST;
	else if(!strcmp(s,"plus"))		rc =  CODE_PLUS;
	else if(!strcmp(s,"minus"))		rc =  CODE_MINUS;
	else if(!strcmp(s,"equal"))		rc =  CODE_EQUAL;
	else if(!strcmp(s,"assign-local"))	rc =  CODE_ASSIGN_LOCAL;
	else if(!strcmp(s,"assign-free"))	rc =  CODE_ASSIGN_FREE;
	else if(!strcmp(s,"assign-global"))	rc =  CODE_ASSIGN_GLOBAL;
	else if(!strcmp(s,"define"))		rc =  CODE_DEFINE;
	else if(!strcmp(s,"conti"))		rc =  CODE_CONTI;
	else if(!strcmp(s,"nuate"))		rc =  CODE_NUATE;
	else if(!strcmp(s,"frame"))		rc =  CODE_FRAME;
	else if(!strcmp(s,"argument"))		rc =  CODE_ARGUMENT;
	else if(!strcmp(s,"shift"))		rc =  CODE_SHIFT;
	else if(!strcmp(s,"apply"))		rc =  CODE_APPLY;
	else if(!strcmp(s,"return"))		rc =  CODE_RETURN;
	else if(!strcmp(s,"#t"))		rc =  CODE_TRUE;
	else if(!strcmp(s,"#f"))		rc =  CODE_FALSE;
	else if(!strcmp(s,"nil"))		rc =  CODE_NIL;
	else{
		val = strtol(s, &endp, 10);
		if(*endp == '\0'){ 			/* number */
			val = val << 2;
			rc = val;
		}else{					/* string */
			p = (char*)malloc(strlen(s) + 1);
			memset(p, strlen(s)+1, 0);
			strcpy(p, s);
			rc = (uint64_t)p;
			rc = rc | 3;
		}
	}

	return rc;
}

/*
 * get code from stdin and store in code
 */
void
get_code()
{
	char row[256];
	int i;
	vm_code c;
	char s[256];

	while(fgets(row, sizeof(row), stdin) != NULL){
		if(row[0]=='#')
			continue;
		sscanf(row, "%d %s\n", &i, s);
		c = get_vm_code(s);
		code[i] = c;
	}
}

/*
 * dump code
 */
void
dump_code(int max)
{
	int i;
	printf("**code**\n");
	for(i=0;i<CODE_MAX && i<max;i++){
		printf("%d %p\n", i, code[i]);
	}
}

/*
 * write data to stdout
 */
void
write_vm_data(vm_data data)
{
	int val;
	int64_t rc;
	struct obj *p;

	if(is_num(data)){
		rc = data;
		rc = rc>>2;
		printf("%d",rc);
	}else if (is_true(data)){
		printf("#t");
	}else if (is_false(data)){
		printf("#f");
	}else if (is_nil(data)){
		printf("nil");
	}else if (is_undefined(data)){
		printf("undef");
	}else if (is_end_of_frame(data)){
		printf("end_of_frame");
	}else if (is_obj(data)){
		p = data - 3;
		//if(p->tag == VM_OBJ_CLOSURE)
			printf("lamnbda<%d,%d>", closure_body(data),
					closure_ebody(data));
	}
}

/*
 * dump stack
 */
void
dump_stack(int max)
{
	int i;
	printf("**stack**\n");
	for(i=0;i<STACK_MAX && i<max;i++){
		printf("%2d ",i);
		write_vm_data(stack[i]);
		printf("\n");
	}
}

/*
 * create_closure
 *
 * u.closure[0]: body start adr
 * u.closure[1]: body end   adr
 *
 * u.closure[i]: vm_data list
 *
 */
vm_data
create_closure(uint32_t n, uint32_t bodyadr, uint32_t ebodyadr, uint32_t s)
{
	struct vm_obj *obj = malloc(sizeof(struct vm_obj));
	obj->tag = VM_OBJ_CLOSURE;
	obj->u.closure = malloc(sizeof(vm_data) * (n+2));
	obj->u.closure[0] = bodyadr << 2;
	obj->u.closure[1] = ebodyadr << 2;
	return ((uint64_t)obj) | 3;
}

/*
 * get closure start address
 */
uint32_t
closure_body(vm_data data)
{
	struct vm_obj *p;
	p = data - 3;
	return p->u.closure[0] >> 2;
}

uint32_t
closure_ebody(vm_data data)
{
	struct vm_obj *p;
	p = data - 3;
	return p->u.closure[1] >> 2;
}

/*
 * execute code stored in variable code
 * and return last accumlator value
 */
vm_data
exec_code()
{
	int val,val2,val3;
	char *str;
	vm_data tmp, tmp2, tmp3;

	vm_data a;		/* accumlator 			*/
	int pc;			/* program counter real number 	*/
	int f;			/* frame pointer real number 	*/
	int argp; 		/* argument pointer real number	*/
	vm_data c;		/* closure pointer 		*/
	int s;			/* stack pointer real number	*/

	pc = f = argp = s = 0;
	a = c = VM_DATA_UNDEFINED;

	for(;;){
		printf("pc= %d\n", pc);
		printf("a=");
		write_vm_data(a);
		printf("\n");

		val = code[pc++];
		switch(val){
			case CODE_HALT:
				return a;
			case CODE_REFER_LOCAL:
				val  = code[pc++] >> 2;	/* n		*/
				a = stack[s-val-1];
				break;
			case CODE_REFER_FREE:
				break;
			case CODE_REFER_GLOBAL:
				tmp  = code[pc++];	/* n		*/
				a = ht_find(global_table, tmp-3);
				break;
			case CODE_INDIRECT:
				break;
			case CODE_CONSTANT:
				if((code[pc] &3 ) == 0){
					/* number */
					a = code[pc++];
				}else if(code[pc] == CODE_TRUE){
					a = VM_DATA_TRUE;
				}else if(code[pc]  == CODE_FALSE){
					a = VM_DATA_FALSE;
				}else if(code[pc] == CODE_NIL){
					a = VM_DATA_NIL;
				}else{
					/* string */
					val = code[pc++];
				}
				break;
			case CODE_CLOSE:
				val  = code[pc++] >> 2;	/* n		*/
				val2 = code[pc++] >> 2;	/* bodyadr 	*/
				val3 = code[pc++] >> 2;	/* ebodyadr	*/
				a = create_closure(val, val2, val3, s);
				break;
			case CODE_BOX:
				break;
			case CODE_TEST:
				val  = code[pc++] >> 2;		/* else adr	*/
				if(a == VM_DATA_FALSE)
					pc = val;
				break;
			case CODE_PLUS:
				break;
			case CODE_MINUS:
				break;
			case CODE_EQUAL:
				break;
			case CODE_ASSIGN_LOCAL:
				break;
			case CODE_ASSIGN_FREE:
				break;
			case CODE_ASSIGN_GLOBAL:
				break;
			case CODE_DEFINE:
				break;
			case CODE_CONTI:
				break;
			case CODE_NUATE:
				break;
			case CODE_FRAME:
				stack[s++] = VM_DATA_END_OF_FRAME;
				stack[s++] = code[pc++];
				stack[s++] = c;
				stack[s++] = argp;
				stack[s++] = f;
				break;
			case CODE_ARGUMENT:
				stack[s++] = a;
				break;
			case CODE_SHIFT:
				break;
			case CODE_APPLY:
				val = code[pc++] >> 2;	/* n 	*/
				pc = closure_body(a);
				f = s-val;
				argp = s;
				c = a;
				break;
			case CODE_RETURN:
				val = code[pc++] >> 2;	/* n 	*/
				s = s-val;
				pc = stack[s-4] >> 2;
				f = stack[s-1];
				argp = stack[s-2];
				c = stack[s-3];
				s = s-5;
				break;
		}
	}
}

/*
 * insert initial data
 */
void
ht_init(struct hashtable *table)
{
	vm_data d;
	d = 123 << 2;
	ht_insert(table, "x", d);
	d = 256 << 2;
	ht_insert(table, "y", d);
}

int
main(int argc, char **argv)
{
	vm_data rc;

	global_table = ht_create();
	ht_init(global_table);

	get_code();
	dump_code(10);
	rc = exec_code();
	write_vm_data(rc);
	printf("\n");
	dump_stack(10);

	ht_destory(global_table);

	return 0;
}
