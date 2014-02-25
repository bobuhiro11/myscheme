#include "common.h"

/*
 * opecode and operand
 *
 * opecode:
 * 	CODE_*****
 * operand:
 * 	0x....00  number
 * 	0x..0001  true
 * 	0x..1001  false
 * 	0x..1101  nil
 * 	0x....11  string (char *)
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
			rc = val << 2;
		}else{					/* string */
			p = (char*)malloc(strlen(s) + 1);
			memset(p, strlen(s)+1, 0);
			strcpy(p, s);
			rc = (uint64_t)p | 3;
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

void
init_code()
{
	code[1000] = CODE_EQUAL;
	code[1001] = CODE_RETURN;
	code[1002] = 2<<2;
	code[1003] = CODE_MINUS;
	code[1004] = CODE_RETURN;
	code[1005] = 2<<2;
	code[1006] = CODE_PLUS;
	code[1007] = CODE_RETURN;
	code[1008] = 2<<2;
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
	struct vm_obj *p;

	if(IS_NUM(data))		printf("%d",data>>2);
	else if(IS_TRUE(data))		printf("#t");
	else if(IS_FALSE(data))		printf("#f");
	else if(IS_NIL(data))		printf("nil");
	else if(IS_UNDEFINED(data))	printf("undef");
	else if(IS_END_OF_FRAME(data))	printf("end_of_frame");
	else if(IS_CLOSURE(data))	printf("lamnbda<%d,%d>",CLOSURE_BODY(data),CLOSURE_EBODY(data));
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
 * u.closure[i]: vm_data list (i > 1)
 *
 */
vm_data
create_closure(uint32_t n, uint32_t bodyadr, uint32_t ebodyadr, uint32_t s)
{
	struct vm_obj *obj = malloc(sizeof(struct vm_obj));
	obj->tag 	   = VM_OBJ_CLOSURE;
	obj->u.closure     = malloc(sizeof(vm_data) * (n+2));
	obj->u.closure[0]  = bodyadr << 2;
	obj->u.closure[1]  = ebodyadr << 2;
	return ((uint64_t)obj) | 3;
}


/*
 * shift n elements of stack top
 */
void
shift_args(uint32_t n, uint32_t m, uint32_t s)
{
	int i;
	printf("n = %d, m = %d\n",n,m);
	for(i=n-1; i>=0; i--){
		stack[s-i-m-1] = stack[s-i-1];
	}
}

/*
 * execute code stored in variable code
 * and return last accumlator value
 */
vm_data
exec_code()
{
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
		//printf("pc= %d\n", pc);
		//printf("a=");
		//write_vm_data(a);
		//printf("s=%d",s);
		//printf("argp=%d",argp);
		//printf("\n");
		//dump_stack(10);

		switch(code[pc++]){
			case CODE_HALT:
				return a;
			case CODE_REFER_LOCAL:
				a = stack[argp-(code[pc++]>>2)-1];
				break;
			case CODE_REFER_FREE:
				break;
			case CODE_REFER_GLOBAL:
				a = ht_find(global_table, code[pc++]-3);
				break;
			case CODE_INDIRECT:
				break;
			case CODE_CONSTANT:
				tmp = code[pc++];
				if((tmp & 3) == 0)		a = tmp;
				else if(tmp == CODE_TRUE)	a = VM_DATA_TRUE;
				else if(tmp  == CODE_FALSE)	a = VM_DATA_FALSE;
				else if(tmp == CODE_NIL)	a = VM_DATA_NIL;
				else				tmp = tmp;
				break;
			case CODE_CLOSE:
				tmp  = code[pc++] >> 2;	/* n		*/
				tmp2 = code[pc++] >> 2;	/* bodyadr 	*/
				tmp3 = code[pc++] >> 2;	/* ebodyadr	*/
				a = create_closure(tmp, tmp2, tmp3, s);
				break;
			case CODE_BOX:
				break;
			case CODE_TEST:
				tmp  = code[pc++] >> 2;	/* else adr	*/
				if(a == VM_DATA_FALSE)	pc = tmp;
				break;
			case CODE_PLUS:
				a = ((stack[s-1]>>2) + (stack[s-2]>>2)) << 2;
				break;
			case CODE_MINUS:
				a = ((stack[s-1]>>2) - (stack[s-2]>>2)) << 2;
				break;
			case CODE_EQUAL:
				a = stack[s-1] == stack[s-2] ? VM_DATA_TRUE : VM_DATA_FALSE;
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
				tmp  = code[pc++]>>2;	/* n	*/
				tmp2 = code[pc++]>>2;	/* m	*/
				shift_args(tmp, tmp2, s);
				argp += tmp - tmp2;
				s    -= tmp2;
				break;
			case CODE_APPLY:
				f	= s - (code[pc++] >> 2);
				pc 	= CLOSURE_BODY(a);
				argp	= s;
				c	= a;
				break;
			case CODE_RETURN:
				s	= s-(code[pc++] >> 2);
				pc 	= stack[s-4] >> 2;
				f 	= stack[s-1];
				argp 	= stack[s-2];
				c 	= stack[s-3];
				s 	= s-5;
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

	ht_insert(table, "x", 123<<2);
	ht_insert(table, "y", 256<<2);
	ht_insert(table, "=", create_closure(0, 1000, 1002,0));
	ht_insert(table, "-", create_closure(0, 1003, 1005,0));
	ht_insert(table, "+", create_closure(0, 1006, 1008,0));
}

int
main(int argc, char **argv)
{
	vm_data rc;

	global_table = ht_create();
	ht_init(global_table);

	ht_dump(global_table);

	init_code();
	get_code();
	dump_code(10);
	rc = exec_code();
	write_vm_data(rc);
	printf("\n");
	dump_stack(10);

	ht_destory(global_table);

	return 0;
}
