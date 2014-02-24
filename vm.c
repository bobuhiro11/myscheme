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

struct vm_data stack[STACK_MAX];

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
write_vm_data(struct vm_data data)
{
	int val;
	switch(data.tag){
		case VM_DATA_INTEGER:
			val = data.u.integer;
			printf("%-6s %8d","int",val);
			break;
		case VM_DATA_END_OF_FRAME:
			printf("%-s", "end_of_frame");
			break;
		case VM_DATA_NIL:
			printf("%-s", "nil");
			break;
		case VM_DATA_TRUE:
			printf("%-s", "true");
			break;
		case VM_DATA_FALSE:
			printf("%-s", "false");
			break;
		case VM_DATA_CLOSURE:
			printf("%-6s <%d %d>", "close",data.u.closure[0].u.integer,
					data.u.closure[1].u.integer);
			break;
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
struct vm_data
create_closure(uint32_t n, uint32_t bodyadr, uint32_t ebodyadr, uint32_t s)
{
	int i;
	struct vm_data closure;

	closure.tag = VM_DATA_CLOSURE;
	closure.u.closure = malloc(sizeof(struct vm_data)*(n+2));

	closure.u.closure[0].tag = VM_DATA_INTEGER;
	closure.u.closure[0].u.integer = bodyadr;
	closure.u.closure[1].tag = VM_DATA_INTEGER;
	closure.u.closure[1].u.integer = ebodyadr;

	for(i=0;i<n;i++){
		closure.u.closure[i+2] = stack[s-i-1];
	}

	return closure;
}

/*
 * get closure start addres
 */
uint32_t
closure_body(struct vm_data data)
{
	return data.u.closure[0].u.integer;
}

/*
 * execute code stored in variable code
 * and return last accumlator value
 */
struct vm_data
exec_code()
{
	int val,val2,val3;
	char *str;

	struct vm_data tmp, tmp2, tmp3;

	struct vm_data a;	/* accumlator 			*/
	int pc;			/* program counter real number 	*/
	int f;			/* frame pointer real number 	*/
	int argp; 		/* argument pointer real number	*/
	struct vm_data c;	/* closure pointer 		*/
	int s;			/* stack pointer real number	*/

	pc = f = argp = s = 0;
	a.tag = VM_DATA_INTEGER;
	a.u.integer = 0;
	c.tag = VM_DATA_NIL;

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
				str  = (char*)code[pc++];	/* string	*/
				str  = str - 3;
				printf("str = %s\n", str);
				a = ht_find(global_table, str);
				break;
			case CODE_INDIRECT:
				break;
			case CODE_CONSTANT:
				if((code[pc] &3 ) == 0){
					/* number */
					val = code[pc++];
					val = val >> 2;
					a.tag = VM_DATA_INTEGER;
					a.u.integer = val;
				}else if(code[pc] == CODE_TRUE){
					a.tag = VM_DATA_TRUE;
					pc++;
				}else if(code[pc]  == CODE_FALSE){
					a.tag = VM_DATA_FALSE;
					pc++;
				}else if(code[pc] == CODE_NIL){
					a.tag = VM_DATA_NIL;
					pc++;
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
				val  = code[pc++] >> 2;	/* else adr	*/
				if(a.tag == VM_DATA_FALSE)
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
				val = code[pc++];	/* return address */
				val = val >> 2;

				tmp.tag = VM_DATA_END_OF_FRAME;
				tmp.u.integer = -1;
				stack[s++] = tmp;

				tmp.tag = VM_DATA_INTEGER;
				tmp.u.integer = val;
				stack[s++] = tmp;

				stack[s++] = c;

				tmp.u.integer = argp;
				stack[s++] = tmp;

				tmp.u.integer = f;
				stack[s++] = tmp;

				break;
			case CODE_ARGUMENT:
				stack[s++] = a;
				break;
			case CODE_SHIFT:
				break;
			case CODE_APPLY:
				val = code[pc++];	/* argument size */
				val = val>>2;

				pc = closure_body(a);
				f = s - val;
				argp = s;
				c = a;
				break;
			case CODE_RETURN:
				val = code[pc++];	/* argument size */
				val = val>>2;
				s -= val;
				pc = stack[s-4].u.integer;
				f = stack[s-1].u.integer;
				argp = stack[s-2].u.integer;
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
	struct vm_data d;
	d.tag = VM_DATA_INTEGER;
	d.u.integer = 123;
	ht_insert(table, "x", d);
	d.u.integer = 256;
	ht_insert(table, "y", d);
}

int
main(int argc, char **argv)
{
	struct vm_data rc;

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
