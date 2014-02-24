#include<stdio.h>
#include<stdint.h>
#include<string.h>
#include<stdlib.h>

#define CODE_MAX 2048
#define STACK_MAX 64

#define CODE_HALT 		0xFF000001	/* for vm_code */
#define CODE_REFER_LOCAL 	0xFF000002
#define CODE_REFER_FREE		0xFF000003
#define CODE_REFER_GLOBAL	0xFF000004
#define CODE_INDIRECT    	0xFF000005
#define CODE_CONSTANT    	0xFF000006
#define CODE_CLOSE       	0xFF000007
#define CODE_BOX         	0xFF000008
#define CODE_TEST        	0xFF000009
#define CODE_PLUS        	0xFF00000a
#define CODE_MINUS       	0xFF00000b
#define CODE_EQUAL       	0xFF00000c
#define CODE_ASSIGN_LOCAL	0xFF00000d
#define CODE_ASSIGN_FREE 	0xFF00000e
#define CODE_ASSIGN_GLOBAL	0xFF00000f
#define CODE_DEFINE      	0xFF000010
#define CODE_CONTI       	0xFF000011
#define CODE_NUATE       	0xFF000012
#define CODE_FRAME       	0xFF000013
#define CODE_ARGUMENT    	0xFF000014
#define CODE_SHIFT       	0xFF000015
#define CODE_APPLY       	0xFF000016
#define CODE_RETURN      	0xFF000017
#define CODE_INVALID 		0xFFFFFFFF

#define VM_DATA_TRUE		0x01		/* for vm_data */
#define VM_DATA_FALSE		0x02
#define VM_DATA_NIL		0x03
#define VM_DATA_EOF		0x04
#define VM_DATA_UNDEFINED	0x05
#define VM_DATA_UNBOUND		0x06
#define VM_DATA_STR		0x07
#define VM_DATA_CLOSURE		0x08
#define VM_DATA_INTEGER		0x09
#define VM_DATA_END_OF_FRAME	0xFD

typedef uint64_t vm_code;

struct vm_data
{
	unsigned char tag;
	union{
		int integer;
		char *str;
		struct vm_data *closure; 
	} u;
};

/*
 * opecode and operand
 *
 * opecode:
 * 	CODE_*****
 * operand:
 * 	0x....00 -> number
 * 	0x....11 -> string (char *)
 */
vm_code code[CODE_MAX];

struct vm_data stack[STACK_MAX];

/*
 * translate code(string) to code(vm_code)
 */
static vm_code get_vm_code(const char* s)
{
	vm_code rc = CODE_INVALID;
	char *endp,*p;
	int val;

	if(!strcmp(s,"halt"))			rc =  CODE_HALT;
	else if(!strcmp(s,"refer-local"))	rc =  CODE_REFER_LOCAL;
	else if(!strcmp(s,"refer-free"))	rc =  CODE_REFER_FREE;
	else if(!strcmp(s,"refer-global"))	rc =  CODE_REFER_GLOBAL;
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
	else if(!strcmp(s,"#t"))		rc =  VM_DATA_TRUE;
	else if(!strcmp(s,"#f"))		rc =  VM_DATA_FALSE;
	else if(!strcmp(s,"nil"))		rc =  VM_DATA_NIL;
	else{
		val = strtol(s, &endp, 10);
		if(*endp == '\0'){ 			/* number */
			val = val << 2;
			rc = val;
		}else{					/* string */
			p = (char*)malloc(strlen(s) + 1);
			strcpy(p, s);
			rc = (vm_code)p | 3;
		}
	}

	return rc;
}

/*
 * get code from stdin and store in code
 */
void get_code()
{
	char row[256];
	int i,c;
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
void dump_code(int max)
{
	int i;
	printf("**code**\n");
	for(i=0;i<CODE_MAX && i<max;i++){
		printf("%d %016X\n", i, code[i]);
	}
}

/*
 * write data to stdout
 */
void write_vm_data(struct vm_data data)
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
 * dump code
 */
void dump_stack(int max)
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
 * u.closure[0]: body start adr, real number
 * u.closure[1]: body end   adr, real number 
 *
 * u.closure[i]: vm_data list
 *
 */
struct vm_data create_closure(uint32_t n, uint32_t bodyadr, uint32_t ebodyadr, uint32_t s)
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
uint32_t closure_body(struct vm_data data)
{
	return data.u.closure[0].u.integer;
}

/*
 * execute code stored in variable code
 * and return last accumlator value
 */
struct vm_data exec_code()
{
	int val,val2,val3;

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
				break;
			case CODE_INDIRECT:
				break;
			case CODE_CONSTANT:
				if((code[pc] & 0x00000003) == 0x00000000){
					/* number */
					val = code[pc++];
					val = val >> 2;
					a.tag = VM_DATA_INTEGER;
					a.u.integer = val;
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

int main(int argc, char **argv)
{
	struct vm_data rc;

	get_code();
	rc = exec_code();
	write_vm_data(rc);
	printf("\n");
	dump_code(10);
	dump_stack(10);

	return 0;
}
