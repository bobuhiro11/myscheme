#include<stdio.h>
#include<stdlib.h>
#include<stdint.h>
#include<string.h>

#define CODE_MAX 2048
#define STACK_MAX 64

#define CODE_HALT 		0xFF000001
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

#define VM_OBJ_STR		0x00000001

typedef uint32_t vm_data;
typedef uint32_t vm_code;

struct vm_obj
{
	uint32_t tag;
	union{
		char *str;
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

vm_data stack[STACK_MAX];

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
	else{
		val = strtol(s, &endp, 10);
		if(*endp == '\0'){ 			/* number */
			val = val << 2;
			rc = val;
		}else{					/* string */
			p = (char*)malloc(strlen(s) + 1);
			strcpy(p, s);
			rc = (vm_code)p | 0x00000003;
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
	for(i=0;i<CODE_MAX && i<max;i++){
		printf("%d %08X\n", i, code[i]);
	}
}

/*
 * execute code stored in variable code
 * and return last accumlator value
 */
vm_data exec_code()
{
	int val;

	vm_data a;		/* accumlator 		*/
	uint32_t pc;		/* program counter 	*/
	uint32_t f;		/* frame pointer 	*/
	uint32_t argp; 		/* argument pointer 	*/
	struct vm_obj c;	/* closure pointer 	*/
	uint32_t s;		/* stack pointer	*/

	a = pc = f = argp = s = 0;

	for(;;){
		switch(code[pc]){
			case CODE_HALT:
				return a;
			case CODE_REFER_LOCAL:
			case CODE_REFER_FREE:
			case CODE_REFER_GLOBAL:
			case CODE_INDIRECT:
			case CODE_CONSTANT:
				pc++;
				if((code[pc] & 0x00000003) == 0x00000000){
					/* number */
					a = code[pc];
				}else{
					/* string */
				}
				pc++;
				break;
			case CODE_CLOSE:
			case CODE_BOX:
			case CODE_TEST:
			case CODE_PLUS:
			case CODE_MINUS:
			case CODE_EQUAL:
			case CODE_ASSIGN_LOCAL:
			case CODE_ASSIGN_FREE:
			case CODE_ASSIGN_GLOBAL:
			case CODE_DEFINE:
			case CODE_CONTI:
			case CODE_NUATE:
			case CODE_FRAME:
			case CODE_ARGUMENT:
			case CODE_SHIFT:
			case CODE_APPLY:
			case CODE_RETURN:
				break;
		}
	}
}

/*
 * write data to stdout
 */
void write_vm_data(vm_data data)
{
	int val;
	int lsb = data & 0x00000003;
	
	if(lsb == 0x00){ 		/* number */
		val = data;
		val = val>>2;
		printf("%d",val);
	}
	printf(" [%08X]\n",data);
}

int main(int argc, char **argv)
{
	vm_data rc;

	get_code();
	dump_code(10);
	rc = exec_code();
	write_vm_data(rc);
	return 0;
}
