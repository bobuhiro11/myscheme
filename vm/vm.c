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

int rom_last_address;
int heap_last_address;

int host_bit;

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

	else if(!strcmp(s,"constnil"))		rc =  CODE_CONSTNIL;
	else if(!strcmp(s,"constnum"))		rc =  CODE_CONSTNUM;
	else if(!strcmp(s,"conststr"))		rc =  CODE_CONSTSTR;
	else if(!strcmp(s,"constboo"))		rc =  CODE_CONSTBOO;
	else if(!strcmp(s,"constsym"))		rc =  CODE_CONSTSYM;

	else if(!strcmp(s,"close"))		rc =  CODE_CLOSE;
	else if(!strcmp(s,"box"))		rc =  CODE_BOX;
	else if(!strcmp(s,"test"))		rc =  CODE_TEST;
	//else if(!strcmp(s,"plus"))		rc =  CODE_PLUS;
	//else if(!strcmp(s,"minus"))		rc =  CODE_MINUS;
	//else if(!strcmp(s,"equal"))		rc =  CODE_EQUAL;
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
	else if(!strcmp(s,"display"))		rc =  CODE_DISPLAY;
	else if(!strcmp(s,"disasm"))		rc =  CODE_DISASM;
	else if(!strcmp(s,"newline"))		rc =  CODE_NEWLINE;
	else if(!strcmp(s,"#t"))		rc =  CODE_TRUE;
	else if(!strcmp(s,"#f"))		rc =  CODE_FALSE;
	else if(!strcmp(s,"nil")
			||!strcmp(s,"()"))	rc =  CODE_NIL;
	else{
		val = strtol(s, &endp, 10);
		if(*endp == '\0'){ 			/* number */
			rc = val << 2;
		}else{					/* string */
			p = (char*)malloc(strlen(s) + 1);
			memset(p, strlen(s)+1, 0);
			strcpy(p, s);
			rc = (vm_code)p | 3;
		}
	}

	return rc;
}

/*
 * get code from stdin and store in code
 *
 * return 0 if the code is not found.
 */
int
get_code()
{
	char row[256];
	int i;
	vm_code c;
	char s[256];

	printf(">>");
	fflush(stdout);

	while(fgets(row, sizeof(row), stdin) != NULL){
		if(strstr(row,"#end")){
			rom_last_address = i;
			return 1;
		}else if(row[0]=='#'){
			continue;
		}
		sscanf(row, "%d %s\n", &i, s);
		c = get_vm_code(s);
		code[i] = c;
	}
	return 0;
}

/*
 * dump as hex format
 */
void
dump_address(vm_data data)
{
	if(host_bit == 32){
		printf("%010p", data);
	}else{
		printf("%018p", data);
	}
}

/*
 * dump code
 */
void
dump_code(int max)
{
	int i; printf("=== code ===\n");
	for(i=0;i<CODE_MAX && i<max;i++){
		printf("%2d ", i);
		dump_address(code[i]);

		if(IS_CODE_CODE(code[i])){
			switch(code[i]){
				case CODE_HALT:		 printf(" ;HALT"); break;
				case CODE_REFER_LOCAL:   printf(" ;REFER_LOCAL"); break;
				case CODE_REFER_FREE:    printf(" ;REFER_FREE"); break;
				case CODE_REFER_GLOBAL:  printf(" ;REFER_GLOBAL"); break;
				case CODE_INDIRECT:      printf(" ;INDIRECT"); break;

				case CODE_CLOSE:         printf(" ;CLOSE"); break;
				case CODE_BOX:           printf(" ;BOX"); break;
				case CODE_TEST:          printf(" ;TEST"); break;
				case CODE_PLUS:          printf(" ;PLUS"); break;
				case CODE_MINUS:         printf(" ;MINUS"); break;
				case CODE_EQUAL:         printf(" ;EQUAL"); break;
				case CODE_ASSIGN_LOCAL:  printf(" ;ASSIGN_LOCAL"); break;
				case CODE_ASSIGN_FREE:   printf(" ;ASSIGN_FREE"); break;
				case CODE_ASSIGN_GLOBAL: printf(" ;ASSIGN_GLOBAL"); break;
				case CODE_DEFINE:        printf(" ;DEFINE"); break;
				case CODE_CONTI:         printf(" ;CONTI"); break;
				case CODE_NUATE:         printf(" ;NUATE"); break;
				case CODE_FRAME:         printf(" ;FRAME"); break;
				case CODE_ARGUMENT:      printf(" ;ARGUMEMT"); break;
				case CODE_SHIFT:         printf(" ;SHIFT"); break;
				case CODE_APPLY:         printf(" ;APPLY"); break;
				case CODE_RETURN:        printf(" ;RETURN"); break;
				case CODE_GT:        	 printf(" ;GT"); break;
				case CODE_LT:        	 printf(" ;LT"); break;
				case CODE_CONS:		 printf(" ;CONS"); break;
				case CODE_CAR:		 printf(" ;CAR"); break;
				case CODE_CDR:		 printf(" ;CDR"); break;

				case CODE_IS_NULL:     	 printf(" ;IS_NULL"); break;
				case CODE_DISPLAY:       printf(" ;DISPLAY"); break;
				case CODE_NEWLINE:     	 printf(" ;NEWLINE"); break;
				case CODE_MUL:         	 printf(" ;MUL"); break;
				case CODE_DIV:           printf(" ;DIV"); break;
				case CODE_MODULO:        printf(" ;MODULO"); break;
				case CODE_DISASM:     	 printf(" ;DISASM"); break;

				case CODE_CONSTNIL:      printf(" ;CONSTNIL"); break;
				case CODE_CONSTBOO:      printf(" ;CONSTBOO"); break;
				case CODE_CONSTSYM:      printf(" ;CONSTSYM"); break;
				case CODE_CONSTSTR:      printf(" ;CONSTSTR"); break;
				case CODE_CONSTNUM:      printf(" ;CONSTNUM"); break;

				case CODE_TRUE:          printf(" ;TRUE"); break;
				case CODE_FALSE:         printf(" ;FALSE"); break;
				case CODE_NIL:           printf(" ;NIL"); break;

			}
		}else if(IS_CODE_NUMBER(code[i])){
			printf(" ;%d", code[i] >> 2);
		}else if(IS_CODE_STRING(code[i])){
			printf(" ;%s", code[i] - 3);
		}else if(IS_CODE_STACK(code[i])){
			printf(" ;stack ");
			dump_stack_serial(code[i]);
		}else if(IS_CODE_NIL(code[i])){
			printf(" ;NIL");
		}else if(IS_CODE_TRUE(code[i])){
			printf(" ;TRUE");
		}else if(IS_CODE_FALSE(code[i])){
			printf(" ;FALSE");
		}
		printf("\n");
	}
}

/*
 * modify stack
 *
 */
vm_data
modify_stack(vm_data data)
{
	int min_adr = rom_last_address;
	struct vm_obj *stack;
	int size;		/* stack size */
	vm_data *p;		/* stack body */
	int i,d;

	stack = code[CLOSURE_BODY(data) + 3] - 3;
	size  = stack->u.stack.size;
	p     = stack->u.stack.p;

	for(i=0;i<size;i++)
		if(p[i] == VM_DATA_END_OF_FRAME && (p[i+1]>>2) < min_adr)
			min_adr = p[i+1] >> 2;

	d = heap_last_address + 1 - min_adr;

	for(i=min_adr;i<=rom_last_address;i++)
		code[i+d] = code[i];

	heap_last_address = rom_last_address + d;

	for(i=0;i<size;i++)
		if(p[i] == VM_DATA_END_OF_FRAME)
			p[i+1] = ((p[i+1] >> 2) + d) << 2;

	return data;
}

vm_data
save_closure_body(vm_data data)
{
	int start = CLOSURE_BODY(data);
	int end   = CLOSURE_EBODY(data);
	int d     = heap_last_address + 1 - start;
	int i;

	SET_CLOSURE_BODY(data, start +d);	
	SET_CLOSURE_EBODY(data, end +d);	
	
	for(i=start; i<=end; i++)
		code[i+d] = code[i];

	heap_last_address = end + d;

	if(code[start + 2] == CODE_NUATE)
		data = modify_stack(data);

	return data;
}

void
assign_global(const char *s,vm_data data)
{
	if(IS_CLOSURE(data))
		data = save_closure_body(data);

	ht_insert(global_table, s, data);
}

vm_data
box(vm_data x)
{
	vm_data *p = malloc(sizeof(vm_data));
	*p = x;
	return ((vm_data)p) | 2;
}

vm_data
unbox(vm_data x)
{
	if(IS_BOX(x)){
		return *((vm_data*)(x-2));
	}else{
		fprintf(stderr, "Error: this is not box.\n");
		return VM_DATA_UNDEFINED;
	}
}

void
setbox(vm_data box, vm_data x)
{
	if(!IS_BOX(box))
		fprintf(stderr, "Error: this is not box.\n");
	*((vm_data*)(box-2)) = x;
}

int
is_list(vm_data data)
{
	if(IS_NIL(data))	return 1;
	else if(IS_PAIR(data))	return is_list(CDR(data));
	else 			return 0;
}

void
write_vm_list(vm_data data, int d)
{
	if(d)			printf("(");	/* first time */
	else if(!IS_NIL(data))	printf(" "); 	/* other time */

	if(IS_NIL(data)){
		printf(")");
	}else{
		write_vm_data(CAR(data));
		write_vm_list(CDR(data),0);
	}
}

/*
 * write data to stdout
 */
void
write_vm_data(vm_data data)
{
	int val;
	vm_data rc;
	struct vm_obj *p;

	if(IS_NUM(data))		printf("%d",data>>2);
	else if(IS_TRUE(data))		printf("#t");
	else if(IS_FALSE(data))		printf("#f");
	else if(IS_NIL(data))		printf("()");
	else if(IS_UNDEFINED(data))	printf("undef");
	else if(IS_END_OF_FRAME(data))	printf("end_of_frame");
	else if(IS_CLOSURE(data))	printf("closure<%d,%d>",CLOSURE_BODY(data),CLOSURE_EBODY(data));
	else if(IS_OBJ(data)){
		/* object */
		if(IS_BOX(data))		{ printf("<box>"); write_vm_data(unbox(data)); }
		else if(IS_STRING(data))	printf("\"%s\"", STRING(data));
		else if(IS_SYMBOL(data))	printf("%s", SYMBOL(data));
		else if(IS_PAIR(data)) {
			if(is_list(data)){
				write_vm_list(data,1);
			}else{

				p = data-3;
				printf("(");
				write_vm_data(p->u.pair.car);
				printf(" . ");
				write_vm_data(p->u.pair.cdr);
				printf(")");
			}
		}else{
			dump_address(data);
			printf(" ;Bad data");
		}
		//printf("[%dB]",OBJ_SIZE(data));
	}
}

/*
 * dump stack for NUATE instraction in disasm
 */
void
dump_stack_serial(vm_code code)
{
	int i;
	int size = ((struct vm_obj*)(code-3))->u.stack.size;
	vm_data *p = ((struct vm_obj*)(code-3))->u.stack.p;
	for(i=0;i<size;i++){
		printf("\n");
		printf("%15s%2d ","",i);
		dump_address(p[i]);
		printf(" ;");
		write_vm_data(p[i]);
	}
}

/*
 * dump stack
 */
void
dump_stack(int max)
{
	int i;
	printf("=== stack ===\n");
	for(i=0;i<STACK_MAX && i<max;i++){
		printf("%2d ",i);
		dump_address(stack[i]);
		printf(" ;");
		write_vm_data(stack[i]);
		printf("\n");
	}
}

/*
 * shift n elements of stack top
 */
void
shift_args(int n, int m, int s)
{
	int i;
	for(i=n-1; i>=0; i--){
		SET_INDEX(s,i+m, INDEX(s,i));
	}
}

int
restore_stack(vm_data x)
{
	int i,n;
	struct vm_obj *obj = x-3;

	if(!IS_STACK(x)){
		fprintf(stderr, "Error: this is not stack object.\n");
	}

	n = obj->u.stack.size;
	for(i=0;i<n;i++){
		stack[i] = obj->u.stack.p[i];
	}
	return n;
}

/*
 * insert codes for continue and update rom last address
 */
void
insert_continuation_code(int s)
{
	code[rom_last_address + 1] = CODE_REFER_LOCAL;
	code[rom_last_address + 2] = 0;
	code[rom_last_address + 3] = CODE_NUATE;
	code[rom_last_address + 4] = gc_alloc_stack(s);
	code[rom_last_address + 5] = CODE_RETURN;
	code[rom_last_address + 6] = 0;

	rom_last_address += 6;
}

vm_data
vm_modulo(int argp, int f)
{
	return ((INDEX(argp,0)>>2) % (INDEX(argp,1)>>2))<<2;
}

vm_data
vm_plus(int argp, int f)
{
	int i;
	vm_data s=0;
	for(i=0; i< (argp-f); i++)
		s += (INDEX(argp,i)>>2);
	return s << 2;
}

vm_data
vm_mul(int argp, int f)
{
	int i;
	vm_data s=1;
	for(i=0; i< (argp-f); i++)
		s *= (INDEX(argp,i)>>2);
	return s << 2;
}

vm_data
vm_minus(int argp, int f)
{
	int i;
	vm_data s = INDEX(argp,0) >> 2;
	for(i=1; i< (argp-f); i++)
		s -= (INDEX(argp,i)>>2);
	return s << 2;
}

vm_data
vm_div(int argp, int f)
{
	int i;
	vm_data s = INDEX(argp,0) >> 2;
	for(i=1; i< (argp-f); i++)
		s /= (INDEX(argp,i)>>2);
	return s << 2;
}

/*
 * execute code stored in variable code
 * and return last accumlator value
 */
vm_data
exec_code()
{
	vm_data tmp, tmp2, tmp3;
	struct vm_obj *p;

	vm_data a;		/* accumlator 			*/
	int pc;			/* program counter real number 	*/
	int f;			/* frame pointer real number 	*/
	int argp; 		/* argument pointer real number	*/
	vm_data c;		/* closure pointer 		*/
	int s;			/* stack pointer real number	*/

	pc = f = argp = s = 0;
	a = c = VM_DATA_UNDEFINED;

	for(;;){
		// printf("pc= %d\n", pc);
		// printf("a=");
		// write_vm_data(a);
		// printf("s=%d\n",s);
		// printf("argp=%d",argp);
		// printf("\n");
		// dump_stack(10);

		switch(code[pc++]){
			case CODE_HALT:
				return a;
			case CODE_REFER_LOCAL:
				a = INDEX(argp, code[pc++]>>2);
				myalloc(7);
				break;
			case CODE_REFER_FREE:
				a = CLOSURE_INDEX(c, code[pc++]>>2);
				myalloc(6);
				break;
			case CODE_REFER_GLOBAL:
				a = ht_find(global_table, code[pc++]-3);
				myalloc(12);
				break;
			case CODE_INDIRECT:
				a = unbox(a);
				break;
			case CODE_CONSTNUM:
				a = code[pc++];
				break;
			case CODE_CONSTSTR:
				// p = myalloc(sizeof(struct vm_obj)+ sizeof(char)*10);
				// p->tag = VM_OBJ_STRING;
				// p->u.string = (code[pc++] - 3);
				// a = (vm_data)p | 3;
				a = gc_alloc_string(code[pc++]-3);
				break;
			case CODE_CONSTSYM:
				//p = myalloc(sizeof(struct vm_obj));
				//p->tag = VM_OBJ_SYMBOL;
				//p->u.symbol = (code[pc++] - 3);
				//a = (vm_data)p | 3;
				a = gc_alloc_symbol(code[pc++]-3);
				break;
			case CODE_CONSTNIL:
				pc++;
				a = VM_DATA_NIL;
				break;
			case CODE_CONSTBOO:
				a = code[pc++] == CODE_TRUE ? VM_DATA_TRUE : VM_DATA_FALSE;
				break;
			case CODE_CLOSE:
				tmp  = code[pc++] >> 2;	/* n		*/
				tmp2 = code[pc++] >> 2;	/* bodyadr 	*/
				tmp3 = code[pc++] >> 2;	/* ebodyadr	*/
				a = gc_alloc_closure(tmp, tmp2, tmp3, s);
				s -= tmp;
				break;
			case CODE_BOX:
				tmp  = code[pc++] >> 2;	/* n		*/
				SET_INDEX(s, tmp, box( INDEX(s, tmp)));
				break;
			case CODE_TEST:
				tmp  = code[pc++] >> 2;	/* else adr	*/
				if(a == VM_DATA_FALSE)	pc = tmp;
				break;
			case CODE_PLUS:
				a = vm_plus(argp, f);
				break;
			case CODE_MINUS:
				a = vm_minus(argp, f);
				break;
			case CODE_MUL:
				a = vm_mul(argp, f);
				break;
			case CODE_DIV:
				a = vm_div(argp, f);
				break;
			case CODE_MODULO:
				a = vm_modulo(argp, f);
				break;
			case CODE_GT:
				a = (INDEX(s,0)>>2) > (INDEX(s,1)>>2)
					? VM_DATA_TRUE : VM_DATA_FALSE;
				break;
			case CODE_LT:
				a = (INDEX(s,0)>>2) < (INDEX(s,1)>>2)
					? VM_DATA_TRUE : VM_DATA_FALSE;
				break;
			case CODE_EQUAL:
				a = INDEX(s,0) == INDEX(s,1) ? VM_DATA_TRUE : VM_DATA_FALSE;
				break;
			case CODE_DISPLAY:
				write_vm_data(a);
				break;
			case CODE_DISASM:
				dump_code(rom_last_address+1);
				printf("\n");
				break;
			case CODE_NEWLINE:
				printf("\n");
				break;
			case CODE_CONS:
				a = gc_alloc_pair();

				CAR(a) = INDEX(s,0);
				CDR(a) = INDEX(s,1);
				break;
			case CODE_CAR:
				a = CAR(INDEX(s,0));
				break;
			case CODE_CDR:
				a = CDR(INDEX(s,0));
				break;
			case CODE_IS_NULL:
				a = INDEX(s,0) == VM_DATA_NIL ? VM_DATA_TRUE : VM_DATA_FALSE;
				break;
			case CODE_ASSIGN_LOCAL:
				tmp  = code[pc++] >> 2;	/* n		*/
				setbox(INDEX(argp, tmp), a);
				break;
			case CODE_ASSIGN_FREE:
				tmp  = code[pc++] >> 2;	/* n		*/
				setbox(CLOSURE_INDEX(c,tmp), a);
				break;
			case CODE_ASSIGN_GLOBAL:
			case CODE_DEFINE:
				tmp  = code[pc++];	/* n		*/
				assign_global(tmp-3, a);
				break;
			case CODE_CONTI:
				a = gc_alloc_closure(0,rom_last_address+1,rom_last_address+6,0);
				insert_continuation_code(s);
				break;
			case CODE_NUATE:
				tmp  = code[pc++];	/* stack	*/
				s = f = restore_stack(tmp);
				break;
			case CODE_FRAME:
				s = PUSH(s, VM_DATA_END_OF_FRAME);
				s = PUSH(s, code[pc++]);
				s = PUSH(s, c);
				s = PUSH(s, argp << 2);
				s = PUSH(s, f << 2);
				break;
			case CODE_ARGUMENT:
				s = PUSH(s, a);
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
				//s	= s-(code[pc++] >> 2);
				pc++;
				s	= f;
				pc 	= INDEX(s,3) >> 2;
				c 	= INDEX(s,2);
				argp 	= INDEX(s,1) >> 2;
				f 	= INDEX(s,0) >> 2;
				s 	= s-5;
				break;
		}
	}
}

void
init_code()
{
	code[HEAP_CODE_BASE +  0] = CODE_EQUAL;
	code[HEAP_CODE_BASE +  1] = CODE_RETURN;
	code[HEAP_CODE_BASE +  2] = 2<<2;
	code[HEAP_CODE_BASE +  3] = CODE_MINUS;
	code[HEAP_CODE_BASE +  4] = CODE_RETURN;
	code[HEAP_CODE_BASE +  5] = 2<<2;
	code[HEAP_CODE_BASE +  6] = CODE_PLUS;
	code[HEAP_CODE_BASE +  7] = CODE_RETURN;
	code[HEAP_CODE_BASE +  8] = 2<<2;
	code[HEAP_CODE_BASE +  9] = CODE_GT;
	code[HEAP_CODE_BASE + 10] = CODE_RETURN;
	code[HEAP_CODE_BASE + 11] = 2<<2;
	code[HEAP_CODE_BASE + 12] = CODE_LT;
	code[HEAP_CODE_BASE + 13] = CODE_RETURN;
	code[HEAP_CODE_BASE + 14] = 2<<2;
	code[HEAP_CODE_BASE + 15] = CODE_CONS;
	code[HEAP_CODE_BASE + 16] = CODE_RETURN;
	code[HEAP_CODE_BASE + 17] = 2<<2;
	code[HEAP_CODE_BASE + 18] = CODE_CAR;
	code[HEAP_CODE_BASE + 19] = CODE_RETURN;
	code[HEAP_CODE_BASE + 20] = 1<<2;
	code[HEAP_CODE_BASE + 21] = CODE_CDR;
	code[HEAP_CODE_BASE + 22] = CODE_RETURN;
	code[HEAP_CODE_BASE + 23] = 1<<2;
	code[HEAP_CODE_BASE + 24] = CODE_IS_NULL;
	code[HEAP_CODE_BASE + 25] = CODE_RETURN;
	code[HEAP_CODE_BASE + 26] = 1<<2;
	code[HEAP_CODE_BASE + 27] = CODE_MUL;
	code[HEAP_CODE_BASE + 28] = CODE_RETURN;
	code[HEAP_CODE_BASE + 29] = 2<<2;
	code[HEAP_CODE_BASE + 30] = CODE_DIV;
	code[HEAP_CODE_BASE + 31] = CODE_RETURN;
	code[HEAP_CODE_BASE + 32] = 1<<2;
	code[HEAP_CODE_BASE + 33] = CODE_MODULO;
	code[HEAP_CODE_BASE + 34] = CODE_RETURN;
	code[HEAP_CODE_BASE + 35] = 2<<2;

	heap_last_address = HEAP_CODE_BASE + 35;
}


/*
 * insert initial data
 */
void
ht_init(struct hashtable *table)
{
	ht_insert(table, "x",        123<<2);
	ht_insert(table, "y",        256<<2);
	ht_insert(table, "=",        gc_alloc_closure(0, HEAP_CODE_BASE +  0, HEAP_CODE_BASE +  2,0));
	ht_insert(table, "-",        gc_alloc_closure(0, HEAP_CODE_BASE +  3, HEAP_CODE_BASE +  5,0));
	ht_insert(table, "+",        gc_alloc_closure(0, HEAP_CODE_BASE +  6, HEAP_CODE_BASE +  8,0));
	ht_insert(table, ">",        gc_alloc_closure(0, HEAP_CODE_BASE +  9, HEAP_CODE_BASE + 11,0));
	ht_insert(table, "<",        gc_alloc_closure(0, HEAP_CODE_BASE + 12, HEAP_CODE_BASE + 14,0));
	ht_insert(table, "cons",     gc_alloc_closure(0, HEAP_CODE_BASE + 15, HEAP_CODE_BASE + 17,0));
	ht_insert(table, "car",      gc_alloc_closure(0, HEAP_CODE_BASE + 18, HEAP_CODE_BASE + 20,0));
	ht_insert(table, "cdr",      gc_alloc_closure(0, HEAP_CODE_BASE + 21, HEAP_CODE_BASE + 23,0));
	ht_insert(table, "null?",    gc_alloc_closure(0, HEAP_CODE_BASE + 24, HEAP_CODE_BASE + 26,0));
	ht_insert(table, "*",        gc_alloc_closure(0, HEAP_CODE_BASE + 27, HEAP_CODE_BASE + 29,0));
	ht_insert(table, "/",        gc_alloc_closure(0, HEAP_CODE_BASE + 30, HEAP_CODE_BASE + 32,0));
	ht_insert(table, "modulo",   gc_alloc_closure(0, HEAP_CODE_BASE + 33, HEAP_CODE_BASE + 35,0));
}

void
dump_info()
{
	printf("=== INFO ===\n");
	printf("HOST_BIT:       %7d\n",host_bit);
	printf("CODE_MAX:       %7d\n",CODE_MAX);
	printf("HEAP_CODE_BASE: %7d\n",HEAP_CODE_BASE);
	printf("STACK_MAX:      %7d\n",STACK_MAX);
}

#ifndef GC_MAIN
int
main(int argc, char **argv)
{
	vm_data rc;

	host_bit = (sizeof(intptr_t) == 4) ? 32 : 64;

	gc_init();
	dump_info();

	global_table = ht_create();
	ht_init(global_table);
	ht_dump(global_table);

	init_code();

	while(get_code()){
		//dump_code(10);
		rc = exec_code();
		//printf("=== result ===\n");
		write_vm_data(rc);
		printf("\n");
		//dump_stack(10);
	}

	ht_destory(global_table);

	return 0;
}
#endif
