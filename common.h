#ifndef COMMON_H
#define COMMON_H

#include<stdio.h>
#include<stdint.h>
#include<string.h>
#include<stdlib.h>

#define PP_HEX2BIN(b)	\
	(((b & 1 <<  0) >>  0) + ((b & 1 <<  3) >>  2) + ((b & 1 <<  6) >>  4) + \
	 ((b & 1 <<  9) >>  6) + ((b & 1 << 12) >>  8) + ((b & 1 << 15) >> 10) + \
	 ((b & 1 << 18) >> 12) + ((b & 1 << 21) >> 14))

#define B(x) PP_HEX2BIN(0 ## x)

#define CODE_MAX 	2048
#define HEAP_CODE_BASE 	(CODE_MAX * 2 / 3)
#define STACK_MAX 	1024

#define CODE_HALT 		0x00000002	/* for vm_code */
#define CODE_REFER_LOCAL 	0x01000002
#define CODE_REFER_FREE 	0x02000002
#define CODE_REFER_GLOBAL	0x03000002
#define CODE_INDIRECT    	0x04000002
#define CODE_CONSTANT    	0x05000002
#define CODE_CLOSE       	0x06000002
#define CODE_BOX         	0x07000002
#define CODE_TEST        	0x08000002
#define CODE_PLUS        	0x09000002
#define CODE_MINUS       	0x0a000002
#define CODE_EQUAL       	0x0b000002
#define CODE_ASSIGN_LOCAL	0x0c000002
#define CODE_ASSIGN_FREE 	0x0d000002
#define CODE_ASSIGN_GLOBAL	0x0e000002
#define CODE_DEFINE      	0x0f000002
#define CODE_CONTI       	0x10000002
#define CODE_NUATE       	0x11000002
#define CODE_FRAME       	0x12000002
#define CODE_ARGUMENT    	0x13000002
#define CODE_SHIFT       	0x14000002
#define CODE_APPLY       	0x15000002
#define CODE_RETURN      	0x16000002
#define CODE_GT 	      	0x17000002
#define CODE_LT 		0x18000002
#define CODE_CONS	      	0x19000002
#define CODE_CAR	      	0x1a000002
#define CODE_CDR	      	0x1b000002
#define CODE_IS_NULL	      	0x1c000002
#define CODE_DISPLAY	      	0x1d000002
#define CODE_NEWLINE	      	0x1e000002
#define CODE_MUL        	0x1f000002
#define CODE_DIV       		0x20000002
#define CODE_MODULO    		0x26000002
#define CODE_INVALID 		0xFF000002

#define CODE_CONSTNIL       	0x21000002
#define CODE_CONSTBOO       	0x22000002
#define CODE_CONSTSYM       	0x23000002
#define CODE_CONSTSTR       	0x24000002
#define CODE_CONSTNUM       	0x25000002

#define CODE_TRUE 		B(0001)
#define CODE_FALSE 		B(1001)
#define CODE_NIL 		B(1101)

#define VM_DATA_TRUE		B(00101)	/* for vm_data */
#define VM_DATA_FALSE		B(01001)
#define VM_DATA_NIL		B(01101)
#define VM_DATA_EOF		B(10001)
#define VM_DATA_UNDEFINED	B(10101)
#define VM_DATA_UNBOUND		B(11001)
#define VM_DATA_END_OF_FRAME	B(11101)

#define VM_OBJ_STRING		0x01 		/* for tag of struct vm_obj */
#define VM_OBJ_CLOSURE		0x02
#define VM_OBJ_STACK		0x03
#define VM_OBJ_PAIR		0x04
#define VM_OBJ_SYMBOL		0x05

#define HASHTABLE_SIZE 		101
#define KEYWORD_BUFLEN 		256

#define IS_CODE_NUMBER(x)	((x & 3) == 0)
#define IS_CODE_TRUE(x)		((x & 0xF) == 1)
#define IS_CODE_FALSE(x)	((x & 0xF) == 9)
#define IS_CODE_NIL(x)		((x & 0xF) == 13)
#define IS_CODE_STRING(x)	((x & 0x3) == 3)
#define IS_CODE_CODE(x)		((x & 3) == 2)

#define IS_NUM(x) 		((x & 3) == 0)
#define IS_TRUE(x) 		((x - VM_DATA_TRUE) == 0)
#define IS_FALSE(x) 		((x - VM_DATA_FALSE) == 0)
#define IS_NIL(x) 		((x - VM_DATA_NIL) == 0)
#define IS_UNDEFINED(x) 	((x - VM_DATA_UNDEFINED) == 0)
#define IS_END_OF_FRAME(x) 	((x - VM_DATA_END_OF_FRAME) == 0)
#define IS_OBJ(x) 		(((x)&3)==3)
#define IS_BOX(x) 		(((x)&3)==2)
#define IS_CLOSURE(x) 		(((x)&3)==3&&(((struct vm_obj*)(x-3))->tag)== VM_OBJ_CLOSURE)
#define IS_STACK(x) 		(((x)&3)==3&&(((struct vm_obj*)(x-3))->tag)== VM_OBJ_STACK)
#define IS_PAIR(x) 		(((x)&3)==3&&(((struct vm_obj*)(x-3))->tag)== VM_OBJ_PAIR)
#define IS_STRING(x) 		(((x)&3)==3&&(((struct vm_obj*)(x-3))->tag)== VM_OBJ_STRING)
#define IS_SYMBOL(x) 		(((x)&3)==3&&(((struct vm_obj*)(x-3))->tag)== VM_OBJ_SYMBOL)

#define CLOSURE_BODY(x)		(((struct vm_obj*)((x)-3)) -> u.closure[0] >> 2)
#define CLOSURE_EBODY(x)	(((struct vm_obj*)((x)-3)) -> u.closure[1] >> 2)
#define CLOSURE_INDEX(x,n)	(((struct vm_obj*)((x)-3)) -> u.closure[(n) + 2])

#define CAR(x)			(((struct vm_obj*)((x)-3)) -> u.pair.car)
#define CDR(x)			(((struct vm_obj*)((x)-3)) -> u.pair.cdr)

#define STRING(x)		(((struct vm_obj*)((x)-3)) -> u.string)
#define SYMBOL(x)		(((struct vm_obj*)((x)-3)) -> u.symbol)

#define PUSH(s,x)		(stack[s] = x, (s)+1)
#define INDEX(s,n)		(stack[(s)-(n)-1])
#define SET_INDEX(s,n,x)	(stack[(s)-(n)-1] = x)

/***************************************************
 * structure definition
 ***************************************************/
typedef uint64_t vm_code;
typedef uint64_t vm_data;

struct vm_obj
{
	unsigned char tag;
	union{
		char *string;
		char *symbol;
		vm_data *closure;
		struct {
			vm_data *p;
			int size;
		} stack;
		struct {
			vm_data *car;
			vm_data *cdr;
		} pair;
	} u;
};

struct hashtable{
		char   key[KEYWORD_BUFLEN];
		vm_data data;
};

/***************************************************
 * function prototype definition
 ***************************************************/
vm_code get_vm_code(const char* s);
void get_code();
void dump_code(int max);
void write_vm_data(vm_data data);
void dump_stack(int max);
vm_data create_closure(uint32_t n, uint32_t bodyadr, uint32_t ebodyadr, int s);
vm_data exec_code();

vm_data ht_insert(struct hashtable *table, const char *key, vm_data data);
vm_data ht_find(const struct hashtable *table, const char *key);
void ht_dump(const struct hashtable *table);
struct hashtable* ht_create();
void ht_destory(struct hashtable *table);

/***************************************************
 * external variable definition
 ***************************************************/
extern vm_code code[CODE_MAX];
extern vm_data stack[STACK_MAX];

#endif
