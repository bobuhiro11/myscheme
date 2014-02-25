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

#define CODE_MAX 2048
#define STACK_MAX 1024

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
#define CODE_GT 	      	0xFF000018
#define CODE_LT		      	0xFF000019
#define CODE_CONS	      	0xFF00001A
#define CODE_CAR	      	0xFF00001B
#define CODE_CDR	      	0xFF00001C
#define CODE_IS_NULL	      	0xFF00001D
#define CODE_INVALID 		0xFFFFFFFF
#define CODE_TRUE 		0x00000001
#define CODE_FALSE 		0x00000009
#define CODE_NIL 		0x0000000d

#define VM_DATA_TRUE		B(00101)	/* for vm_data */
#define VM_DATA_FALSE		B(01001)
#define VM_DATA_NIL		B(01101)
#define VM_DATA_EOF		B(10001)
#define VM_DATA_UNDEFINED	B(10101)
#define VM_DATA_UNBOUND		B(11001)
#define VM_DATA_END_OF_FRAME	B(11101)

#define VM_OBJ_STR		0x01 		/* for tag of struct vm_obj */
#define VM_OBJ_CLOSURE		0x02
#define VM_OBJ_STACK		0x03
#define VM_OBJ_PAIR		0x04

#define HASHTABLE_SIZE 		101
#define KEYWORD_BUFLEN 		256

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

#define CLOSURE_BODY(x)		(((struct vm_obj*)((x)-3)) -> u.closure[0] >> 2)
#define CLOSURE_EBODY(x)	(((struct vm_obj*)((x)-3)) -> u.closure[1] >> 2)
#define CLOSURE_INDEX(x,n)	(((struct vm_obj*)((x)-3)) -> u.closure[(n) + 2])

#define CAR(x)			(((struct vm_obj*)((x)-3)) -> u.pair.car)
#define CDR(x)			(((struct vm_obj*)((x)-3)) -> u.pair.cdr)

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
		char *str;
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
