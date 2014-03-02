CC  					:= gcc
CFLAGS     		:= -g -w
TARGET				:= vm/myscmvm
COMPILER 			:= compiler/main.scm

SOURCES_C     := vm/vm.c vm/hashtable.c
HEADER				:= vm/common.h

TESTS					:= $(wildcard test/*.scm)

$(TARGET): $(SOURCES_C) $(HEADER)
	$(CC) $(CFLAGS) -o $(TARGET) $(SOURCES_C)

clean:
	rm -f $(TARGET)

test: $(TARGET) $(TESTS)
	@for test in $(TESTS) ; do \
		(cat $$test | $(COMPILER) | ./$(TARGET) ) ;\
		done

run: $(TARGET)
	$(COMPILER) | $(TARGET)
