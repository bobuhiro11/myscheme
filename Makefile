CC  					:= gcc
CFLAGS     		:= -g -w
TARGET				:= myscmvm

SOURCES_C     := vm.c hashtable.c
HEADER				:= common.h

TESTS					:= $(wildcard test/*.scm)

$(TARGET): $(SOURCES_C) $(HEADER)
	$(CC) $(CFLAGS) -o $(TARGET) $(SOURCES_C)

clean:
	rm -f $(TARGET)

test: $(TARGET) $(TESTS)
	@for test in $(TESTS) ; do \
		(cat $$test | ./main.scm | ./$(TARGET) ) ;\
		done
