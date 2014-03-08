CC  			  := gcc
CFLAGS     	:= -g -w
TARGET			:= vm/myscmvm
TARGET_GC		:= vm/myscmvm_gc
COMPILER		:= compiler/main.scm

SOURCES_C  	:= vm/vm.c vm/hashtable.c vm/gc.c
HEADER			:= vm/common.h
TESTS			  := $(wildcard test/*.scm)

$(TARGET): $(SOURCES_C) $(HEADER)
	$(CC) $(CFLAGS) -o $(TARGET) $(SOURCES_C)

$(TARGET_GC): $(SOURCES_C) $(HEADER)
	$(CC) $(CFLAGS) -DGC_MAIN -o $(TARGET_GC) $(SOURCES_C)

clean:
	rm -f $(TARGET)

test: $(TARGET) $(TESTS)
	@for test in $(TESTS) ; do \
		(cat $$test | $(COMPILER) | ./$(TARGET) ) ;\
		done

run: $(TARGET)
	$(COMPILER) | $(TARGET)

gc: $(TARGET_GC)
	$(TARGET_GC)
