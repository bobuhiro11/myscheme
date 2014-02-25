CC  					:= gcc
CFLAGS     		:= -g -w
TARGET				:= myscmvm

SOURCES_C     := vm.c hashtable.c

$(TARGET): $(SOURCES_C)
	$(CC) $(CFLAGS) -o $(TARGET) $(SOURCES_C)

clean:
	rm -f $(TARGET)
