
CC = clang 
CFLAGS = -Wall -fPIC
TARGET = libsimple_http.so
SRC = simple_http.c

all: $(TARGET)

$(TARGET): $(SRC)
	$(CC) $(CFLAGS) -shared -o $@ $^

clean:
	rm -f $(TARGET)

.PHONY: all clean
