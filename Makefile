consult-filter: consult-filter.c Makefile
	gcc -std=c11 -D_POSIX_C_SOURCE=1 -O2 -Wall -Wextra -o $@ $<
