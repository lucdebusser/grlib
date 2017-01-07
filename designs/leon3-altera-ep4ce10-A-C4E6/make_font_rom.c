#include "stdio.h"
#include "stdlib.h"
#include "string.h"

void main( void )
{
FILE* fp = fopen("font_rom.vhd", "r");
char* r;
char s[4096];
unsigned char byte;
int i;
int address = 0;
	printf("DEPTH = 2048;\r\n");
	printf("WIDTH = 8;\r\n");
	printf("ADDRESS_RADIX = HEX;\r\n");
	printf("DATA_RADIX = BIN;\r\n");
	printf("CONTENT\r\n");
	printf("\r\n");
	printf("BEGIN\r\n");



	do {
		memset( s, 4096, 0 );
		r = fgets(s, 4095, fp);
		if (r)
		{
			if ( s[3]=='\"' )
			{
				printf("%02X : ",address++);
				for (i=0;i<8;i++)
				{
					printf( "%c", s[4+i] );
				}
				printf(";\r\n");
			}

		}
	} while (r);
	
	
	fclose( fp );

	printf("\r\nEND;\r\n");
}
