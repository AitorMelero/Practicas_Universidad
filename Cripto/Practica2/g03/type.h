/**************************************************************
 * File: type.h
 * Author: Aitor Melero, Ana Roa
 * Date: 25/09/2020
 * Last_Date: 18/10/2020
 * Function: Definicion de tipos.
 * ***********************************************************/

#ifndef TYPE_H
#define	TYPE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <gmp.h>
#include "DES_tables.c"
#include "AES_tables.c"
#include "operaciones_bit.h"
#include "DES.h"
#include "CBC.h"
#include "TDEA.h"


typedef enum {FALSE = 0 , TRUE = 1} BOOL;
typedef enum {ERROR = -1, OK = 0} STATUS;



#endif	/* TYPE_H */
