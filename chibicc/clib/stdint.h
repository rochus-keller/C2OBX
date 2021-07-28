// added by rochus, just a subset of the GCC stdint.h

typedef signed char		int8_t;
typedef short int		int16_t;
typedef int			int32_t;
typedef long long int		int64_t;

typedef unsigned char		uint8_t;
typedef unsigned short int	uint16_t;
typedef unsigned int			uint32_t;
typedef unsigned long int	uint64_t;

typedef int			intptr_t;
typedef unsigned int		uintptr_t;


typedef long long int		intmax_t;
typedef unsigned long long int	uintmax_t;


/* Limits of integral types.  */

/* Minimum of signed integral types.  */
# define INT8_MIN		(-128)
# define INT16_MIN		(-32767-1)
# define INT32_MIN		(-2147483647-1)
# define INT64_MIN		(-__INT64_C(9223372036854775807)-1)
/* Maximum of signed integral types.  */
# define INT8_MAX		(127)
# define INT16_MAX		(32767)
# define INT32_MAX		(2147483647)
# define INT64_MAX		(__INT64_C(9223372036854775807))

/* Maximum of unsigned integral types.  */
# define UINT8_MAX		(255)
# define UINT16_MAX		(65535)
# define UINT32_MAX		(4294967295U)
# define UINT64_MAX		(18446744073709551615)

