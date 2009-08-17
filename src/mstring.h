#if !defined(MSTRING_H)
#define MSTRING_H

#include <stdlib.h>
#include "general.h"
#include "error.h"

class mstring {
	char *value ;

public:
	mstring() {value = 0 ;}
	mstring(char* val) { copy(val);	}
	mstring(mstring &val) {copy(val) ; }
    ~mstring(){ destroy() ;  }
    void destroy() ;
    int len() ;
    void copy(const char *val);
    void copy(mstring &val) ;
    void copyFrom(mstring &val, int fromIdx) ;
    mstring& operator=(mstring &Source) ;
    mstring& operator=(char *Source) ;
    char *getValue() {	return value ;  }
    bool isDefined() { 	return (value != 0);  }
    int compareTo(mstring &Source) ;
    void trimWhite();
};

#endif
