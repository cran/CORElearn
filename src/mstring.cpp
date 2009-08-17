#include <stdlib.h>
#include <string.h>
#include "general.h"
#include "error.h"
#include "utils.h"
#include "mstring.h"


    void mstring::destroy () {
    	if (value) {
    		delete [] value ;
    		value = 0 ;
    	}
    }

    int mstring::len() {
    	if (value)
    		return (int)strlen(value) ;
    	else return 0 ;
    }
    void mstring::copy(const char *val) {
       destroy() ;
       strcpy(value=new char[strlen(val)+1], val) ;
    }
    void mstring::copy(mstring &val) {
       destroy() ;
   	   strcpy(value=new char[strlen(val.value)+1], val.value) ;
    }
    // copies everything from fromIdx forward
    void mstring::copyFrom(mstring &val, int fromIdx) {
       destroy() ;
       if (fromIdx < val.len())
   	     strcpy(value=new char[strlen(val.value)+1-fromIdx], val.value+fromIdx) ;
    }

    mstring& mstring::operator=(mstring &Source)
    {
       copy(Source) ;
       return *this ;
    }
    mstring& mstring::operator=(char *Source)
    {
       copy(Source) ;
       return *this ;
    }
    int mstring::compareTo(mstring &Source) {
    	return strcmp(value, Source.value);
    }
    void mstring::trimWhite(){
    	::trimWhite(this->value);
    }
