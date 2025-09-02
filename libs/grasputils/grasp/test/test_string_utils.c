#include <stdio.h>
#include <stdlib.h>
#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>
#include <CUnit/Console.h>
#include <CUnit/Automated.h>
#include <time.h>
#include "../utils/string_utils.h"

/*
 * CUnit Test Suite
 */

int init_suite(void) {
    return 0;
}

int clean_suite(void) {
    return 0;
}

void test_strtolower(){
    CU_ASSERT(strcmp(strtolower("AsD"),"asd")==0);
    CU_ASSERT(strcmp(strtolower("asd"),"asd")==0);
    CU_ASSERT(strcmp(strtolower("ASD"),"asd")==0);
}

void test_chunkify(){
    int c;
    char ** test;
    
    test=chunkify( "test1", "", &c );
    
    CU_ASSERT(strcmp(test[0],"test1")==0);
    CU_ASSERT(strcmp(test[0],"test")!=0);
    CU_ASSERT(c==1);
    
    test=chunkify( "test2", " ", &c );
    CU_ASSERT(strcmp(test[0],"test2")==0);
    CU_ASSERT(strcmp(test[0],"test")!=0);
    CU_ASSERT(c==1);
    
    test=chunkify( "test3 test4", " ", &c );
    CU_ASSERT(c==2);
    CU_ASSERT(strcmp(test[0],"test3")==0);
    CU_ASSERT(strcmp(test[1],"test4")==0);
    
    test=chunkify( "", " ", &c );
    CU_ASSERT(c==0);
}



int main() {
    CU_pSuite frameworkSuite = NULL;

    /* Initialize the CUnit test registry */
    if (CUE_SUCCESS != CU_initialize_registry())
        return CU_get_error();

    /* Add a suite to the registry */
    
    frameworkSuite = CU_add_suite("time_utils_tests", init_suite, clean_suite);
    if (NULL == frameworkSuite) {
        CU_cleanup_registry();
        return CU_get_error();
    }   
    
  
    /* Add the tests to the suite */
    if (
        (NULL == CU_add_test(frameworkSuite   , "test_strtolower"       , test_strtolower   ))
        || (NULL == CU_add_test(frameworkSuite   , "test_chunkify"       , test_chunkify   ))
       ) {
        CU_cleanup_registry();
        return CU_get_error();
    }
  
    /* Run all tests using the CUnit Basic interface */
    //CU_basic_set_mode(CU_BRM_VERBOSE);
    CU_basic_run_tests();
    CU_cleanup_registry();
    return CU_get_error();
}
    
