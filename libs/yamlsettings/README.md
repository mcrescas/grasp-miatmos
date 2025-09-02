README
======



PREREQUISITES
-------------

To compile gcc and gfortran are required and also libyaml, GLib libraries and pkg-config. 


HOW TO USE THE LIBRARY
----------------------


Define a ditionary (explained below), a code example:

```
    typedef struct settings_values_ {
        bool debug;
    }settings_values;

    settings_values settings;

    // Static definition of a dictionary
    yamlsettings_parameter parameters[]= {
         /*  name    , memory direction, counter memory direction   , func to set variable type (length of value), initial value        ,  number of elements , allow_array        , parameter description       ,  validators , {input function, output function, assigned}   */
        {"debug"  , &settings.debug   , NULL                        , YS_DATA_TYPE_BOOLEAN                       , {ys_d_all,{"false"}} , {0,{}}              , YS_PARAM_SCALAR    , "Print debug information "  , {   }       , YAMLSETTINGS_END_VAR  },                  
    };
    grasp_settings_parameter_array *dictionary;
    
    dictionary = (grasp_settings_parameter_array *) trackmem_malloc(sizeof (grasp_settings_parameter_array));
    assert(dictionary!=NULL);
    
    dictionary->nparameters=sizeof(parameters)/sizeof(yamlsettings_parameter);
    
    dictionary->parameters = (yamlsettings_parameter *) trackmem_malloc(sizeof (yamlsettings_parameter)*dictionary->nparameters);
    assert(dictionary->parameters!=NULL);
    
    for(i=0;i<sizeof(parameters)/sizeof(yamlsettings_parameter);i++){
       yamlsettings_copy_parameter(&(parameters[i]),&(dictionary->parameters[i]));
    }
```
 
Then, read the settings

```
    // first element of argc is the name of the settings file, rest are command line arguments
    // Lar argument is the mode of lecture. In this example since argv[0] is the name of the settings file we pass MANDATORY and the library will use argv[0] to open the file
    // It can be FORBIDDEN (there is not settingsd file, it is only command line arguments) or OPTIONAL in case both are allowed (It can be a bug if the settings file has the same name than a parameter).
    // finally, it can be YS_PARSE_SETTINGS_INLINE which means that first argument is the string with the content of the settings file. In this special case the BASEPATH will be
    // the place where the code is running (getcwd) except if the content starts by ! symbol. In that case, from argv[0] will be extracted the path between ! and ; and then the rest will be the content of the
    // settings file. For example !/home/pepe/example.yml;debug: true\n"
    errors=yamlsettings_read_file(*dictionary, argc, argcv, /*pre_process_functions*/ NULL, /*post_process_functions*/ NULL, YS_PARSE_SETTINGS_FILE_MANDATORY);
```


HOW TO DEFINE A DICTIONARY
----------------------------

To fill grasp_settings_dictionary with one line more. To do that:

1.- Add the var in test_input.yml. It will be used later but beginning from this point helps you see the result
2.- Add var definition in grasp_settings.h and mo_grasp_settings.f90
3.- Add the paramter information in grasp_settings_dictionary_get (grasp_settings_dictionary.c). The description of each field is:
                                                                                                                                                                                                                                                                                                ,                 validator 1                 ,                   validator 2                         ,                    validator 3                               
       - name
       - memory direction  
       - counter memory direction
       - extra info mem dir
       - variable type 
       - initial value
       - number of elements
       - parameter description
       - validator: (you can define multiple validators)
            - validator function 
            - first parameter
            - second parameter
            - ...
If you don't need special methods to set and get your parameter you can use END_GRASP_VAR and to finish here without continue defining arguments.
       - input function: Function called to store a value. To a store a single var you can use default method. If you need something special (like store arrays) you can define new functions in grasp_settings_assing_data_custom.c  (You can read section: HOW TO ADD A NEW VALIDATOR)    
       - debug function: Function called to print parameter information. To a store a single var you can use default method. If you need something special (like store arrays) you can define new functions in grasp_settings_assing_data_custom.c (You can read section: HOW TO ADD A NEW VALIDATOR)
       - assigned: Always 0.
 

Note: Initial value has to have exactly same format that variable type will print. If not, dump function will not work properly.

Final comment: At this point all must be work and compile

4.- Add test in order to verify that the added parameter is readed sucessfully. Edit mo_grasp_settings.fun
    and an assert in test_reading. 


HOW TO ADD NEW VAR TYPE (outdated)
-----------------------
Usually when you want to add a parameter you will use a existing data type but, for exaple, if you
want to add new enumeration you must to define it. If you want to do this you must follow this steps:
1.- In file grasp_settings_dev.h add new element in the enumeration grasp_types (in grasp_setting_dev.h)
2.- In file grasp_settings_dev.h increase constant GRASP_NUMBER_OF_TYPES in grasp_settings_dev
3.- If the data type added is an enumeration you must defined in grasp_settings_data_type.c (editing grasp_settings_enumerations variable)
4.- If it is not an enumartion In file grasp_settings_data_types.c complete function grasp_settings_data_type_assign in order to know how to store it
5.- If it is not file grasp_settings_data_types.c complete function grasp_settings_data_type_print in order to know how transform the value to a string



HOW TO ADD A NEW VALIDATOR 
--------------------------
Validator is a function that it check the content of parameter and return 0 if it is ok.
If value is invalid return -1 and print an error.
1. Add the validator definition in grasp_settings_validators.h
2. Write validator in grasp_settings_validators.c
3. Add a test in libyamlinput_test.c

 

HOW TO WRITE SPECIAL FUNTION TO SAVE PARAMETER
----------------------------------------------
For define a new input function you must write a function that return a integer (0 if is well processed) 
and this function must be like arguments: grasp_settings_parameter *param,GNode *node,grasp_settings *settings. 
Then you code how transform this node and store in de mem_pos of this param.
In order to be able to debug a dictionary every parameter will be printable. To do that, write a debug function
that return a string with name and values of the new parameter. 



