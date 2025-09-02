#include "python_grasp_static.h"
#include "yamlsettings/yamlsettings.h"

#define PyCHECK(returnVals...) if (PyErr_Occurred()) { PyErr_Print(); return returnVals; }

input_driver_init_callback input_driver_init_callback_func = NULL;
input_driver_process_segment_callback input_driver_process_segment_callback_func = NULL;
input_driver_finalize_callback input_driver_finalize_callback_func = NULL;

void register_input_driver_init_callback(input_driver_init_callback callback) {
    input_driver_init_callback_func = callback;
}

void register_input_driver_process_segment_callback(input_driver_process_segment_callback callback) {
    input_driver_process_segment_callback_func = callback;
}

void register_input_driver_finalize_callback(input_driver_finalize_callback callback) {
    input_driver_finalize_callback_func = callback;
}

void initialize_py_interpreter() {
    if(input_driver_init_callback_func != NULL || input_driver_process_segment_callback_func != NULL) {
        // if the callbacks are already set then we were invoked from an already
        // existing python interpreter via ctypes - no need to initialize
        return;
    }
    if(!Py_IsInitialized()) {
        Py_Initialize();
    }
}

void add_python_module_path(int index, char*path) {
    /**
     * @brief Insert the given path to sys.path of the current python interpreter
     * at the given position
     */
    initialize_py_interpreter();
    PyObject* sysPath = PySys_GetObject("path");
    PyObject *tmp = PyObject_CallMethod(sysPath, "insert", "i s", index, path);
    Py_XDECREF(tmp);
    PyCHECK();
}

void python_callback_init(grasp_settings *settings, grasp_tile_description_t *input_information, char* py_module) {
    yamlsettings_dictionary_t *dictionary;
    char* settings_buffer = NULL;
    size_t settings_buffer_size = 0;
    FILE* settings_stream = open_memstream(&settings_buffer, &settings_buffer_size);
    dictionary=grasp_settings_dictionary_get(settings);
    yamlsettings_debug_dictionary(settings_stream, dictionary, "");
    fclose(settings_stream); // flush contents from stream to settings_buffer

    if(input_driver_init_callback_func != NULL) {
        // called from existing python session
        input_driver_init_callback_func(settings_buffer, (size_t)input_information, sizeof(grasp_tile_description_t), py_module);
    } else {
        // called from the command line (fresh GRASP process - no python exists yet)
        PyObject* initFunc = get_python_function("grasp.code.clib.grasp_callback", "grasp_input_driver_init");
        PyCHECK();
        PyObject *pCall = PyObject_CallFunction(initFunc, "s l l s", settings_buffer, (size_t)input_information, sizeof(grasp_tile_description_t), py_module);
        PyCHECK();

        Py_XDECREF(initFunc);
        Py_XDECREF(pCall);
        PyCHECK();
    }

    // cleanup:
    free(settings_buffer);
}

void python_callback_process_segment(char* py_module, grasp_segment_t* segment, int col, int row, int itime) {
    if (input_driver_process_segment_callback_func != NULL) {
        // called from existing python session
        input_driver_process_segment_callback_func(py_module, (size_t)segment, sizeof(grasp_segment_t), col, row, itime);
    } else {
        // called from the command line (fresh GRASP process - no python exists yet)
        PyObject* initFunc = get_python_function("grasp.code.clib.grasp_callback", "grasp_input_driver_process_segment");
        PyObject *pCall = PyObject_CallFunction(initFunc, "s l l i i i", py_module, (size_t)segment, sizeof(grasp_segment_t), col, row, itime);
        PyCHECK();

        Py_XDECREF(initFunc);
        Py_XDECREF(pCall);
        PyCHECK();
    }
}

void python_callback_finalize(char* py_module) {
    if (input_driver_finalize_callback_func != NULL) {
        // called from existing python session
        input_driver_finalize_callback_func(py_module);
    } else {
        // called from the command line (fresh GRASP process - no python exists yet)
        PyObject* initFunc = get_python_function("grasp.code.clib.grasp_callback", "grasp_input_driver_finalize");
        PyObject *pCall = PyObject_CallFunction(initFunc, "s", py_module);
        PyCHECK();

        Py_XDECREF(initFunc);
        Py_XDECREF(pCall);
        PyCHECK();
    }
}

PyObject* get_python_function(char* py_module, char* function) {
    initialize_py_interpreter();
    PyObject *pModule = PyImport_ImportModule(py_module);
    PyCHECK(Py_None);
    PyObject *pFunc = PyObject_GetAttrString(pModule, function);
    PyCHECK(Py_None);
    Py_XDECREF(pModule);
    return pFunc;
}